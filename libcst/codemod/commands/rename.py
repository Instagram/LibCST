# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
import argparse
from typing import Dict, Optional, Sequence, Union

import libcst as cst
from libcst.codemod import CodemodContext, VisitorBasedCodemodCommand
from libcst.codemod.visitors import AddImportsVisitor, RemoveImportsVisitor
from libcst.helpers import get_full_name_for_node
from libcst.metadata import QualifiedNameProvider


class RenameCommand(VisitorBasedCodemodCommand):
    """
    Rename all instances of a local or imported object.
    """

    DESCRIPTION: str = "Rename all instances of a local or imported object."

    METADATA_DEPENDENCIES = (QualifiedNameProvider,)

    @staticmethod
    def add_args(parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--old_name",
            dest="old_name",
            required=True,
            help="Full dotted name of object to rename. Eg: `foo.bar.baz`",
        )

        parser.add_argument(
            "--new_name",
            dest="new_name",
            required=True,
            help="Full dotted name of replacement object. Eg: `foo.bar.baz`",
        )

    def __init__(self, context: CodemodContext, old_name: str, new_name: str) -> None:
        super().__init__(context)
        self.old_name = old_name
        self.new_name = new_name

        old_names = self.old_name.rpartition(".")
        new_names = self.new_name.rpartition(".")

        # Mapping for easy lookup later.
        self.equivalents: Dict[str, str] = {
            old_names[2]: new_names[2],
            "".join(old_names[:2]): new_names[0],
        }

        self.scheduled_imports = False

    def visit_Import(self, node: cst.Import) -> None:
        for import_alias in node.names:
            alias_name = get_full_name_for_node(import_alias.name)
            if alias_name is not None:
                self.record_asname(import_alias, alias_name)

                # If the import statement is exactly equivalent to the old name, replace it.
                if alias_name == self.old_name:
                    self.cleanup_imports(node=node, new_module=self.new_name)

    def leave_ImportFrom(
        self, original_node: cst.ImportFrom, updated_node: cst.ImportFrom
    ) -> cst.ImportFrom:
        module = updated_node.module
        if module is not None:
            imported_module_name = get_full_name_for_node(module)
            names = original_node.names

            if imported_module_name is None or not isinstance(names, Sequence):
                return updated_node
            elif imported_module_name == self.old_name:
                # Simply rename the imported module on the spot.
                return updated_node.with_changes(
                    module=cst.parse_expression(self.new_name)
                )
            else:
                new_names = []
                for import_alias in names:
                    alias_name = get_full_name_for_node(import_alias.name)
                    if alias_name is not None:
                        qual_name = f"{imported_module_name}.{alias_name}"
                        if self.old_name == qual_name:
                            self.record_asname(import_alias, alias_name)

                            replacement_module = self.gen_replacement(
                                imported_module_name + "."
                            )
                            replacement_obj = self.gen_replacement(alias_name)
                            new_import_alias_name: cst.BaseExpression = cst.parse_expression(
                                replacement_obj
                            )
                            if not isinstance(
                                new_import_alias_name, (cst.Attribute, cst.Name)
                            ):
                                raise Exception("Something went wrong!")
                            # Rename on the spot only if this is the only imported name under the module.
                            if len(names) == 1:
                                return updated_node.with_changes(
                                    module=cst.parse_expression(replacement_module),
                                    names=(
                                        cst.ImportAlias(name=new_import_alias_name),
                                    ),
                                )
                            # Or if the module name is to stay the same.
                            elif replacement_module == imported_module_name:
                                new_names.append(
                                    cst.ImportAlias(name=new_import_alias_name)
                                )
                            else:
                                # Otherwise, discard the import and schedule a new import.
                                self.cleanup_imports(
                                    new_module=replacement_module,
                                    new_obj=replacement_obj,
                                )
                        else:
                            new_names.append(import_alias)

                return updated_node.with_changes(names=new_names)
        return updated_node

    def leave_Name(self, original_node: cst.Name, updated_node: cst.Name) -> cst.Name:
        if QualifiedNameProvider.has_name(self, original_node, self.old_name):
            replacement_name: str = self.gen_replacement(original_node.value)
            return updated_node.with_changes(value=replacement_name)

        return updated_node

    def leave_Attribute(
        self, original_node: cst.Attribute, updated_node: cst.Attribute
    ) -> Union[cst.Name, cst.Attribute]:
        if (
            QualifiedNameProvider.has_name(self, original_node, self.old_name,)
            or get_full_name_for_node(original_node) == self.old_name
        ):
            new_value, _, new_attr = self.new_name.rpartition(".")

            self.cleanup_imports(node=original_node.value, new_module=new_value)
            return updated_node.with_changes(
                value=cst.parse_expression(new_value), attr=cst.Name(value=new_attr)
            )

        return updated_node

    def cleanup_imports(
        self,
        node: Optional[cst.CSTNode] = None,
        new_module: Optional[str] = None,
        new_obj: Optional[str] = None,
    ) -> None:
        if not self.scheduled_imports and any([node, new_module]):
            if node is not None:
                RemoveImportsVisitor.remove_unused_import_by_node(self.context, node)
            if new_module is not None and new_obj is not None:
                AddImportsVisitor.add_needed_import(
                    self.context, module=new_module, obj=new_obj
                )
            elif new_module is not None:
                AddImportsVisitor.add_needed_import(self.context, module=new_module)
            self.scheduled_imports = True

    def gen_replacement(self, original_name: str) -> str:
        index = original_name.rfind(".")
        parts = (
            (original_name[: index + 1], original_name[index + 1 :])
            if index > -1
            else (original_name,)
        )
        replacement_parts = []

        for part in parts:
            if part in self.equivalents:
                replacement_parts.append(self.equivalents[part])
                replacement_parts.append(".")
            elif part + "." in self.equivalents:
                replacement_parts.append(self.equivalents[part + "."])
                replacement_parts.append(".")

        return "".join(replacement_parts).rstrip(".")

    def record_asname(self, import_alias: cst.ImportAlias, alias_name: str) -> None:
        # Record the import's `as` name if it has one, and map it to the equivalent new name.
        as_name = import_alias.asname
        name = as_name.name if as_name is not None else None
        if name is not None and isinstance(name, cst.Name):
            self.equivalents[name.value + "."] = self.gen_replacement(alias_name)
