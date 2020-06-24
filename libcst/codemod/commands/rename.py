# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
import argparse
from typing import Sequence, Set, Union

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

        old_module, _, old_mod_or_obj = self.old_name.rpartition(".")
        new_module, _, new_mod_or_obj = self.new_name.rpartition(".")

        self.old_module: str = old_module
        self.new_module: str = new_module
        self.old_mod_or_obj: str = old_mod_or_obj
        self.new_mod_or_obj: str = new_mod_or_obj

        self.scheduled_removals: Set[cst.CSTNode] = set()
        self.bypass_imports = False
        self.schedule_import = False

    def visit_Import(self, node: cst.Import) -> None:
        for import_alias in node.names:
            alias_name = get_full_name_for_node(import_alias.name)
            if alias_name is not None:

                # If the import statement is exactly equivalent to the old name, it will be taken care of in `leave_Name` or `leave_Attribute`.
                if alias_name == self.old_name:
                    self.bypass_imports = True
                # If the import is a parent module of `self.old_name`, it will be renamed in `leave_Import`.
                elif self.old_name.startswith(alias_name + "."):
                    pass
                # If we are renaming a top-level module of the import, we know that the rename will be taken care of in `leave_Name` or `leave_Attribute`.
                elif alias_name.startswith(self.old_name + "."):
                    self.bypass_imports = True

    def leave_Import(
        self, original_node: cst.Import, updated_node: cst.Import
    ) -> cst.Import:
        new_names = []
        for import_alias in updated_node.names:
            import_alias_name = import_alias.name
            import_alias_full_name = get_full_name_for_node(import_alias_name)
            if import_alias_full_name is None:
                raise Exception("Could not parse full name for ImportAlias.name node.")

            if isinstance(import_alias_name, cst.Name) and self.old_name.startswith(
                import_alias_full_name + "."
            ):
                # Might, be in use elsewhere in the code, so schedule a potential removal, and add another alias
                new_names.append(import_alias)
                self.scheduled_removals.add(original_node)
                new_names.append(
                    cst.ImportAlias(
                        name=cst.Name(
                            value=self.gen_replacement_module(import_alias_full_name)
                        )
                    )
                )
                self.bypass_imports = True
            elif isinstance(
                import_alias_name, cst.Attribute
            ) and self.old_name.startswith(import_alias_full_name + "."):
                # Same idea as above.
                new_names.append(import_alias)
                self.scheduled_removals.add(original_node)
                new_name_node: cst.BaseExpression = cst.parse_expression(
                    self.gen_replacement_module(import_alias_full_name)
                )
                if not isinstance(new_name_node, (cst.Name, cst.Attribute)):
                    raise Exception(
                        "`parse_expression()` on dotted path returned non-Attribute-or-Name."
                    )
                new_names.append(cst.ImportAlias(name=new_name_node))
                self.bypass_imports = True
            else:
                new_names.append(import_alias)

        # Finally, we need to record the 'as' name using the original node.
        for import_alias in original_node.names:
            alias_name = get_full_name_for_node(import_alias.name)
            if alias_name is not None:
                if alias_name == self.old_name or self.old_name.startswith(
                    alias_name + "."
                ):
                    self.record_asname(import_alias, alias_name)
        return updated_node.with_changes(names=new_names)

    def visit_ImportFrom(self, node: cst.ImportFrom) -> None:
        module = node.module
        if module is None:
            return
        imported_module_name = get_full_name_for_node(module)
        if imported_module_name is None:
            return

        if imported_module_name == self.old_name:
            # If the imported module is exactly equivalent to the old name, it will be taken care of in `leave_Name` or `leave_Attribute`.
            self.bypass_imports = True
        elif self.old_name.startswith(imported_module_name + "."):
            # If the imported module is a parent module of `self.old_name`, it will be renamed in `leave_ImportFrom`.
            pass
        elif imported_module_name.startswith(self.old_name + "."):
            # If we are renaming a parent module of the current module, we know that the rename will be taken care of in `leave_Name` or `leave_Attribute`.
            self.bypass_imports = True

    def leave_ImportFrom(
        self, original_node: cst.ImportFrom, updated_node: cst.ImportFrom
    ) -> cst.ImportFrom:
        module = updated_node.module
        if module is None:
            return updated_node
        imported_module_name = get_full_name_for_node(module)
        names = original_node.names

        if imported_module_name is None or not isinstance(names, Sequence):
            return updated_node

        else:
            new_names = []
            for import_alias in names:
                alias_name = get_full_name_for_node(import_alias.name)
                if alias_name is not None:
                    qual_name = f"{imported_module_name}.{alias_name}"
                    if self.old_name == qual_name:

                        replacement_module = self.gen_replacement_module(
                            imported_module_name
                        )
                        replacement_obj = self.gen_replacement(alias_name)

                        # Record the 'as' name after we finish generating the new names for the import.
                        self.record_asname(import_alias, alias_name)
                        new_import_alias_name: cst.BaseExpression = cst.parse_expression(
                            replacement_obj
                        )
                        if not isinstance(new_import_alias_name, cst.Name):
                            raise Exception(
                                "`parse_expression()` on `import ... from` returned non-Name."
                            )
                        # Rename on the spot only if this is the only imported name under the module.
                        if len(names) == 1:
                            self.bypass_imports = True
                            return updated_node.with_changes(
                                module=cst.parse_expression(replacement_module),
                                names=(cst.ImportAlias(name=new_import_alias_name),),
                            )
                        # Or if the module name is to stay the same.
                        elif replacement_module == imported_module_name:
                            self.bypass_imports = True
                            new_names.append(
                                cst.ImportAlias(name=new_import_alias_name)
                            )
                        else:
                            # Otherwise, don't append this import and schedule a new import.
                            self.schedule_import = True
                    elif self.old_name.startswith(qual_name + "."):
                        # This import might be in use elsewhere in the code, so schedule a potential removal,
                        # and if the module is the same, add an extra alias, if not, schedule a whole new import.
                        new_names.append(import_alias)
                        self.scheduled_removals.add(original_node)
                        new_module_name = self.gen_replacement_module(
                            imported_module_name
                        )
                        if new_module_name:
                            if new_module_name == imported_module_name:
                                new_names.append(
                                    cst.ImportAlias(
                                        name=cst.Name(
                                            value=self.gen_replacement(alias_name)
                                        )
                                    )
                                )
                                self.bypass_imports = True
                            else:
                                new_object_name = self.gen_replacement(alias_name)
                                self.schedule_import = True
                        else:
                            (
                                new_module_name,
                                _,
                                new_object_name,
                            ) = self.new_name.rpartition(".")
                            self.schedule_import = True
                    else:
                        new_names.append(import_alias)

            return updated_node.with_changes(names=new_names)
        return updated_node

    def leave_Name(self, original_node: cst.Name, updated_node: cst.Name) -> cst.Name:
        full_name_for_node: str = original_node.value
        full_replacement_name = self.gen_replacement(full_name_for_node)
        if QualifiedNameProvider.has_name(self, original_node, self.old_name) or (
            not self.get_metadata(QualifiedNameProvider, original_node, set())
            and full_replacement_name == self.new_name
        ):
            replacement_name: str = self.gen_replacement(original_node.value)
            return updated_node.with_changes(value=replacement_name)

        return updated_node

    def leave_Attribute(
        self, original_node: cst.Attribute, updated_node: cst.Attribute
    ) -> Union[cst.Name, cst.Attribute]:
        full_name_for_node = get_full_name_for_node(original_node)
        if full_name_for_node is None:
            raise Exception("Could not parse full name for Attribute node.")
        full_replacement_name = self.gen_replacement(full_name_for_node)
        if QualifiedNameProvider.has_name(self, original_node, self.old_name,) or (
            not self.get_metadata(QualifiedNameProvider, original_node, set())
            and full_replacement_name == self.new_name
        ):
            new_value, _, new_attr = self.new_name.rpartition(".")
            self.scheduled_removals.add(original_node.value)
            self.schedule_import = True
            if full_replacement_name == self.new_name:
                return updated_node.with_changes(
                    value=cst.parse_expression(new_value), attr=cst.Name(value=new_attr)
                )
            return cst.Name(value=new_attr)

        return updated_node

    def leave_Module(
        self, original_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        for removal_node in self.scheduled_removals:
            RemoveImportsVisitor.remove_unused_import_by_node(
                self.context, removal_node
            )
        if not self.bypass_imports and self.schedule_import:
            if self.new_module is not None:
                AddImportsVisitor.add_needed_import(
                    self.context, module=self.new_module, obj=self.new_mod_or_obj
                )
        return updated_node

    def gen_replacement(self, original_name: str) -> str:
        if original_name == self.old_mod_or_obj:
            return self.new_mod_or_obj
        elif original_name == ".".join([self.old_module, self.old_mod_or_obj]):
            return self.new_name
        elif original_name.endswith("." + self.old_mod_or_obj):
            return self.new_mod_or_obj
        else:
            return self.gen_replacement_module(original_name)

    def gen_replacement_module(self, original_module: str) -> str:
        return self.new_module if original_module == self.old_module else ""

    def record_asname(self, import_alias: cst.ImportAlias, import_name: str) -> None:
        # Record the import's `as` name if it has one, and set the corresponding attribute to it.
        as_name = import_alias.asname
        name = as_name.name if as_name is not None else None
        if name is not None and isinstance(name, cst.Name):
            if import_name == self.old_mod_or_obj:
                self.old_mod_or_obj = name.value
            elif import_name == self.old_module:
                self.old_module = name.value
