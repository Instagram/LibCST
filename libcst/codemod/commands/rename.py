# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
import argparse
from typing import Optional, Sequence, Union

import libcst as cst
from libcst.codemod import CodemodContext, VisitorBasedCodemodCommand
from libcst.codemod.visitors import AddImportsVisitor, RemoveImportsVisitor
from libcst.helpers import get_full_name_for_node
from libcst.metadata import QualifiedName, QualifiedNameProvider, QualifiedNameSource


class RenameCommand(VisitorBasedCodemodCommand):
    """
    Rename all instances of a local or imported object.
    """

    DESCRIPTION: str = "Rename all instances of a local or imported object."

    METADATA_DEPENDENCIES = (QualifiedNameProvider,)

    @staticmethod
    def add_args(parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--orig_module",
            dest="orig_module",
            help="Name of the module to rename. Leave blank if renaming a local variable.",
        )
        parser.add_argument(
            "--orig_object",
            dest="orig_object",
            required=True,
            help="Name of the object in module or variable to rename.",
        )
        parser.add_argument(
            "--new_module",
            dest="new_module",
            help="New name of the module. Leave blank if renaming a local variable, or if same as orig_module.",
        )
        parser.add_argument(
            "--new_object",
            dest="new_object",
            required=True,
            help="New name of the object in module or variable.",
        )

    def __init__(
        self,
        context: CodemodContext,
        orig_object: str,
        new_object: str,
        orig_module: Optional[str] = None,
        new_module: Optional[str] = None,
    ) -> None:
        super().__init__(context)

        self.orig_module: Optional[str] = orig_module
        self.orig_object: str = orig_object
        self.orig_name: str = (
            f"{orig_module}.{orig_object}" if orig_module is not None else orig_object
        )

        self.new_module: Optional[
            str
        ] = new_module if new_module is not None else orig_module
        self.new_object: str = new_object
        self.new_name: str = (
            f"{new_module}.{new_object}" if new_module is not None else new_object
        )

        self.source: QualifiedNameSource = (
            QualifiedNameSource.IMPORT
            if orig_module is not None
            else QualifiedNameSource.LOCAL
        )

    def leave_Import(
        self, original_node: cst.Import, updated_node: cst.Import
    ) -> Union[cst.Import, cst.RemovalSentinel]:
        for import_alias in original_node.names:
            name = get_full_name_for_node(import_alias.name)
            if name is not None and name == self.orig_module:
                # Schedule this import to be potentially removed
                RemoveImportsVisitor.remove_unused_import_by_node(
                    self.context, original_node
                )
        return original_node

    def leave_ImportFrom(
        self, original_node: cst.ImportFrom, updated_node: cst.ImportFrom
    ) -> Union[cst.ImportFrom, cst.RemovalSentinel]:
        names = original_node.names

        import_module = original_node.module
        import_module_name = (
            get_full_name_for_node(import_module) if import_module is not None else None
        )
        if (
            import_module_name is None
            or import_module_name != self.orig_module
            or not isinstance(names, Sequence)
        ):
            return updated_node

        if (
            len(names) == 1
            and get_full_name_for_node(names[0].name) == self.orig_object
        ):
            # Remove this ImportFrom altogether
            return cst.RemoveFromParent()

        new_names = []
        for import_alias in names:
            qualname = (
                f"{import_module_name}.{get_full_name_for_node(import_alias.name)}"
            )
            if qualname != self.orig_name:
                new_names.append(import_alias)

        return updated_node.with_changes(names=new_names)

    def leave_Attribute(
        self, original_node: cst.Attribute, updated_node: cst.Attribute
    ) -> cst.Attribute:
        if QualifiedNameProvider.has_name(
            self, original_node, QualifiedName(name=self.orig_name, source=self.source),
        ):
            full_name = get_full_name_for_node(original_node)
            if full_name is None:
                raise Exception("Logic error!")
            module_name = ".".join(full_name.split(".")[:-1])
            # New assignment for pyre check.
            new_import = self.new_module
            if new_import is not None:
                AddImportsVisitor.add_needed_import(
                    self.context, module=new_import, asname=None,
                )
            # New assignment for pyre check.
            new_value = self.new_module if self.new_module is not None else module_name
            # If we reach here, new_value should never technically be None since new_module and module_asname cannot both be None.
            if new_value is not None:
                return updated_node.with_changes(
                    value=cst.parse_expression(new_value),
                    attr=cst.Name(value=self.new_object.split(".")[-1]),
                )

        return updated_node

    def leave_Name(self, original_node: cst.Name, updated_node: cst.Name) -> cst.Name:
        if QualifiedNameProvider.has_name(
            self, original_node, QualifiedName(name=self.orig_name, source=self.source),
        ):
            name = get_full_name_for_node(original_node)
            if name is None:
                raise Exception("Logic error!")

            # New assignment for pyre check.
            new_import = self.new_module
            if new_import is not None:
                AddImportsVisitor.add_needed_import(
                    self.context, module=new_import, obj=self.new_object, asname=None
                )

            return updated_node.with_changes(value=self.new_object)

        return updated_node
