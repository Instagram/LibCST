# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
import argparse
from typing import Callable, Optional, Sequence, Set, Tuple, Union

import libcst as cst
from libcst.codemod import CodemodContext, VisitorBasedCodemodCommand
from libcst.codemod.visitors import AddImportsVisitor, RemoveImportsVisitor
from libcst.helpers import get_full_name_for_node
from libcst.metadata import QualifiedNameProvider


def leave_import_decorator(
    method: Callable[..., Union[cst.Import, cst.ImportFrom]]
) -> Callable[..., Union[cst.Import, cst.ImportFrom]]:
    # We want to record any 'as name' that is relevant but only after we leave the corresponding Import/ImportFrom node since
    # we don't want the 'as name' to interfere with children 'Name' and 'Attribute' nodes.
    def wrapper(
        self: "RenameCommand",
        original_node: Union[cst.Import, cst.ImportFrom],
        updated_node: Union[cst.Import, cst.ImportFrom],
    ) -> Union[cst.Import, cst.ImportFrom]:
        updated_node = method(self, original_node, updated_node)
        if original_node != updated_node:
            self.record_asname(original_node)
        return updated_node

    return wrapper


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
            help=(
                "Full dotted name of replacement object. You may provide a single-colon-delimited name to specify how you want the new import to be structured."
                + "\nEg: `foo:bar.baz` will be translated to `from foo import bar`."
                + "\nIf no ':' character is provided, the import statement will default to `from foo.bar import baz` for a `new_name` value of `foo.bar.baz`"
                + " or simply replace the old import on the spot if the old import is an exact match."
            ),
        )

    def __init__(self, context: CodemodContext, old_name: str, new_name: str) -> None:
        super().__init__(context)

        new_module, has_colon, new_mod_or_obj = new_name.rpartition(":")
        # Exit early if improperly formatted args.
        if ":" in new_module:
            raise ValueError("Error: `new_name` should contain at most one colon.")
        if ":" in old_name:
            raise ValueError("Error: `old_name` should not contain any colons.")

        if not has_colon or not new_module:
            new_module, _, new_mod_or_obj = new_name.rpartition(".")

        self.new_name: str = new_name.replace(":", ".").strip(".")
        self.new_module: str = new_module.replace(":", ".").strip(".")
        self.new_mod_or_obj: str = new_mod_or_obj

        # If `new_name` contains a single colon at the end, then we assume the user wants the import
        # to be structured as 'import new_name'. So both self.new_mod_or_obj and self.old_mod_or_obj
        # will be empty in this case.
        if not self.new_mod_or_obj:
            old_module = old_name
            old_mod_or_obj = ""
        else:
            old_module, _, old_mod_or_obj = old_name.rpartition(".")

        self.old_name: str = old_name
        self.old_module: str = old_module
        self.old_mod_or_obj: str = old_mod_or_obj

        self.as_name: Optional[Tuple[str, str]] = None

        # A set of nodes that have been renamed to help with the cleanup of now potentially unused
        # imports, during import cleanup in `leave_Module`.
        self.scheduled_removals: Set[cst.CSTNode] = set()
        # If an import has been renamed while inside an `Import` or `ImportFrom` node, we want to flag
        # this so that we do not end up with two of the same import.
        self.bypass_import = False

    def visit_Import(self, node: cst.Import) -> None:
        for import_alias in node.names:
            alias_name = get_full_name_for_node(import_alias.name)
            if alias_name is not None:
                if alias_name == self.old_name or alias_name.startswith(
                    self.old_name + "."
                ):
                    # If the import statement is exactly equivalent to the old name, or we are renaming a top-level module of the import,
                    # it will be taken care of in `leave_Name` or `leave_Attribute` when visiting the Name and Attribute children of this Import.
                    self.bypass_import = True

    @leave_import_decorator
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
                # Might, be in use elsewhere in the code, so schedule a potential removal, and add another alias.
                new_names.append(import_alias)
                self.scheduled_removals.add(original_node)
                new_names.append(
                    cst.ImportAlias(
                        name=cst.Name(
                            value=self.gen_replacement_module(import_alias_full_name)
                        )
                    )
                )
                self.bypass_import = True
            elif isinstance(
                import_alias_name, cst.Attribute
            ) and self.old_name.startswith(import_alias_full_name + "."):
                # Same idea as above.
                new_names.append(import_alias)
                self.scheduled_removals.add(original_node)
                new_name_node: Union[
                    cst.Attribute, cst.Name
                ] = self.gen_name_or_attr_node(
                    self.gen_replacement_module(import_alias_full_name)
                )
                new_names.append(cst.ImportAlias(name=new_name_node))
                self.bypass_import = True
            else:
                new_names.append(import_alias)

        return updated_node.with_changes(names=new_names)

    def visit_ImportFrom(self, node: cst.ImportFrom) -> None:
        module = node.module
        if module is None:
            return
        imported_module_name = get_full_name_for_node(module)
        if imported_module_name is None:
            return
        if imported_module_name == self.old_name or imported_module_name.startswith(
            self.old_name + "."
        ):
            # If the imported module is exactly equivalent to the old name or we are renaming a parent module of the current module,
            # it will be taken care of in `leave_Name` or `leave_Attribute` when visiting the children of this ImportFrom.
            self.bypass_import = True

    @leave_import_decorator
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
                        if not replacement_obj:
                            # The user has requested an `import` statement rather than an `from ... import`.
                            # This will be taken care of in `leave_Module`, in the meantime, schedule for potential removal.
                            new_names.append(import_alias)
                            self.scheduled_removals.add(original_node)
                            continue

                        new_import_alias_name: Union[
                            cst.Attribute, cst.Name
                        ] = self.gen_name_or_attr_node(replacement_obj)
                        # Rename on the spot only if this is the only imported name under the module.
                        if len(names) == 1:
                            self.bypass_import = True
                            return updated_node.with_changes(
                                module=cst.parse_expression(replacement_module),
                                names=(cst.ImportAlias(name=new_import_alias_name),),
                            )
                        # Or if the module name is to stay the same.
                        elif replacement_module == imported_module_name:
                            self.bypass_import = True
                            new_names.append(
                                cst.ImportAlias(name=new_import_alias_name)
                            )
                    else:
                        if self.old_name.startswith(qual_name + "."):
                            # This import might be in use elsewhere in the code, so schedule a potential removal.
                            self.scheduled_removals.add(original_node)
                        new_names.append(import_alias)

            return updated_node.with_changes(names=new_names)
        return updated_node

    def leave_Name(
        self, original_node: cst.Name, updated_node: cst.Name
    ) -> Union[cst.Attribute, cst.Name]:
        full_name_for_node: str = original_node.value
        full_replacement_name = self.gen_replacement(full_name_for_node)

        # If a node has no associated QualifiedName, we are still inside an import statement.
        inside_import_statement: bool = not self.get_metadata(
            QualifiedNameProvider, original_node, set()
        )
        if QualifiedNameProvider.has_name(self, original_node, self.old_name) or (
            inside_import_statement and full_replacement_name == self.new_name
        ):
            if not full_replacement_name:
                full_replacement_name = self.new_name
            if not inside_import_statement:
                self.scheduled_removals.add(original_node)
            return self.gen_name_or_attr_node(full_replacement_name)

        return updated_node

    def leave_Attribute(
        self, original_node: cst.Attribute, updated_node: cst.Attribute
    ) -> Union[cst.Name, cst.Attribute]:
        full_name_for_node = get_full_name_for_node(original_node)
        if full_name_for_node is None:
            raise Exception("Could not parse full name for Attribute node.")
        full_replacement_name = self.gen_replacement(full_name_for_node)

        # If a node has no associated QualifiedName, we are still inside an import statement.
        inside_import_statement: bool = not self.get_metadata(
            QualifiedNameProvider, original_node, set()
        )
        if (
            QualifiedNameProvider.has_name(
                self,
                original_node,
                self.old_name,
            )
            or (inside_import_statement and full_replacement_name == self.new_name)
        ):
            new_value, new_attr = self.new_module, self.new_mod_or_obj
            if not inside_import_statement:
                self.scheduled_removals.add(original_node.value)
            if full_replacement_name == self.new_name:
                return updated_node.with_changes(
                    value=cst.parse_expression(new_value),
                    attr=cst.Name(value=new_attr.rstrip(".")),
                )

            return self.gen_name_or_attr_node(new_attr)

        return updated_node

    def leave_Module(
        self, original_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        for removal_node in self.scheduled_removals:
            RemoveImportsVisitor.remove_unused_import_by_node(
                self.context, removal_node
            )
        # If bypass_import is False, we know that no import statements were directly renamed, and the fact
        # that we have any `self.scheduled_removals` tells us we encountered a matching `old_name` in the code.
        if not self.bypass_import and self.scheduled_removals:
            if self.new_module:
                new_obj: Optional[str] = (
                    self.new_mod_or_obj.split(".")[0] if self.new_mod_or_obj else None
                )
                AddImportsVisitor.add_needed_import(
                    self.context, module=self.new_module, obj=new_obj
                )
        return updated_node

    def gen_replacement(self, original_name: str) -> str:
        module_as_name = self.as_name
        if module_as_name is not None:
            if original_name == module_as_name[0]:
                original_name = module_as_name[1]
            elif original_name.startswith(module_as_name[0] + "."):
                original_name = original_name.replace(
                    module_as_name[0] + ".", module_as_name[1] + ".", 1
                )

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

    def gen_name_or_attr_node(
        self, dotted_expression: str
    ) -> Union[cst.Attribute, cst.Name]:
        name_or_attr_node: cst.BaseExpression = cst.parse_expression(dotted_expression)
        if not isinstance(name_or_attr_node, (cst.Name, cst.Attribute)):
            raise Exception(
                "`parse_expression()` on dotted path returned non-Attribute-or-Name."
            )
        return name_or_attr_node

    def record_asname(self, original_node: Union[cst.Import, cst.ImportFrom]) -> None:
        # Record the import's `as` name if it has one, and set the attribute mapping.
        names = original_node.names
        if not isinstance(names, Sequence):
            return
        for import_alias in names:
            alias_name = get_full_name_for_node(import_alias.name)
            if isinstance(original_node, cst.ImportFrom):
                module = original_node.module
                if module is None:
                    return
                module_name = get_full_name_for_node(module)
                if module_name is None:
                    return
                qual_name = f"{module_name}.{alias_name}"
            else:
                qual_name = alias_name
            if qual_name is not None and alias_name is not None:
                if qual_name == self.old_name or self.old_name.startswith(
                    qual_name + "."
                ):
                    as_name_optional = import_alias.asname
                    as_name_node = (
                        as_name_optional.name if as_name_optional is not None else None
                    )
                    if as_name_node is not None and isinstance(
                        as_name_node, (cst.Name, cst.Attribute)
                    ):
                        full_as_name = get_full_name_for_node(as_name_node)
                        if full_as_name is not None:
                            self.as_name = (full_as_name, alias_name)
