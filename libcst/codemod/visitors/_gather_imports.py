# pyre-strict
from typing import Dict, List, Optional, Sequence, Set, Tuple, Union

import libcst
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareVisitor


class GatherImportsVisitor(ContextAwareVisitor):
    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)
        # Track the available imports in this transform
        self.module_imports: Set[str] = set()
        self.object_mapping: Dict[str, Set[str]] = {}
        # Track the aliased imports in this transform
        self.module_aliases: Dict[str, str] = {}
        self.alias_mapping: Dict[str, List[Tuple[str, str]]] = {}
        # Track all of the imports found in this transform
        self.all_imports: List[Union[libcst.Import, libcst.ImportFrom]] = []

    def _get_string_name(self, node: Optional[libcst.CSTNode]) -> str:
        if node is None:
            return ""
        elif isinstance(node, libcst.Name):
            return node.value
        elif isinstance(node, libcst.Attribute):
            return self._get_string_name(node.value) + "." + node.attr.value
        else:
            raise Exception(f"Invalid node type {type(node)}!")

    def visit_Import(self, node: libcst.Import) -> None:
        # Track this import statement for later analysis.
        self.all_imports.append(node)

        for name in node.names:
            asname = name.asname
            if asname is not None:
                # Track this as an aliased module
                self.module_aliases[
                    self._get_string_name(name.name)
                ] = libcst.ensure_type(asname.name, libcst.Name).value
            else:
                # Get the module we're importing as a string.
                self.module_imports.add(self._get_string_name(name.name))

    def visit_ImportFrom(self, node: libcst.ImportFrom) -> None:
        # Track this import statement for later analysis.
        self.all_imports.append(node)

        if len(node.relative) > 0 or node.module is None:
            # Don't support relative-only imports at the moment.
            return

        # Get the module we're importing as a string.
        module = self._get_string_name(node.module)
        nodenames = node.names
        if isinstance(nodenames, libcst.ImportStar):
            # We cover everything, no need to bother tracking other things
            self.object_mapping[module] = set("*")
            return
        elif isinstance(nodenames, Sequence):
            # Get the list of imports we're aliasing in this import
            new_aliases = [
                # pyre-ignore We check ia.asname below, this is safe
                (self._get_string_name(ia.name), ia.asname.name.value)
                for ia in nodenames
                if ia.asname is not None
            ]
            if new_aliases:
                if module not in self.alias_mapping:
                    self.alias_mapping[module] = []
                self.alias_mapping[module].extend(new_aliases)

            # Get the list of imports we're importing in this import
            new_objects = {
                self._get_string_name(ia.name) for ia in nodenames if ia.asname is None
            }
            if new_objects:
                if module not in self.object_mapping:
                    self.object_mapping[module] = set()

                # Make sure that we don't add to a '*' module
                if "*" in self.object_mapping[module]:
                    self.object_mapping[module] = set("*")
                    return

                self.object_mapping[module].update(new_objects)
