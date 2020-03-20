# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict

from libcst import Import, ImportFrom
from libcst.codemod import VisitorBasedCodemodCommand
from libcst.codemod.visitors import RemoveImportsVisitor


class RemoveUnusedImportsCommand(VisitorBasedCodemodCommand):
    """
    Remove all unused imports from a file based on scope analysis.

    This command analyses individual files in isolation and does not attempt
    to track cross-references between them. If a symbol is imported in a file
    but otherwise unused in it, that import will be removed even if it is being
    referenced from another file.

    It currently doesn't keep track of string type annotations, so an import
    for `MyType` used only in `def f() -> "MyType"` will be removed.
    """

    DESCRIPTION: str = (
        "Remove all imports that are not used in a file. "
        "Note: only considers the file in isolation. "
        "Note: does not account for usages in string type annotations. "
    )

    def visit_Import(self, node: Import) -> bool:
        RemoveImportsVisitor.remove_unused_import_by_node(self.context, node)
        return False

    def visit_ImportFrom(self, node: ImportFrom) -> bool:
        RemoveImportsVisitor.remove_unused_import_by_node(self.context, node)
        return False
