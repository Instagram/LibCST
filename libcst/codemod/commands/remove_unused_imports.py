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
    DESCRIPTION: str = "Remove all imports that are not used in a file."

    def visit_Import(self, node: Import) -> bool:
        RemoveImportsVisitor.remove_unused_import_by_node(self.context, node)
        return False

    def visit_ImportFrom(self, node: ImportFrom) -> bool:
        RemoveImportsVisitor.remove_unused_import_by_node(self.context, node)
        return False
