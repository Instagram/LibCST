# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import libcst as cst
from libcst import parse_module
from libcst._batched_visitor import BatchableCSTVisitor, visit_batched
from libcst._nodes._internal import CodeRange
from libcst._visitors import CSTTransformer
from libcst.metadata.position_provider import SyntacticPositionProvider
from libcst.testing.utils import UnitTest


class PositionProviderTest(UnitTest):
    def test_visitor_provider(self) -> None:
        """
        Sets 2 metadata entries for every node:
            SimpleProvider -> 1
            DependentProvider - > 2
        """

        test = self

        class DependentVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (SyntacticPositionProvider,)

            def visit_Pass(self, node: cst.Pass) -> None:
                range = self.get_metadata(SyntacticPositionProvider, node)
                test.assertEqual(range, CodeRange.create((1, 0), (1, 4)))

        module = parse_module("pass")
        module.visit(DependentVisitor())

    def test_batchable_provider(self) -> None:
        test = self

        class ABatchable(BatchableCSTVisitor):
            METADATA_DEPENDENCIES = (SyntacticPositionProvider,)

            def visit_Pass(self, node: cst.Pass) -> None:
                range = self.get_metadata(SyntacticPositionProvider, node)
                test.assertEqual(range, CodeRange.create((1, 0), (1, 4)))

        visit_batched(parse_module("pass"), [ABatchable()])
