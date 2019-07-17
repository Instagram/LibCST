# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import libcst.nodes as cst
from libcst.metadata.position_provider import SyntacticPositionProvider
from libcst.nodes._internal import CodeRange
from libcst.parser import parse_module
from libcst.testing.utils import UnitTest
from libcst.visitors import CSTTransformer


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
