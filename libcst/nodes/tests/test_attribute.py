# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodePosition
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class AttributeTest(CSTNodeTest):
    @data_provider(
        (
            # Simple attribute access
            (
                cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                "foo.bar",
                CodePosition((1, 0), (1, 7)),
            ),
            # Parenthesized attribute access
            (
                cst.Attribute(
                    lpar=(cst.LeftParen(),),
                    value=cst.Name("foo"),
                    attr=cst.Name("bar"),
                    rpar=(cst.RightParen(),),
                ),
                "(foo.bar)",
                CodePosition((1, 1), (1, 8)),
            ),
            # Make sure that spacing works
            (
                cst.Attribute(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    value=cst.Name("foo"),
                    dot=cst.Dot(
                        whitespace_before=cst.SimpleWhitespace(" "),
                        whitespace_after=cst.SimpleWhitespace(" "),
                    ),
                    attr=cst.Name("bar"),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "( foo . bar )",
                CodePosition((1, 2), (1, 11)),
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodePosition] = None
    ) -> None:
        self.validate_node(node, code, parse_expression, expected_position=position)

    @data_provider(
        (
            (
                lambda: cst.Attribute(
                    cst.Name("foo"), cst.Name("bar"), lpar=(cst.LeftParen(),)
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.Attribute(
                    cst.Name("foo"), cst.Name("bar"), rpar=(cst.RightParen(),)
                ),
                "right paren without left paren",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
