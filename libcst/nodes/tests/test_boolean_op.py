# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class BooleanOperationTest(CSTNodeTest):
    @data_provider(
        (
            # Simple boolean operations
            (
                cst.BooleanOperation(cst.Name("foo"), cst.And(), cst.Name("bar")),
                "foo and bar",
            ),
            (
                cst.BooleanOperation(cst.Name("foo"), cst.Or(), cst.Name("bar")),
                "foo or bar",
            ),
            # Parenthesized boolean operation
            (
                cst.BooleanOperation(
                    lpar=(cst.LeftParen(),),
                    left=cst.Name("foo"),
                    operator=cst.Or(),
                    right=cst.Name("bar"),
                    rpar=(cst.RightParen(),),
                ),
                "(foo or bar)",
            ),
            (
                cst.BooleanOperation(
                    left=cst.Name(
                        "foo", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                    operator=cst.Or(
                        whitespace_before=cst.SimpleWhitespace(""),
                        whitespace_after=cst.SimpleWhitespace(""),
                    ),
                    right=cst.Name(
                        "bar", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                ),
                "(foo)or(bar)",
            ),
            # Make sure that spacing works
            (
                cst.BooleanOperation(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    left=cst.Name("foo"),
                    operator=cst.And(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    right=cst.Name("bar"),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "( foo  and  bar )",
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        self.validate_node(node, code, parse_expression)

    @data_provider(
        (
            (
                lambda: cst.BooleanOperation(
                    cst.Name("foo"), cst.And(), cst.Name("bar"), lpar=(cst.LeftParen(),)
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.BooleanOperation(
                    cst.Name("foo"),
                    cst.And(),
                    cst.Name("bar"),
                    rpar=(cst.RightParen(),),
                ),
                "right paren without left paren",
            ),
            (
                lambda: cst.BooleanOperation(
                    left=cst.Name("foo"),
                    operator=cst.Or(
                        whitespace_before=cst.SimpleWhitespace(""),
                        whitespace_after=cst.SimpleWhitespace(""),
                    ),
                    right=cst.Name("bar"),
                ),
                "at least one space around boolean operator",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
