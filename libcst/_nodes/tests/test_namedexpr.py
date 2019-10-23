# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Any

import libcst as cst
from libcst._nodes.tests.base import CSTNodeTest
from libcst.metadata import CodeRange
from libcst.testing.utils import data_provider


class NamedExprTest(CSTNodeTest):
    @data_provider(
        (
            # Simple named expression
            {
                "node": cst.NamedExpr(cst.Name("x"), cst.Float("5.5")),
                "code": "x := 5.5",
                "parser": None,
                "expected_position": None,
            },
            # Parenthesized named expression
            {
                "node": cst.NamedExpr(
                    lpar=(cst.LeftParen(),),
                    target=cst.Name("foo"),
                    value=cst.Integer("5"),
                    rpar=(cst.RightParen(),),
                ),
                "code": "(foo := 5)",
                "parser": None,
                "expected_position": None,
            },
            # Make sure that spacing works
            {
                "node": cst.NamedExpr(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    target=cst.Name("foo"),
                    whitespace_before_walrus=cst.SimpleWhitespace("  "),
                    whitespace_after_walrus=cst.SimpleWhitespace("  "),
                    value=cst.Name("bar"),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "code": "( foo  :=  bar )",
                "parser": None,
                "expected_position": CodeRange((1, 2), (1, 14)),
            },
        )
    )
    def test_valid(self, **kwargs: Any) -> None:
        self.validate_node(**kwargs)

    @data_provider(
        (
            {
                "get_node": (
                    lambda: cst.NamedExpr(
                        cst.Name("foo"), cst.Name("bar"), lpar=(cst.LeftParen(),),
                    )
                ),
                "expected_re": "left paren without right paren",
            },
            {
                "get_node": (
                    lambda: cst.NamedExpr(
                        cst.Name("foo"), cst.Name("bar"), rpar=(cst.RightParen(),),
                    )
                ),
                "expected_re": "right paren without left paren",
            },
        )
    )
    def test_invalid(self, **kwargs: Any) -> None:
        self.assert_invalid(**kwargs)
