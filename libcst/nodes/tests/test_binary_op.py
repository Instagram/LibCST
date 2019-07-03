# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class BinaryOperationTest(CSTNodeTest):
    @data_provider(
        (
            # Simple binary operations
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.Add(), cst.Number(cst.Float("5.5"))
                ),
                "foo + 5.5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.Subtract(), cst.Number(cst.Float("5.5"))
                ),
                "foo - 5.5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.LeftShift(), cst.Number(cst.Integer("5"))
                ),
                "foo << 5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.RightShift(), cst.Number(cst.Integer("5"))
                ),
                "foo >> 5",
            ),
            (
                cst.BinaryOperation(cst.Name("foo"), cst.BitAnd(), cst.Name("bar")),
                "foo & bar",
            ),
            (
                cst.BinaryOperation(cst.Name("foo"), cst.BitXor(), cst.Name("bar")),
                "foo ^ bar",
            ),
            (
                cst.BinaryOperation(cst.Name("foo"), cst.BitOr(), cst.Name("bar")),
                "foo | bar",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.Multiply(), cst.Number(cst.Float("5.5"))
                ),
                "foo * 5.5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.MatrixMultiply(), cst.Number(cst.Float("5.5"))
                ),
                "foo @ 5.5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.Divide(), cst.Number(cst.Float("5.5"))
                ),
                "foo / 5.5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.Modulo(), cst.Number(cst.Float("5.5"))
                ),
                "foo % 5.5",
            ),
            (
                cst.BinaryOperation(
                    cst.Name("foo"), cst.FloorDivide(), cst.Number(cst.Float("5.5"))
                ),
                "foo // 5.5",
            ),
            # Parenthesized binary operation
            (
                cst.BinaryOperation(
                    lpar=(cst.LeftParen(),),
                    left=cst.Name("foo"),
                    operator=cst.LeftShift(),
                    right=cst.Number(cst.Integer("5")),
                    rpar=(cst.RightParen(),),
                ),
                "(foo << 5)",
            ),
            # Make sure that spacing works
            (
                cst.BinaryOperation(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    left=cst.Name("foo"),
                    operator=cst.Multiply(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    right=cst.Name("bar"),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "( foo  *  bar )",
                CodeRange.create((1, 2), (1, 13)),
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, parse_expression, expected_position=position)

    @data_provider(
        (
            (
                lambda: cst.BinaryOperation(
                    cst.Name("foo"),
                    # pyre-fixme[6]: Expected `BaseBinaryOp` for 2nd param but got
                    #  `Plus`.
                    cst.Plus(),
                    cst.Name("bar"),
                    lpar=(cst.LeftParen(),),
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.BinaryOperation(
                    cst.Name("foo"),
                    # pyre-fixme[6]: Expected `BaseBinaryOp` for 2nd param but got
                    #  `Plus`.
                    cst.Plus(),
                    cst.Name("bar"),
                    rpar=(cst.RightParen(),),
                ),
                "right paren without left paren",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
