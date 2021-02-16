# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any

import libcst as cst
from libcst import parse_expression
from libcst._nodes.tests.base import CSTNodeTest
from libcst.metadata import CodeRange
from libcst.testing.utils import data_provider


class BinaryOperationTest(CSTNodeTest):
    @data_provider(
        (
            # Simple binary operations
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.Add(), cst.Float("5.5")
                ),
                "code": "foo + 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.Subtract(), cst.Float("5.5")
                ),
                "code": "foo - 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.LeftShift(), cst.Integer("5")
                ),
                "code": "foo << 5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.RightShift(), cst.Integer("5")
                ),
                "code": "foo >> 5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.BitAnd(), cst.Name("bar")
                ),
                "code": "foo & bar",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.BitXor(), cst.Name("bar")
                ),
                "code": "foo ^ bar",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.BitOr(), cst.Name("bar")
                ),
                "code": "foo | bar",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.Multiply(), cst.Float("5.5")
                ),
                "code": "foo * 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.MatrixMultiply(), cst.Float("5.5")
                ),
                "code": "foo @ 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.Divide(), cst.Float("5.5")
                ),
                "code": "foo / 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.Modulo(), cst.Float("5.5")
                ),
                "code": "foo % 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.BinaryOperation(
                    cst.Name("foo"), cst.FloorDivide(), cst.Float("5.5")
                ),
                "code": "foo // 5.5",
                "parser": parse_expression,
                "expected_position": None,
            },
            # Parenthesized binary operation
            {
                "node": cst.BinaryOperation(
                    lpar=(cst.LeftParen(),),
                    left=cst.Name("foo"),
                    operator=cst.LeftShift(),
                    right=cst.Integer("5"),
                    rpar=(cst.RightParen(),),
                ),
                "code": "(foo << 5)",
                "parser": parse_expression,
                "expected_position": None,
            },
            # Make sure that spacing works
            {
                "node": cst.BinaryOperation(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    left=cst.Name("foo"),
                    operator=cst.Multiply(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    right=cst.Name("bar"),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "code": "( foo  *  bar )",
                "parser": parse_expression,
                "expected_position": CodeRange((1, 2), (1, 13)),
            },
            # Make sure operators are left associative
            {
                "node": cst.BinaryOperation(
                    left=cst.BinaryOperation(
                        left=cst.Name("foo"),
                        operator=cst.Add(),
                        right=cst.Name("bar"),
                    ),
                    operator=cst.Add(),
                    right=cst.Name("baz"),
                ),
                "code": "foo + bar + baz",
                "parser": parse_expression,
                "expected_position": None,
            },
            # Except for Power which is right associative
            {
                "node": cst.BinaryOperation(
                    left=cst.Name("foo"),
                    operator=cst.Power(),
                    right=cst.BinaryOperation(
                        left=cst.Name("bar"),
                        operator=cst.Power(),
                        right=cst.Name("baz"),
                    ),
                ),
                "code": "foo ** bar ** baz",
                "parser": parse_expression,
                "expected_position": None,
            },
        )
    )
    def test_valid(self, **kwargs: Any) -> None:
        self.validate_node(**kwargs)

    @data_provider(
        (
            {
                "get_node": (
                    lambda: cst.BinaryOperation(
                        cst.Name("foo"),
                        cst.Add(),
                        cst.Name("bar"),
                        lpar=(cst.LeftParen(),),
                    )
                ),
                "expected_re": "left paren without right paren",
            },
            {
                "get_node": (
                    lambda: cst.BinaryOperation(
                        cst.Name("foo"),
                        cst.Add(),
                        cst.Name("bar"),
                        rpar=(cst.RightParen(),),
                    )
                ),
                "expected_re": "right paren without left paren",
            },
        )
    )
    def test_invalid(self, **kwargs: Any) -> None:
        self.assert_invalid(**kwargs)


    @data_provider(
        (
            # Make sure operands are implicitly parenthesized 
            {
                "node": cst.BinaryOperation(
                    left=cst.Name("foo"),
                    operator=cst.Multiply(),
                    right=cst.BinaryOperation(
                        left=cst.Name("bar"),
                        operator=cst.Multiply(),
                        right=cst.Name("baz"),
                    ),
                ),
                "code": "foo * (bar * baz)",
            },
            # But only when necessary
            {
                "node": cst.BinaryOperation(
                    left=cst.Name("foo"),
                    operator=cst.Add(),
                    right=cst.BinaryOperation(
                        left=cst.Name("bar"),
                        operator=cst.Multiply(),
                        right=cst.Name("baz"),
                    ),
                ),
                "code": "foo + bar * baz",
            },
        )
    )
    def test_implicit_parens(self, node: cst.CSTNode, code: str) -> None:
        self.assertEqual(
            cst.Module([]).code_for_node(node),
            code
        )
