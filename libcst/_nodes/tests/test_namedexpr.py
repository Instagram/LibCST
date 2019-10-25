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


def _parse_expression_force_38(code: str) -> cst.BaseExpression:
    return cst.parse_expression(
        code, config=cst.PartialParserConfig(python_version="3.8")
    )


def _parse_statement_force_38(code: str) -> cst.BaseCompoundStatement:
    statement = cst.parse_statement(
        code, config=cst.PartialParserConfig(python_version="3.8")
    )
    if not isinstance(statement, cst.BaseCompoundStatement):
        raise Exception("This function is expecting to parse compound statements only!")
    return statement


class NamedExprTest(CSTNodeTest):
    @data_provider(
        (
            # Simple named expression
            {
                "node": cst.NamedExpr(cst.Name("x"), cst.Float("5.5")),
                "code": "x := 5.5",
                "parser": None,  # Walrus operator is illegal as top-level statement
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
                "parser": _parse_expression_force_38,
                "expected_position": CodeRange((1, 1), (1, 9)),
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
                "parser": _parse_expression_force_38,
                "expected_position": CodeRange((1, 2), (1, 14)),
            },
            # Make sure we can use these where allowed in if/while statements
            {
                "node": cst.While(
                    test=cst.NamedExpr(
                        target=cst.Name(value="x"),
                        value=cst.Call(func=cst.Name(value="some_input")),
                    ),
                    body=cst.SimpleStatementSuite(body=[cst.Pass()]),
                ),
                "code": "while x := some_input(): pass\n",
                "parser": _parse_statement_force_38,
                "expected_position": None,
            },
            {
                "node": cst.If(
                    test=cst.NamedExpr(
                        target=cst.Name(value="x"),
                        value=cst.Call(func=cst.Name(value="some_input")),
                    ),
                    body=cst.SimpleStatementSuite(body=[cst.Pass()]),
                ),
                "code": "if x := some_input(): pass\n",
                "parser": _parse_statement_force_38,
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
                    lambda: cst.NamedExpr(
                        cst.Name("foo"), cst.Name("bar"), lpar=(cst.LeftParen(),)
                    )
                ),
                "expected_re": "left paren without right paren",
            },
            {
                "get_node": (
                    lambda: cst.NamedExpr(
                        cst.Name("foo"), cst.Name("bar"), rpar=(cst.RightParen(),)
                    )
                ),
                "expected_re": "right paren without left paren",
            },
        )
    )
    def test_invalid(self, **kwargs: Any) -> None:
        self.assert_invalid(**kwargs)
