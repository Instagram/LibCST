# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Callable, Optional

import libcst as cst
from libcst import parse_expression
from libcst._nodes.tests.base import CSTNodeTest
from libcst.metadata import CodeRange
from libcst.testing.utils import data_provider


class TemplatedStringTest(CSTNodeTest):
    @data_provider(
        (
            # Simple t-string with only text
            (
                cst.TemplatedString(
                    parts=(cst.TemplatedStringText("hello world"),),
                ),
                't"hello world"',
                True,
            ),
            # t-string with one expression
            (
                cst.TemplatedString(
                    parts=(
                        cst.TemplatedStringText("hello "),
                        cst.TemplatedStringExpression(
                            expression=cst.Name("name"),
                        ),
                    ),
                ),
                't"hello {name}"',
                True,
            ),
            # t-string with multiple expressions
            (
                cst.TemplatedString(
                    parts=(
                        cst.TemplatedStringText("a="),
                        cst.TemplatedStringExpression(expression=cst.Name("a")),
                        cst.TemplatedStringText(", b="),
                        cst.TemplatedStringExpression(expression=cst.Name("b")),
                    ),
                ),
                't"a={a}, b={b}"',
                True,
                CodeRange((1, 0), (1, 15)),
            ),
            # t-string with nested expression
            (
                cst.TemplatedString(
                    parts=(
                        cst.TemplatedStringText("sum="),
                        cst.TemplatedStringExpression(
                            expression=cst.BinaryOperation(
                                left=cst.Name("a"),
                                operator=cst.Add(),
                                right=cst.Name("b"),
                            )
                        ),
                    ),
                ),
                't"sum={a + b}"',
                True,
            ),
            # t-string with spacing in expression
            (
                cst.TemplatedString(
                    parts=(
                        cst.TemplatedStringText("x = "),
                        cst.TemplatedStringExpression(
                            whitespace_before_expression=cst.SimpleWhitespace(" "),
                            expression=cst.Name("x"),
                            whitespace_after_expression=cst.SimpleWhitespace(" "),
                        ),
                    ),
                ),
                't"x = { x }"',
                True,
            ),
            # t-string with escaped braces
            (
                cst.TemplatedString(
                    parts=(cst.TemplatedStringText("{{foo}}"),),
                ),
                't"{{foo}}"',
                True,
            ),
            # t-string with only an expression
            (
                cst.TemplatedString(
                    parts=(
                        cst.TemplatedStringExpression(expression=cst.Name("value")),
                    ),
                ),
                't"{value}"',
                True,
            ),
            # t-string with whitespace and newlines
            (
                cst.TemplatedString(
                    parts=(
                        cst.TemplatedStringText("line1\\n"),
                        cst.TemplatedStringExpression(expression=cst.Name("x")),
                        cst.TemplatedStringText("\\nline2"),
                    ),
                ),
                't"line1\\n{x}\\nline2"',
                True,
            ),
            # t-string with parenthesis (not typical, but test node construction)
            (
                cst.TemplatedString(
                    lpar=(cst.LeftParen(),),
                    parts=(cst.TemplatedStringText("foo"),),
                    rpar=(cst.RightParen(),),
                ),
                '(t"foo")',
                True,
            ),
            # t-string with whitespace in delimiters
            (
                cst.TemplatedString(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    parts=(cst.TemplatedStringText("foo"),),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                '( t"foo" )',
                True,
            ),
            # Test TemplatedStringText and TemplatedStringExpression individually
            (
                cst.TemplatedStringText("abc"),
                "abc",
                False,
                CodeRange((1, 0), (1, 3)),
            ),
            (
                cst.TemplatedStringExpression(expression=cst.Name("foo")),
                "{foo}",
                False,
                CodeRange((1, 0), (1, 5)),
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        check_parsing: bool,
        position: Optional[CodeRange] = None,
    ) -> None:
        if check_parsing:
            self.validate_node(node, code, parse_expression, expected_position=position)
        else:
            self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            (
                lambda: cst.TemplatedString(
                    parts=(cst.TemplatedStringText("foo"),),
                    lpar=(cst.LeftParen(),),
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.TemplatedString(
                    parts=(cst.TemplatedStringText("foo"),),
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
