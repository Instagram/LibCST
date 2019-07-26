# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst import parse_statement
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest, DummyIndentedBlock
from libcst.testing.utils import data_provider


class SimpleStatementTest(CSTNodeTest):
    @data_provider(
        (
            # a single-element SimpleStatementLine
            (cst.SimpleStatementLine((cst.Pass(),)), "pass\n", parse_statement),
            # a multi-element SimpleStatementLine
            (
                cst.SimpleStatementLine(
                    (cst.Pass(semicolon=cst.Semicolon()), cst.Continue())
                ),
                "pass;continue\n",
                parse_statement,
            ),
            # a multi-element SimpleStatementLine with whitespace
            (
                cst.SimpleStatementLine(
                    (
                        cst.Pass(
                            semicolon=cst.Semicolon(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            )
                        ),
                        cst.Continue(),
                    )
                ),
                "pass ;  continue\n",
                parse_statement,
            ),
            # A more complicated SimpleStatementLine
            (
                cst.SimpleStatementLine(
                    (
                        cst.Pass(semicolon=cst.Semicolon()),
                        cst.Continue(semicolon=cst.Semicolon()),
                        cst.Break(),
                    )
                ),
                "pass;continue;break\n",
                parse_statement,
            ),
            # a multi-element SimpleStatementLine, inferred semicolons
            (
                cst.SimpleStatementLine((cst.Pass(), cst.Continue(), cst.Break())),
                "pass; continue; break\n",
                None,  # No test for parsing, since we are using sentinels.
            ),
            # some expression statements
            (
                cst.SimpleStatementLine((cst.Expr(cst.Name("None")),)),
                "None\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine((cst.Expr(cst.Name("True")),)),
                "True\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine((cst.Expr(cst.Name("False")),)),
                "False\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine((cst.Expr(cst.Ellipses()),)),
                "...\n",
                parse_statement,
            ),
            # Test some numbers
            (
                cst.SimpleStatementLine((cst.Expr(cst.Integer("5")),)),
                "5\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine((cst.Expr(cst.Float("5.5")),)),
                "5.5\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine((cst.Expr(cst.Imaginary("5j")),)),
                "5j\n",
                parse_statement,
            ),
            # Test some numbers with parens
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Integer(
                                "5", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            )
                        ),
                    )
                ),
                "(5)\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Float(
                                "5.5", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            )
                        ),
                    )
                ),
                "(5.5)\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Imaginary(
                                "5j", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            )
                        ),
                    )
                ),
                "(5j)\n",
                parse_statement,
            ),
            # Test some strings
            (
                cst.SimpleStatementLine((cst.Expr(cst.SimpleString('"abc"')),)),
                '"abc"\n',
                parse_statement,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.ConcatenatedString(
                                cst.SimpleString('"abc"'), cst.SimpleString('"def"')
                            )
                        ),
                    )
                ),
                '"abc""def"\n',
                parse_statement,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.ConcatenatedString(
                                left=cst.SimpleString('"abc"'),
                                whitespace_between=cst.SimpleWhitespace(" "),
                                right=cst.ConcatenatedString(
                                    left=cst.SimpleString('"def"'),
                                    whitespace_between=cst.SimpleWhitespace(" "),
                                    right=cst.SimpleString('"ghi"'),
                                ),
                            )
                        ),
                    )
                ),
                '"abc" "def" "ghi"\n',
                parse_statement,
            ),
            # Test parenthesis rules
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Ellipses(
                                lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            )
                        ),
                    )
                ),
                "(...)\n",
                parse_statement,
            ),
            # Test parenthesis with whitespace ownership
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Ellipses(
                                lpar=(
                                    cst.LeftParen(
                                        whitespace_after=cst.SimpleWhitespace(" ")
                                    ),
                                ),
                                rpar=(
                                    cst.RightParen(
                                        whitespace_before=cst.SimpleWhitespace(" ")
                                    ),
                                ),
                            )
                        ),
                    )
                ),
                "( ... )\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Ellipses(
                                lpar=(
                                    cst.LeftParen(
                                        whitespace_after=cst.SimpleWhitespace(" ")
                                    ),
                                    cst.LeftParen(
                                        whitespace_after=cst.SimpleWhitespace("  ")
                                    ),
                                    cst.LeftParen(
                                        whitespace_after=cst.SimpleWhitespace("   ")
                                    ),
                                ),
                                rpar=(
                                    cst.RightParen(
                                        whitespace_before=cst.SimpleWhitespace("   ")
                                    ),
                                    cst.RightParen(
                                        whitespace_before=cst.SimpleWhitespace("  ")
                                    ),
                                    cst.RightParen(
                                        whitespace_before=cst.SimpleWhitespace(" ")
                                    ),
                                ),
                            )
                        ),
                    )
                ),
                "( (  (   ...   )  ) )\n",
                parse_statement,
            ),
            # Test parenthesis rules with expressions
            (
                cst.SimpleStatementLine(
                    (
                        cst.Expr(
                            cst.Ellipses(
                                lpar=(
                                    cst.LeftParen(
                                        whitespace_after=cst.ParenthesizedWhitespace(
                                            first_line=cst.TrailingWhitespace(),
                                            empty_lines=(
                                                cst.EmptyLine(
                                                    comment=cst.Comment(
                                                        "# Wow, a comment!"
                                                    )
                                                ),
                                            ),
                                            indent=True,
                                            last_line=cst.SimpleWhitespace("    "),
                                        )
                                    ),
                                ),
                                rpar=(
                                    cst.RightParen(
                                        whitespace_before=cst.ParenthesizedWhitespace(
                                            first_line=cst.TrailingWhitespace(),
                                            empty_lines=(),
                                            indent=True,
                                            last_line=cst.SimpleWhitespace(""),
                                        )
                                    ),
                                ),
                            )
                        ),
                    )
                ),
                "(\n# Wow, a comment!\n    ...\n)\n",
                parse_statement,
            ),
            # test trailing whitespace
            (
                cst.SimpleStatementLine(
                    (cst.Pass(),),
                    trailing_whitespace=cst.TrailingWhitespace(
                        whitespace=cst.SimpleWhitespace("  "),
                        comment=cst.Comment("# trailing comment"),
                    ),
                ),
                "pass  # trailing comment\n",
                parse_statement,
            ),
            # test leading comment
            (
                cst.SimpleStatementLine(
                    (cst.Pass(),),
                    leading_lines=(cst.EmptyLine(comment=cst.Comment("# comment")),),
                ),
                "# comment\npass\n",
                parse_statement,
            ),
            # test indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.SimpleStatementLine(
                        (cst.Pass(),),
                        leading_lines=(
                            cst.EmptyLine(comment=cst.Comment("# comment")),
                        ),
                    ),
                ),
                "    # comment\n    pass\n",
                None,
            ),
            # test suite variant
            (cst.SimpleStatementSuite((cst.Pass(),)), " pass\n", None),
            (
                cst.SimpleStatementSuite(
                    (cst.Pass(),), leading_whitespace=cst.SimpleWhitespace("")
                ),
                "pass\n",
                None,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
        position: Optional[CodeRange] = None,
    ) -> None:
        self.validate_node(node, code, parser, expected_position=position)
