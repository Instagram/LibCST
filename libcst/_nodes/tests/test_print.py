# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Any, Callable

import libcst as cst
from libcst._nodes.tests.base import CSTNodeTest, parse_statement_as
from libcst.metadata import CodeRange
from libcst.testing.utils import data_provider


class PrintTest(CSTNodeTest):
    @data_provider(
        (
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            [
                                cst.Py2PrintExpr(
                                    cst.Name("abc"),
                                ),
                            ],
                            whitespace_after_print=cst.SimpleWhitespace(" "),
                        )
                    ]
                ),
                "code": "print abc\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": CodeRange((1, 0), (1, 9)),
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            [
                                cst.Py2PrintExpr(
                                    cst.Name("abc"),
                                ),
                            ],
                            whitespace_after_print=cst.SimpleWhitespace("   "),
                        )
                    ]
                ),
                "code": "print   abc\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": CodeRange((1, 0), (1, 11)),
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            [
                                cst.Py2PrintExpr(
                                    cst.Name(
                                        "abc",
                                        lpar=[cst.LeftParen()],
                                        rpar=[cst.RightParen()],
                                    ),
                                ),
                            ],
                            whitespace_after_print=cst.SimpleWhitespace(""),
                        )
                    ]
                ),
                "code": "print(abc)\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": CodeRange((1, 0), (1, 10)),
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            [
                                cst.Py2PrintExpr(
                                    cst.Name("abc"),
                                )
                            ],
                            whitespace_after_print=cst.SimpleWhitespace("  "),
                            semicolon=cst.Semicolon(),
                        )
                    ]
                ),
                "code": "print  abc;\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": CodeRange((1, 0), (1, 10)),
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            [
                                cst.Py2PrintExpr(
                                    cst.Tuple(
                                        [
                                            cst.Element(
                                                value=cst.Name("abc"),
                                                comma=cst.Comma(),
                                            ),
                                        ],
                                    ),
                                )
                            ],
                            whitespace_after_print=cst.SimpleWhitespace("  "),
                            semicolon=cst.Semicolon(
                                whitespace_before=cst.SimpleWhitespace(" ")
                            ),
                        )
                    ]
                ),
                "code": "print  (abc,) ;\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": CodeRange((1, 0), (1, 13)),
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            items=[
                                cst.Py2PrintExpr(
                                    item=cst.Name(
                                        value="x",
                                    ),
                                ),
                            ],
                            print_to=cst.Attribute(
                                value=cst.Name(
                                    value="sys",
                                    lpar=[],
                                    rpar=[],
                                ),
                                attr=cst.Name(
                                    value="stderr",
                                    lpar=[],
                                    rpar=[],
                                ),
                                dot=cst.Dot(
                                    whitespace_before=cst.SimpleWhitespace(
                                        value="",
                                    ),
                                    whitespace_after=cst.SimpleWhitespace(
                                        value="",
                                    ),
                                ),
                                lpar=[],
                                rpar=[],
                            ),
                            print_to_comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(value="    "),
                                whitespace_after=cst.SimpleWhitespace(value="     "),
                            ),
                            whitespace_after_print=cst.SimpleWhitespace(value="  "),
                            whitespace_before_print_to=cst.SimpleWhitespace(
                                value="   "
                            ),
                        ),
                    ]
                ),
                "code": "print  >>   sys.stderr    ,     x\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": None,
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(items=[]),
                    ]
                ),
                "code": "print\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": None,
            },
            {
                "node": cst.SimpleStatementLine(
                    [
                        cst.Py2Print(
                            items=[],
                            print_to=cst.Attribute(
                                value=cst.Name(
                                    value="sys",
                                    lpar=[],
                                    rpar=[],
                                ),
                                attr=cst.Name(
                                    value="stderr",
                                    lpar=[],
                                    rpar=[],
                                ),
                                dot=cst.Dot(
                                    whitespace_before=cst.SimpleWhitespace(
                                        value="",
                                    ),
                                    whitespace_after=cst.SimpleWhitespace(
                                        value="",
                                    ),
                                ),
                                lpar=[],
                                rpar=[],
                            ),
                            whitespace_after_print=cst.SimpleWhitespace(value="  "),
                            whitespace_before_print_to=cst.SimpleWhitespace(
                                value="   "
                            ),
                        ),
                    ]
                ),
                "code": "print  >>   sys.stderr\n",
                "parser": parse_statement_as(python_version="2.6"),
                "expected_position": None,
            },
        )
    )
    def test_valid(self, **kwargs: Any) -> None:
        self.validate_node(**kwargs)

    @data_provider(
        (
            {
                "code": "print 1+1",
                "parser": parse_statement_as(python_version="2.6"),
                "expect_success": True,
            },
            {
                "code": "print 1+1",
                "parser": parse_statement_as(
                    python_version="2.7", future_imports=frozenset(("print_function",))
                ),
                "expect_success": False,
            },
            {
                "code": "print 1+1,",
                "parser": parse_statement_as(python_version="2.6"),
                "expect_success": True,
            },
            {
                "code": "print >> sys.stderr, 1+1",
                "parser": parse_statement_as(python_version="2.6"),
                "expect_success": True,
            },
            {
                "code": "print>>sys.stderr, 1+1",
                "parser": parse_statement_as(python_version="2.6"),
                "expect_success": True,
            },
            {
                "code": "print 1+1",
                "parser": parse_statement_as(python_version="3.6"),
                "expect_success": False,
            },
        )
    )
    def test_versions(
        self,
        code: str,
        parser: Callable[[str], cst.BaseExpression],
        expect_success: bool,
    ) -> None:
        self.assert_parses(code, parser, expect_success)
