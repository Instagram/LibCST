# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class ReturnCreateTest(CSTNodeTest):
    @data_provider(
        (
            (cst.SimpleStatementLine([cst.Return()]), "return\n"),
            (cst.SimpleStatementLine([cst.Return(cst.Name("abc"))]), "return abc\n"),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            (
                lambda: cst.Return(
                    cst.Name("abc"), whitespace_after_return=cst.SimpleWhitespace("")
                ),
                "Must have at least one space after 'return'.",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class ReturnParseTest(CSTNodeTest):
    @data_provider(
        (
            (
                cst.SimpleStatementLine(
                    [cst.Return(whitespace_after_return=cst.SimpleWhitespace(""))]
                ),
                "return\n",
            ),
            (
                cst.SimpleStatementLine(
                    [
                        cst.Return(
                            cst.Name("abc"),
                            whitespace_after_return=cst.SimpleWhitespace(" "),
                        )
                    ]
                ),
                "return abc\n",
            ),
            (
                cst.SimpleStatementLine(
                    [
                        cst.Return(
                            cst.Name("abc"),
                            whitespace_after_return=cst.SimpleWhitespace("   "),
                        )
                    ]
                ),
                "return   abc\n",
            ),
            (
                cst.SimpleStatementLine(
                    [
                        cst.Return(
                            cst.Name(
                                "abc", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                            ),
                            whitespace_after_return=cst.SimpleWhitespace(""),
                        )
                    ]
                ),
                "return(abc)\n",
            ),
            (
                cst.SimpleStatementLine(
                    [
                        cst.Return(
                            cst.Name("abc"),
                            whitespace_after_return=cst.SimpleWhitespace(" "),
                            semicolon=cst.Semicolon(),
                        )
                    ]
                ),
                "return abc;\n",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, parse_statement, expected_position=position)
