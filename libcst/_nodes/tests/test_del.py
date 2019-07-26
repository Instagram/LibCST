# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class DelTest(CSTNodeTest):
    @data_provider(
        (
            (cst.SimpleStatementLine([cst.Del(cst.Name("abc"))]), "del abc\n"),
            (
                cst.SimpleStatementLine(
                    [
                        cst.Del(
                            cst.Name("abc"),
                            whitespace_after_del=cst.SimpleWhitespace("   "),
                        )
                    ]
                ),
                "del   abc\n",
            ),
            (
                cst.SimpleStatementLine(
                    [
                        cst.Del(
                            cst.Name(
                                "abc", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                            ),
                            whitespace_after_del=cst.SimpleWhitespace(""),
                        )
                    ]
                ),
                "del(abc)\n",
            ),
            (
                cst.SimpleStatementLine(
                    [cst.Del(cst.Name("abc"), semicolon=cst.Semicolon())]
                ),
                "del abc;\n",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, parse_statement, expected_position=position)

    @data_provider(
        (
            (
                lambda: cst.Del(
                    cst.Name("abc"), whitespace_after_del=cst.SimpleWhitespace("")
                ),
                "Must have at least one space after 'del'.",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
