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


class AwaitTest(CSTNodeTest):
    @data_provider(
        (
            # Some simple calls
            (cst.Await(cst.Name("test")), "await test"),
            (cst.Await(cst.Call(cst.Name("test"))), "await test()"),
            # Whitespace
            (
                cst.Await(
                    cst.Name("test"),
                    whitespace_after_await=cst.SimpleWhitespace("  "),
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "( await  test )",
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        # We don't have sentinel nodes for atoms, so we know that 100% of atoms
        # can be parsed identically to their creation.
        self.validate_node(node, code, parse_expression)

    @data_provider(
        (
            # Expression wrapping parenthesis rules
            (
                lambda: cst.Await(cst.Name("foo"), lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Await(cst.Name("foo"), rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.Await(
                    cst.Name("foo"), whitespace_after_await=cst.SimpleWhitespace("")
                ),
                "at least one space after await",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
