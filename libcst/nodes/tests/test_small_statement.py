# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Optional

import libcst as cst
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class SmallStatementTest(CSTNodeTest):
    @data_provider(
        (
            (cst.Pass(), "pass"),
            (cst.Pass(semicolon=cst.Semicolon()), "pass;"),
            (
                cst.Pass(
                    semicolon=cst.Semicolon(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("    "),
                    )
                ),
                "pass  ;    ",
            ),
            (cst.Continue(), "continue"),
            (cst.Continue(semicolon=cst.Semicolon()), "continue;"),
            (
                cst.Continue(
                    semicolon=cst.Semicolon(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("    "),
                    )
                ),
                "continue  ;    ",
            ),
            (cst.Break(), "break"),
            (cst.Break(semicolon=cst.Semicolon()), "break;"),
            (
                cst.Break(
                    semicolon=cst.Semicolon(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("    "),
                    )
                ),
                "break  ;    ",
            ),
            (
                cst.Expr(cst.BinaryOperation(cst.Name("x"), cst.Add(), cst.Name("y"))),
                "x + y",
            ),
            (
                cst.Expr(
                    cst.BinaryOperation(cst.Name("x"), cst.Add(), cst.Name("y")),
                    semicolon=cst.Semicolon(),
                ),
                "x + y;",
            ),
            (
                cst.Expr(
                    cst.BinaryOperation(cst.Name("x"), cst.Add(), cst.Name("y")),
                    semicolon=cst.Semicolon(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("    "),
                    ),
                ),
                "x + y  ;    ",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)
