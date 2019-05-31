# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest, DummyIndentedBlock
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class IfTest(CSTNodeTest):
    @data_provider(
        (
            # Simple if without elif or else
            (
                cst.If(
                    cst.Name("conditional"), cst.SimpleStatementSuite((cst.Pass(),))
                ),
                "if conditional: pass\n",
                parse_statement,
            ),
            # else clause
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "if conditional: pass\nelse: pass\n",
                parse_statement,
            ),
            # elif clause
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    orelse=cst.If(
                        cst.Name("other_conditional"),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                        orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                    ),
                ),
                "if conditional: pass\nelif other_conditional: pass\nelse: pass\n",
                parse_statement,
            ),
            # indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.If(
                        cst.Name("conditional"),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                        orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                    ),
                ),
                "    if conditional: pass\n    else: pass\n",
                None,
            ),
            # with an indented body
            (
                DummyIndentedBlock(
                    "    ",
                    cst.If(
                        cst.Name("conditional"),
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                    ),
                ),
                "    if conditional:\n        pass\n",
                None,
            ),
            # leading_lines
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    leading_lines=(
                        cst.EmptyLine(comment=cst.Comment("# leading comment")),
                    ),
                ),
                "# leading comment\nif conditional: pass\n",
                parse_statement,
            ),
            # whitespace before/after test and else
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_before_test=cst.SimpleWhitespace("   "),
                    whitespace_after_test=cst.SimpleWhitespace("  "),
                    orelse=cst.Else(
                        cst.SimpleStatementSuite((cst.Pass(),)),
                        whitespace_before_colon=cst.SimpleWhitespace(" "),
                    ),
                ),
                "if   conditional  : pass\nelse : pass\n",
                parse_statement,
            ),
            # empty lines between if/elif/else clauses, not captured by the suite.
            (
                cst.If(
                    cst.Name("test_a"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    orelse=cst.If(
                        cst.Name("test_b"),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                        leading_lines=(cst.EmptyLine(),),
                        orelse=cst.Else(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            leading_lines=(cst.EmptyLine(),),
                        ),
                    ),
                ),
                "if test_a: pass\n\nelif test_b: pass\n\nelse: pass\n",
                parse_statement,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
    ) -> None:
        self.validate_node(node, code, parser)
