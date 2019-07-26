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


class WhileTest(CSTNodeTest):
    @data_provider(
        (
            # Simple while block
            (
                cst.While(
                    cst.Call(cst.Name("iter")), cst.SimpleStatementSuite((cst.Pass(),))
                ),
                "while iter(): pass\n",
                parse_statement,
            ),
            # While block with else
            (
                cst.While(
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "while iter(): pass\nelse: pass\n",
                parse_statement,
            ),
            # indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.While(
                        cst.Call(cst.Name("iter")),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                    ),
                ),
                "    while iter(): pass\n",
                None,
            ),
            # while an indented body
            (
                DummyIndentedBlock(
                    "    ",
                    cst.While(
                        cst.Call(cst.Name("iter")),
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                    ),
                ),
                "    while iter():\n        pass\n",
                None,
            ),
            # leading_lines
            (
                cst.While(
                    cst.Call(cst.Name("iter")),
                    cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                    leading_lines=(
                        cst.EmptyLine(comment=cst.Comment("# leading comment")),
                    ),
                ),
                "# leading comment\nwhile iter():\n    pass\n",
                parse_statement,
            ),
            (
                cst.While(
                    cst.Call(cst.Name("iter")),
                    cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                    cst.Else(
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                        leading_lines=(
                            cst.EmptyLine(comment=cst.Comment("# else comment")),
                        ),
                    ),
                    leading_lines=(
                        cst.EmptyLine(comment=cst.Comment("# leading comment")),
                    ),
                ),
                "# leading comment\nwhile iter():\n    pass\n# else comment\nelse:\n    pass\n",
                None,
            ),
            # Weird spacing rules
            (
                cst.While(
                    cst.Call(
                        cst.Name("iter"),
                        lpar=(cst.LeftParen(),),
                        rpar=(cst.RightParen(),),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_while=cst.SimpleWhitespace(""),
                ),
                "while(iter()): pass\n",
                parse_statement,
            ),
            # Whitespace
            (
                cst.While(
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_while=cst.SimpleWhitespace("  "),
                    whitespace_before_colon=cst.SimpleWhitespace("  "),
                ),
                "while  iter()  : pass\n",
                parse_statement,
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

    @data_provider(
        (
            (
                lambda: cst.While(
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_while=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'while' keyword",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
