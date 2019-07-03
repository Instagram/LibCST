# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest, DummyIndentedBlock
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class ForTest(CSTNodeTest):
    @data_provider(
        (
            # Simple for block
            (
                cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "for target in iter(): pass\n",
                parse_statement,
            ),
            # Simple async for block
            (
                cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(),
                ),
                "async for target in iter(): pass\n",
                parse_statement,
            ),
            # For block with else
            (
                cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "for target in iter(): pass\nelse: pass\n",
                parse_statement,
            ),
            # indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.For(
                        cst.Name("target"),
                        cst.Call(cst.Name("iter")),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                    ),
                ),
                "    for target in iter(): pass\n",
                None,
            ),
            # for an indented body
            (
                DummyIndentedBlock(
                    "    ",
                    cst.For(
                        cst.Name("target"),
                        cst.Call(cst.Name("iter")),
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                    ),
                ),
                "    for target in iter():\n        pass\n",
                None,
            ),
            # leading_lines
            (
                cst.For(
                    cst.Name("target"),
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
                "# leading comment\nfor target in iter():\n    pass\n# else comment\nelse:\n    pass\n",
                None,
            ),
            # Weird spacing rules
            (
                cst.For(
                    cst.Name(
                        "target", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                    cst.Call(
                        cst.Name("iter"),
                        lpar=(cst.LeftParen(),),
                        rpar=(cst.RightParen(),),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_for=cst.SimpleWhitespace(""),
                    whitespace_before_in=cst.SimpleWhitespace(""),
                    whitespace_after_in=cst.SimpleWhitespace(""),
                ),
                "for(target)in(iter()): pass\n",
                parse_statement,
            ),
            # Whitespace
            (
                cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_for=cst.SimpleWhitespace("  "),
                    whitespace_before_in=cst.SimpleWhitespace("  "),
                    whitespace_after_in=cst.SimpleWhitespace("  "),
                    whitespace_before_colon=cst.SimpleWhitespace("  "),
                ),
                "for  target  in  iter()  : pass\n",
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
                lambda: cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_for=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'for' keyword",
            ),
            (
                lambda: cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_before_in=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space before 'in' keyword",
            ),
            (
                lambda: cst.For(
                    cst.Name("target"),
                    cst.Call(cst.Name("iter")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_in=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'in' keyword",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
