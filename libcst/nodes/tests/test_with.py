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


class WithTest(CSTNodeTest):
    @data_provider(
        (
            # Simple with block
            (
                cst.With(
                    (cst.WithItem(cst.Call(cst.Name("context_mgr"))),),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "with context_mgr(): pass\n",
                parse_statement,
            ),
            # Simple async with block
            (
                cst.With(
                    (cst.WithItem(cst.Call(cst.Name("context_mgr"))),),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(),
                ),
                "async with context_mgr(): pass\n",
                parse_statement,
            ),
            # Multiple context managers
            (
                cst.With(
                    (
                        cst.WithItem(cst.Call(cst.Name("foo"))),
                        cst.WithItem(cst.Call(cst.Name("bar"))),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "with foo(), bar(): pass\n",
                None,
            ),
            (
                cst.With(
                    (
                        cst.WithItem(
                            cst.Call(cst.Name("foo")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.WithItem(cst.Call(cst.Name("bar"))),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "with foo(), bar(): pass\n",
                parse_statement,
            ),
            # With block containing variable for context manager.
            (
                cst.With(
                    (
                        cst.WithItem(
                            cst.Call(cst.Name("context_mgr")),
                            cst.AsName(cst.Name("ctx")),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "with context_mgr() as ctx: pass\n",
                parse_statement,
            ),
            # indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.With(
                        (cst.WithItem(cst.Call(cst.Name("context_mgr"))),),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                    ),
                ),
                "    with context_mgr(): pass\n",
                None,
            ),
            # with an indented body
            (
                DummyIndentedBlock(
                    "    ",
                    cst.With(
                        (cst.WithItem(cst.Call(cst.Name("context_mgr"))),),
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                    ),
                ),
                "    with context_mgr():\n        pass\n",
                None,
            ),
            # leading_lines
            (
                cst.With(
                    (cst.WithItem(cst.Call(cst.Name("context_mgr"))),),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    leading_lines=(
                        cst.EmptyLine(comment=cst.Comment("# leading comment")),
                    ),
                ),
                "# leading comment\nwith context_mgr(): pass\n",
                parse_statement,
            ),
            # Weird spacing rules
            (
                cst.With(
                    (
                        cst.WithItem(
                            cst.Call(
                                cst.Name("context_mgr"),
                                lpar=(cst.LeftParen(),),
                                rpar=(cst.RightParen(),),
                            )
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_with=cst.SimpleWhitespace(""),
                ),
                "with(context_mgr()): pass\n",
                parse_statement,
            ),
            # Whitespace
            (
                cst.With(
                    (
                        cst.WithItem(
                            cst.Call(cst.Name("context_mgr")),
                            cst.AsName(
                                cst.Name("ctx"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_with=cst.SimpleWhitespace("  "),
                    whitespace_before_colon=cst.SimpleWhitespace("  "),
                ),
                "with  context_mgr()  as  ctx  : pass\n",
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

    @data_provider(
        (
            (
                lambda: cst.With(
                    (), cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),))
                ),
                "A With statement must have at least one WithItem",
            ),
            (
                lambda: cst.With(
                    (
                        cst.WithItem(
                            cst.Call(cst.Name("foo")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                    ),
                    cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                ),
                "The last WithItem in a With cannot have a trailing comma",
            ),
            (
                lambda: cst.With(
                    (cst.WithItem(cst.Call(cst.Name("context_mgr"))),),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_with=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after with keyword",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
