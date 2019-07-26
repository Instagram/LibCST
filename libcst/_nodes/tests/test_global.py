# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst import parse_statement
from libcst._helpers import ensure_type
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class GlobalConstructionTest(CSTNodeTest):
    @data_provider(
        (
            # Single global statement
            (cst.Global((cst.NameItem(cst.Name("a")),)), "global a"),
            # Multiple entries in global statement
            (
                cst.Global((cst.NameItem(cst.Name("a")), cst.NameItem(cst.Name("b")))),
                "global a, b",
            ),
            # Whitespace rendering test
            (
                cst.Global(
                    (
                        cst.NameItem(
                            cst.Name("a"),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace("  "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.NameItem(cst.Name("b")),
                    ),
                    whitespace_after_global=cst.SimpleWhitespace("  "),
                ),
                "global  a  ,  b",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            # Validate construction
            (
                lambda: cst.Global(()),
                "A Global statement must have at least one NameItem",
            ),
            # Validate whitespace handling
            (
                lambda: cst.Global(
                    (cst.NameItem(cst.Name("a")),),
                    whitespace_after_global=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'global' keyword",
            ),
            # Validate comma handling
            (
                lambda: cst.Global((cst.NameItem(cst.Name("a"), comma=cst.Comma()),)),
                "The last NameItem in a Global cannot have a trailing comma",
            ),
            # Validate paren handling
            (
                lambda: cst.Global(
                    (
                        cst.NameItem(
                            cst.Name(
                                "a", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            )
                        ),
                    )
                ),
                "Cannot have parens around names in NameItem",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class GlobalParsingTest(CSTNodeTest):
    @data_provider(
        (
            # Single global statement
            (cst.Global((cst.NameItem(cst.Name("a")),)), "global a"),
            # Multiple entries in global statement
            (
                cst.Global(
                    (
                        cst.NameItem(
                            cst.Name("a"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.NameItem(cst.Name("b")),
                    )
                ),
                "global a, b",
            ),
            # Whitespace rendering test
            (
                cst.Global(
                    (
                        cst.NameItem(
                            cst.Name("a"),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace("  "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.NameItem(cst.Name("b")),
                    ),
                    whitespace_after_global=cst.SimpleWhitespace("  "),
                ),
                "global  a  ,  b",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(
            node,
            code,
            lambda code: ensure_type(
                parse_statement(code), cst.SimpleStatementLine
            ).body[0],
            expected_position=position,
        )
