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


class NonlocalConstructionTest(CSTNodeTest):
    @data_provider(
        (
            # Single nonlocal statement
            (cst.Nonlocal((cst.NameItem(cst.Name("a")),)), "nonlocal a"),
            # Multiple entries in nonlocal statement
            (
                cst.Nonlocal(
                    (cst.NameItem(cst.Name("a")), cst.NameItem(cst.Name("b")))
                ),
                "nonlocal a, b",
            ),
            # Whitespace rendering test
            (
                cst.Nonlocal(
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
                    whitespace_after_nonlocal=cst.SimpleWhitespace("  "),
                ),
                "nonlocal  a  ,  b",
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
                lambda: cst.Nonlocal(()),
                "A Nonlocal statement must have at least one NameItem",
            ),
            # Validate whitespace handling
            (
                lambda: cst.Nonlocal(
                    (cst.NameItem(cst.Name("a")),),
                    whitespace_after_nonlocal=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'nonlocal' keyword",
            ),
            # Validate comma handling
            (
                lambda: cst.Nonlocal((cst.NameItem(cst.Name("a"), comma=cst.Comma()),)),
                "The last NameItem in a Nonlocal cannot have a trailing comma",
            ),
            # Validate paren handling
            (
                lambda: cst.Nonlocal(
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


class NonlocalParsingTest(CSTNodeTest):
    @data_provider(
        (
            # Single nonlocal statement
            (cst.Nonlocal((cst.NameItem(cst.Name("a")),)), "nonlocal a"),
            # Multiple entries in nonlocal statement
            (
                cst.Nonlocal(
                    (
                        cst.NameItem(
                            cst.Name("a"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.NameItem(cst.Name("b")),
                    )
                ),
                "nonlocal a, b",
            ),
            # Whitespace rendering test
            (
                cst.Nonlocal(
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
                    whitespace_after_nonlocal=cst.SimpleWhitespace("  "),
                ),
                "nonlocal  a  ,  b",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(
            node,
            code,
            # pyre-fixme[16]: `BaseSuite` has no attribute `__getitem__`.
            lambda code: parse_statement(code).body[0],
            expected_position=position,
        )
