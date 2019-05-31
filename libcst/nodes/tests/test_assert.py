# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class AssertConstructionTest(CSTNodeTest):
    @data_provider(
        (
            # Simple assert
            (cst.Assert(cst.Name("True")), "assert True"),
            # Assert with message
            (
                cst.Assert(
                    cst.Name("True"), cst.SimpleString('"Value should be true"')
                ),
                'assert True, "Value should be true"',
            ),
            # Whitespace oddities test
            (
                cst.Assert(
                    cst.Name("True", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    whitespace_after_assert=cst.SimpleWhitespace(""),
                ),
                "assert(True)",
            ),
            # Whitespace rendering test
            (
                cst.Assert(
                    whitespace_after_assert=cst.SimpleWhitespace("  "),
                    test=cst.Name("True"),
                    comma=cst.Comma(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    msg=cst.SimpleString('"Value should be true"'),
                ),
                'assert  True  ,  "Value should be true"',
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        self.validate_node(node, code)

    @data_provider(
        (
            # Validate whitespace handling
            (
                lambda: cst.Assert(
                    cst.Name("True"), whitespace_after_assert=cst.SimpleWhitespace("")
                ),
                "Must have at least one space after 'assert'",
            ),
            # Validate comma handling
            (
                lambda: cst.Assert(test=cst.Name("True"), comma=cst.Comma()),
                "Cannot have trailing comma after 'test'",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class AssertParsingTest(CSTNodeTest):
    @data_provider(
        (
            # Simple assert
            (cst.Assert(cst.Name("True")), "assert True"),
            # Assert with message
            (
                cst.Assert(
                    cst.Name("True"),
                    cst.SimpleString('"Value should be true"'),
                    comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                ),
                'assert True, "Value should be true"',
            ),
            # Whitespace oddities test
            (
                cst.Assert(
                    cst.Name("True", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    whitespace_after_assert=cst.SimpleWhitespace(""),
                ),
                "assert(True)",
            ),
            # Whitespace rendering test
            (
                cst.Assert(
                    whitespace_after_assert=cst.SimpleWhitespace("  "),
                    test=cst.Name("True"),
                    comma=cst.Comma(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    msg=cst.SimpleString('"Value should be true"'),
                ),
                'assert  True  ,  "Value should be true"',
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        # pyre-fixme[16]: `BaseSuite` has no attribute `__getitem__`.
        self.validate_node(node, code, lambda code: parse_statement(code).body[0])
