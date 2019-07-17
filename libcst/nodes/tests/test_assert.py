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


class AssertConstructionTest(CSTNodeTest):
    @data_provider(
        (
            # Simple assert
            {"node": cst.Assert(cst.Name("True")), "code": "assert True"},
            # Assert with message
            {
                "node": cst.Assert(
                    cst.Name("True"), cst.SimpleString('"Value should be true"')
                ),
                "code": 'assert True, "Value should be true"',
            },
            # Whitespace oddities test
            {
                "node": cst.Assert(
                    cst.Name("True", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    whitespace_after_assert=cst.SimpleWhitespace(""),
                ),
                "code": "assert(True)",
            },
            # Whitespace rendering test
            {
                "node": cst.Assert(
                    whitespace_after_assert=cst.SimpleWhitespace("  "),
                    test=cst.Name("True"),
                    comma=cst.Comma(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    msg=cst.SimpleString('"Value should be true"'),
                ),
                "code": 'assert  True  ,  "Value should be true"',
            },
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            # Validate whitespace handling
            {
                "get_node": (
                    lambda: cst.Assert(
                        cst.Name("True"),
                        whitespace_after_assert=cst.SimpleWhitespace(""),
                    )
                ),
                "expected_re": "Must have at least one space after 'assert'",
            },
            # Validate comma handling
            {
                "get_node": (
                    lambda: cst.Assert(test=cst.Name("True"), comma=cst.Comma())
                ),
                "expected_re": "Cannot have trailing comma after 'test'",
            },
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
            {"node": cst.Assert(cst.Name("True")), "code": "assert True"},
            # Assert with message
            {
                "node": cst.Assert(
                    cst.Name("True"),
                    cst.SimpleString('"Value should be true"'),
                    comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                ),
                "code": 'assert True, "Value should be true"',
            },
            # Whitespace oddities test
            {
                "node": cst.Assert(
                    cst.Name("True", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    whitespace_after_assert=cst.SimpleWhitespace(""),
                ),
                "code": "assert(True)",
            },
            # Whitespace rendering test
            {
                "node": cst.Assert(
                    whitespace_after_assert=cst.SimpleWhitespace("  "),
                    test=cst.Name("True"),
                    comma=cst.Comma(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    msg=cst.SimpleString('"Value should be true"'),
                ),
                "code": 'assert  True  ,  "Value should be true"',
            },
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
