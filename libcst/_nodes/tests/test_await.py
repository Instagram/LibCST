# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Any

import libcst as cst
from libcst import CodeRange, parse_expression
from libcst._nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class AwaitTest(CSTNodeTest):
    @data_provider(
        (
            # Some simple calls
            # pyre-fixme[6]: Incompatible parameter type
            {
                "node": cst.Await(cst.Name("test")),
                "code": "await test",
                "parser": parse_expression,
                "expected_position": None,
            },
            {
                "node": cst.Await(cst.Call(cst.Name("test"))),
                "code": "await test()",
                "parser": parse_expression,
                "expected_position": None,
            },
            # Whitespace
            {
                "node": cst.Await(
                    cst.Name("test"),
                    whitespace_after_await=cst.SimpleWhitespace("  "),
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "code": "( await  test )",
                "parser": parse_expression,
                "expected_position": CodeRange.create((1, 2), (1, 13)),
            },
        )
    )
    def test_valid(self, **kwargs: Any) -> None:
        # We don't have sentinel nodes for atoms, so we know that 100% of atoms
        # can be parsed identically to their creation.
        self.validate_node(**kwargs)

    @data_provider(
        (
            # Expression wrapping parenthesis rules
            {
                "get_node": (
                    lambda: cst.Await(cst.Name("foo"), lpar=(cst.LeftParen(),))
                ),
                "expected_re": "left paren without right paren",
            },
            {
                "get_node": (
                    lambda: cst.Await(cst.Name("foo"), rpar=(cst.RightParen(),))
                ),
                "expected_re": "right paren without left paren",
            },
            {
                "get_node": (
                    lambda: cst.Await(
                        cst.Name("foo"), whitespace_after_await=cst.SimpleWhitespace("")
                    )
                ),
                "expected_re": "at least one space after await",
            },
        )
    )
    def test_invalid(self, **kwargs: Any) -> None:
        self.assert_invalid(**kwargs)
