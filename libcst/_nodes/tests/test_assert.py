# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from typing import Any

from parameterized import parameterized, param

import libcst as cst
from libcst import parse_statement
from libcst._nodes.tests.base import CSTNodeTest
from libcst.helpers import ensure_type
from libcst.metadata import CodeRange


class AssertConstructionTest(CSTNodeTest):
    @parameterized.expand(
        (
            param(
                "Simple assert",
                node=cst.Assert(cst.Name("True")),
                code="assert True",
                parser=None,
                expected_position=None,
            ),
            param(
                "Assert with message",
                node=cst.Assert(
                    cst.Name("True"), cst.SimpleString('"Value should be true"')
                ),
                code='assert True, "Value should be true"',
                parser=None,
                expected_position=None,
            ),
            param(
                "Whitespace oddities test",
                node=cst.Assert(
                    cst.Name("True", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    whitespace_after_assert=cst.SimpleWhitespace(""),
                ),
                code="assert(True)",
                parser=None,
                expected_position=CodeRange((1, 0), (1, 12)),
            ),
            param(
                "Whitespace rendering test",
                node=cst.Assert(
                    whitespace_after_assert=cst.SimpleWhitespace("  "),
                    test=cst.Name("True"),
                    comma=cst.Comma(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    msg=cst.SimpleString('"Value should be true"'),
                ),
                code='assert  True  ,  "Value should be true"',
                parser=None,
                expected_position=CodeRange((1, 0), (1, 39)),
            ),
        )
    )
    def test_valid(self, _name: str, **kwargs: Any) -> None:
        self.validate_node(**kwargs)

    @parameterized.expand(
        (
            param(
                "Validate whitespace handling",
                get_node=(
                    lambda: cst.Assert(
                        cst.Name("True"),
                        whitespace_after_assert=cst.SimpleWhitespace(""),
                    )
                ),
                expected_re="Must have at least one space after 'assert'",
            ),
            param(
                "Validate comma handling",
                get_node=(lambda: cst.Assert(test=cst.Name("True"), comma=cst.Comma())),
                expected_re="Cannot have trailing comma after 'test'",
            ),
        )
    )
    def test_invalid(self, _name: str, **kwargs: Any) -> None:
        self.assert_invalid(**kwargs)


def _assert_parser(code: str) -> cst.Assert:
    return ensure_type(
        ensure_type(parse_statement(code), cst.SimpleStatementLine).body[0], cst.Assert
    )


class AssertParsingTest(CSTNodeTest):
    @parameterized.expand(
        (
            param(
                "simple assert",
                node=cst.Assert(cst.Name("True")),
                code="assert True",
                parser=_assert_parser,
                expected_position=None,
            ),
            param(
                "assert with message",
                node=cst.Assert(
                    cst.Name("True"),
                    cst.SimpleString('"Value should be true"'),
                    comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                ),
                code='assert True, "Value should be true"',
                parser=_assert_parser,
                expected_position=None,
            ),
            param(
                "whitespace oddities test",
                node=cst.Assert(
                    cst.Name("True", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    whitespace_after_assert=cst.SimpleWhitespace(""),
                ),
                code="assert(True)",
                parser=_assert_parser,
                expected_position=None,
            ),
            param(
                "whitespace rendering test",
                node=cst.Assert(
                    whitespace_after_assert=cst.SimpleWhitespace("  "),
                    test=cst.Name("True"),
                    comma=cst.Comma(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    msg=cst.SimpleString('"Value should be true"'),
                ),
                code='assert  True  ,  "Value should be true"',
                parser=_assert_parser,
                expected_position=None,
            ),
        )
    )
    def test_valid(self, _name: str, **kwargs: Any) -> None:
        self.validate_node(**kwargs)
