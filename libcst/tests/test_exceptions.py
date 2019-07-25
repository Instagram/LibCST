# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from textwrap import dedent

from libcst._exceptions import ParserSyntaxError
from libcst.testing.utils import UnitTest, data_provider


class ExceptionsTest(UnitTest):
    @data_provider(
        [
            (
                ParserSyntaxError(
                    message="some message",
                    encountered=None,  # EOF
                    expected=None,  # EOF
                    pos=(1, 0),
                    lines=["abcd"],
                ),
                dedent(
                    """
                    Syntax Error: some message @ 1:1.
                    Encountered end of file (EOF), but expected end of file (EOF).

                    abcd
                    ^
                    """
                ).strip(),
            ),
            (
                ParserSyntaxError(
                    message="some message",
                    encountered="encountered_value",
                    expected=["expected_value"],
                    pos=(1, 2),
                    lines=["\tabcd\r\n"],
                ),
                dedent(
                    """
                    Syntax Error: some message @ 1:10.
                    Encountered 'encountered_value', but expected one of ['expected_value'].

                            abcd
                             ^
                    """
                ).strip(),
            ),
        ]
    )
    def test_parser_syntax_error_str(
        self, err: ParserSyntaxError, expected: str
    ) -> None:
        self.assertEqual(str(err), expected)
