# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import pickle
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

    def test_pickle(self) -> None:
        """
        It's common to use LibCST with multiprocessing to process files in parallel.
        Multiprocessing uses pickle by default, so we should make sure our errors can be
        pickled/unpickled.
        """
        orig_exception = ParserSyntaxError(
            "some message", lines=["abcd"], raw_line=1, raw_column=0
        )
        pickled_blob = pickle.dumps(orig_exception)
        new_exception = pickle.loads(pickled_blob)
        self.assertEqual(repr(orig_exception), repr(new_exception))
        self.assertEqual(str(orig_exception), str(new_exception))
