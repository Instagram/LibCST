# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import pickle
from textwrap import dedent

import libcst as cst
from libcst.testing.utils import data_provider, UnitTest


class ExceptionsTest(UnitTest):
    @data_provider(
        {
            "simple": (
                cst.ParserSyntaxError(
                    "some message", lines=["abcd"], raw_line=1, raw_column=0
                ),
                dedent(
                    """
                    Syntax Error @ 1:1.
                    some message

                    abcd
                    ^
                    """
                ).strip(),
            ),
            "tab_expansion": (
                cst.ParserSyntaxError(
                    "some message", lines=["\tabcd\r\n"], raw_line=1, raw_column=2
                ),
                dedent(
                    """
                    Syntax Error @ 1:10.
                    some message

                            abcd
                             ^
                    """
                ).strip(),
            ),
            "shows_last_line_with_text": (
                cst.ParserSyntaxError(
                    "some message",
                    lines=["abcd\n", "efgh\n", "\n", "\n", "\n", "\n", "\n"],
                    raw_line=5,
                    raw_column=0,
                ),
                dedent(
                    """
                    Syntax Error @ 5:1.
                    some message

                    efgh
                        ^
                    """
                ).strip(),
            ),
            "empty_file": (
                cst.ParserSyntaxError(
                    "some message", lines=[""], raw_line=1, raw_column=0
                ),
                dedent(
                    """
                    Syntax Error @ 1:1.
                    some message
                    """
                    # There's no code snippet here because the input file was empty.
                ).strip(),
            ),
        }
    )
    def test_parser_syntax_error_str(
        self, err: cst.ParserSyntaxError, expected: str
    ) -> None:
        self.assertEqual(str(err), expected)

    def test_pickle(self) -> None:
        """
        It's common to use LibCST with multiprocessing to process files in parallel.
        Multiprocessing uses pickle by default, so we should make sure our errors can be
        pickled/unpickled.
        """
        orig_exception = cst.ParserSyntaxError(
            "some message", lines=["abcd"], raw_line=1, raw_column=0
        )
        pickled_blob = pickle.dumps(orig_exception)
        new_exception = pickle.loads(pickled_blob)
        self.assertEqual(repr(orig_exception), repr(new_exception))
        self.assertEqual(str(orig_exception), str(new_exception))

    def test_parser_error_line_numbers(self) -> None:
        """
        Regression test for issue #1472: parser syntax errors should report
        the correct line number and column position.
        """
        # Test case 1: error on first line of multi-line input
        example1 = "x = (1 2)\ny = 3\nz = 4\n"
        with self.assertRaises(cst.ParserSyntaxError) as cm:
            cst.parse_module(example1)
        err = cm.exception
        # The error is at line 1, column 8 (0-indexed)
        self.assertEqual(err.raw_line, 1, "Error should be on line 1")
        self.assertEqual(err.raw_column, 8, "Error should be at column 8")
        # editor_line and editor_column should be 1-indexed
        self.assertEqual(err.editor_line, 1)
        self.assertEqual(err.editor_column, 9)
        # The error message should show the correct location
        self.assertIn("Syntax Error @ 1:9", str(err))

        # Test case 2: error on last line
        example2 = "x = 1\ny = (1 2)\n"
        with self.assertRaises(cst.ParserSyntaxError) as cm:
            cst.parse_module(example2)
        err = cm.exception
        # The error is at line 2, column 8 (0-indexed)
        self.assertEqual(err.raw_line, 2, "Error should be on line 2")
        self.assertEqual(err.raw_column, 8, "Error should be at column 8")
        # editor_line and editor_column should be 1-indexed
        self.assertEqual(err.editor_line, 2)
        self.assertEqual(err.editor_column, 9)
        # The error message should show the correct location
        self.assertIn("Syntax Error @ 2:9", str(err))

    def test_parser_error_tab_adjustment_on_correct_line(self) -> None:
        """
        Regression test for issue #1472: tab adjustment logic should use the
        correct line to calculate the column offset.
        """
        # Error on line 1 with tabs - should not be affected by tabs on other lines
        example = "x = (1 2)\n\t\ty = 3\n"
        with self.assertRaises(cst.ParserSyntaxError) as cm:
            cst.parse_module(example)
        err = cm.exception
        self.assertEqual(err.raw_line, 1)
        self.assertEqual(err.raw_column, 8)
        # No tabs on line 1, so column should not be clamped
        self.assertEqual(err.editor_column, 9)
