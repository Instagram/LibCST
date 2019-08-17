# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from textwrap import dedent
from typing import Callable

import libcst as cst
from libcst.testing.utils import UnitTest, data_provider


class ParseErrorsTest(UnitTest):
    @data_provider(
        {
            # _wrapped_tokenize raises these exceptions
            "wrapped_tokenize__invalid_token": (
                lambda: cst.parse_module("'"),
                dedent(
                    """
                    Syntax Error @ 1:1.
                    "'" is not a valid token.

                    '
                    ^
                    """
                ).strip(),
            ),
            "wrapped_tokenize__expected_dedent": (
                lambda: cst.parse_module("if False:\n    pass\n  pass"),
                dedent(
                    """
                    Syntax Error @ 3:1.
                    Inconsistent indentation. Expected a dedent.

                      pass
                    ^
                    """
                ).strip(),
            ),
            "wrapped_tokenize__mismatched_braces": (
                lambda: cst.parse_module("abcd)"),
                dedent(
                    """
                    Syntax Error @ 1:5.
                    Encountered a closing brace without a matching opening brace.

                    abcd)
                        ^
                    """
                ).strip(),
            ),
            # _base_parser raises these exceptions
            "base_parser__unexpected_indent": (
                lambda: cst.parse_module("    abcd"),
                dedent(
                    """
                    Syntax Error @ 1:5.
                    Incomplete input. Unexpectedly encountered an indent.

                        abcd
                        ^
                    """
                ).strip(),
            ),
            "base_parser__unexpected_dedent": (
                lambda: cst.parse_module("if False:\n    (el for el\n"),
                dedent(
                    """
                    Syntax Error @ 3:1.
                    Incomplete input. Encountered a dedent, but expected 'in'.

                        (el for el
                                  ^
                    """
                ).strip(),
            ),
            "base_parser__multiple_possibilities": (
                lambda: cst.parse_module("try: pass"),
                dedent(
                    """
                    Syntax Error @ 2:1.
                    Incomplete input. Encountered end of file (EOF), but expected 'except', or 'finally'.

                    try: pass
                             ^
                    """
                ).strip(),
            ),
            # conversion functions raise these exceptions.
            # `_base_parser` is responsible for attaching location information.
            "convert_nonterminal__dict_unpacking": (
                lambda: cst.parse_expression("{**el for el in []}"),
                dedent(
                    """
                    Syntax Error @ 1:19.
                    Internal error: dict unpacking cannot be used in dict comprehension

                    {**el for el in []}
                                      ^
                    """
                ).strip(),
            ),
        }
    )
    def test_parser_syntax_error_str(
        self, parse_fn: Callable[[], object], expected: str
    ) -> None:
        with self.assertRaises(cst.ParserSyntaxError) as cm:
            parse_fn()
        self.assertEqual(str(cm.exception), expected)
