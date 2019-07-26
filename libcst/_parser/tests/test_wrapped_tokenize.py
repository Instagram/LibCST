# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Sequence

from parso.python.token import PythonTokenTypes
from parso.utils import parse_version_string

from libcst._exceptions import ParserSyntaxError
from libcst._parser._types.whitespace_state import WhitespaceState
from libcst._parser._wrapped_tokenize import Token, tokenize
from libcst.testing.utils import UnitTest, data_provider


_PY38 = parse_version_string("3.8.0")


class WrappedTokenizeTest(UnitTest):
    maxDiff = 10000

    @data_provider(
        {
            "simple": (
                "pass;\n",
                (
                    Token(
                        type=PythonTokenTypes.NAME,
                        string="pass",
                        start_pos=(1, 0),
                        end_pos=(1, 4),
                        whitespace_before=WhitespaceState(
                            line=1, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=1, column=4, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.OP,
                        string=";",
                        start_pos=(1, 4),
                        end_pos=(1, 5),
                        whitespace_before=WhitespaceState(
                            line=1, column=4, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=1, column=5, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.NEWLINE,
                        string="\n",
                        start_pos=(1, 5),
                        end_pos=(2, 0),
                        whitespace_before=WhitespaceState(
                            line=1, column=5, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=2, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.ENDMARKER,
                        string="",
                        start_pos=(2, 0),
                        end_pos=(2, 0),
                        whitespace_before=WhitespaceState(
                            line=2, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=2, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                ),
            ),
            "with_indent": (
                "if foo:\n    bar\n",
                (
                    Token(
                        type=PythonTokenTypes.NAME,
                        string="if",
                        start_pos=(1, 0),
                        end_pos=(1, 2),
                        whitespace_before=WhitespaceState(
                            line=1, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=1, column=2, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.NAME,
                        string="foo",
                        start_pos=(1, 3),
                        end_pos=(1, 6),
                        whitespace_before=WhitespaceState(
                            line=1, column=2, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=1, column=6, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.OP,
                        string=":",
                        start_pos=(1, 6),
                        end_pos=(1, 7),
                        whitespace_before=WhitespaceState(
                            line=1, column=6, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=1, column=7, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.NEWLINE,
                        string="\n",
                        start_pos=(1, 7),
                        end_pos=(2, 0),
                        whitespace_before=WhitespaceState(
                            line=1, column=7, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=2,
                            column=0,
                            absolute_indent="    ",
                            is_parenthesized=False,
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.INDENT,
                        string="",
                        start_pos=(2, 4),
                        end_pos=(2, 4),
                        whitespace_before=WhitespaceState(
                            line=2,
                            column=0,
                            absolute_indent="    ",
                            is_parenthesized=False,
                        ),
                        whitespace_after=WhitespaceState(
                            line=2,
                            column=0,
                            absolute_indent="    ",
                            is_parenthesized=False,
                        ),
                        relative_indent="    ",
                    ),
                    Token(
                        type=PythonTokenTypes.NAME,
                        string="bar",
                        start_pos=(2, 4),
                        end_pos=(2, 7),
                        whitespace_before=WhitespaceState(
                            line=2,
                            column=0,
                            absolute_indent="    ",
                            is_parenthesized=False,
                        ),
                        whitespace_after=WhitespaceState(
                            line=2,
                            column=7,
                            absolute_indent="    ",
                            is_parenthesized=False,
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.NEWLINE,
                        string="\n",
                        start_pos=(2, 7),
                        end_pos=(3, 0),
                        whitespace_before=WhitespaceState(
                            line=2,
                            column=7,
                            absolute_indent="    ",
                            is_parenthesized=False,
                        ),
                        whitespace_after=WhitespaceState(
                            line=3, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.DEDENT,
                        string="",
                        start_pos=(3, 0),
                        end_pos=(3, 0),
                        whitespace_before=WhitespaceState(
                            line=3, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=3, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                    Token(
                        type=PythonTokenTypes.ENDMARKER,
                        string="",
                        start_pos=(3, 0),
                        end_pos=(3, 0),
                        whitespace_before=WhitespaceState(
                            line=3, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        whitespace_after=WhitespaceState(
                            line=3, column=0, absolute_indent="", is_parenthesized=False
                        ),
                        relative_indent=None,
                    ),
                ),
            ),
        }
    )
    def test_tokenize(self, code: str, expected: Sequence[Token]) -> None:
        tokens = tuple(tokenize(code, _PY38))
        self.assertSequenceEqual(tokens, expected)
        for a, b in zip(tokens, tokens[1:]):
            # These must be the same object, so if whitespace gets consumed (mutated) at
            # the end of token a, it shows up at the beginning of token b.
            self.assertIs(a.whitespace_after, b.whitespace_before)

    def test_errortoken(self) -> None:
        with self.assertRaisesRegex(ParserSyntaxError, "invalid token"):
            # use tuple() to read everything
            # The copyright symbol isn't a valid token
            tuple(tokenize("\u00a9", _PY38))

    def test_error_dedent(self) -> None:
        with self.assertRaisesRegex(ParserSyntaxError, "inconsistent indentation"):
            # create some inconsistent indents to generate an ERROR_DEDENT token
            tuple(tokenize("    a\n  b", _PY38))
