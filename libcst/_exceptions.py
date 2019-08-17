# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from enum import Enum, auto
from typing import Any, Callable, Iterable, Optional, Sequence, Tuple, Union

from parso.pgen2.generator import ReservedString
from parso.python.token import PythonTokenTypes, TokenType
from typing_extensions import final

from libcst._parser._types.token import Token
from libcst._tabs import expand_tabs


_EOF_STR: str = "end of file (EOF)"
_INDENT_STR: str = "an indent"
_DEDENT_STR: str = "a dedent"
_NEWLINE_CHARS: str = "\r\n"


class EOFSentinel(Enum):
    EOF = auto()


def get_expected_str(
    encountered: Union[Token, EOFSentinel],
    expected: Union[Iterable[Union[TokenType, ReservedString]], EOFSentinel],
) -> str:
    if isinstance(encountered, EOFSentinel):
        encountered_str = _EOF_STR
    else:
        encountered_str = repr(encountered.string)

    if isinstance(expected, EOFSentinel):
        expected_names = _EOF_STR
    else:
        expected_str = repr([
            el.name if isinstance(el, TokenType) else el.value
            for el in expected
        ])

    return f"Encountered {encountered_str}, but expected {expected_str}."


# pyre-fixme[2]: 'Any' type isn't pyre-strict.
def _parser_syntax_error_unpickle(kwargs: Any) -> "ParserSyntaxError":
    return ParserSyntaxError(**kwargs)


@final
class ParserSyntaxError(Exception):
    """
    Contains error information about the parser tree.
    """

    message: str

    # An internal value used to compute `editor_column` and to pretty-print where the
    # syntax error occurred in the code.
    _lines: Sequence[str]

    # These are internal values for the public raw_line/raw_column properties. They may
    # get backfilled later.
    _raw_line: int
    _raw_column: int

    def __init__(
        self,
        message: str,
        *,
        lines: Sequence[str],
        raw_line: int = -1,
        raw_column: int = -1,
    ) -> None:
        super(ParserSyntaxError, self).__init__(message)
        self.message = message
        self._lines = lines
        self._raw_line = raw_line
        self._raw_column = raw_column

    def __reduce__(
        self
    ) -> Tuple[Callable[..., "ParserSyntaxError"], Tuple[object, ...]]:
        return (
            _parser_syntax_error_unpickle,
            (
                {
                    "message": self.message,
                    "lines": self._lines,
                    "raw_line": self._raw_line,
                    "raw_column": self._raw_column,
                },
            ),
        )

    def __str__(self) -> str:
        """
        A human-readable error message of where the syntax error is in their code.
        """
        context = self.context
        return (
            f"Syntax Error @ {self.editor_line}:{self.editor_column}.\n"
            + f"{self.message}"
            + (f"\n\n{context}" if context is not None else "")
        )

    def __repr__(self) -> str:
        return (
            f"ParserSyntaxError("
            + f"{self.message!r}, lines=[...], raw_line={self._raw_line!r}, "
            + f"raw_column={self._raw_column!r})"
        )

    @property
    def context(self) -> str:
        displayed_line = self.editor_line
        displayed_column = self.editor_column

        formatted_source_line = expand_tabs(self._lines[displayed_line - 1]).rstrip(
            _NEWLINE_CHARS
        )
        # fmt: off
        return (
            f"{formatted_source_line}\n"
            + f"{' ' * (displayed_column - 1)}^"
        )
        # fmt: on

    @property
    def raw_line(self) -> int:
        # Internally we might initialize _raw_line to -1 when we don't know the line
        # number yet. This should never be exposed to the user, because we should
        # backfill this before the user can access it.
        assert self._raw_line >= 1, "ParserSyntaxError was not fully initialized"
        return self._raw_line

    @property
    def raw_column(self) -> int:
        # Internally we might initialize _raw_line to -1 when we don't know the line
        # number yet.
        assert self._raw_column >= 0, "ParserSyntaxError was not fully initialized"
        return self._raw_column

    @property
    def editor_line(self) -> int:
        """
        The one-indexed line in the user's editor.
        """
        return self.raw_line  # raw_line is already one-indexed.

    @property
    def editor_column(self) -> int:
        """
        The one-indexed column in the user's editor, assuming tabs expand to 1-8 spaces.
        """
        prefix_str = self._lines[self.raw_line - 1][: self.raw_column]
        tab_adjusted_column = len(expand_tabs(prefix_str))
        # Text editors use a one-indexed column, so we need to add one to our
        # zero-indexed column to get a human-readable result.
        return tab_adjusted_column + 1


class MetadataException(Exception):
    pass
