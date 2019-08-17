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
    Contains an error encountered while trying to parse a piece of source code. This
    exception shouldn't be constructed directly by the user, but instead may be raised
    by calls to :func:`parse_module`, :func:`parse_expression`, or
    :func:`parse_statement`.

    This does not inherit from :class:`SyntaxError` because Python's may raise a
    :class:`SyntaxError` for any number of reasons, potentially leading to unintended
    behavior.
    """

    #: A human-readable explanation of the syntax error without information about where
    #: the error occurred.
    #:
    #: For a human-readable explanation of the error alongside information about where
    #: it occurred, use :meth:`__str__` (via ``str(ex)``) instead.
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
        A multi-line human-readable error message of where the syntax error is in their
        code. For example::

            Syntax Error @ 2:1.
            Incomplete input. Encountered end of file (EOF), but expected 'except', or 'finally'.

            try: pass
                     ^
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
        """
        A formatted string containing the line of code with the syntax error along with
        a caret indicating the exact column where the error occured.
        """
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
        """
        The one-indexed line where the error occured.
        """
        # Internally we might initialize _raw_line to -1 when we don't know the line
        # number yet. This should never be exposed to the user, because we should
        # backfill this before the user can access it.
        assert self._raw_line >= 1, "ParserSyntaxError was not fully initialized"
        return self._raw_line

    @property
    def raw_column(self) -> int:
        """
        The zero-indexed column as a number of characters from the start of the line
        where the error occured.
        """
        # Internally we might initialize _raw_line to -1 when we don't know the line
        # number yet.
        assert self._raw_column >= 0, "ParserSyntaxError was not fully initialized"
        return self._raw_column

    @property
    def editor_line(self) -> int:
        """
        The expected one-indexed line in the user's editor. This is the same as
        :attr:`raw_line`.
        """
        return self.raw_line  # raw_line is already one-indexed.

    @property
    def editor_column(self) -> int:
        """
        The expected one-indexed column that's likely to match the behavior of the
        user's editor, assuming tabs expand to 1-8 spaces. This is the column number
        shown when the syntax error is printed out with `str`.

        This assumes single-width characters. However, because python doesn't ship with
        a wcwidth function, it's hard to handle this properly without a third-party
        dependency.

        For a raw zero-indexed character offset without tab expansion, see
        :attr:`raw_column`.
        """
        prefix_str = self._lines[self.raw_line - 1][: self.raw_column]
        tab_adjusted_column = len(expand_tabs(prefix_str))
        # Text editors use a one-indexed column, so we need to add one to our
        # zero-indexed column to get a human-readable result.
        return tab_adjusted_column + 1


class MetadataException(Exception):
    pass
