# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Iterable, Optional, Sequence, Tuple

from libcst._tabs import expand_tabs


_EOF_STR: str = "end of file (EOF)"
_NEWLINE_CHARS: str = "\r\n"


class ParserSyntaxError(Exception):
    """
    Contains error information about the parser tree.
    """

    def __init__(
        self,
        message: str,
        encountered: Optional[str],  # None means EOF
        expected: Optional[Iterable[str]],  # None means EOF
        pos: Tuple[int, int],  # (one-indexed line, zero-indexed column)
        lines: Sequence[str],  # source code, used to generate a human-readable output
    ) -> None:
        self.message = message
        self.encountered = encountered
        self.expected = expected
        self.pos = pos
        self.lines = lines
        # Make pickling this error work
        super(ParserSyntaxError, self).__init__(
            message, encountered, expected, pos, lines
        )

    def __str__(self) -> str:
        """
        A human-readable error message of where the syntax error is in their code.
        """
        if self.encountered is not None:
            encountered_str = repr(self.encountered)
        else:
            encountered_str = _EOF_STR

        if self.expected is not None:
            expected_str = f"one of {repr(list(self.expected))}"
        else:
            expected_str = _EOF_STR

        pos_line, pos_column = self.pos
        editor_line = self.editor_line
        editor_column = self.editor_column

        return (
            f"Syntax Error: {self.message} @ {editor_line}:{editor_column}.\n"
            + f"Encountered {encountered_str}, but expected {expected_str}.\n\n"
            + f"{expand_tabs(self.lines[pos_line - 1]).rstrip(_NEWLINE_CHARS)}\n"
            + f"{' ' * (editor_column - 1)}^"
        )

    @property
    def editor_line(self) -> int:
        """
        The one-indexed line in the user's editor.
        """
        return self.pos[0]  # the line in pos is already one-indexed.

    @property
    def editor_column(self) -> int:
        """
        The one-indexed column in the user's editor, assuming tabs expand to 1-8 spaces.
        """
        pos_line, pos_column = self.pos
        tab_adjusted_column = len(expand_tabs(self.lines[pos_line - 1][:pos_column]))
        # Text editors use a one-indexed column, so we need to add one to our
        # zero-indexed column to get a human-readable result.
        return tab_adjusted_column + 1


class MetadataException(Exception):
    pass
