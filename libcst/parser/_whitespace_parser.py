# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
"""
Parso doesn't attempt to parse (or even emit tokens for) whitespace or comments that
isn't syntatically important. Instead, we're just given the whitespace as a "prefix" of
the token.

However, in our CST, whitespace is gathered into far more detailed objects than a simple
str.

Fortunately this isn't hard for us to parse ourselves, so we just use our own
hand-rolled recursive descent parser.
"""

from typing import Optional, Sequence, Union

import libcst.nodes as cst
from libcst.nodes._whitespace import COMMENT_RE, NEWLINE_RE, SIMPLE_WHITESPACE_RE
from libcst.parser._types.config import BaseWhitespaceParserConfig
from libcst.parser._types.whitespace_state import WhitespaceState as State


# BEGIN PARSER ENTRYPOINTS


def parse_simple_whitespace(
    config: BaseWhitespaceParserConfig, state: State
) -> cst.SimpleWhitespace:
    # The match never fails because the pattern can match an empty string
    lines = config.lines
    # pyre-fixme[16]: Optional type has no attribute `group`.
    ws_line = SIMPLE_WHITESPACE_RE.match(lines[state.line - 1], state.column).group(0)
    ws_line_list = [ws_line]
    while "\\" in ws_line:
        # continuation character
        state.line += 1
        state.column = 0
        # pyre-fixme[16]: Optional type has no attribute `group`.
        ws_line = SIMPLE_WHITESPACE_RE.match(lines[state.line - 1], state.column).group(
            0
        )
        ws_line_list.append(ws_line)

    # TODO: we could special-case the common case where there's no continuation
    # character to avoid list construction and joining.

    # once we've finished collecting continuation characters
    state.column += len(ws_line)
    return cst.SimpleWhitespace("".join(ws_line_list))


def parse_empty_lines(
    config: BaseWhitespaceParserConfig, state: State
) -> Sequence[cst.EmptyLine]:
    result = []
    el = _parse_empty_line(config, state)
    while el is not None:
        result.append(el)
        el = _parse_empty_line(config, state)
    return result


def parse_trailing_whitespace(
    config: BaseWhitespaceParserConfig, state: State
) -> cst.TrailingWhitespace:
    trailing_whitespace = _parse_trailing_whitespace(config, state)
    if trailing_whitespace is None:
        raise Exception(
            "Internal Error: Failed to parse TrailingWhitespace. This should never "
            + "happen because a TrailingWhitespace is never optional in the grammar, "
            + "so this error should've been caught by parso first."
        )
    return trailing_whitespace


def parse_parenthesizable_whitespace(
    config: BaseWhitespaceParserConfig, state: State
) -> Union[cst.SimpleWhitespace, cst.ParenthesizedWhitespace]:
    if state.is_parenthesized:
        # First, try parenthesized (don't need speculation because it either
        # parses or doesn't modify state).
        parenthesized_whitespace = _parse_parenthesized_whitespace(config, state)
        if parenthesized_whitespace is not None:
            return parenthesized_whitespace
    # Now, just parse and return a simple whitespace
    return parse_simple_whitespace(config, state)


# END PARSER ENTRYPOINTS
# BEGIN PARSER INTERNAL PRODUCTIONS


def _parse_empty_line(
    config: BaseWhitespaceParserConfig, state: State
) -> Optional[cst.EmptyLine]:
    # begin speculative parsing
    speculative_state = State(
        state.line, state.column, state.absolute_indent, state.is_parenthesized
    )
    indent = _parse_indent(config, speculative_state)
    whitespace = parse_simple_whitespace(config, speculative_state)
    comment = _parse_comment(config, speculative_state)
    newline = _parse_newline(config, speculative_state)
    if newline is None:
        # speculative parsing failed
        return None
    # speculative parsing succeeded
    state.line = speculative_state.line
    state.column = speculative_state.column
    # don't need to copy absolute_indent/is_parenthesized because they don't change.
    return cst.EmptyLine(indent, whitespace, comment, newline)


def _parse_indent(config: BaseWhitespaceParserConfig, state: State) -> bool:
    """
    Returns True if indentation was found, otherwise False.
    """
    absolute_indent = state.absolute_indent
    line_str = config.lines[state.line - 1]
    if state.column != 0:
        if state.column == len(line_str) and state.line == len(config.lines):
            # We're at EOF, treat this as a failed speculative parse
            return False
        raise Exception("Internal Error: Column should be 0 when parsing an indent.")
    if line_str.startswith(absolute_indent, state.column):
        state.column += len(absolute_indent)
        return True
    return False


def _parse_comment(
    config: BaseWhitespaceParserConfig, state: State
) -> Optional[cst.Comment]:
    comment_match = COMMENT_RE.match(config.lines[state.line - 1], state.column)
    if comment_match is None:
        return None
    comment = comment_match.group(0)
    state.column += len(comment)
    return cst.Comment(comment)


def _parse_newline(
    config: BaseWhitespaceParserConfig, state: State
) -> Optional[cst.Newline]:
    # begin speculative parsing
    line_str = config.lines[state.line - 1]
    newline_match = NEWLINE_RE.match(line_str, state.column)
    if newline_match is not None:
        # speculative parsing succeeded
        newline_str = newline_match.group(0)
        state.column += len(newline_str)
        if state.column != len(line_str):
            raise Exception("Internal Error: Found a newline, but it wasn't the EOL.")
        if state.line < len(config.lines):
            # this newline was the end of a line, and there's another line,
            # therefore we should move to the next line
            state.line += 1
            state.column = 0
        if newline_str == config.default_newline:
            # Just inherit it from the Module instead of explicitly setting it.
            return cst.Newline()
        else:
            return cst.Newline(newline_str)
    else:  # no newline was found, speculative parsing failed
        return None


def _parse_trailing_whitespace(
    config: BaseWhitespaceParserConfig, state: State
) -> Optional[cst.TrailingWhitespace]:
    # Begin speculative parsing
    speculative_state = State(
        state.line, state.column, state.absolute_indent, state.is_parenthesized
    )
    whitespace = parse_simple_whitespace(config, speculative_state)
    comment = _parse_comment(config, speculative_state)
    newline = _parse_newline(config, speculative_state)
    if newline is None:
        # Speculative parsing failed
        return None
    # Speculative parsing succeeded
    state.line = speculative_state.line
    state.column = speculative_state.column
    # don't need to copy absolute_indent/is_parenthesized because they don't change.
    return cst.TrailingWhitespace(whitespace, comment, newline)


def _parse_parenthesized_whitespace(
    config: BaseWhitespaceParserConfig, state: State
) -> Optional[cst.ParenthesizedWhitespace]:
    first_line = _parse_trailing_whitespace(config, state)
    if first_line is None:
        # Speculative parsing failed
        return None
    empty_lines = ()
    while True:
        empty_line = _parse_empty_line(config, state)
        if empty_line is None:
            # This isn't an empty line, so parse it below
            break
        empty_lines = empty_lines + (empty_line,)
    indent = _parse_indent(config, state)
    last_line = parse_simple_whitespace(config, state)
    return cst.ParenthesizedWhitespace(first_line, empty_lines, indent, last_line)
