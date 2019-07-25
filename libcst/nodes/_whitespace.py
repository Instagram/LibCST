# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import re
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, Pattern, Sequence

from libcst._add_slots import add_slots
from libcst._visitors import CSTVisitorT
from libcst.nodes._base import BaseLeaf, BaseValueToken, CSTNode, CSTValidationError
from libcst.nodes._internal import (
    CodegenState,
    visit_optional,
    visit_required,
    visit_sequence,
)


# SimpleWhitespace includes continuation characters, which must be followed immediately
# by a newline. SimpleWhitespace does not include other kinds of newlines, because those
# may have semantic significance.
SIMPLE_WHITESPACE_RE: Pattern[str] = re.compile(r"([ \f\t]|\\(\r\n?|\n))*", re.UNICODE)
NEWLINE_RE: Pattern[str] = re.compile(r"\r\n?|\n", re.UNICODE)
COMMENT_RE: Pattern[str] = re.compile(r"#[^\r\n]*", re.UNICODE)


class BaseParenthesizableWhitespace(CSTNode, ABC):
    """
    This is the kind of whitespace you might see inside the body of a statement or
    expression between two tokens. This is the most common type of whitespace.

    The list of allowed characters in a whitespace depends on whether it is found
    inside a parentesized expression or not. This class allows nodes which can be
    found inside or outside a (), [] or {} section to accept either whitespace
    form.

    https://docs.python.org/3/reference/lexical_analysis.html#implicit-line-joining

    ParenthesizableWhitespace may contain a backslash character (`\\`), when used as a
    line-continuation character. While the continuation character isn't technically
    "whitespace", it serves the same purpose.

    ParenthesizableWhitespace is often non-semantic (optional), but in cases where whitespace
    solves a grammar ambiguity between tokens (e.g. `if test`, versus `iftest`), it has
    some semantic value.
    """

    # TODO: Should we somehow differentiate places where we require non-zero whitespace
    # with a separate type?

    @property
    @abstractmethod
    def empty(self) -> bool:
        ...


@add_slots
@dataclass(frozen=True)
class SimpleWhitespace(BaseParenthesizableWhitespace, BaseValueToken):

    value: str

    def _validate(self) -> None:
        if SIMPLE_WHITESPACE_RE.fullmatch(self.value) is None:
            raise CSTValidationError(
                f"Got non-whitespace value for whitespace node: {repr(self.value)}"
            )

    @property
    def empty(self) -> bool:
        return len(self.value) == 0


@add_slots
@dataclass(frozen=True)
class Newline(BaseLeaf):
    """
    Represents the newline that ends an EmptyLine or a statement (as part of
    TrailingWhitespace).

    Other newlines may occur in the document after continuation characters (the
    backslash, `\\`), but those newlines are treated as part of the SimpleWhitespace.
    """

    # A value of 'None' indicates that the module's default newline sequence should be
    # used. A value is allowed only because python modules are permitted to mix multiple
    # unambiguous newline markers.
    value: Optional[str] = None

    def _validate(self) -> None:
        if self.value and NEWLINE_RE.fullmatch(self.value) is None:
            raise CSTValidationError(
                f"Got an invalid value for newline node: {repr(self.value)}"
            )

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token(state.default_newline if self.value is None else self.value)


@add_slots
@dataclass(frozen=True)
class Comment(BaseValueToken):
    """
    A comment including the leading pound (`#`) character.

    The leading pound character is included in the 'value' property (instead of being
    stripped) to help re-enforce the idea that whitespace immediately after the pound
    character may be significant. E.g:

        # comment with whitespace at the start (usually preferred), versus
        #comment without whitespace at the start (usually not desirable)

    Usually wrapped in a TrailingWhitespace or EmptyLine node.
    """

    value: str

    def _validate(self) -> None:
        if COMMENT_RE.fullmatch(self.value) is None:
            raise CSTValidationError(
                f"Got non-comment value for comment node: {repr(self.value)}"
            )


@add_slots
@dataclass(frozen=True)
class TrailingWhitespace(CSTNode):
    """
    The whitespace at the end of a line after a statement. If a line contains only
    whitespace, EmptyLine should be used instead.
    """

    whitespace: SimpleWhitespace = SimpleWhitespace("")
    comment: Optional[Comment] = None
    newline: Newline = Newline()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "TrailingWhitespace":
        return TrailingWhitespace(
            whitespace=visit_required("whitespace", self.whitespace, visitor),
            comment=visit_optional("comment", self.comment, visitor),
            newline=visit_required("newline", self.newline, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace._codegen(state)
        if self.comment is not None:
            self.comment._codegen(state)
        self.newline._codegen(state)


@add_slots
@dataclass(frozen=True)
class EmptyLine(CSTNode):
    """
    Represents a line with only whitespace/comments. Usually statements will own any
    EmptyLine nodes above themselves, and a Module will own the document's header/footer
    EmptyLine nodes.
    """

    # An empty line doesn't have to correspond to the current indentation level. For
    # example, this happens when all trailing whitespace is stripped.
    indent: bool = True
    # Extra whitespace after the indent, but before the comment
    whitespace: SimpleWhitespace = SimpleWhitespace("")
    comment: Optional[Comment] = None
    newline: Newline = Newline()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "EmptyLine":
        return EmptyLine(
            indent=self.indent,
            whitespace=visit_required("whitespace", self.whitespace, visitor),
            comment=visit_optional("comment", self.comment, visitor),
            newline=visit_required("newline", self.newline, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        if self.indent:
            state.add_indent_tokens()
        self.whitespace._codegen(state)
        if self.comment is not None:
            self.comment._codegen(state)
        self.newline._codegen(state)


@add_slots
@dataclass(frozen=True)
class ParenthesizedWhitespace(BaseParenthesizableWhitespace):

    # The whitespace that comes after the previous node, up to and including
    # the end-of-line comment.
    first_line: TrailingWhitespace = TrailingWhitespace()

    # Any lines that contain only indentation and/or comments
    empty_lines: Sequence[EmptyLine] = ()

    # Whether or not the final comment is indented regularly
    indent: bool = False

    # Extra whitespace after the indent, but before the next node
    last_line: SimpleWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "ParenthesizedWhitespace":
        return ParenthesizedWhitespace(
            first_line=visit_required("first_line", self.first_line, visitor),
            empty_lines=visit_sequence("empty_lines", self.empty_lines, visitor),
            indent=self.indent,
            last_line=visit_required("last_line", self.last_line, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.first_line._codegen(state)
        for line in self.empty_lines:
            line._codegen(state)
        if self.indent:
            state.add_indent_tokens()
        self.last_line._codegen(state)

    @property
    def empty(self) -> bool:
        # Its not possible to have a ParenthesizedWhitespace with zero characers.
        # If we did, the TrailingWhitespace would not have parsed.
        return False
