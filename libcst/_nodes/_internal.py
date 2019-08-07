# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import re
from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import (
    TYPE_CHECKING,
    Iterable,
    Iterator,
    List,
    Optional,
    Pattern,
    Sequence,
    Tuple,
    TypeVar,
    Union,
)

from libcst._add_slots import add_slots
from libcst._maybe_sentinel import MaybeSentinel
from libcst._removal_sentinel import RemovalSentinel


if TYPE_CHECKING:
    # These are circular dependencies only used for typing purposes
    from libcst._nodes._base import CSTNode  # noqa: F401
    from libcst._visitors import CSTVisitorT
    from libcst.metadata.position_provider import (  # noqa: F401
        BasicPositionProvider,
        SyntacticPositionProvider,
        PositionProvider,
    )


_CSTNodeT = TypeVar("_CSTNodeT", bound="CSTNode")
_CodePositionT = Union[Tuple[int, int], "CodePosition"]


NEWLINE_RE: Pattern[str] = re.compile(r"\r\n?|\n")


@add_slots
@dataclass(frozen=True)
class CodePosition:
    line: int
    column: int


@add_slots
@dataclass(frozen=True)
class CodeRange:
    start: CodePosition
    end: CodePosition

    @classmethod
    def create(cls, start: Tuple[int, int], end: Tuple[int, int]) -> "CodeRange":
        return CodeRange(CodePosition(start[0], start[1]), CodePosition(end[0], end[1]))


@dataclass(frozen=False)
class CodegenState:
    # These are derived from a Module
    default_indent: str
    default_newline: str

    provider: Optional["PositionProvider"]

    indent_tokens: List[str] = field(default_factory=list)
    tokens: List[str] = field(default_factory=list)

    line: int = 1  # one-indexed
    column: int = 0  # zero-indexed

    def increase_indent(self, value: str) -> None:
        self.indent_tokens.append(value)

    def decrease_indent(self) -> None:
        self.indent_tokens.pop()

    def add_indent_tokens(self) -> None:
        self.tokens.extend(self.indent_tokens)

    def add_token(self, value: str) -> None:
        self.tokens.append(value)

    def record_position(self, node: _CSTNodeT, position: CodeRange) -> None:
        pass

    @contextmanager
    def record_syntactic_position(
        self,
        node: _CSTNodeT,
        *,
        start_node: Optional[_CSTNodeT] = None,
        end_node: Optional[_CSTNodeT] = None,
    ) -> Iterator[None]:
        yield


@dataclass(frozen=False)
class BasicCodegenState(CodegenState):
    """
    Pass to codegen to record the basic position of nodes.
    """

    provider: "PositionProvider"

    def add_indent_tokens(self) -> None:
        self.tokens.extend(self.indent_tokens)
        for token in self.indent_tokens:
            self._update_position(token)

    def add_token(self, value: str) -> None:
        self.tokens.append(value)
        self._update_position(value)

    def _update_position(self, value: str) -> None:
        """
        Computes new line and column numbers from adding the token [value].
        """
        segments = NEWLINE_RE.split(value)
        if len(segments) == 1:  # contains no newlines
            # no change to self.lines
            self.column += len(value)
        else:
            self.line += len(segments) - 1
            # newline resets column back to 0, but a trailing token may shift column
            self.column = len(segments[-1])

    def record_position(self, node: _CSTNodeT, position: CodeRange) -> None:
        # Don't overwrite existing position information
        # (i.e. semantic position has already been recorded)
        if node not in self.provider._computed:
            self.provider._computed[node] = position

        # TODO: remove this
        if type(self.provider) not in node._metadata:
            node._metadata[type(self.provider)] = position


class SyntacticCodegenState(BasicCodegenState):
    """
    Pass to codegen to record the syntatic position of nodes.
    """

    @contextmanager
    def record_syntactic_position(
        self,
        node: _CSTNodeT,
        *,
        start_node: Optional[_CSTNodeT] = None,
        end_node: Optional[_CSTNodeT] = None,
    ) -> Iterator[None]:
        start = CodePosition(self.line, self.column)
        try:
            yield
        finally:
            end = CodePosition(self.line, self.column)

            # Override with positions hoisted from child nodes if provided
            start = (
                start_node._metadata[type(self.provider)].start
                if start_node is not None
                else start
            )
            end = (
                end_node._metadata[type(self.provider)].end
                if end_node is not None
                else end
            )

            self.provider._computed[node] = CodeRange(start, end)

            # TODO: remove this
            node._metadata[type(self.provider)] = CodeRange(start, end)


def visit_required(
    fieldname: str, node: _CSTNodeT, visitor: "CSTVisitorT"
) -> _CSTNodeT:
    """
    Given a node, visits the node using `visitor`. If removal is attempted by the
    visitor, an exception is raised.
    """
    result = node.visit(visitor)
    if isinstance(result, RemovalSentinel):
        raise TypeError(
            f"We got a RemovalSentinel while visiting a {type(node).__name__}. This "
            + "node's parent does not allow it to be removed."
        )
    return result


def visit_optional(
    fieldname: str, node: Optional[_CSTNodeT], visitor: "CSTVisitorT"
) -> Optional[_CSTNodeT]:
    """
    Given an optional node, visits the node if it exists with `visitor`. If the node is
    removed, returns None.
    """
    if node is None:
        return None
    result = node.visit(visitor)
    return None if isinstance(result, RemovalSentinel) else result


def visit_sentinel(
    fieldname: str, node: Union[_CSTNodeT, MaybeSentinel], visitor: "CSTVisitorT"
) -> Union[_CSTNodeT, MaybeSentinel]:
    """
    Given a node that can be a real value or a sentinel value, visits the node if it
    is real with `visitor`. If the node is removed, returns MaybeSentinel.
    """
    if isinstance(node, MaybeSentinel):
        return MaybeSentinel.DEFAULT
    result = node.visit(visitor)
    return MaybeSentinel.DEFAULT if isinstance(result, RemovalSentinel) else result


def visit_iterable(
    fieldname: str, children: Iterable[_CSTNodeT], visitor: "CSTVisitorT"
) -> Iterable[_CSTNodeT]:
    """
    Given an iterable of children, visits each child with `visitor`, and yields the new
    children with any `RemovalSentinel` values removed.
    """
    for child in children:
        new_child = child.visit(visitor)
        if not isinstance(new_child, RemovalSentinel):
            yield new_child


def visit_sequence(
    fieldname: str, children: Sequence[_CSTNodeT], visitor: "CSTVisitorT"
) -> Sequence[_CSTNodeT]:
    """
    A convenience wrapper for `visit_iterable` that returns a sequence instead of an
    iterable.
    """
    return tuple(visit_iterable(fieldname, children, visitor))


def visit_body_iterable(
    fieldname: str, children: Sequence[_CSTNodeT], visitor: "CSTVisitorT"
) -> Iterable[_CSTNodeT]:
    """
    Similar to visit_iterable above, but capable of discarding empty SimpleStatementLine
    nodes in order to preserve correct pass insertion behavior.
    """

    for child in children:
        new_child = child.visit(visitor)

        # Don't yield a child if we removed it.
        if isinstance(new_child, RemovalSentinel):
            continue

        # Don't yield a child if the old child wasn't empty
        # and the new child is. This means a RemovalSentinel
        # caused a child of this node to be dropped, and it
        # is now useless.
        if (not child._is_removable()) and new_child._is_removable():
            continue

        # Safe to yield child in this case.
        yield new_child


def visit_body_sequence(
    fieldname: str, children: Sequence[_CSTNodeT], visitor: "CSTVisitorT"
) -> Sequence[_CSTNodeT]:
    """
    A convenience wrapper for `visit_body_iterable` that returns a sequence
    instead of an iterable.
    """
    return tuple(visit_body_iterable(fieldname, children, visitor))
