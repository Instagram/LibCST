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
    Union,
    cast,
    overload,
)

from libcst._add_slots import add_slots
from libcst._maybe_sentinel import MaybeSentinel
from libcst._removal_sentinel import RemovalSentinel
from libcst._types import CSTNodeT


if TYPE_CHECKING:
    # These are circular dependencies only used for typing purposes
    from libcst._nodes.base import CSTNode  # noqa: F401
    from libcst._visitors import CSTVisitorT
    from libcst.metadata.position_provider import (  # noqa: F401
        BasicPositionProvider,
        SyntacticPositionProvider,
        PositionProvider,
    )


_CodePositionT = Union[Tuple[int, int], "CodePosition"]


NEWLINE_RE: Pattern[str] = re.compile(r"\r\n?|\n")


@add_slots
@dataclass(frozen=True)
class CodePosition:
    #: Line numbers are 1-indexed.
    line: int
    #: Column numbers are 0-indexed.
    column: int


@add_slots
@dataclass(frozen=True)
class CodeRange:
    #: Starting position of a node (inclusive).
    start: CodePosition
    #: Ending position of a node (exclusive).
    end: CodePosition

    @overload
    def __init__(self, start: CodePosition, end: CodePosition) -> None:
        ...

    @overload
    def __init__(self, start: Tuple[int, int], end: Tuple[int, int]) -> None:
        ...

    def __init__(self, start: _CodePositionT, end: _CodePositionT) -> None:
        if isinstance(start, tuple) and isinstance(end, tuple):
            object.__setattr__(self, "start", CodePosition(start[0], start[1]))
            object.__setattr__(self, "end", CodePosition(end[0], end[1]))
        else:
            start = cast(CodePosition, start)
            end = cast(CodePosition, end)
            object.__setattr__(self, "start", start)
            object.__setattr__(self, "end", end)


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

    def record_position(self, node: CSTNodeT, position: CodeRange) -> None:
        pass

    @contextmanager
    def record_syntactic_position(
        self,
        node: CSTNodeT,
        *,
        start_node: Optional[CSTNodeT] = None,
        end_node: Optional[CSTNodeT] = None,
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

    def record_position(self, node: CSTNodeT, position: CodeRange) -> None:
        # Don't overwrite existing position information
        # (i.e. semantic position has already been recorded)
        if node not in self.provider._computed:
            self.provider._computed[node] = position


class SyntacticCodegenState(BasicCodegenState):
    """
    Pass to codegen to record the syntatic position of nodes.
    """

    @contextmanager
    def record_syntactic_position(
        self,
        node: CSTNodeT,
        *,
        start_node: Optional[CSTNodeT] = None,
        end_node: Optional[CSTNodeT] = None,
    ) -> Iterator[None]:
        start = CodePosition(self.line, self.column)
        try:
            yield
        finally:
            end = CodePosition(self.line, self.column)

            # Override with positions hoisted from child nodes if provided
            start = (
                self.provider._computed[start_node].start
                if start_node is not None
                else start
            )
            end = self.provider._computed[end_node].end if end_node is not None else end

            self.provider._computed[node] = CodeRange(start, end)


def visit_required(
    parent: "CSTNode", fieldname: str, node: CSTNodeT, visitor: "CSTVisitorT"
) -> CSTNodeT:
    """
    Given a node, visits the node using `visitor`. If removal is attempted by the
    visitor, an exception is raised.
    """
    visitor.on_visit_attribute(parent, fieldname)
    result = node.visit(visitor)
    if isinstance(result, RemovalSentinel):
        raise TypeError(
            f"We got a RemovalSentinel while visiting a {type(node).__name__}. This "
            + "node's parent does not allow it to be removed."
        )
    visitor.on_leave_attribute(parent, fieldname)
    return result


def visit_optional(
    parent: "CSTNode", fieldname: str, node: Optional[CSTNodeT], visitor: "CSTVisitorT"
) -> Optional[CSTNodeT]:
    """
    Given an optional node, visits the node if it exists with `visitor`. If the node is
    removed, returns None.
    """
    if node is None:
        visitor.on_visit_attribute(parent, fieldname)
        visitor.on_leave_attribute(parent, fieldname)
        return None
    visitor.on_visit_attribute(parent, fieldname)
    result = node.visit(visitor)
    visitor.on_leave_attribute(parent, fieldname)
    return None if isinstance(result, RemovalSentinel) else result


def visit_sentinel(
    parent: "CSTNode",
    fieldname: str,
    node: Union[CSTNodeT, MaybeSentinel],
    visitor: "CSTVisitorT",
) -> Union[CSTNodeT, MaybeSentinel]:
    """
    Given a node that can be a real value or a sentinel value, visits the node if it
    is real with `visitor`. If the node is removed, returns MaybeSentinel.
    """
    if isinstance(node, MaybeSentinel):
        visitor.on_visit_attribute(parent, fieldname)
        visitor.on_leave_attribute(parent, fieldname)
        return MaybeSentinel.DEFAULT
    visitor.on_visit_attribute(parent, fieldname)
    result = node.visit(visitor)
    visitor.on_leave_attribute(parent, fieldname)
    return MaybeSentinel.DEFAULT if isinstance(result, RemovalSentinel) else result


def visit_iterable(
    parent: "CSTNode",
    fieldname: str,
    children: Iterable[CSTNodeT],
    visitor: "CSTVisitorT",
) -> Iterable[CSTNodeT]:
    """
    Given an iterable of children, visits each child with `visitor`, and yields the new
    children with any `RemovalSentinel` values removed.
    """
    visitor.on_visit_attribute(parent, fieldname)
    for child in children:
        new_child = child.visit(visitor)
        if not isinstance(new_child, RemovalSentinel):
            yield new_child
    visitor.on_leave_attribute(parent, fieldname)


def visit_sequence(
    parent: "CSTNode",
    fieldname: str,
    children: Sequence[CSTNodeT],
    visitor: "CSTVisitorT",
) -> Sequence[CSTNodeT]:
    """
    A convenience wrapper for `visit_iterable` that returns a sequence instead of an
    iterable.
    """
    return tuple(visit_iterable(parent, fieldname, children, visitor))


def visit_body_iterable(
    parent: "CSTNode",
    fieldname: str,
    children: Sequence[CSTNodeT],
    visitor: "CSTVisitorT",
) -> Iterable[CSTNodeT]:
    """
    Similar to visit_iterable above, but capable of discarding empty SimpleStatementLine
    nodes in order to preserve correct pass insertion behavior.
    """

    visitor.on_visit_attribute(parent, fieldname)
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
    visitor.on_leave_attribute(parent, fieldname)


def visit_body_sequence(
    parent: "CSTNode",
    fieldname: str,
    children: Sequence[CSTNodeT],
    visitor: "CSTVisitorT",
) -> Sequence[CSTNodeT]:
    """
    A convenience wrapper for `visit_body_iterable` that returns a sequence
    instead of an iterable.
    """
    return tuple(visit_body_iterable(parent, fieldname, children, visitor))
