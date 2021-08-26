# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import inspect
import re
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, Pattern, Sequence, Union

from libcst._add_slots import add_slots
from libcst._maybe_sentinel import MaybeSentinel
from libcst._nodes.base import CSTNode, CSTValidationError
from libcst._nodes.expression import (
    Annotation,
    Arg,
    Asynchronous,
    Attribute,
    BaseAssignTargetExpression,
    BaseDelTargetExpression,
    BaseExpression,
    Call,
    ConcatenatedString,
    ExpressionPosition,
    From,
    LeftParen,
    List,
    Name,
    Parameters,
    RightParen,
    SimpleString,
    Tuple,
)
from libcst._nodes.internal import (
    CodegenState,
    visit_body_sequence,
    visit_optional,
    visit_required,
    visit_sentinel,
    visit_sequence,
)
from libcst._nodes.op import AssignEqual, BaseAugOp, Comma, Dot, ImportStar, Semicolon
from libcst._nodes.whitespace import (
    BaseParenthesizableWhitespace,
    EmptyLine,
    SimpleWhitespace,
    TrailingWhitespace,
)
from libcst._visitors import CSTVisitorT

_INDENT_WHITESPACE_RE: Pattern[str] = re.compile(r"[ \f\t]+", re.UNICODE)


class BaseSuite(CSTNode, ABC):
    """
    A dummy base-class for both :class:`SimpleStatementSuite` and :class:`IndentedBlock`.
    This exists to simplify type definitions and isinstance checks.

        A suite is a group of statements controlled by a clause. A suite can be one or
        more semicolon-separated simple statements on the same line as the header,
        following the header’s colon, or it can be one or more indented statements on
        subsequent lines.

        -- https://docs.python.org/3/reference/compound_stmts.html
    """

    body: Union[Sequence["BaseStatement"], Sequence["BaseSmallStatement"]]


class BaseStatement(CSTNode, ABC):
    """
    A class that exists to allow for typing to specify that any statement is allowed
    in a particular location.
    """

    pass


class BaseSmallStatement(CSTNode, ABC):
    """
    Encapsulates a small statement, like ``del`` or ``pass``, and optionally adds a
    trailing semicolon. A small statement is always contained inside a
    :class:`SimpleStatementLine` or :class:`SimpleStatementSuite`. This exists to
    simplify type definitions and isinstance checks.
    """

    #: An optional semicolon that appears after a small statement. This is optional
    #: for the last small statement in a :class:`SimpleStatementLine` or
    #: :class:`SimpleStatementSuite`, but all other small statements inside a simple
    #: statement must contain a semicolon to disambiguate multiple small statements
    #: on the same line.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    @abstractmethod
    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        ...


@add_slots
@dataclass(frozen=True)
class Del(BaseSmallStatement):
    """
    Represents a ``del`` statement. ``del`` is always followed by a target.
    """

    #: The target expression will be deleted. This can be a name, a tuple,
    #: an item of a list, an item of a dictionary, or an attribute.
    target: BaseDelTargetExpression

    #: The whitespace after the ``del`` keyword.
    whitespace_after_del: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if (
            self.whitespace_after_del.empty
            and not self.target._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            )
        ):
            raise CSTValidationError("Must have at least one space after 'del'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Del":
        return Del(
            target=visit_required(self, "target", self.target, visitor),
            whitespace_after_del=visit_required(
                self, "whitespace_after_del", self.whitespace_after_del, visitor
            ),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("del")
            self.whitespace_after_del._codegen(state)
            self.target._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Pass(BaseSmallStatement):
    """
    Represents a ``pass`` statement.
    """

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Pass":
        return Pass(
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor)
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("pass")

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Break(BaseSmallStatement):
    """
    Represents a ``break`` statement, which is used to break out of a :class:`For`
    or :class:`While` loop early.
    """

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Break":
        return Break(
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor)
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("break")

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Continue(BaseSmallStatement):
    """
    Represents a ``continue`` statement, which is used to skip to the next iteration
    in a :class:`For` or :class:`While` loop.
    """

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Continue":
        return Continue(
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor)
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("continue")

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Return(BaseSmallStatement):
    """
    Represents a ``return`` or a ``return x`` statement.
    """

    #: The optional expression that will be evaluated and returned.
    value: Optional[BaseExpression] = None

    #: Optional whitespace after the ``return`` keyword before the optional
    #: value expression.
    whitespace_after_return: Union[
        SimpleWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        value = self.value
        if value is not None:
            whitespace_after_return = self.whitespace_after_return
            has_no_gap = (
                not isinstance(whitespace_after_return, MaybeSentinel)
                and whitespace_after_return.empty
            )
            if has_no_gap and not value._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            ):
                raise CSTValidationError("Must have at least one space after 'return'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Return":
        return Return(
            whitespace_after_return=visit_sentinel(
                self, "whitespace_after_return", self.whitespace_after_return, visitor
            ),
            value=visit_optional(self, "value", self.value, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("return")
            whitespace_after_return = self.whitespace_after_return
            value = self.value
            if isinstance(whitespace_after_return, MaybeSentinel):
                if value is not None:
                    state.add_token(" ")
            else:
                whitespace_after_return._codegen(state)
            if value is not None:
                value._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Expr(BaseSmallStatement):
    """
    An expression used as a statement, where the result is unused and unassigned.
    The most common place you will find this is in function calls where the return
    value is unneeded.
    """

    #: The expression itself. Python will evaluate the expression but not assign
    #: the result anywhere.
    value: BaseExpression

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Expr":
        return Expr(
            value=visit_required(self, "value", self.value, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            self.value._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


class _BaseSimpleStatement(CSTNode, ABC):
    """
    A simple statement is a series of small statements joined together by semicolons.

        simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE

    Whitespace between each small statement is owned by the small statements themselves.
    It can be found on the required semicolon that will be attached to each non-terminal
    small statement.
    """

    #: Sequence of small statements. All but the last statement are required to have
    #: a semicolon.
    body: Sequence[BaseSmallStatement]

    #: Any trailing comment and the final ``NEWLINE``, which is part of small statement's
    #: grammar.
    trailing_whitespace: TrailingWhitespace

    def _validate(self) -> None:
        body = self.body
        for small_stmt in body[:-1]:
            if small_stmt.semicolon is None:
                raise CSTValidationError(
                    "All but the last SmallStatement in a SimpleStatementLine or "
                    + "SimpleStatementSuite must have a trailing semicolon. Otherwise, "
                    + "there's no way to syntatically disambiguate each SmallStatement "
                    + "on the same line."
                )

    def _codegen_impl(self, state: CodegenState) -> None:
        body = self.body
        if body:
            laststmt = len(body) - 1
            with state.record_syntactic_position(self, end_node=body[laststmt]):
                for idx, stmt in enumerate(body):
                    stmt._codegen(state, default_semicolon=(idx != laststmt))
        else:
            # Empty simple statement blocks are not syntactically valid in Python
            # unless they contain a 'pass' statement, so add one here.
            with state.record_syntactic_position(self):
                state.add_token("pass")

        self.trailing_whitespace._codegen(state)


@add_slots
@dataclass(frozen=True)
class SimpleStatementLine(_BaseSimpleStatement, BaseStatement):
    """
    A simple statement that's part of an IndentedBlock or Module. A simple statement is
    a series of small statements joined together by semicolons.

    This isn't differentiated from a :class:`SimpleStatementSuite` in the grammar, but
    because a :class:`SimpleStatementLine` can own additional whitespace that a
    :class:`SimpleStatementSuite` doesn't have, we're differentiating it in the CST.
    """

    #: Sequence of small statements. All but the last statement are required to have
    #: a semicolon.
    body: Sequence[BaseSmallStatement]

    #: Sequence of empty lines appearing before this simple statement line.
    leading_lines: Sequence[EmptyLine] = ()

    #: Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    trailing_whitespace: TrailingWhitespace = TrailingWhitespace.field()

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "SimpleStatementLine":
        return SimpleStatementLine(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            body=visit_sequence(self, "body", self.body, visitor),
            trailing_whitespace=visit_required(
                self, "trailing_whitespace", self.trailing_whitespace, visitor
            ),
        )

    def _is_removable(self) -> bool:
        # If we have an empty body, we are removable since we don't represent
        # anything concrete.
        return not self.body

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()
        _BaseSimpleStatement._codegen_impl(self, state)


@add_slots
@dataclass(frozen=True)
class SimpleStatementSuite(_BaseSimpleStatement, BaseSuite):
    """
    A simple statement that's used as a suite. A simple statement is a series of small
    statements joined together by semicolons. A suite is the thing that follows the
    colon in a compound statement.

    .. code-block::

        if test:<leading_whitespace><body><trailing_whitespace>

    This isn't differentiated from a :class:`SimpleStatementLine` in the grammar, but
    because the two classes need to track different whitespace, we're differentiating
    it in the CST.
    """

    #: Sequence of small statements. All but the last statement are required to have
    #: a semicolon.
    body: Sequence[BaseSmallStatement]

    #: The whitespace between the colon in the parent statement and the body.
    leading_whitespace: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    trailing_whitespace: TrailingWhitespace = TrailingWhitespace.field()

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "SimpleStatementSuite":
        return SimpleStatementSuite(
            leading_whitespace=visit_required(
                self, "leading_whitespace", self.leading_whitespace, visitor
            ),
            body=visit_sequence(self, "body", self.body, visitor),
            trailing_whitespace=visit_required(
                self, "trailing_whitespace", self.trailing_whitespace, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.leading_whitespace._codegen(state)
        _BaseSimpleStatement._codegen_impl(self, state)


@add_slots
@dataclass(frozen=True)
class Else(CSTNode):
    """
    An ``else`` clause that appears optionally after an :class:`If`, :class:`While`,
    :class:`Try`, or :class:`For` statement.

    This node does not match ``elif`` clauses in :class:`If` statements. It also
    does not match the required ``else`` clause in an :class:`IfExp` expression
    (``a = if b else c``).
    """

    #: The body of else clause.
    body: BaseSuite

    #: Sequence of empty lines appearing before this compound statement line.
    leading_lines: Sequence[EmptyLine] = ()

    #: The whitespace appearing after the ``else`` keyword but before the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Else":
        return Else(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self, end_node=self.body):
            state.add_token("else")
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)


class BaseCompoundStatement(BaseStatement, ABC):
    """
    Encapsulates a compound statement, like ``if True: pass`` or ``while True: pass``.
    This exists to simplify type definitions and isinstance checks.

        Compound statements contain (groups of) other statements; they affect or control
        the execution of those other statements in some way. In general, compound
        statements span multiple lines, although in simple incarnations a whole compound
        statement may be contained in one line.

        -- https://docs.python.org/3/reference/compound_stmts.html
    """

    #: The body of this compound statement.
    body: BaseSuite

    #: Any empty lines or comments appearing before this statement.
    leading_lines: Sequence[EmptyLine]


@add_slots
@dataclass(frozen=True)
class If(BaseCompoundStatement):
    """
    An ``if`` statement. ``test`` holds a single test expression.

    ``elif`` clauses don’t have a special representation in the AST, but rather appear as
    extra :class:`If` nodes within the ``orelse`` section of the previous one.
    """

    #: The expression that, when evaluated, should give us a truthy/falsey value.
    test: BaseExpression  # TODO: should be a test_nocond

    #: The body of this compound statement.
    body: BaseSuite

    #: An optional ``elif`` or ``else`` clause. :class:`If` signifies an ``elif`` block.
    #: :class:`Else` signifies an ``else`` block. ``None`` signifies no ``else`` or
    #:``elif`` block.
    orelse: Union["If", Else, None] = None

    #: Sequence of empty lines appearing before this compound statement line.
    leading_lines: Sequence[EmptyLine] = ()

    #: The whitespace appearing after the ``if`` keyword but before the test expression.
    whitespace_before_test: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: The whitespace appearing after the test expression but before the colon.
    whitespace_after_test: SimpleWhitespace = SimpleWhitespace.field("")

    # TODO: _validate

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "If":
        return If(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_before_test=visit_required(
                self, "whitespace_before_test", self.whitespace_before_test, visitor
            ),
            test=visit_required(self, "test", self.test, visitor),
            whitespace_after_test=visit_required(
                self, "whitespace_after_test", self.whitespace_after_test, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
            orelse=visit_optional(self, "orelse", self.orelse, visitor),
        )

    def _codegen_impl(self, state: CodegenState, is_elif: bool = False) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        end_node = self.body if self.orelse is None else self.orelse
        with state.record_syntactic_position(self, end_node=end_node):
            state.add_token("elif" if is_elif else "if")
            self.whitespace_before_test._codegen(state)
            self.test._codegen(state)
            self.whitespace_after_test._codegen(state)
            state.add_token(":")
            self.body._codegen(state)
            orelse = self.orelse
            if orelse is not None:
                if isinstance(orelse, If):  # special-case elif
                    orelse._codegen(state, is_elif=True)
                else:  # is an Else clause
                    orelse._codegen(state)


@add_slots
@dataclass(frozen=True)
class IndentedBlock(BaseSuite):
    """
    Represents a block of statements beginning with an ``INDENT`` token and ending in a
    ``DEDENT`` token. Used as the body of compound statements, such as an if statement's
    body.

    A common alternative to an :class:`IndentedBlock` is a :class:`SimpleStatementSuite`,
    which can also be used as a :class:`BaseSuite`, meaning that it can be used as the
    body of many compound statements.

    An :class:`IndentedBlock` always occurs after a colon in a
    :class:`BaseCompoundStatement`, so it owns the trailing whitespace for the compound
    statement's clause.

    .. code-block::

        if test: # IndentedBlock's header
            body
    """

    #: Sequence of statements belonging to this indented block.
    body: Sequence[BaseStatement]

    #: Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    header: TrailingWhitespace = TrailingWhitespace.field()

    #: A string represents a specific indentation. A ``None`` value uses the modules's
    #: default indentation. This is included because indentation is allowed to be
    #: inconsistent across a file, just not ambiguously.
    indent: Optional[str] = None

    #: Any trailing comments or lines after the dedent that are owned by this indented
    #: block. Statements own preceeding and same-line trailing comments, but not
    #: trailing lines, so it falls on :class:`IndentedBlock` to own it. In the case
    #: that a statement follows an :class:`IndentedBlock`, that statement will own the
    #: comments and lines that are at the same indent as the statement, and this
    #: :class:`IndentedBlock` will own the comments and lines that are indented further.
    footer: Sequence[EmptyLine] = ()

    def _validate(self) -> None:
        indent = self.indent
        if indent is not None:
            if len(indent) == 0:
                raise CSTValidationError(
                    "An indented block must have a non-zero width indent."
                )
            if _INDENT_WHITESPACE_RE.fullmatch(indent) is None:
                raise CSTValidationError(
                    "An indent must be composed of only whitespace characters."
                )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "IndentedBlock":
        return IndentedBlock(
            header=visit_required(self, "header", self.header, visitor),
            indent=self.indent,
            body=visit_body_sequence(self, "body", self.body, visitor),
            footer=visit_sequence(self, "footer", self.footer, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.header._codegen(state)

        indent = self.indent
        state.increase_indent(state.default_indent if indent is None else indent)

        if self.body:
            with state.record_syntactic_position(
                self, start_node=self.body[0], end_node=self.body[-1]
            ):
                for stmt in self.body:
                    # IndentedBlock is responsible for adjusting the current indentation level,
                    # but its children are responsible for actually adding that indentation to
                    # the token list.
                    stmt._codegen(state)
        else:
            # Empty indented blocks are not syntactically valid in Python unless
            # they contain a 'pass' statement, so add one here.
            state.add_indent_tokens()
            with state.record_syntactic_position(self):
                state.add_token("pass")
            state.add_token(state.default_newline)

        for f in self.footer:
            f._codegen(state)

        state.decrease_indent()


@add_slots
@dataclass(frozen=True)
class AsName(CSTNode):
    """
    An ``as name`` clause inside an :class:`ExceptHandler`, :class:`ImportAlias` or
    :class:`WithItem` node.
    """

    #: Identifier that the parent node will be aliased to.
    name: Union[Name, Tuple, List]

    #: Whitespace between the parent node and the ``as`` keyword.
    whitespace_before_as: BaseParenthesizableWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace between the ``as`` keyword and the name.
    whitespace_after_as: BaseParenthesizableWhitespace = SimpleWhitespace.field(" ")

    def _validate(self) -> None:
        if self.whitespace_after_as.empty:
            raise CSTValidationError(
                "There must be at least one space between 'as' and name."
            )
        if self.whitespace_before_as.empty:
            raise CSTValidationError("There must be at least one space before 'as'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AsName":
        return AsName(
            whitespace_before_as=visit_required(
                self, "whitespace_before_as", self.whitespace_before_as, visitor
            ),
            name=visit_required(self, "name", self.name, visitor),
            whitespace_after_as=visit_required(
                self, "whitespace_after_as", self.whitespace_after_as, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace_before_as._codegen(state)
        state.add_token("as")
        self.whitespace_after_as._codegen(state)
        self.name._codegen(state)


@add_slots
@dataclass(frozen=True)
class ExceptHandler(CSTNode):
    """
    An ``except`` clause that appears optionally after a :class:`Try` statement.
    """

    #: The body of the except.
    body: BaseSuite

    #: The type of exception this catches. Can be a tuple in some cases,
    #: or ``None`` if the code is catching all exceptions.
    type: Optional[BaseExpression] = None

    #: The optional name that a caught exception is assigned to.
    name: Optional[AsName] = None

    #: Sequence of empty lines appearing before this compound statement line.
    leading_lines: Sequence[EmptyLine] = ()

    #: The whitespace between the ``except`` keyword and the type attribute.
    whitespace_after_except: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: The whitespace after any type or name node (whichever comes last) and
    #: the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate(self) -> None:
        name = self.name
        if self.type is None and name is not None:
            raise CSTValidationError("Cannot have a name for an empty type.")
        if name is not None and not isinstance(name.name, Name):
            raise CSTValidationError(
                "Must use a Name node for AsName name inside ExceptHandler."
            )
        type_ = self.type
        if type_ is not None and self.whitespace_after_except.empty:
            # Space is only required when the first char in `type` could start
            # an identifier.  In the most common cases, we want to allow
            # grouping or tuple parens.
            if isinstance(type_, Name) and not type_.lpar:
                raise CSTValidationError(
                    "Must have at least one space after except when ExceptHandler has a type."
                )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ExceptHandler":
        return ExceptHandler(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_after_except=visit_required(
                self, "whitespace_after_except", self.whitespace_after_except, visitor
            ),
            type=visit_optional(self, "type", self.type, visitor),
            name=visit_optional(self, "name", self.name, visitor),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self, end_node=self.body):
            state.add_token("except")
            self.whitespace_after_except._codegen(state)
            typenode = self.type
            if typenode is not None:
                typenode._codegen(state)
            namenode = self.name
            if namenode is not None:
                namenode._codegen(state)
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)


@add_slots
@dataclass(frozen=True)
class Finally(CSTNode):
    """
    A ``finally`` clause that appears optionally after a :class:`Try` statement.
    """

    #: The body of the except.
    body: BaseSuite

    #: Sequence of empty lines appearing before this compound statement line.
    leading_lines: Sequence[EmptyLine] = ()

    #: The whitespace that appears after the ``finally`` keyword but before
    #: the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Finally":
        return Finally(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self, end_node=self.body):
            state.add_token("finally")
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)


@add_slots
@dataclass(frozen=True)
class Try(BaseCompoundStatement):
    """
    A ``try`` statement.
    """

    #: The suite that is wrapped with a try statement.
    body: BaseSuite

    #: A list of zero or more exception handlers.
    handlers: Sequence[ExceptHandler] = ()

    #: An optional else case.
    orelse: Optional[Else] = None

    #: An optional finally case.
    finalbody: Optional[Finally] = None

    #: Sequence of empty lines appearing before this compound statement line.
    leading_lines: Sequence[EmptyLine] = ()

    #: The whitespace that appears after the ``try`` keyword but before
    #: the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate(self) -> None:
        if len(self.handlers) == 0 and self.finalbody is None:
            raise CSTValidationError(
                "A Try statement must have at least one ExceptHandler or Finally"
            )
        if len(self.handlers) == 0 and self.orelse is not None:
            raise CSTValidationError(
                "A Try statement must have at least one ExceptHandler in order "
                + "to have an Else."
            )
        # Check bare excepts are always at the last position
        if any(handler.type is None for handler in self.handlers[:-1]):
            raise CSTValidationError("The bare except: handler must be the last one.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Try":
        return Try(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
            handlers=visit_sequence(self, "handlers", self.handlers, visitor),
            orelse=visit_optional(self, "orelse", self.orelse, visitor),
            finalbody=visit_optional(self, "finalbody", self.finalbody, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        end_node = self.body
        if len(self.handlers) > 0:
            end_node = self.handlers[-1]
        orelse = self.orelse
        end_node = end_node if orelse is None else orelse
        finalbody = self.finalbody
        end_node = end_node if finalbody is None else finalbody
        with state.record_syntactic_position(self, end_node=end_node):
            state.add_token("try")
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)
            for handler in self.handlers:
                handler._codegen(state)
            if orelse is not None:
                orelse._codegen(state)
            if finalbody is not None:
                finalbody._codegen(state)


@add_slots
@dataclass(frozen=True)
class ImportAlias(CSTNode):
    """
    An import, with an optional :class:`AsName`. Used in both :class:`Import` and
    :class:`ImportFrom` to specify a single import out of another module.
    """

    #: Name or Attribute node representing the object we are importing.
    name: Union[Attribute, Name]

    #: Local alias we will import the above object as.
    asname: Optional[AsName] = None

    #: Any trailing comma that appears after this import. This is optional for the
    #: last :class:`ImportAlias` in a :class:`Import` or :class:`ImportFrom`, but all
    #: other import aliases inside an import must contain a comma to disambiguate
    #: multiple imports.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        asname = self.asname
        if asname is not None and not isinstance(asname.name, Name):
            raise CSTValidationError(
                "Must use a Name node for AsName name inside ImportAlias."
            )
        try:
            self.evaluated_name
        except Exception as e:
            if str(e) == "Logic error!":
                raise CSTValidationError(
                    "The imported name must be a valid qualified name."
                )
            raise e

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ImportAlias":
        return ImportAlias(
            name=visit_required(self, "name", self.name, visitor),
            asname=visit_optional(self, "asname", self.asname, visitor),
            comma=visit_sentinel(self, "comma", self.comma, visitor),
        )

    def _codegen_impl(self, state: CodegenState, default_comma: bool = False) -> None:
        with state.record_syntactic_position(self):
            self.name._codegen(state)
            asname = self.asname
            if asname is not None:
                asname._codegen(state)

        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            state.add_token(", ")
        elif isinstance(comma, Comma):
            comma._codegen(state)

    def _name(self, node: CSTNode) -> str:
        # Unrolled version of get_full_name_for_node to avoid circular imports.
        if isinstance(node, Name):
            return node.value
        elif isinstance(node, Attribute):
            return f"{self._name(node.value)}.{node.attr.value}"
        else:
            raise Exception("Logic error!")

    @property
    def evaluated_name(self) -> str:
        """
        Returns the string name this :class:`ImportAlias` represents.
        """
        return self._name(self.name)

    @property
    def evaluated_alias(self) -> Optional[str]:
        """
        Returns the string name for any alias that this :class:`ImportAlias`
        has. If there is no ``asname`` attribute, this returns ``None``.
        """
        asname = self.asname
        if asname is not None:
            return self._name(asname.name)
        return None


@add_slots
@dataclass(frozen=True)
class Import(BaseSmallStatement):
    """
    An ``import`` statement.
    """

    #: One or more names that are being imported, with optional local aliases.
    names: Sequence[ImportAlias]

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    #: The whitespace that appears after the ``import`` keyword but before
    #: the first import alias.
    whitespace_after_import: SimpleWhitespace = SimpleWhitespace.field(" ")

    def _validate(self) -> None:
        if len(self.names) == 0:
            raise CSTValidationError(
                "An ImportStatement must have at least one ImportAlias"
            )
        if isinstance(self.names[-1].comma, Comma):
            raise CSTValidationError(
                "An ImportStatement does not allow a trailing comma"
            )
        if self.whitespace_after_import.empty:
            raise CSTValidationError("Must have at least one space after import.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Import":
        return Import(
            whitespace_after_import=visit_required(
                self, "whitespace_after_import", self.whitespace_after_import, visitor
            ),
            names=visit_sequence(self, "names", self.names, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("import")
            self.whitespace_after_import._codegen(state)
            lastname = len(self.names) - 1
            for i, name in enumerate(self.names):
                name._codegen(state, default_comma=(i != lastname))

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class ImportFrom(BaseSmallStatement):
    """
    A ``from x import y`` statement.
    """

    #: Name or Attribute node representing the module we're importing from.
    #: This is optional as :class:`ImportFrom` allows purely relative imports.
    module: Optional[Union[Attribute, Name]]

    #: One or more names that are being imported from the specified module,
    #: with optional local aliases.
    names: Union[Sequence[ImportAlias], ImportStar]

    #: Sequence of :class:`Dot` nodes indicating relative import level.
    relative: Sequence[Dot] = ()

    #: Optional open parenthesis for multi-line import continuation.
    lpar: Optional[LeftParen] = None

    #: Optional close parenthesis for multi-line import continuation.
    rpar: Optional[RightParen] = None

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    #: The whitespace that appears after the ``from`` keyword but before
    #: the module and any relative import dots.
    whitespace_after_from: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: The whitespace that appears after the module but before the
    #: ``import`` keyword.
    whitespace_before_import: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: The whitespace that appears after the ``import`` keyword but
    #: before the first import name or optional left paren.
    whitespace_after_import: SimpleWhitespace = SimpleWhitespace.field(" ")

    def _validate_module(self) -> None:
        if self.module is None and len(self.relative) == 0:
            raise CSTValidationError(
                "Must have a module specified if there is no relative import."
            )

    def _validate_names(self) -> None:
        names = self.names
        if isinstance(names, Sequence):
            if len(names) == 0:
                raise CSTValidationError(
                    "An ImportFrom must have at least one ImportAlias"
                )
            for name in names[:-1]:
                if name.comma is None:
                    raise CSTValidationError("Non-final ImportAliases require a comma")
            if self.lpar is not None and self.rpar is None:
                raise CSTValidationError("Cannot have left paren without right paren.")
            if self.lpar is None and self.rpar is not None:
                raise CSTValidationError("Cannot have right paren without left paren.")
        if isinstance(names, ImportStar):
            if self.lpar is not None or self.rpar is not None:
                raise CSTValidationError(
                    "An ImportFrom using ImportStar cannot have parens"
                )

    def _validate_whitespace(self) -> None:
        if self.whitespace_after_from.empty and not self.relative:
            raise CSTValidationError("Must have at least one space after from.")
        if self.whitespace_before_import.empty and not (
            self.relative and self.module is None
        ):
            raise CSTValidationError("Must have at least one space before import.")
        if (
            self.whitespace_after_import.empty
            and self.lpar is None
            and not isinstance(self.names, ImportStar)
        ):
            raise CSTValidationError("Must have at least one space after import.")

    def _validate(self) -> None:
        self._validate_module()
        self._validate_names()
        self._validate_whitespace()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ImportFrom":
        names = self.names
        return ImportFrom(
            whitespace_after_from=visit_required(
                self, "whitespace_after_from", self.whitespace_after_from, visitor
            ),
            relative=visit_sequence(self, "relative", self.relative, visitor),
            module=visit_optional(self, "module", self.module, visitor),
            whitespace_before_import=visit_required(
                self, "whitespace_before_import", self.whitespace_before_import, visitor
            ),
            whitespace_after_import=visit_required(
                self, "whitespace_after_import", self.whitespace_after_import, visitor
            ),
            lpar=visit_optional(self, "lpar", self.lpar, visitor),
            names=(
                visit_required(self, "names", names, visitor)
                if isinstance(names, ImportStar)
                else visit_sequence(self, "names", names, visitor)
            ),
            rpar=visit_optional(self, "rpar", self.rpar, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        names = self.names
        end_node = names[-1] if isinstance(names, Sequence) else names
        end_node = end_node if self.rpar is None else self.rpar
        with state.record_syntactic_position(self, end_node=end_node):
            state.add_token("from")
            self.whitespace_after_from._codegen(state)
            for dot in self.relative:
                dot._codegen(state)
            module = self.module
            if module is not None:
                module._codegen(state)
            self.whitespace_before_import._codegen(state)
            state.add_token("import")
            self.whitespace_after_import._codegen(state)
            lpar = self.lpar
            if lpar is not None:
                lpar._codegen(state)
            if isinstance(names, Sequence):
                lastname = len(names) - 1
                for i, name in enumerate(names):
                    name._codegen(state, default_comma=(i != lastname))
            if isinstance(names, ImportStar):
                names._codegen(state)
            rpar = self.rpar
            if rpar is not None:
                rpar._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class AssignTarget(CSTNode):
    """
    A target for an :class:`Assign`. Owns the equals sign and the whitespace around it.
    """

    #: The target expression being assigned to.
    target: BaseAssignTargetExpression

    #: The whitespace appearing before the equals sign.
    whitespace_before_equal: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: The whitespace appearing after the equals sign.
    whitespace_after_equal: SimpleWhitespace = SimpleWhitespace.field(" ")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AssignTarget":
        return AssignTarget(
            target=visit_required(self, "target", self.target, visitor),
            whitespace_before_equal=visit_required(
                self, "whitespace_before_equal", self.whitespace_before_equal, visitor
            ),
            whitespace_after_equal=visit_required(
                self, "whitespace_after_equal", self.whitespace_after_equal, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with state.record_syntactic_position(self):
            self.target._codegen(state)

        self.whitespace_before_equal._codegen(state)
        state.add_token("=")
        self.whitespace_after_equal._codegen(state)


@add_slots
@dataclass(frozen=True)
class Assign(BaseSmallStatement):
    """
    An assignment statement such as ``x = y`` or ``x = y = z``. Unlike
    :class:`AnnAssign`, this does not allow type annotations but does
    allow for multiple targets.
    """

    #: One or more targets that are being assigned to.
    targets: Sequence[AssignTarget]

    #: The expression being assigned to the targets.
    value: BaseExpression

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if len(self.targets) == 0:
            raise CSTValidationError(
                "An Assign statement must have at least one AssignTarget"
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Assign":
        return Assign(
            targets=visit_sequence(self, "targets", self.targets, visitor),
            value=visit_required(self, "value", self.value, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            for target in self.targets:
                target._codegen(state)
            self.value._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class AnnAssign(BaseSmallStatement):
    """
    An assignment statement such as ``x: int = 5`` or ``x: int``. This only
    allows for one assignment target unlike :class:`Assign` but it includes
    a variable annotation. Also unlike :class:`Assign`, the assignment target
    is optional, as it is possible to annotate an expression without assigning
    to it.
    """

    #: The target that is being annotated and possibly assigned to.
    target: BaseAssignTargetExpression

    #: The annotation for the target.
    annotation: Annotation

    #: The optional expression being assigned to the target.
    value: Optional[BaseExpression] = None

    #: The equals sign used to denote assignment if there is a value.
    equal: Union[AssignEqual, MaybeSentinel] = MaybeSentinel.DEFAULT

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if self.value is None and isinstance(self.equal, AssignEqual):
            raise CSTValidationError(
                "Must have a value when specifying an AssignEqual."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AnnAssign":
        return AnnAssign(
            target=visit_required(self, "target", self.target, visitor),
            annotation=visit_required(self, "annotation", self.annotation, visitor),
            equal=visit_sentinel(self, "equal", self.equal, visitor),
            value=visit_optional(self, "value", self.value, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            self.target._codegen(state)
            self.annotation._codegen(state, default_indicator=":")
            equal = self.equal
            if equal is MaybeSentinel.DEFAULT and self.value is not None:
                state.add_token(" = ")
            elif isinstance(equal, AssignEqual):
                equal._codegen(state)
            value = self.value
            if value is not None:
                value._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class AugAssign(BaseSmallStatement):
    """
    An augmented assignment statement, such as ``x += 5``.
    """

    #: Target that is being operated on and assigned to.
    target: BaseAssignTargetExpression

    #: The augmented assignment operation being performed.
    operator: BaseAugOp

    #: The value used with the above operator to calculate the new assignment.
    value: BaseExpression

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AugAssign":
        return AugAssign(
            target=visit_required(self, "target", self.target, visitor),
            operator=visit_required(self, "operator", self.operator, visitor),
            value=visit_required(self, "value", self.value, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            self.target._codegen(state)
            self.operator._codegen(state)
            self.value._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Decorator(CSTNode):
    """
    A single decorator that decorates a :class:`FunctionDef` or a :class:`ClassDef`.
    """

    #: The decorator that will return a new function wrapping the parent
    #: of this decorator.
    decorator: Union[Name, Attribute, Call]

    #: Line comments and empty lines before this decorator. The parent
    #: :class:`FunctionDef` or :class:`ClassDef` node owns leading lines before
    #: the first decorator so that if the first decorator is removed, spacing is preserved.
    leading_lines: Sequence[EmptyLine] = ()

    #: Whitespace after the ``@`` and before the decorator expression itself.
    whitespace_after_at: SimpleWhitespace = SimpleWhitespace.field("")

    #: Optional trailing comment and newline following the decorator before the next line.
    trailing_whitespace: TrailingWhitespace = TrailingWhitespace.field()

    def _validate(self) -> None:
        decorator = self.decorator
        if len(decorator.lpar) > 0 or len(decorator.rpar) > 0:
            raise CSTValidationError(
                "Cannot have parens around decorator in a Decorator."
            )
        if isinstance(decorator, Call) and not isinstance(
            decorator.func, (Name, Attribute)
        ):
            raise CSTValidationError(
                "Decorator call function must be Name or Attribute node."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Decorator":
        return Decorator(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_after_at=visit_required(
                self, "whitespace_after_at", self.whitespace_after_at, visitor
            ),
            decorator=visit_required(self, "decorator", self.decorator, visitor),
            trailing_whitespace=visit_required(
                self, "trailing_whitespace", self.trailing_whitespace, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self):
            state.add_token("@")
            self.whitespace_after_at._codegen(state)
            self.decorator._codegen(state)

        self.trailing_whitespace._codegen(state)


def get_docstring_impl(
    body: Union[BaseSuite, Sequence[Union[SimpleStatementLine, BaseCompoundStatement]]],
    clean: bool,
) -> Optional[str]:
    """
    Implementation Reference:
    - :func:`ast.get_docstring` https://docs.python.org/3/library/ast.html#ast.get_docstring
    and https://github.com/python/cpython/blob/89aa4694fc8c6d190325ef8ed6ce6a6b8efb3e50/Lib/ast.py#L254
    - PEP 257 https://www.python.org/dev/peps/pep-0257/
    """
    if isinstance(body, Sequence):
        if body:
            expr = body[0]
        else:
            return None
    else:
        expr = body
    while isinstance(expr, (BaseSuite, SimpleStatementLine)):
        if len(expr.body) == 0:
            return None
        expr = expr.body[0]
    if not isinstance(expr, Expr):
        return None
    val = expr.value
    if isinstance(val, (SimpleString, ConcatenatedString)):
        evaluated_value = val.evaluated_value
    else:
        return None

    if evaluated_value is not None and clean:
        return inspect.cleandoc(evaluated_value)
    return evaluated_value


@add_slots
@dataclass(frozen=True)
class FunctionDef(BaseCompoundStatement):
    """
    A function definition.
    """

    #: The function name.
    name: Name

    #: The function parameters. Present even if there are no params.
    params: Parameters

    #: The function body.
    body: BaseSuite

    #: Sequence of decorators applied to this function. Decorators are listed in
    #: order that they appear in source (top to bottom) as apposed to the order
    #: that they are applied to the function at runtime.
    decorators: Sequence[Decorator] = ()

    #: An optional return annotation, if the function is annotated.
    returns: Optional[Annotation] = None

    #: Optional async modifier, if this is an async function.
    asynchronous: Optional[Asynchronous] = None

    #: Leading empty lines and comments before the first decorator. We
    #: assume any comments before the first decorator are owned by the
    #: function definition itself. If there are no decorators, this will
    #: still contain all of the empty lines and comments before the
    #: function definition.
    leading_lines: Sequence[EmptyLine] = ()

    #: Empty lines and comments between the final decorator and the
    #: :class:`FunctionDef` node. In the case of no decorators, this will be empty.
    lines_after_decorators: Sequence[EmptyLine] = ()

    #: Whitespace after the ``def`` keyword and before the function name.
    whitespace_after_def: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the function name and before the opening parenthesis for
    #: the parameters.
    whitespace_after_name: SimpleWhitespace = SimpleWhitespace.field("")

    #: Whitespace after the opening parenthesis for the parameters but before
    #: the first param itself.
    whitespace_before_params: BaseParenthesizableWhitespace = SimpleWhitespace.field("")

    #: Whitespace after the closing parenthesis or return annotation and before
    #: the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate(self) -> None:
        if len(self.name.lpar) > 0 or len(self.name.rpar) > 0:
            raise CSTValidationError("Cannot have parens around Name in a FunctionDef.")
        if self.whitespace_after_def.empty:
            raise CSTValidationError(
                "There must be at least one space between 'def' and name."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "FunctionDef":
        return FunctionDef(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            decorators=visit_sequence(self, "decorators", self.decorators, visitor),
            lines_after_decorators=visit_sequence(
                self, "lines_after_decorators", self.lines_after_decorators, visitor
            ),
            asynchronous=visit_optional(
                self, "asynchronous", self.asynchronous, visitor
            ),
            whitespace_after_def=visit_required(
                self, "whitespace_after_def", self.whitespace_after_def, visitor
            ),
            name=visit_required(self, "name", self.name, visitor),
            whitespace_after_name=visit_required(
                self, "whitespace_after_name", self.whitespace_after_name, visitor
            ),
            whitespace_before_params=visit_required(
                self, "whitespace_before_params", self.whitespace_before_params, visitor
            ),
            params=visit_required(self, "params", self.params, visitor),
            returns=visit_optional(self, "returns", self.returns, visitor),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        for decorator in self.decorators:
            decorator._codegen(state)
        for lad in self.lines_after_decorators:
            lad._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self, end_node=self.body):
            asynchronous = self.asynchronous
            if asynchronous is not None:
                asynchronous._codegen(state)
            state.add_token("def")
            self.whitespace_after_def._codegen(state)
            self.name._codegen(state)
            self.whitespace_after_name._codegen(state)
            state.add_token("(")
            self.whitespace_before_params._codegen(state)
            self.params._codegen(state)
            state.add_token(")")
            returns = self.returns
            if returns is not None:
                returns._codegen(state, default_indicator="->")
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)

    def get_docstring(self, clean: bool = True) -> Optional[str]:
        """
        When docstring is available, returns a :func:`inspect.cleandoc` cleaned docstring.
        Otherwise, returns ``None``.
        """
        return get_docstring_impl(self.body, clean)


@add_slots
@dataclass(frozen=True)
class ClassDef(BaseCompoundStatement):
    """
    A class definition.
    """

    #: The class name.
    name: Name

    #: The class body.
    body: BaseSuite

    #: Sequence of base classes this class inherits from.
    bases: Sequence[Arg] = ()

    #: Sequence of keywords, such as "metaclass".
    keywords: Sequence[Arg] = ()

    #: Sequence of decorators applied to this class.
    decorators: Sequence[Decorator] = ()

    #: Optional open parenthesis used when there are bases or keywords.
    lpar: Union[LeftParen, MaybeSentinel] = MaybeSentinel.DEFAULT

    #: Optional close parenthesis used when there are bases or keywords.
    rpar: Union[RightParen, MaybeSentinel] = MaybeSentinel.DEFAULT

    #: Leading empty lines and comments before the first decorator. We
    #: assume any comments before the first decorator are owned by the
    #: class definition itself. If there are no decorators, this will
    #: still contain all of the empty lines and comments before the
    #: class definition.
    leading_lines: Sequence[EmptyLine] = ()

    #: Empty lines and comments between the final decorator and the
    #: :class:`ClassDef` node. In the case of no decorators, this will be empty.
    lines_after_decorators: Sequence[EmptyLine] = ()

    #: Whitespace after the ``class`` keyword and before the class name.
    whitespace_after_class: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the class name and before the opening parenthesis for
    #: the bases and keywords.
    whitespace_after_name: SimpleWhitespace = SimpleWhitespace.field("")

    #: Whitespace after the closing parenthesis or class name and before
    #: the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate_whitespace(self) -> None:
        if self.whitespace_after_class.empty:
            raise CSTValidationError(
                "There must be at least one space between 'class' and name."
            )

    def _validate_parens(self) -> None:
        if len(self.name.lpar) > 0 or len(self.name.rpar) > 0:
            raise CSTValidationError("Cannot have parens around Name in a ClassDef.")
        if isinstance(self.lpar, MaybeSentinel) and isinstance(self.rpar, RightParen):
            raise CSTValidationError(
                "Do not mix concrete LeftParen/RightParen with MaybeSentinel."
            )
        if isinstance(self.lpar, LeftParen) and isinstance(self.rpar, MaybeSentinel):
            raise CSTValidationError(
                "Do not mix concrete LeftParen/RightParen with MaybeSentinel."
            )

    def _validate_args(self) -> None:
        if any((arg.keyword is not None) for arg in self.bases):
            raise CSTValidationError("Bases must be arguments without keywords.")
        if any((arg.keyword is None and arg.star != "**") for arg in self.keywords):
            raise CSTValidationError(
                "Keywords must be arguments with keywords or dictionary expansions."
            )

    def _validate(self) -> None:
        self._validate_whitespace()
        self._validate_parens()
        self._validate_args()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ClassDef":
        return ClassDef(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            decorators=visit_sequence(self, "decorators", self.decorators, visitor),
            lines_after_decorators=visit_sequence(
                self, "lines_after_decorators", self.lines_after_decorators, visitor
            ),
            whitespace_after_class=visit_required(
                self, "whitespace_after_class", self.whitespace_after_class, visitor
            ),
            name=visit_required(self, "name", self.name, visitor),
            whitespace_after_name=visit_required(
                self, "whitespace_after_name", self.whitespace_after_name, visitor
            ),
            lpar=visit_sentinel(self, "lpar", self.lpar, visitor),
            bases=visit_sequence(self, "bases", self.bases, visitor),
            keywords=visit_sequence(self, "keywords", self.keywords, visitor),
            rpar=visit_sentinel(self, "rpar", self.rpar, visitor),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:  # noqa: C901
        for ll in self.leading_lines:
            ll._codegen(state)
        for decorator in self.decorators:
            decorator._codegen(state)
        for lad in self.lines_after_decorators:
            lad._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self, end_node=self.body):
            state.add_token("class")
            self.whitespace_after_class._codegen(state)
            self.name._codegen(state)
            self.whitespace_after_name._codegen(state)
            lpar = self.lpar
            if isinstance(lpar, MaybeSentinel):
                if self.bases or self.keywords:
                    state.add_token("(")
            elif isinstance(lpar, LeftParen):
                lpar._codegen(state)
            args = [*self.bases, *self.keywords]
            last_arg = len(args) - 1
            for i, arg in enumerate(args):
                arg._codegen(state, default_comma=(i != last_arg))
            rpar = self.rpar
            if isinstance(rpar, MaybeSentinel):
                if self.bases or self.keywords:
                    state.add_token(")")
            elif isinstance(rpar, RightParen):
                rpar._codegen(state)
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)

    def get_docstring(self, clean: bool = True) -> Optional[str]:
        """
        Returns a :func:`inspect.cleandoc` cleaned docstring if the docstring is available, ``None`` otherwise.
        """
        return get_docstring_impl(self.body, clean)


@add_slots
@dataclass(frozen=True)
class WithItem(CSTNode):
    """
    A single context manager in a :class:`With` block, with an optional variable name.
    """

    #: Expression that evaluates to a context manager.
    item: BaseExpression

    #: Variable to assign the context manager to, if it is needed in the
    #: :class:`With` body.
    asname: Optional[AsName] = None

    #: This is forbidden for the last :class:`WithItem` in a :class:`With`, but all
    #: other items inside a with block must contain a comma to separate them.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "WithItem":
        return WithItem(
            item=visit_required(self, "item", self.item, visitor),
            asname=visit_optional(self, "asname", self.asname, visitor),
            comma=visit_sentinel(self, "comma", self.comma, visitor),
        )

    def _codegen_impl(self, state: CodegenState, default_comma: bool = False) -> None:
        with state.record_syntactic_position(self):
            self.item._codegen(state)
            asname = self.asname
            if asname is not None:
                asname._codegen(state)

        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            state.add_token(", ")
        elif isinstance(comma, Comma):
            comma._codegen(state)


@add_slots
@dataclass(frozen=True)
class With(BaseCompoundStatement):
    """
    A ``with`` statement.
    """

    #: A sequence of one or more items that evaluate to context managers.
    items: Sequence[WithItem]

    #: The suite that is wrapped with this statement.
    body: BaseSuite

    #: Optional async modifier if this is an ``async with`` statement.
    asynchronous: Optional[Asynchronous] = None

    #: Sequence of empty lines appearing before this with statement.
    leading_lines: Sequence[EmptyLine] = ()

    #: Whitespace after the ``with`` keyword and before the first item.
    whitespace_after_with: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the last item and before the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate(self) -> None:
        if len(self.items) == 0:
            raise CSTValidationError(
                "A With statement must have at least one WithItem."
            )
        if self.items[-1].comma != MaybeSentinel.DEFAULT:
            raise CSTValidationError(
                "The last WithItem in a With cannot have a trailing comma."
            )
        if self.whitespace_after_with.empty and not self.items[
            0
        ].item._safe_to_use_with_word_operator(ExpressionPosition.RIGHT):
            raise CSTValidationError("Must have at least one space after with keyword.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "With":
        return With(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            asynchronous=visit_optional(
                self, "asynchronous", self.asynchronous, visitor
            ),
            whitespace_after_with=visit_required(
                self, "whitespace_after_with", self.whitespace_after_with, visitor
            ),
            items=visit_sequence(self, "items", self.items, visitor),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        with state.record_syntactic_position(self, end_node=self.body):
            asynchronous = self.asynchronous
            if asynchronous is not None:
                asynchronous._codegen(state)
            state.add_token("with")
            self.whitespace_after_with._codegen(state)
            last_item = len(self.items) - 1
            for i, item in enumerate(self.items):
                item._codegen(state, default_comma=(i != last_item))
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)


@add_slots
@dataclass(frozen=True)
class For(BaseCompoundStatement):
    """
    A ``for target in iter`` statement.
    """

    #: The target of the iterator in the for statement.
    target: BaseAssignTargetExpression

    #: The iterable expression we will loop over.
    iter: BaseExpression

    #: The suite that is wrapped with this statement.
    body: BaseSuite

    #: An optional else case which will be executed if there is no
    #: :class:`Break` statement encountered while looping.
    orelse: Optional[Else] = None

    #: Optional async modifier, if this is an `async for` statement.
    asynchronous: Optional[Asynchronous] = None

    #: Sequence of empty lines appearing before this for statement.
    leading_lines: Sequence[EmptyLine] = ()

    #: Whitespace after the ``for`` keyword and before the target.
    whitespace_after_for: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the target and before the ``in`` keyword.
    whitespace_before_in: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the ``in`` keyword and before the iter.
    whitespace_after_in: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the iter and before the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate(self) -> None:
        if (
            self.whitespace_after_for.empty
            and not self.target._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            )
        ):
            raise CSTValidationError(
                "Must have at least one space after 'for' keyword."
            )

        if (
            self.whitespace_before_in.empty
            and not self.target._safe_to_use_with_word_operator(ExpressionPosition.LEFT)
        ):
            raise CSTValidationError(
                "Must have at least one space before 'in' keyword."
            )

        if (
            self.whitespace_after_in.empty
            and not self.iter._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError("Must have at least one space after 'in' keyword.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "For":
        return For(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            asynchronous=visit_optional(
                self, "asynchronous", self.asynchronous, visitor
            ),
            whitespace_after_for=visit_required(
                self, "whitespace_after_for", self.whitespace_after_for, visitor
            ),
            target=visit_required(self, "target", self.target, visitor),
            whitespace_before_in=visit_required(
                self, "whitespace_before_in", self.whitespace_before_in, visitor
            ),
            whitespace_after_in=visit_required(
                self, "whitespace_after_in", self.whitespace_after_in, visitor
            ),
            iter=visit_required(self, "iter", self.iter, visitor),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
            orelse=visit_optional(self, "orelse", self.orelse, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        end_node = self.body if self.orelse is None else self.orelse
        with state.record_syntactic_position(self, end_node=end_node):
            asynchronous = self.asynchronous
            if asynchronous is not None:
                asynchronous._codegen(state)
            state.add_token("for")
            self.whitespace_after_for._codegen(state)
            self.target._codegen(state)
            self.whitespace_before_in._codegen(state)
            state.add_token("in")
            self.whitespace_after_in._codegen(state)
            self.iter._codegen(state)
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)
            orelse = self.orelse
            if orelse is not None:
                orelse._codegen(state)


@add_slots
@dataclass(frozen=True)
class While(BaseCompoundStatement):
    """
    A ``while`` statement.
    """

    #: The test we will loop against.
    test: BaseExpression

    #: The suite that is wrapped with this statement.
    body: BaseSuite

    #: An optional else case which will be executed if there is no
    #: :class:`Break` statement encountered while looping.
    orelse: Optional[Else] = None

    #: Sequence of empty lines appearing before this while statement.
    leading_lines: Sequence[EmptyLine] = ()

    #: Whitespace after the ``while`` keyword and before the test.
    whitespace_after_while: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Whitespace after the test and before the colon.
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace.field("")

    def _validate(self) -> None:
        if (
            self.whitespace_after_while.empty
            and not self.test._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError(
                "Must have at least one space after 'while' keyword."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "While":
        return While(
            leading_lines=visit_sequence(
                self, "leading_lines", self.leading_lines, visitor
            ),
            whitespace_after_while=visit_required(
                self, "whitespace_after_while", self.whitespace_after_while, visitor
            ),
            test=visit_required(self, "test", self.test, visitor),
            whitespace_before_colon=visit_required(
                self, "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required(self, "body", self.body, visitor),
            orelse=visit_optional(self, "orelse", self.orelse, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        for ll in self.leading_lines:
            ll._codegen(state)
        state.add_indent_tokens()

        end_node = self.body if self.orelse is None else self.orelse
        with state.record_syntactic_position(self, end_node=end_node):
            state.add_token("while")
            self.whitespace_after_while._codegen(state)
            self.test._codegen(state)
            self.whitespace_before_colon._codegen(state)
            state.add_token(":")
            self.body._codegen(state)
            orelse = self.orelse
            if orelse is not None:
                orelse._codegen(state)


@add_slots
@dataclass(frozen=True)
class Raise(BaseSmallStatement):
    """
    A ``raise exc`` or ``raise exc from cause`` statement.
    """

    #: The exception that we should raise.
    exc: Optional[BaseExpression] = None

    #: Optionally, a ``from cause`` clause to allow us to raise an exception
    #: out of another exception's context.
    cause: Optional[From] = None

    #: Any whitespace appearing between the ``raise`` keyword and the exception.
    whitespace_after_raise: Union[
        SimpleWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        # Validate correct construction
        if self.exc is None and self.cause is not None:
            raise CSTValidationError(
                "Must have an 'exc' when specifying 'clause'. on Raise."
            )

        # Validate spacing between "raise" and "exc"
        exc = self.exc
        if exc is not None:
            whitespace_after_raise = self.whitespace_after_raise
            has_no_gap = (
                not isinstance(whitespace_after_raise, MaybeSentinel)
                and whitespace_after_raise.empty
            )
            if has_no_gap and not exc._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            ):
                raise CSTValidationError("Must have at least one space after 'raise'.")

        # Validate spacing between "exc" and "from"
        cause = self.cause
        if exc is not None and cause is not None:
            whitespace_before_from = cause.whitespace_before_from
            has_no_gap = (
                not isinstance(whitespace_before_from, MaybeSentinel)
                and whitespace_before_from.empty
            )
            # pyre-ignore Pyre thinks exc is Optional
            if has_no_gap and not exc._safe_to_use_with_word_operator(
                ExpressionPosition.LEFT
            ):
                raise CSTValidationError("Must have at least one space before 'from'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Raise":
        return Raise(
            whitespace_after_raise=visit_sentinel(
                self, "whitespace_after_raise", self.whitespace_after_raise, visitor
            ),
            exc=visit_optional(self, "exc", self.exc, visitor),
            cause=visit_optional(self, "cause", self.cause, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            exc = self.exc
            cause = self.cause
            state.add_token("raise")
            whitespace_after_raise = self.whitespace_after_raise
            if isinstance(whitespace_after_raise, MaybeSentinel):
                if exc is not None:
                    state.add_token(" ")
            else:
                whitespace_after_raise._codegen(state)
            if exc is not None:
                exc._codegen(state)
            if cause is not None:
                cause._codegen(state, default_space=" ")

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Assert(BaseSmallStatement):
    """
    An assert statement such as ``assert x > 5`` or ``assert x > 5, 'Uh oh!'``
    """

    #: The test we are going to assert on.
    test: BaseExpression

    #: The optional message to display if the test evaluates to a falsey value.
    msg: Optional[BaseExpression] = None

    #: A comma separating test and message, if there is a message.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    #: Whitespace appearing after the ``assert`` keyword and before the test.
    whitespace_after_assert: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        # Validate whitespace
        if (
            self.whitespace_after_assert.empty
            and not self.test._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError("Must have at least one space after 'assert'.")

        # Validate comma rules
        if self.msg is None and isinstance(self.comma, Comma):
            raise CSTValidationError("Cannot have trailing comma after 'test'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Assert":
        return Assert(
            whitespace_after_assert=visit_required(
                self, "whitespace_after_assert", self.whitespace_after_assert, visitor
            ),
            test=visit_required(self, "test", self.test, visitor),
            comma=visit_sentinel(self, "comma", self.comma, visitor),
            msg=visit_optional(self, "msg", self.msg, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("assert")
            self.whitespace_after_assert._codegen(state)
            self.test._codegen(state)

            comma = self.comma
            msg = self.msg
            if isinstance(comma, MaybeSentinel):
                if msg is not None:
                    state.add_token(", ")
            else:
                comma._codegen(state)
            if msg is not None:
                msg._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class NameItem(CSTNode):
    """
    A single identifier name inside a :class:`Global` or :class:`Nonlocal` statement.

    This exists because a list of names in a ``global`` or ``nonlocal`` statement need
    to be separated by a comma, which ends up owned by the :class:`NameItem` node.
    """

    #: Identifier name.
    name: Name

    #: This is forbidden for the last :class:`NameItem` in a
    #: :class:`Global`/:class:`Nonlocal`, but all other tems inside a ``global`` or
    #: ``nonlocal`` statement must contain a comma to separate them.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        # No parens around names here
        if len(self.name.lpar) > 0 or len(self.name.rpar) > 0:
            raise CSTValidationError("Cannot have parens around names in NameItem.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "NameItem":
        return NameItem(
            name=visit_required(self, "name", self.name, visitor),
            comma=visit_sentinel(self, "comma", self.comma, visitor),
        )

    def _codegen_impl(self, state: CodegenState, default_comma: bool = False) -> None:
        with state.record_syntactic_position(self):
            self.name._codegen(state)

        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            state.add_token(", ")
        elif isinstance(comma, Comma):
            comma._codegen(state)


@add_slots
@dataclass(frozen=True)
class Global(BaseSmallStatement):
    """
    A ``global`` statement.
    """

    #: A list of one or more names.
    names: Sequence[NameItem]

    #: Whitespace appearing after the ``global`` keyword and before the first name.
    whitespace_after_global: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if len(self.names) == 0:
            raise CSTValidationError(
                "A Global statement must have at least one NameItem."
            )
        if self.names[-1].comma != MaybeSentinel.DEFAULT:
            raise CSTValidationError(
                "The last NameItem in a Global cannot have a trailing comma."
            )
        if self.whitespace_after_global.empty:
            raise CSTValidationError(
                "Must have at least one space after 'global' keyword."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Global":
        return Global(
            whitespace_after_global=visit_required(
                self, "whitespace_after_global", self.whitespace_after_global, visitor
            ),
            names=visit_sequence(self, "names", self.names, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("global")
            self.whitespace_after_global._codegen(state)
            last_name = len(self.names) - 1
            for i, name in enumerate(self.names):
                name._codegen(state, default_comma=(i != last_name))

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@add_slots
@dataclass(frozen=True)
class Nonlocal(BaseSmallStatement):
    """
    A ``nonlocal`` statement.
    """

    #: A list of one or more names.
    names: Sequence[NameItem]

    #: Whitespace appearing after the ``global`` keyword and before the first name.
    whitespace_after_nonlocal: SimpleWhitespace = SimpleWhitespace.field(" ")

    #: Optional semicolon when this is used in a statement line. This semicolon
    #: owns the whitespace on both sides of it when it is used.
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if len(self.names) == 0:
            raise CSTValidationError(
                "A Nonlocal statement must have at least one NameItem."
            )
        if self.names[-1].comma != MaybeSentinel.DEFAULT:
            raise CSTValidationError(
                "The last NameItem in a Nonlocal cannot have a trailing comma."
            )
        if self.whitespace_after_nonlocal.empty:
            raise CSTValidationError(
                "Must have at least one space after 'nonlocal' keyword."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Nonlocal":
        return Nonlocal(
            whitespace_after_nonlocal=visit_required(
                self,
                "whitespace_after_nonlocal",
                self.whitespace_after_nonlocal,
                visitor,
            ),
            names=visit_sequence(self, "names", self.names, visitor),
            semicolon=visit_sentinel(self, "semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        with state.record_syntactic_position(self):
            state.add_token("nonlocal")
            self.whitespace_after_nonlocal._codegen(state)
            last_name = len(self.names) - 1
            for i, name in enumerate(self.names):
                name._codegen(state, default_comma=(i != last_name))

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)
