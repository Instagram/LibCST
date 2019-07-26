# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, Sequence, Union

from libcst._add_slots import add_slots
from libcst._maybe_sentinel import MaybeSentinel
from libcst._nodes._base import CSTNode, CSTValidationError
from libcst._nodes._expression import (
    Annotation,
    Arg,
    Asynchronous,
    Attribute,
    BaseAssignTargetExpression,
    BaseAtom,
    BaseDelTargetExpression,
    BaseExpression,
    Call,
    ExpressionPosition,
    From,
    LeftParen,
    Name,
    Parameters,
    RightParen,
)
from libcst._nodes._internal import (
    CodegenState,
    visit_body_sequence,
    visit_optional,
    visit_required,
    visit_sentinel,
    visit_sequence,
)
from libcst._nodes._op import AssignEqual, BaseAugOp, Comma, Dot, ImportStar, Semicolon
from libcst._nodes._whitespace import (
    BaseParenthesizableWhitespace,
    EmptyLine,
    SimpleWhitespace,
    TrailingWhitespace,
)
from libcst._visitors import CSTVisitorT


_INDENT_WHITESPACE_RE = re.compile(r"[ \f\t]+", re.UNICODE)


class BaseSuite(CSTNode, ABC):
    """
    A dummy base-class for both SimpleStatementLine and IndentedBlock. This exists to
    simplify type definitions and isinstance checks.

    > A suite is a group of statements controlled by a clause. A suite can be one or
    > more semicolon-separated simple statements on the same line as the header,
    > following the header’s colon, or it can be one or more indented statements on
    > subsequent lines.

    -- https://docs.python.org/3/reference/compound_stmts.html
    """

    body: Union[
        Sequence[Union["SimpleStatementLine", "BaseCompoundStatement"]],
        Sequence["BaseSmallStatement"],
    ]


class BaseSmallStatement(CSTNode, ABC):
    """
    Encapsulates a small statement, like "del" or "pass", and optionally adds a trailing
    semicolon. A SmallStatement is always contained inside a SimpleStatementLine or
    SimpleStatementSuite.
    """

    # This is optional for the last SmallStatement in a SimpleStatementLine or
    # SimpleStatementSuite, but all other SmallStatements inside a simple statement must
    # contain a semicolon to disambiguate multiple small statements on the same line.
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
    Represents a `del` statement. `del` is always followed by a target.
    """

    target: BaseDelTargetExpression
    whitespace_after_del: SimpleWhitespace = SimpleWhitespace(" ")

    # Optional semicolon when this is used in a statement line
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
            target=visit_required("target", self.target, visitor),
            whitespace_after_del=visit_required(
                "whitespace_after_del", self.whitespace_after_del, visitor
            ),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Pass":
        return Pass(semicolon=visit_sentinel("semicolon", self.semicolon, visitor))

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

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Break":
        return Break(semicolon=visit_sentinel("semicolon", self.semicolon, visitor))

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

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Continue":
        return Continue(semicolon=visit_sentinel("semicolon", self.semicolon, visitor))

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
    value: Optional[BaseExpression] = None

    whitespace_after_return: Union[
        SimpleWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT

    # Optional semicolon when this is used in a statement line
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
                "whitespace_after_return", self.whitespace_after_return, visitor
            ),
            value=visit_optional("value", self.value, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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
    """

    # The expression itself
    value: BaseExpression

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Expr":
        return Expr(
            value=visit_required("value", self.value, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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
    """

    body: Sequence[BaseSmallStatement]
    # a NEWLINE token is actually part of simple_stmt's grammar
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
class SimpleStatementLine(_BaseSimpleStatement):
    """
    A simple statement that's part of an IndentedBlock or Module. A simple statement is
    a series of small statements joined together by semicolons.

    This isn't differentiated from a SimpleStatementSuite in the grammar, but because a
    SimpleStatementLine can own additional whitespace that a SimpleStatementSuite
    doesn't have, we're differentiating it in the CST.
    """

    body: Sequence[BaseSmallStatement]
    leading_lines: Sequence[EmptyLine] = ()
    trailing_whitespace: TrailingWhitespace = TrailingWhitespace()

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "SimpleStatementLine":
        return SimpleStatementLine(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            body=visit_sequence("body", self.body, visitor),
            trailing_whitespace=visit_required(
                "trailing_whitespace", self.trailing_whitespace, visitor
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

      if test:<leading_whitespace><body><trailing_whitespace>

    This isn't differentiated from a SimpleStatementLine in the grammar, but because the
    two classes need to track different whitespace, we're differentiating it in the CST.
    """

    body: Sequence[BaseSmallStatement]
    leading_whitespace: SimpleWhitespace = SimpleWhitespace(" ")
    trailing_whitespace: TrailingWhitespace = TrailingWhitespace()

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "SimpleStatementSuite":
        return SimpleStatementSuite(
            leading_whitespace=visit_required(
                "leading_whitespace", self.leading_whitespace, visitor
            ),
            body=visit_sequence("body", self.body, visitor),
            trailing_whitespace=visit_required(
                "trailing_whitespace", self.trailing_whitespace, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.leading_whitespace._codegen(state)
        _BaseSimpleStatement._codegen_impl(self, state)


@add_slots
@dataclass(frozen=True)
class Else(CSTNode):
    """
    An `else` clause that appears optionally after an `If`, `While`, `Try`, or `For`
    statement.

    This node does not match `elif` clauses in `If` statements. It also does not match
    the required `else` clause in an `if` expression (`a = if b else c`).
    """

    body: BaseSuite
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Else":
        return Else(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
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


class BaseCompoundStatement(CSTNode, ABC):
    """
    > Compound statements contain (groups of) other statements; they affect or control
    > the execution of those other statements in some way. In general, compound
    > statements span multiple lines, although in simple incarnations a whole compound
    > statement may be contained in one line.

    -- https://docs.python.org/3/reference/compound_stmts.html
    """

    body: BaseSuite
    leading_lines: Sequence[EmptyLine]


@add_slots
@dataclass(frozen=True)
class If(BaseCompoundStatement):
    """
    An `if` statement. `test` holds a single test expression.

    `elif` clauses don’t have a special representation in the AST, but rather appear as
    extra `If` nodes within the `orelse` section of the previous one.
    """

    test: BaseExpression  # TODO: should be a test_nocond
    body: BaseSuite
    # A value of orelse with the type of:
    # - If signifies an elif block.
    # - Else signifies an else block.
    # - None signifies no else or elif block.
    orelse: Union["If", Else, None] = None

    # Whitespace:
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_before_test: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_after_test: SimpleWhitespace = SimpleWhitespace("")

    # TODO: _validate

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "If":
        return If(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_before_test=visit_required(
                "whitespace_before_test", self.whitespace_before_test, visitor
            ),
            test=visit_required("test", self.test, visitor),
            whitespace_after_test=visit_required(
                "whitespace_after_test", self.whitespace_after_test, visitor
            ),
            body=visit_required("body", self.body, visitor),
            orelse=visit_optional("orelse", self.orelse, visitor),
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
    Represents a block of statements beginning with an INDENT token and ending in a
    DEDENT token. Used as the body of compound statements, such as an if statement's
    body.

    A common alternative to an IndentedBlock is a SimpleStatement, which can also be
    used as a BaseSuite, meaning that it can be used as the body of many compound
    statements.
    """

    body: Sequence[Union[SimpleStatementLine, BaseCompoundStatement]]

    # An IndentedBlock always occurs after a colon in a BaseCompoundStatement, so it
    # owns the trailing whitespace for the compound statement's clause.
    #
    #   if test: # IndentedBlock's header
    #       body
    header: TrailingWhitespace = TrailingWhitespace()

    # A str represents a specific indentation. A None value uses the modules's default
    # indentation.
    #
    # This is because indentation is allowed to be inconsistent across a file, just not
    # ambiguously.
    indent: Optional[str] = None

    # There may be some trailing comments or lines after the dedent. Statements own
    # preceeding and same-line trailing comments, but not trailing lines, so it falls on
    # IndentedBlock to own it.
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
            header=visit_required("header", self.header, visitor),
            indent=self.indent,
            body=visit_body_sequence("body", self.body, visitor),
            footer=visit_sequence("footer", self.footer, visitor),
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
    An `as name` clause inside an `ExceptHandler`, `ImportAlias` or `WithItem` node.
    """

    # Identifier that the parent node will be aliased to.
    name: Name  # TODO: This should be Union[Name, Tuple, List] once we support those

    # Whitespace nodes
    whitespace_before_as: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after_as: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

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
                "whitespace_before_as", self.whitespace_before_as, visitor
            ),
            name=visit_required("name", self.name, visitor),
            whitespace_after_as=visit_required(
                "whitespace_after_as", self.whitespace_after_as, visitor
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
    An `except` clause that appears optionally after a `Try` statement.
    """

    # The body of the except
    body: BaseSuite

    # The type of exception this catches. Can be a tuple in some cases,
    # or none for an empty exception.
    type: Optional[BaseExpression] = None

    # The name that a caught exception is assigned to
    name: Optional[AsName] = None

    # Whitespace nodes
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_after_except: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        if self.type is None and self.name is not None:
            raise CSTValidationError("Cannot have a name for an empty type.")
        if self.name is not None and not isinstance(self.name.name, Name):
            raise CSTValidationError(
                "Must use a Name node for AsName name inside ExceptHandler."
            )
        if self.type is not None and self.whitespace_after_except.empty:
            raise CSTValidationError(
                "Must have at least one space after except when ExceptHandler has a type."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ExceptHandler":
        return ExceptHandler(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_after_except=visit_required(
                "whitespace_after_except", self.whitespace_after_except, visitor
            ),
            type=visit_optional("type", self.type, visitor),
            name=visit_optional("name", self.name, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
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
    A `finally` clause that appears optionally after a `Try` statement.
    """

    body: BaseSuite
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Finally":
        return Finally(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
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
    A `try` statement.
    """

    # The suite that is wrapped with a try statement.
    body: BaseSuite

    # A list of zero or more exception handlers.
    handlers: Sequence[ExceptHandler] = ()

    # An optional else case.
    orelse: Optional[Else] = None

    # An optional finally case.
    finalbody: Optional[Finally] = None

    # Whitespace
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        if len(self.handlers) == 0 and self.finalbody is None:
            raise CSTValidationError(
                "A Try statement must have at least one ExceptHandler or Finally"
            )
        if len(self.handlers) == 0 and self.orelse is not None:
            raise CSTValidationError(
                "A Try statement must have at least one ExceptHandler in order "
                + "to have an Else"
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Try":
        return Try(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
            handlers=visit_sequence("handlers", self.handlers, visitor),
            orelse=visit_optional("orelse", self.orelse, visitor),
            finalbody=visit_optional("finalbody", self.finalbody, visitor),
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


@dataclass(frozen=True)
class ImportAlias(CSTNode):
    """
    An import, with an optional AsName.
    """

    # Name or Attribute node representing the module
    name: Union[Attribute, Name]

    # Alias if it exists
    asname: Optional[AsName] = None

    # This is optional for the last ImportAlias in a Import or ImportFrom, but all
    # other ImportAliases inside an import must contain a comma to disambiguate
    # multiple small statements on the same line.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if self.asname is not None and not isinstance(self.asname.name, Name):
            raise CSTValidationError(
                "Must use a Name node for AsName name inside ImportAlias."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ImportAlias":
        return ImportAlias(
            name=visit_required("name", self.name, visitor),
            asname=visit_optional("asname", self.asname, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
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


@dataclass(frozen=True)
class Import(BaseSmallStatement):
    """
    An `import` statement.
    """

    # One or more names that are being imported
    names: Sequence[ImportAlias]

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Whitespace
    whitespace_after_import: SimpleWhitespace = SimpleWhitespace(" ")

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
                "whitespace_after_import", self.whitespace_after_import, visitor
            ),
            names=visit_sequence("names", self.names, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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


@dataclass(frozen=True)
class ImportFrom(BaseSmallStatement):
    """
    A `from x import y` statement.
    """

    # Name or Attribute node representing the module
    module: Optional[Union[Attribute, Name]]

    # One or more names that are being imported from the module
    names: Union[Sequence[ImportAlias], ImportStar]

    # Sequence of Dot nodes indicating relative import.
    relative: Sequence[Dot] = ()

    # Optional open parenthesis for multi-line import continuation.
    lpar: Optional[LeftParen] = None

    # Optional open parenthesis for multi-line import continuation.
    rpar: Optional[RightParen] = None

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Whitespace nodes owned by ImportFrom.
    whitespace_after_from: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_before_import: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_after_import: SimpleWhitespace = SimpleWhitespace(" ")

    def _validate_module(self) -> None:
        if self.module is None and len(self.relative) == 0:
            raise CSTValidationError(
                "Must have a module specified if there is no relative import."
            )

    def _validate_names(self) -> None:
        if isinstance(self.names, Sequence):
            if len(self.names) == 0:
                raise CSTValidationError(
                    "An ImportFrom must have at least one ImportAlias"
                )
            for name in self.names[:-1]:
                if name.comma is None:
                    raise CSTValidationError("Non-final ImportAliases require a comma")
            if self.lpar is not None and self.rpar is None:
                raise CSTValidationError("Cannot have left paren without right paren.")
            if self.lpar is None and self.rpar is not None:
                raise CSTValidationError("Cannot have right paren without left paren.")
        if isinstance(self.names, ImportStar):
            if self.lpar is not None or self.rpar is not None:
                raise CSTValidationError(
                    "An ImportFrom using ImportStar cannot have parens"
                )

    def _validate_whitespace(self) -> None:
        if self.whitespace_after_from.empty:
            raise CSTValidationError("Must have at least one space after from.")
        if self.whitespace_before_import.empty:
            raise CSTValidationError("Must have at least one space before import.")
        if self.whitespace_after_import.empty and self.lpar is None:
            raise CSTValidationError("Must have at least one space after import.")

    def _validate(self) -> None:
        self._validate_module()
        self._validate_names()
        self._validate_whitespace()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ImportFrom":
        names = self.names
        return ImportFrom(
            whitespace_after_from=visit_required(
                "whitespace_after_from", self.whitespace_after_from, visitor
            ),
            relative=visit_sequence("relative", self.relative, visitor),
            module=visit_optional("module", self.module, visitor),
            whitespace_before_import=visit_required(
                "whitespace_before_import", self.whitespace_before_import, visitor
            ),
            whitespace_after_import=visit_required(
                "whitespace_after_import", self.whitespace_after_import, visitor
            ),
            lpar=visit_optional("lpar", self.lpar, visitor),
            names=(
                visit_required("names", names, visitor)
                if isinstance(names, ImportStar)
                else visit_sequence("names", names, visitor)
            ),
            rpar=visit_optional("rpar", self.rpar, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
        end_node = self.names[-1] if isinstance(self.names, Sequence) else self.names
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
            if isinstance(self.names, Sequence):
                lastname = len(self.names) - 1
                for i, name in enumerate(self.names):
                    name._codegen(state, default_comma=(i != lastname))
            if isinstance(self.names, ImportStar):
                self.names._codegen(state)
            rpar = self.rpar
            if rpar is not None:
                rpar._codegen(state)

        semicolon = self.semicolon
        if isinstance(semicolon, MaybeSentinel):
            if default_semicolon:
                state.add_token("; ")
        elif isinstance(semicolon, Semicolon):
            semicolon._codegen(state)


@dataclass(frozen=True)
class AssignTarget(CSTNode):
    """
    A target for an assignment. Owns the equals.
    """

    # The target being assigned to.
    target: BaseAssignTargetExpression

    # Whitespace
    whitespace_before_equal: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_after_equal: SimpleWhitespace = SimpleWhitespace(" ")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AssignTarget":
        return AssignTarget(
            target=visit_required("target", self.target, visitor),
            whitespace_before_equal=visit_required(
                "whitespace_before_equal", self.whitespace_before_equal, visitor
            ),
            whitespace_after_equal=visit_required(
                "whitespace_after_equal", self.whitespace_after_equal, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with state.record_syntactic_position(self):
            self.target._codegen(state)

        self.whitespace_before_equal._codegen(state)
        state.add_token("=")
        self.whitespace_after_equal._codegen(state)


@dataclass(frozen=True)
class Assign(BaseSmallStatement):
    """
    An assignment statement.
    """

    # One or more targets that are being assigned to.
    targets: Sequence[AssignTarget]

    # The expression being assigned to the targets.
    value: BaseExpression

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if len(self.targets) == 0:
            raise CSTValidationError(
                "An Assign statement must have at least one AssignTarget"
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Assign":
        return Assign(
            targets=visit_sequence("targets", self.targets, visitor),
            value=visit_required("value", self.value, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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


@dataclass(frozen=True)
class AnnAssign(BaseSmallStatement):
    """
    An assignment statement.
    """

    # One or more targets that are being assigned to.
    target: BaseExpression

    # The annotation for the target.
    annotation: Annotation

    # The optional expression being assigned to the target.
    value: Optional[BaseExpression] = None

    # The equals sign used to denote assignment if there is a value.
    equal: Union[AssignEqual, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        if (
            isinstance(self.annotation.indicator, str)
            and self.annotation.indicator != ":"
        ):
            raise CSTValidationError("An Annotation must be denoted with a ':'.")
        if self.value is None and isinstance(self.equal, AssignEqual):
            raise CSTValidationError(
                "Must have a value when specifying an AssignEqual."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AnnAssign":
        return AnnAssign(
            target=visit_required("target", self.target, visitor),
            annotation=visit_required("annotation", self.annotation, visitor),
            equal=visit_sentinel("equal", self.equal, visitor),
            value=visit_optional("value", self.value, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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


@dataclass(frozen=True)
class AugAssign(BaseSmallStatement):
    """
    An augmented assignment statement.
    """

    # Target that is being assigned to
    target: BaseExpression

    # The augmented assignment operation being performed
    operator: BaseAugOp

    # The being assigned to the target.
    value: BaseExpression

    # Optional semicolon when this is used in a statement line
    semicolon: Union[Semicolon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "AugAssign":
        return AugAssign(
            target=visit_required("target", self.target, visitor),
            operator=visit_required("operator", self.operator, visitor),
            value=visit_required("value", self.value, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
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
    A single decorator that decorates a FunctionDef or a ClassDef.
    """

    # The decorator that will return a new function wrapping the parent
    # of this decorator.
    decorator: Union[Name, Attribute, Call]

    # Line comments and empty lines before this decorator. The parent FunctionDef
    # or ClassDef node owns leading lines before the comments of the first
    # decorator so that if the first decorator is removed, spacing is preserved.
    leading_lines: Sequence[EmptyLine] = ()

    # Whitespace between various tokens making up the decorator
    whitespace_after_at: SimpleWhitespace = SimpleWhitespace("")

    # Whitespace following the decorator before the next line
    trailing_whitespace: TrailingWhitespace = TrailingWhitespace()

    def _validate(self) -> None:
        if len(self.decorator.lpar) > 0 or len(self.decorator.rpar) > 0:
            raise CSTValidationError(
                "Cannot have parens around decorator in a Decorator."
            )
        if isinstance(self.decorator, Call) and not isinstance(
            self.decorator.func, (BaseAtom, Attribute)
        ):
            raise CSTValidationError(
                "Decorator call function must be an atom or attribute."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Decorator":
        return Decorator(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_after_at=visit_required(
                "whitespace_after_at", self.whitespace_after_at, visitor
            ),
            decorator=visit_required("decorator", self.decorator, visitor),
            trailing_whitespace=visit_required(
                "trailing_whitespace", self.trailing_whitespace, visitor
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


@add_slots
@dataclass(frozen=True)
class FunctionDef(BaseCompoundStatement):
    """
    A function definition.
    """

    # The function name.
    name: Name

    # The function parameters. Present even if there are no params.
    params: Parameters

    # The function body.
    body: BaseSuite

    # List of decorators applied to this function.
    decorators: Sequence[Decorator] = ()

    # An optional return type annotation
    returns: Optional[Annotation] = None

    # Optional async modifier.
    asynchronous: Optional[Asynchronous] = None

    # Leading empty lines and comments before the first decorator. We
    # assume any comments before the first decorator are owned by the
    # function definition itself. If there are no decorators, this will
    # still contain all of the empty lines and comments before the
    # function definition.
    leading_lines: Sequence[EmptyLine] = ()

    # Empty lines and comments between the final decorator and the
    # FunctionDef node. In the case of no decorators, this will be empty.
    lines_after_decorators: Sequence[EmptyLine] = ()

    # Whitespace between various tokens making up the functiondef
    whitespace_after_def: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_after_name: SimpleWhitespace = SimpleWhitespace("")
    whitespace_before_params: SimpleWhitespace = SimpleWhitespace("")
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        if len(self.name.lpar) > 0 or len(self.name.rpar) > 0:
            raise CSTValidationError("Cannot have parens around Name in a FunctionDef.")
        if self.whitespace_after_def.empty:
            raise CSTValidationError(
                "There must be at least one space between 'def' and name."
            )
        if (
            self.returns is not None
            and isinstance(self.returns.indicator, str)
            and self.returns.indicator != "->"
        ):
            raise CSTValidationError("A return Annotation must be denoted with a '->'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "FunctionDef":
        return FunctionDef(
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            decorators=visit_sequence("decorators", self.decorators, visitor),
            lines_after_decorators=visit_sequence(
                "lines_after_decorators", self.lines_after_decorators, visitor
            ),
            asynchronous=visit_optional("asynchronous", self.asynchronous, visitor),
            whitespace_after_def=visit_required(
                "whitespace_after_def", self.whitespace_after_def, visitor
            ),
            name=visit_required("name", self.name, visitor),
            whitespace_after_name=visit_required(
                "whitespace_after_name", self.whitespace_after_name, visitor
            ),
            whitespace_before_params=visit_required(
                "whitespace_before_params", self.whitespace_before_params, visitor
            ),
            params=visit_required("params", self.params, visitor),
            returns=visit_optional("returns", self.returns, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
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


@add_slots
@dataclass(frozen=True)
class ClassDef(BaseCompoundStatement):
    """
    A class definition.
    """

    # The class name.
    name: Name

    # The class body.
    body: BaseSuite

    # The base classes this class inherits from
    bases: Sequence[Arg] = ()

    # Any keywords, such as "metaclass"
    keywords: Sequence[Arg] = ()

    # List of decorators applied to this function.
    decorators: Sequence[Decorator] = ()

    # Optional open parenthesis used when there are bases or keywords.
    lpar: Union[LeftParen, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Optional close parenthesis used when there are bases or keywords.
    rpar: Union[RightParen, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Leading empty lines and comments before the first decorator. We
    # assume any comments before the first decorator are owned by the
    # class definition itself. If there are no decorators, this will
    # still contain all of the empty lines and comments before the
    # class definition.
    leading_lines: Sequence[EmptyLine] = ()

    # Empty lines and comments between the final decorator and the
    # ClassDef node. In the case of no decorators, this will be empty.
    lines_after_decorators: Sequence[EmptyLine] = ()

    # Whitespace between various tokens making up the functiondef
    whitespace_after_class: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_after_name: SimpleWhitespace = SimpleWhitespace("")
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

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
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            decorators=visit_sequence("decorators", self.decorators, visitor),
            lines_after_decorators=visit_sequence(
                "lines_after_decorators", self.lines_after_decorators, visitor
            ),
            whitespace_after_class=visit_required(
                "whitespace_after_class", self.whitespace_after_class, visitor
            ),
            name=visit_required("name", self.name, visitor),
            whitespace_after_name=visit_required(
                "whitespace_after_name", self.whitespace_after_name, visitor
            ),
            lpar=visit_sentinel("lpar", self.lpar, visitor),
            bases=visit_sequence("bases", self.bases, visitor),
            keywords=visit_sequence("keywords", self.keywords, visitor),
            rpar=visit_sentinel("rpar", self.rpar, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
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


@dataclass(frozen=True)
class WithItem(CSTNode):
    """
    A single context manager in a with block, with an optional variable name.
    """

    # Expression that evaluates to a context manager.
    item: BaseExpression

    # Variable to assign the context manager to.
    asname: Optional[AsName] = None

    # This is forbidden for the last WithItem in a With, but all other WithItems
    # inside a with block must contain a comma to separate them.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "WithItem":
        return WithItem(
            item=visit_required("item", self.item, visitor),
            asname=visit_optional("asname", self.asname, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
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
    A `with` statement.
    """

    # A list of one or more WithItems.
    items: Sequence[WithItem]

    # The suite that is wrapped with this statement.
    body: BaseSuite

    # Optional async modifier.
    asynchronous: Optional[Asynchronous] = None

    # Whitespace
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_after_with: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

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
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            asynchronous=visit_optional("asynchronous", self.asynchronous, visitor),
            whitespace_after_with=visit_required(
                "whitespace_after_with", self.whitespace_after_with, visitor
            ),
            items=visit_sequence("items", self.items, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
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
    A `for` statement.
    """

    # The target of the iterator in the for statement.
    target: BaseAssignTargetExpression

    # The iterable expression we will loop over.
    iter: BaseExpression

    # The suite that is wrapped with this statement.
    body: BaseSuite

    # An optional else case.
    orelse: Optional[Else] = None

    # Optional async modifier.
    asynchronous: Optional[Asynchronous] = None

    # Whitespace
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_after_for: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_before_in: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_after_in: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

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
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            asynchronous=visit_optional("asynchronous", self.asynchronous, visitor),
            whitespace_after_for=visit_required(
                "whitespace_after_for", self.whitespace_after_for, visitor
            ),
            target=visit_required("target", self.target, visitor),
            whitespace_before_in=visit_required(
                "whitespace_before_in", self.whitespace_before_in, visitor
            ),
            whitespace_after_in=visit_required(
                "whitespace_after_in", self.whitespace_after_in, visitor
            ),
            iter=visit_required("iter", self.iter, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
            orelse=visit_optional("orelse", self.orelse, visitor),
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
    A `while` statement.
    """

    # The test we will loop against.
    test: BaseExpression

    # The suite that is wrapped with this statement.
    body: BaseSuite

    # An optional else case.
    orelse: Optional[Else] = None

    # Whitespace
    leading_lines: Sequence[EmptyLine] = ()
    whitespace_after_while: SimpleWhitespace = SimpleWhitespace(" ")
    whitespace_before_colon: SimpleWhitespace = SimpleWhitespace("")

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
            leading_lines=visit_sequence("leading_lines", self.leading_lines, visitor),
            whitespace_after_while=visit_required(
                "whitespace_after_while", self.whitespace_after_while, visitor
            ),
            test=visit_required("test", self.test, visitor),
            whitespace_before_colon=visit_required(
                "whitespace_before_colon", self.whitespace_before_colon, visitor
            ),
            body=visit_required("body", self.body, visitor),
            orelse=visit_optional("orelse", self.orelse, visitor),
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
    exc: Optional[BaseExpression] = None

    cause: Optional[From] = None

    whitespace_after_raise: Union[
        SimpleWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT

    # Optional semicolon when this is used in a statement line
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
            whitespace_before_from = self.cause.whitespace_before_from
            has_no_gap = (
                not isinstance(whitespace_before_from, MaybeSentinel)
                and whitespace_before_from.empty
            )
            if has_no_gap and not exc._safe_to_use_with_word_operator(
                ExpressionPosition.LEFT
            ):
                raise CSTValidationError("Must have at least one space before 'from'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Raise":
        return Raise(
            whitespace_after_raise=visit_sentinel(
                "whitespace_after_raise", self.whitespace_after_raise, visitor
            ),
            exc=visit_optional("exc", self.exc, visitor),
            cause=visit_optional("cause", self.cause, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
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
    An assert statement such as "assert x > 5" or "assert x > 5, 'Uh oh!'"
    """

    # The test we are going to assert on.
    test: BaseExpression

    # The optional message to display if the assert fails.
    msg: Optional[BaseExpression] = None

    # A comma separating test and message, if there is a message.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Whitespace nodes.
    whitespace_after_assert: SimpleWhitespace = SimpleWhitespace(" ")

    # Optional semicolon when this is used in a statement line.
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
                "whitespace_after_assert", self.whitespace_after_assert, visitor
            ),
            test=visit_required("test", self.test, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
            msg=visit_optional("msg", self.msg, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
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


@dataclass(frozen=True)
class NameItem(CSTNode):
    """
    A single identifier name inside a Global or Nonlocal statement.
    """

    # Identifier name.
    name: Name

    # This is forbidden for the last NameItem in a Global/Nonlocal, but all other
    # NameItems inside a with block must contain a comma to separate them.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        # No parens around names here
        if len(self.name.lpar) > 0 or len(self.name.rpar) > 0:
            raise CSTValidationError("Cannot have parens around names in NameItem.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "NameItem":
        return NameItem(
            name=visit_required("name", self.name, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
        )

    def _codegen_impl(self, state: CodegenState, default_comma: bool = False) -> None:
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
    A `global` statement.
    """

    # A list of one or more NameItems.
    names: Sequence[NameItem]

    # Whitespace
    whitespace_after_global: SimpleWhitespace = SimpleWhitespace(" ")

    # Optional semicolon when this is used in a statement line.
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
                "whitespace_after_global", self.whitespace_after_global, visitor
            ),
            names=visit_sequence("names", self.names, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
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
    A `nonlocal` statement.
    """

    # A list of one or more NameItems.
    names: Sequence[NameItem]

    # Whitespace
    whitespace_after_nonlocal: SimpleWhitespace = SimpleWhitespace(" ")

    # Optional semicolon when this is used in a statement line.
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
                "whitespace_after_nonlocal", self.whitespace_after_nonlocal, visitor
            ),
            names=visit_sequence("names", self.names, visitor),
            semicolon=visit_sentinel("semicolon", self.semicolon, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_semicolon: bool = False
    ) -> None:
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
