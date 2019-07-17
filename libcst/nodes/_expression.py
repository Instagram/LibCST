# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import re
from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import dataclass
from enum import Enum, auto
from tokenize import (
    Floatnumber as FLOATNUMBER_RE,
    Imagnumber as IMAGNUMBER_RE,
    Intnumber as INTNUMBER_RE,
)
from typing import Callable, Generator, Optional, Sequence, Union

from typing_extensions import Literal

from libcst._add_slots import add_slots
from libcst._maybe_sentinel import MaybeSentinel
from libcst.nodes._base import (
    AnnotationIndicatorSentinel,
    CSTCodegenError,
    CSTNode,
    CSTValidationError,
)
from libcst.nodes._internal import (
    CodegenState,
    visit_optional,
    visit_required,
    visit_sentinel,
    visit_sequence,
)
from libcst.nodes._op import (
    AssignEqual,
    BaseBinaryOp,
    BaseBooleanOp,
    BaseCompOp,
    BaseUnaryOp,
    Colon,
    Comma,
    Dot,
    In,
    Is,
    IsNot,
    Not,
    NotIn,
)
from libcst.nodes._whitespace import BaseParenthesizableWhitespace, SimpleWhitespace
from libcst.visitors import CSTVisitorT


@add_slots
@dataclass(frozen=True)
class LeftSquareBracket(CSTNode):
    """
    Used by various nodes to denote a subscript or list section. This doesn't own
    the whitespace to the left of it since this is owned by the parent node.
    """

    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "LeftSquareBracket":
        return LeftSquareBracket(
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token("[")
        self.whitespace_after._codegen(state)


@add_slots
@dataclass(frozen=True)
class RightSquareBracket(CSTNode):
    """
    Used by various nodes to denote a subscript or list section. This doesn't own
    the whitespace to the right of it since this is owned by the parent node.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "RightSquareBracket":
        return RightSquareBracket(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.add_token("]")


@add_slots
@dataclass(frozen=True)
class LeftCurlyBrace(CSTNode):
    """
    Used by various nodes to denote a dict or set. This doesn't own the whitespace to
    the left of it since this is owned by the parent node.
    """

    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "LeftCurlyBrace":
        return LeftCurlyBrace(
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token("{")
        self.whitespace_after._codegen(state)


@add_slots
@dataclass(frozen=True)
class RightCurlyBrace(CSTNode):
    """
    Used by various nodes to denote a dict or set. This doesn't own the whitespace to
    the right of it since this is owned by the parent node.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "RightCurlyBrace":
        return RightCurlyBrace(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.add_token("}")


@add_slots
@dataclass(frozen=True)
class LeftParen(CSTNode):
    """
    Used by various nodes to denote a parenthesized section. This doesn't own
    the whitespace to the left of it since this is owned by the parent node.
    """

    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "LeftParen":
        return LeftParen(
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token("(")
        self.whitespace_after._codegen(state)


@add_slots
@dataclass(frozen=True)
class RightParen(CSTNode):
    """
    Used by various nodes to denote a parenthesized section. This doesn't own
    the whitespace to the right of it since this is owned by the parent node.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "RightParen":
        return RightParen(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.add_token(")")


@add_slots
@dataclass(frozen=True)
class Asynchronous(CSTNode):
    """
    Used by asynchronous function definitions, as well as `async for` and `async with`.
    """

    whitespace_after: SimpleWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        if len(self.whitespace_after.value) < 1:
            raise CSTValidationError("Must have at least one space after Asynchronous.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Asynchronous":
        return Asynchronous(
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            )
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token("async")
        self.whitespace_after._codegen(state)


class _BaseParenthesizedNode(CSTNode, ABC):
    """
    We don't want to have another level of indirection for parenthesis in
    our tree, since that makes us more of a CST than an AST. So, all the
    expressions or atoms that can be wrapped in parenthesis will subclass
    this to get that functionality.
    """

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        if self.lpar and not self.rpar:
            raise CSTValidationError("Cannot have left paren without right paren.")
        if not self.lpar and self.rpar:
            raise CSTValidationError("Cannot have right paren without left paren.")
        if len(self.lpar) != len(self.rpar):
            raise CSTValidationError("Cannot have unbalanced parens.")

    @contextmanager
    def _parenthesize(self, state: CodegenState) -> Generator[None, None, None]:
        for lpar in self.lpar:
            lpar._codegen(state)
        with state.record_syntactic_position(self):
            yield
        for rpar in self.rpar:
            rpar._codegen(state)


class ExpressionPosition(Enum):
    LEFT = auto()
    RIGHT = auto()


class BaseExpression(_BaseParenthesizedNode, ABC):
    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        """
        Returns true if this expression is safe to be use with a word operator
        such as "not" without space between the operator an ourselves. Examples
        where this is true are "not(True)", "(1)in[1,2,3]", etc. This base
        function handles parenthesized nodes, but certain nodes such as tuples,
        dictionaries and lists will override this to signifiy that they're always
        safe.
        """

        return len(self.lpar) > 0 and len(self.rpar) > 0


class BaseAtom(BaseExpression, ABC):
    """
    > Atoms are the most basic elements of expressions. The simplest atoms are
    > identifiers or literals. Forms enclosed in parentheses, brackets or braces are
    > also categorized syntactically as atoms.

    -- https://docs.python.org/3/reference/expressions.html#atoms
    """

    pass


class BaseAssignTargetExpression(BaseExpression, ABC):
    """
    An expression that's valid on the left side of an assign statement.

    Python's grammar defines all expression as valid in this position, but the AST
    compiler further restricts the allowed types, which is what this type attempts to
    express.

    See also: https://github.com/python/cpython/blob/v3.8.0a4/Python/ast.c#L1120
    """

    pass


class BaseDelTargetExpression(BaseExpression, ABC):
    """
    An expression that's valid on the right side of a 'del' statement.

    Python's grammar defines all expression as valid in this position, but the AST
    compiler further restricts the allowed types, which is what this type attempts to
    express.

    This is similar to a BaseAssignTargetExpression, but excludes `Starred`.

    See also: https://github.com/python/cpython/blob/v3.8.0a4/Python/ast.c#L1120
    and: https://github.com/python/cpython/blob/v3.8.0a4/Python/compile.c#L4854
    """

    pass


@add_slots
@dataclass(frozen=True)
class Name(BaseAssignTargetExpression, BaseDelTargetExpression, BaseAtom):
    # The actual identifier string
    value: str

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Name":
        return Name(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=self.value,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _validate(self) -> None:
        super(Name, self)._validate()
        if len(self.value) == 0:
            raise CSTValidationError("Cannot have empty name identifier.")
        if not self.value.isidentifier():
            raise CSTValidationError("Name is not a valid identifier.")

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token(self.value)


@add_slots
@dataclass(frozen=True)
class Ellipses(BaseAtom):
    """
    An ellipses "..."
    """

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Ellipses":
        return Ellipses(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token("...")


class BaseNumber(BaseAtom, ABC):
    """
    A type that can be used anywhere that you need to explicitly take any
    number type.
    """

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        """
        Numbers are funny. The expression "5in [1,2,3,4,5]" is a valid expression
        which evaluates to "True". So, encapsulate that here by allowing zero spacing
        with the left hand side of an expression with a comparison operator.
        """
        if position == ExpressionPosition.LEFT:
            return True
        return super(BaseNumber, self)._safe_to_use_with_word_operator(position)


@add_slots
@dataclass(frozen=True)
class Integer(BaseNumber):
    value: str

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Integer":
        return Integer(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=self.value,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _validate(self) -> None:
        super(Integer, self)._validate()
        if not re.fullmatch(INTNUMBER_RE, self.value):
            raise CSTValidationError("Number is not a valid integer.")

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token(self.value)


@add_slots
@dataclass(frozen=True)
class Float(BaseNumber):
    value: str

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Float":
        return Float(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=self.value,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _validate(self) -> None:
        super(Float, self)._validate()
        if not re.fullmatch(FLOATNUMBER_RE, self.value):
            raise CSTValidationError("Number is not a valid float.")

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token(self.value)


@add_slots
@dataclass(frozen=True)
class Imaginary(BaseNumber):
    value: str

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Imaginary":
        return Imaginary(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=self.value,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _validate(self) -> None:
        super(Imaginary, self)._validate()
        if not re.fullmatch(IMAGNUMBER_RE, self.value):
            raise CSTValidationError("Number is not a valid imaginary.")

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token(self.value)


class BaseString(BaseAtom, ABC):
    """
    A type that can be used anywhere that you need to explicitly take any
    string.
    """

    pass


@add_slots
@dataclass(frozen=True)
class SimpleString(BaseString):
    value: str

    # Sequence of open parenthesis for precidence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precidence dictation.
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        super(SimpleString, self)._validate()

        # Validate any prefix
        prefix = self._get_prefix()
        if prefix not in ("", "r", "u", "b", "br", "rb"):
            raise CSTValidationError("Invalid string prefix.")
        prefixlen = len(prefix)
        # Validate wrapping quotes
        if len(self.value) < (prefixlen + 2):
            raise CSTValidationError("String must have enclosing quotes.")
        if (
            self.value[prefixlen] not in ['"', "'"]
            or self.value[prefixlen] != self.value[-1]
        ):
            raise CSTValidationError("String must have matching enclosing quotes.")
        # Check validity of triple-quoted strings
        if len(self.value) >= (prefixlen + 6):
            if self.value[prefixlen] == self.value[prefixlen + 1]:
                # We know this isn't an empty string, so there needs to be a third
                # identical enclosing token.
                if (
                    self.value[prefixlen] != self.value[prefixlen + 2]
                    or self.value[prefixlen] != self.value[-2]
                    or self.value[prefixlen] != self.value[-3]
                ):
                    raise CSTValidationError(
                        "String must have matching enclosing quotes."
                    )
        # We should check the contents as well, but this is pretty complicated,
        # partially due to triple-quoted strings.

    def _get_prefix(self) -> str:
        prefix = ""
        for c in self.value:
            if c in ['"', "'"]:
                break
            prefix += c
        return prefix.lower()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "SimpleString":
        return SimpleString(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=self.value,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token(self.value)


class BaseFormattedStringContent(CSTNode, ABC):
    """
    A type that can be used anywhere that you need to take any part of a f-string.
    """

    pass


@add_slots
@dataclass(frozen=True)
class FormattedStringText(BaseFormattedStringContent):
    # The raw string value.
    value: str

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "FormattedStringText":
        return FormattedStringText(value=self.value)

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token(self.value)


@add_slots
@dataclass(frozen=True)
class FormattedStringExpression(BaseFormattedStringContent):
    # The expression we will render when printing the string
    expression: BaseExpression

    # An optional conversion specifier
    conversion: Optional[str] = None

    # An optional format specifier
    format_spec: Optional[Sequence[BaseFormattedStringContent]] = None

    # Whitespace
    whitespace_before_expression: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after_expression: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        if self.conversion is not None and self.conversion not in ("s", "r", "a"):
            raise CSTValidationError("Invalid f-string conversion.")

    def _visit_and_replace_children(
        self, visitor: CSTVisitorT
    ) -> "FormattedStringExpression":
        format_spec = self.format_spec
        return FormattedStringExpression(
            whitespace_before_expression=visit_required(
                "whitespace_before_expression",
                self.whitespace_before_expression,
                visitor,
            ),
            expression=visit_required("expression", self.expression, visitor),
            whitespace_after_expression=visit_required(
                "whitespace_after_expression", self.whitespace_after_expression, visitor
            ),
            conversion=self.conversion,
            format_spec=(
                visit_sequence("format_spec", format_spec, visitor)
                if format_spec is not None
                else None
            ),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with state.record_syntactic_position(self):
            state.add_token("{")
            self.whitespace_before_expression._codegen(state)
            self.expression._codegen(state)
            self.whitespace_after_expression._codegen(state)
            conversion = self.conversion
            if conversion is not None:
                state.add_token("!")
                state.add_token(conversion)
            format_spec = self.format_spec
            if format_spec is not None:
                state.add_token(":")
                for spec in format_spec:
                    spec._codegen(state)
            state.add_token("}")


@add_slots
@dataclass(frozen=True)
class FormattedString(BaseString):
    # Sequence of formatted string parts
    parts: Sequence[BaseFormattedStringContent]

    # String start indicator
    start: str = 'f"'

    # String end indicator
    end: str = '"'

    # Sequence of open parenthesis for precidence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precidence dictation.
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        super(FormattedString, self)._validate()

        # Validate any prefix
        prefix = self._get_prefix()
        if prefix not in ("f", "fr", "rf"):
            raise CSTValidationError("Invalid f-string prefix.")

        # Validate wrapping quotes
        starttoken = self.start[len(prefix) :]
        if starttoken != self.end:
            raise CSTValidationError("f-string must have matching enclosing quotes.")

        # Validate valid wrapping quote usage
        if starttoken not in ('"', "'", '"""', "'''"):
            raise CSTValidationError("Invalid f-string enclosing quotes.")

    def _get_prefix(self) -> str:
        prefix = ""
        for c in self.start:
            if c in ['"', "'"]:
                break
            prefix += c
        return prefix.lower()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "FormattedString":
        return FormattedString(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            start=self.start,
            parts=visit_sequence("parts", self.parts, visitor),
            end=self.end,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token(self.start)
            for part in self.parts:
                part._codegen(state)
            state.add_token(self.end)


@add_slots
@dataclass(frozen=True)
class ConcatenatedString(BaseString):
    # String on the left of the concatenation.
    left: Union[SimpleString, FormattedString]

    # String on the right of the concatenation.
    right: Union[SimpleString, FormattedString, "ConcatenatedString"]

    # Sequence of open parenthesis for precidence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precidence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace between strings.
    whitespace_between: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        super(ConcatenatedString, self)._validate()

        # Strings that are concatenated cannot have parens.
        if bool(self.left.lpar) or bool(self.left.rpar):
            raise CSTValidationError("Cannot concatenate parenthesized strings.")
        if bool(self.right.lpar) or bool(self.right.rpar):
            raise CSTValidationError("Cannot concatenate parenthesized strings.")

        # Cannot concatenate str and bytes
        leftbytes = "b" in self.left._get_prefix()
        if isinstance(self.right, ConcatenatedString):
            rightbytes = "b" in self.right.left._get_prefix()
        elif isinstance(self.right, SimpleString):
            rightbytes = "b" in self.right._get_prefix()
        elif isinstance(self.right, FormattedString):
            rightbytes = "b" in self.right._get_prefix()
        else:
            raise Exception("Logic error!")
        if leftbytes != rightbytes:
            raise CSTValidationError("Cannot concatenate string and bytes.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ConcatenatedString":
        return ConcatenatedString(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            left=visit_required("left", self.left, visitor),
            whitespace_between=visit_required(
                "whitespace_between", self.whitespace_between, visitor
            ),
            right=visit_required("right", self.right, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.left._codegen(state)
            self.whitespace_between._codegen(state)
            self.right._codegen(state)


@add_slots
@dataclass(frozen=True)
class ComparisonTarget(CSTNode):
    """
    A target for a comparison. Owns the comparison operator itself.
    """

    # The actual comparison operator
    operator: BaseCompOp

    # The right hand side of the comparison operation
    comparator: BaseExpression

    def _validate(self) -> None:
        # Validate operator spacing rules
        if (
            isinstance(self.operator, (In, NotIn, Is, IsNot))
            and self.operator.whitespace_after.empty
            and not self.comparator._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            )
        ):
            raise CSTValidationError(
                "Must have at least one space around comparison operator."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ComparisonTarget":
        return ComparisonTarget(
            operator=visit_required("operator", self.operator, visitor),
            comparator=visit_required("comparator", self.comparator, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.operator._codegen(state)
        self.comparator._codegen(state)


@add_slots
@dataclass(frozen=True)
class Comparison(BaseExpression):
    """
    Any comparison such as "x < y < z"
    """

    # The left hand side of the comparison operation
    left: BaseExpression

    # The actual comparison operator
    comparisons: Sequence[ComparisonTarget]

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        if super(Comparison, self)._safe_to_use_with_word_operator(position):
            # we have parenthesis
            return True
        if position == ExpressionPosition.LEFT:
            return self.comparisons[-1].comparator._safe_to_use_with_word_operator(
                ExpressionPosition.LEFT
            )
        else:  # position == ExpressionPosition.RIGHT:
            return self.left._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)

    def _validate(self) -> None:
        # Perform any validation on base type
        super(Comparison, self)._validate()

        if len(self.comparisons) == 0:
            raise CSTValidationError("Must have at least one ComparisonTarget.")

        # Validate operator spacing rules
        previous_comparator = self.left
        for target in self.comparisons:
            operator = target.operator
            if (
                isinstance(operator, (In, NotIn, Is, IsNot))
                and operator.whitespace_before.empty
                and not previous_comparator._safe_to_use_with_word_operator(
                    ExpressionPosition.LEFT
                )
            ):
                raise CSTValidationError(
                    "Must have at least one space around comparison operator."
                )
            previous_comparator = target.comparator

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Comparison":
        return Comparison(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            left=visit_required("left", self.left, visitor),
            comparisons=visit_sequence("comparisons", self.comparisons, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.left._codegen(state)
            for comp in self.comparisons:
                comp._codegen(state)


@add_slots
@dataclass(frozen=True)
class UnaryOperation(BaseExpression):
    """
    Any generic unary expression, such as "not x" or "-x".
    """

    # The unary operator applied to the expression
    operator: BaseUnaryOp

    # The actual expression or atom
    expression: BaseExpression

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        # Perform any validation on base type
        super(UnaryOperation, self)._validate()

        if (
            isinstance(self.operator, Not)
            and self.operator.whitespace_after.empty
            and not self.expression._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            )
        ):
            raise CSTValidationError("Must have at least one space after not operator.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "UnaryOperation":
        return UnaryOperation(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            operator=visit_required("operator", self.operator, visitor),
            expression=visit_required("expression", self.expression, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.operator._codegen(state)
            self.expression._codegen(state)


@add_slots
@dataclass(frozen=True)
class BinaryOperation(BaseExpression):
    """
    Any binary operation such as "x << y" or "y + z".
    """

    # The left hand side of the operation
    left: BaseExpression

    # The actual operator
    operator: BaseBinaryOp

    # The right hand side of the operation
    right: BaseExpression

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "BinaryOperation":
        return BinaryOperation(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            left=visit_required("left", self.left, visitor),
            operator=visit_required("operator", self.operator, visitor),
            right=visit_required("right", self.right, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.left._codegen(state)
            self.operator._codegen(state)
            self.right._codegen(state)


@add_slots
@dataclass(frozen=True)
class BooleanOperation(BaseExpression):
    """
    Any boolean operation such as "x or y" or "z and w"
    """

    # The left hand side of the operation
    left: BaseExpression

    # The actual operator
    operator: BaseBooleanOp

    # The right hand side of the operation
    right: BaseExpression

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        # Paren validation and such
        super(BooleanOperation, self)._validate()
        # Validate spacing rules
        if (
            self.operator.whitespace_before.empty
            and not self.left._safe_to_use_with_word_operator(ExpressionPosition.LEFT)
        ):
            raise CSTValidationError(
                "Must have at least one space around boolean operator."
            )
        if (
            self.operator.whitespace_after.empty
            and not self.right._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError(
                "Must have at least one space around boolean operator."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "BooleanOperation":
        return BooleanOperation(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            left=visit_required("left", self.left, visitor),
            operator=visit_required("operator", self.operator, visitor),
            right=visit_required("right", self.right, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.left._codegen(state)
            self.operator._codegen(state)
            self.right._codegen(state)


@dataclass(frozen=True)
class Attribute(BaseAssignTargetExpression, BaseDelTargetExpression):
    """
    An attribute reference, such as "x.y". Note that in the case of
    "x.y.z", the outer attribute will have an attr of "z" and the
    value will be another Attribute referencing the "y" attribute on
    "x".
    """

    # Expression which, when evaluated, will have 'attr' as an attribute
    value: BaseExpression

    # Name of the attribute being accessed.
    attr: Name

    # Separating dot, with any whitespace it owns.
    dot: Dot = Dot()

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Attribute":
        return Attribute(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=visit_required("value", self.value, visitor),
            dot=visit_required("dot", self.dot, visitor),
            attr=visit_required("attr", self.attr, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.value._codegen(state)
            self.dot._codegen(state)
            self.attr._codegen(state)


@add_slots
@dataclass(frozen=True)
class Index(CSTNode):
    """
    Any index as passed to a subscript.
    """

    # The index value itself.
    value: BaseExpression

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Index":
        return Index(value=visit_required("value", self.value, visitor))

    def _codegen_impl(self, state: CodegenState) -> None:
        with state.record_syntactic_position(self):
            self.value._codegen(state)


@add_slots
@dataclass(frozen=True)
class Slice(CSTNode):
    """
    Any slice operation in a subscript, such as "1:", "2:3:4", etc. Note
    that the grammar does NOT allow parenthesis around a slice so they
    are not supported here.
    """

    # The lower bound in the slice, if present
    lower: Optional[BaseExpression]

    # The upper bound in the slice, if present
    upper: Optional[BaseExpression]

    # The step in the slice, if present
    step: Optional[BaseExpression] = None

    # The first slice operator
    first_colon: Colon = Colon()

    # The second slice operator, usually omitted
    second_colon: Union[Colon, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Slice":
        return Slice(
            lower=visit_optional("lower", self.lower, visitor),
            first_colon=visit_required("first_colon", self.first_colon, visitor),
            upper=visit_optional("upper", self.upper, visitor),
            second_colon=visit_sentinel("second_colon", self.second_colon, visitor),
            step=visit_optional("step", self.step, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with state.record_syntactic_position(self):
            lower = self.lower
            if lower is not None:
                lower._codegen(state)
            self.first_colon._codegen(state)
            upper = self.upper
            if upper is not None:
                upper._codegen(state)
            second_colon = self.second_colon
            if second_colon is MaybeSentinel.DEFAULT and self.step is not None:
                state.add_token(":")
            elif isinstance(second_colon, Colon):
                second_colon._codegen(state)
            step = self.step
            if step is not None:
                step._codegen(state)


@add_slots
@dataclass(frozen=True)
class ExtSlice(CSTNode):
    """
    A list of slices, such as "1:2, 3". Not used in the stdlib but still
    valid. This also does not allow for wrapping parenthesis.
    "x".
    """

    # A slice or index that is part of the extslice.
    slice: Union[Index, Slice]

    # Separating comma, with any whitespace it owns.
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ExtSlice":
        return ExtSlice(
            slice=visit_required("slice", self.slice, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
        )

    def _codegen_impl(self, state: CodegenState, default_comma: bool = False) -> None:
        with state.record_syntactic_position(self):
            self.slice._codegen(state)

        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            state.add_token(", ")
        elif isinstance(comma, Comma):
            comma._codegen(state)


@add_slots
@dataclass(frozen=True)
class Subscript(BaseAssignTargetExpression, BaseDelTargetExpression):
    """
    A subscript reference such as "x[2]".
    """

    # Expression which, when evaluated, will be subscripted.
    value: BaseExpression

    # Subscript to take on the value.
    slice: Union[Index, Slice, Sequence[ExtSlice]]

    # Open bracket surrounding the slice
    lbracket: LeftSquareBracket = LeftSquareBracket()

    # Close bracket surrounding the slice
    rbracket: RightSquareBracket = RightSquareBracket()

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace
    whitespace_after_value: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        super(Subscript, self)._validate()
        if isinstance(self.slice, Sequence):
            # Validate valid commas
            if len(self.slice) < 1:
                raise CSTValidationError("Cannot have empty ExtSlice.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Subscript":
        slice = self.slice
        return Subscript(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            value=visit_required("value", self.value, visitor),
            whitespace_after_value=visit_required(
                "whitespace_after_value", self.whitespace_after_value, visitor
            ),
            lbracket=visit_required("lbracket", self.lbracket, visitor),
            slice=visit_required("slice", slice, visitor)
            if isinstance(slice, (Index, Slice))
            else visit_sequence("slice", slice, visitor),
            rbracket=visit_required("rbracket", self.rbracket, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.value._codegen(state)
            self.whitespace_after_value._codegen(state)
            self.lbracket._codegen(state)
            if isinstance(self.slice, (Index, Slice)):
                self.slice._codegen(state)
            elif isinstance(self.slice, Sequence):
                lastslice = len(self.slice) - 1
                for i, slice in enumerate(self.slice):
                    slice._codegen(state, default_comma=(i != lastslice))
            else:
                # We can make pyre happy this way!
                raise Exception("Logic error!")
            self.rbracket._codegen(state)


@add_slots
@dataclass(frozen=True)
class Annotation(CSTNode):
    """
    An annotation.
    """

    # The annotation itself.
    annotation: Union[Name, Attribute, BaseString, Subscript]

    # The indicator token before the annotation.
    indicator: Union[
        str, AnnotationIndicatorSentinel
    ] = AnnotationIndicatorSentinel.DEFAULT

    # Whitespace
    whitespace_before_indicator: Union[
        BaseParenthesizableWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT
    whitespace_after_indicator: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        if isinstance(self.indicator, str) and self.indicator not in [":", "->"]:
            raise CSTValidationError(
                "An Annotation indicator must be one of ':', '->'."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Annotation":
        return Annotation(
            whitespace_before_indicator=visit_sentinel(
                "whitespace_before_indicator", self.whitespace_before_indicator, visitor
            ),
            indicator=self.indicator,
            whitespace_after_indicator=visit_required(
                "whitespace_after_indicator", self.whitespace_after_indicator, visitor
            ),
            annotation=visit_required("annotation", self.annotation, visitor),
        )

    def _codegen_impl(
        self, state: CodegenState, default_indicator: Optional[str] = None
    ) -> None:
        # First, figure out the indicator which tells us default whitespace.
        indicator = self.indicator
        if isinstance(indicator, AnnotationIndicatorSentinel):
            if default_indicator is None:
                raise CSTCodegenError(
                    "Must specify a concrete default_indicator if default used on indicator."
                )
            indicator = default_indicator

        # Now, output the whitespace
        whitespace_before_indicator = self.whitespace_before_indicator
        if isinstance(whitespace_before_indicator, BaseParenthesizableWhitespace):
            whitespace_before_indicator._codegen(state)
        elif isinstance(whitespace_before_indicator, MaybeSentinel):
            if indicator == "->":
                state.add_token(" ")
        else:
            raise Exception("Logic error!")

        # Now, output the indicator and the rest of the annotation
        state.add_token(indicator)
        self.whitespace_after_indicator._codegen(state)

        with state.record_syntactic_position(self):
            self.annotation._codegen(state)


@add_slots
@dataclass(frozen=True)
class ParamStar(CSTNode):
    """
    A sentinel indicator on a Parameter list to denote that the following params
    are kwonly args.
    """

    # Comma that comes after the star.
    comma: Comma = Comma(whitespace_after=SimpleWhitespace(" "))

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ParamStar":
        return ParamStar(comma=visit_required("comma", self.comma, visitor))

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token("*")
        self.comma._codegen(state)


@add_slots
@dataclass(frozen=True)
class Param(CSTNode):
    """
    A single parameter in a Parameter list. May contain a type annotation and
    in some cases a default.
    """

    # The parameter name itself
    name: Name

    # Any optional annotation
    annotation: Optional[Annotation] = None

    # The equals sign used to denote assignment if there is a default.
    equal: Union[AssignEqual, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Any optional default
    default: Optional[BaseExpression] = None

    # Any trailing comma
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Optional star appearing before name for star_arg and star_kwarg
    star: Union[str, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Whitespace
    whitespace_after_star: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after_param: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        if self.default is None and isinstance(self.equal, AssignEqual):
            raise CSTValidationError(
                "Must have a default when specifying an AssignEqual."
            )
        if isinstance(self.star, str) and self.star not in ("", "*", "**"):
            raise CSTValidationError("Must specify either '', '*' or '**' for star.")
        if (
            self.annotation is not None
            and isinstance(self.annotation.indicator, str)
            and self.annotation.indicator != ":"
        ):
            raise CSTValidationError("A param Annotation must be denoted with a ':'.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Param":
        return Param(
            star=self.star,
            whitespace_after_star=visit_required(
                "whitespace_after_star", self.whitespace_after_star, visitor
            ),
            name=visit_required("name", self.name, visitor),
            annotation=visit_optional("annotation", self.annotation, visitor),
            equal=visit_sentinel("equal", self.equal, visitor),
            default=visit_optional("default", self.default, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
            whitespace_after_param=visit_required(
                "whitespace_after_param", self.whitespace_after_param, visitor
            ),
        )

    def _codegen_impl(
        self,
        state: CodegenState,
        default_star: Optional[str] = None,
        default_comma: bool = False,
    ) -> None:
        with state.record_syntactic_position(self):
            star = self.star
            if isinstance(star, MaybeSentinel):
                if default_star is None:
                    raise CSTCodegenError(
                        "Must specify a concrete default_star if default used on star."
                    )
                star = default_star
            if isinstance(star, str):
                state.add_token(star)
            self.whitespace_after_star._codegen(state)
            self.name._codegen(state)

        annotation = self.annotation
        if annotation is not None:
            annotation._codegen(state, default_indicator=":")
        equal = self.equal
        if equal is MaybeSentinel.DEFAULT and self.default is not None:
            state.add_token(" = ")
        elif isinstance(equal, AssignEqual):
            equal._codegen(state)
        default = self.default
        if default is not None:
            default._codegen(state)
        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            state.add_token(", ")
        elif isinstance(comma, Comma):
            comma._codegen(state)

        self.whitespace_after_param._codegen(state)


@add_slots
@dataclass(frozen=True)
class Parameters(CSTNode):
    """
    A function or lambda parameter list.
    """

    # Positional parameters.
    params: Sequence[Param] = ()

    # Positional parameters with defaults.
    default_params: Sequence[Param] = ()

    # Optional parameter that captures unspecified positional arguments or a sentinel
    # star that dictates parameters following are kwonly args.
    star_arg: Union[Param, ParamStar, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Keyword-only params that may or may not have defaults.
    kwonly_params: Sequence[Param] = ()

    # Optional parameter that captures unspecified kwargs.
    star_kwarg: Optional[Param] = None

    def _validate_stars_sequence(self, vals: Sequence[Param], *, section: str) -> None:
        if len(vals) == 0:
            return
        for val in vals:
            if isinstance(val.star, str) and val.star != "":
                raise CSTValidationError(
                    f"Expecting a star prefix of '' for {section} Param."
                )

    def _validate_kwonlystar(self) -> None:
        if isinstance(self.star_arg, ParamStar) and len(self.kwonly_params) == 0:
            raise CSTValidationError(
                "Must have at least one kwonly param if ParamStar is used."
            )

    def _validate_defaults(self) -> None:
        for param in self.params:
            if param.default is not None:
                raise CSTValidationError(
                    "Cannot have defaults for params. Place them in default_params."
                )
        for param in self.default_params:
            if param.default is None:
                raise CSTValidationError(
                    "Must have defaults for default_params. Place non-defaults in params."
                )
        if isinstance(self.star_arg, Param) and self.star_arg.default is not None:
            raise CSTValidationError("Cannot have default for star_arg.")
        if self.star_kwarg is not None and self.star_kwarg.default is not None:
            raise CSTValidationError("Cannot have default for star_kwarg.")

    def _validate_stars(self) -> None:
        if len(self.params) > 0:
            self._validate_stars_sequence(self.params, section="params")
        if len(self.default_params) > 0:
            self._validate_stars_sequence(self.default_params, section="default_params")
        star_arg = self.star_arg
        if (
            isinstance(star_arg, Param)
            and isinstance(star_arg.star, str)
            and star_arg.star != "*"
        ):
            raise CSTValidationError(
                "Expecting a star prefix of '*' for star_arg Param."
            )
        if len(self.kwonly_params) > 0:
            self._validate_stars_sequence(self.kwonly_params, section="kwonly_params")
        star_kwarg = self.star_kwarg
        if (
            star_kwarg is not None
            and isinstance(star_kwarg.star, str)
            and star_kwarg.star != "**"
        ):
            raise CSTValidationError(
                "Expecting a star prefix of '**' for star_kwarg Param."
            )

    def _validate(self) -> None:
        # Validate kwonly_param star placement semantics.
        self._validate_kwonlystar()
        # Validate defaults semantics for params, default_params and star_arg/star_kwarg.
        self._validate_defaults()
        # Validate that we don't have random stars on non star_kwarg.
        self._validate_stars()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Parameters":
        return Parameters(
            params=visit_sequence("params", self.params, visitor),
            default_params=visit_sequence(
                "default_params", self.default_params, visitor
            ),
            star_arg=visit_sentinel("star_arg", self.star_arg, visitor),
            kwonly_params=visit_sequence("kwonly_params", self.kwonly_params, visitor),
            star_kwarg=visit_optional("star_kwarg", self.star_kwarg, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        # TODO: remove this when fallback to syntactic whitespace becomes available
        with state.record_syntactic_position(self):
            # Compute the star existence first so we can ask about whether
            # each element is the last in the list or not.
            star_arg = self.star_arg
            if isinstance(star_arg, MaybeSentinel):
                starincluded = len(self.kwonly_params) > 0
            elif isinstance(star_arg, (Param, ParamStar)):
                starincluded = True
            else:
                starincluded = False
            # Render out the params first, computing necessary trailing commas.
            lastparam = len(self.params) - 1
            more_values = (
                len(self.default_params) > 0
                or starincluded
                or len(self.kwonly_params) > 0
                or self.star_kwarg is not None
            )
            for i, param in enumerate(self.params):
                param._codegen(
                    state, default_star="", default_comma=(i < lastparam or more_values)
                )
            # Render out the default_params next, computing necessary trailing commas.
            lastparam = len(self.default_params) - 1
            more_values = (
                starincluded
                or len(self.kwonly_params) > 0
                or self.star_kwarg is not None
            )
            for i, param in enumerate(self.default_params):
                param._codegen(
                    state, default_star="", default_comma=(i < lastparam or more_values)
                )
            # Render out optional star sentinel if its explicitly included or
            # if we are inferring it from kwonly_params. Otherwise, render out the
            # optional star_arg.
            if isinstance(star_arg, MaybeSentinel):
                if starincluded:
                    state.add_token("*, ")
            elif isinstance(star_arg, Param):
                more_values = len(self.kwonly_params) > 0 or self.star_kwarg is not None
                star_arg._codegen(state, default_star="*", default_comma=more_values)
            elif isinstance(star_arg, ParamStar):
                star_arg._codegen(state)
            # Render out the kwonly_args next, computing necessary trailing commas.
            lastparam = len(self.kwonly_params) - 1
            more_values = self.star_kwarg is not None
            for i, param in enumerate(self.kwonly_params):
                param._codegen(
                    state, default_star="", default_comma=(i < lastparam or more_values)
                )
            # Finally, render out any optional star_kwarg
            star_kwarg = self.star_kwarg
            if star_kwarg is not None:
                star_kwarg._codegen(state, default_star="**", default_comma=False)


@add_slots
@dataclass(frozen=True)
class Lambda(BaseExpression):
    # The parameters to the lambda
    params: Parameters

    # The body of the lambda
    body: BaseExpression

    # The colon separating the parameters from the body
    colon: Colon = Colon(whitespace_after=SimpleWhitespace(" "))

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace
    whitespace_after_lambda: Union[
        BaseParenthesizableWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        # Validate parents
        super(Lambda, self)._validate()
        # Sum up all parameters
        all_params = [
            *self.params.params,
            *self.params.default_params,
            *self.params.kwonly_params,
        ]
        if isinstance(self.params.star_arg, Param):
            all_params.append(self.params.star_arg)
        if self.params.star_kwarg is not None:
            all_params.append(self.params.star_kwarg)
        # Check for nonzero parameters because several checks care
        # about this.
        if len(all_params) > 0:
            for param in all_params:
                if param.annotation is not None:
                    raise CSTValidationError(
                        "Lambda params cannot have type annotations."
                    )
            if (
                isinstance(self.whitespace_after_lambda, BaseParenthesizableWhitespace)
                and self.whitespace_after_lambda.empty
            ):
                raise CSTValidationError(
                    "Must have at least one space after lambda when specifying params"
                )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Lambda":
        return Lambda(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            whitespace_after_lambda=visit_sentinel(
                "whitespace_after_lambda", self.whitespace_after_lambda, visitor
            ),
            params=visit_required("params", self.params, visitor),
            colon=visit_required("colon", self.colon, visitor),
            body=visit_required("body", self.body, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token("lambda")
            whitespace_after_lambda = self.whitespace_after_lambda
            if isinstance(whitespace_after_lambda, MaybeSentinel):
                if not (
                    len(self.params.params) == 0
                    and len(self.params.default_params) == 0
                    and not isinstance(self.params.star_arg, Param)
                    and len(self.params.kwonly_params) == 0
                    and self.params.star_kwarg is None
                ):
                    # We have one or more params, provide a space
                    state.add_token(" ")
            elif isinstance(whitespace_after_lambda, BaseParenthesizableWhitespace):
                whitespace_after_lambda._codegen(state)
            self.params._codegen(state)
            self.colon._codegen(state)
            self.body._codegen(state)


@add_slots
@dataclass(frozen=True)
class Arg(CSTNode):
    """
    A single argument to a Call. It may be a * or a ** expansion, or it may be in
    the form of "keyword=expression" for named arguments.
    """

    # The argument expression itself
    value: BaseExpression

    # Optional keyword for the argument
    keyword: Optional[Name] = None

    # The equals sign used to denote assignment if there is a keyword.
    equal: Union[AssignEqual, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Any trailing comma
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Optional star appearing before name for * and ** expansion
    star: Literal["", "*", "**"] = ""

    # Whitespace
    whitespace_after_star: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after_arg: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _validate(self) -> None:
        if self.keyword is None and isinstance(self.equal, AssignEqual):
            raise CSTValidationError(
                "Must have a keyword when specifying an AssignEqual."
            )
        if self.star not in ("", "*", "**"):
            raise CSTValidationError("Must specify either '', '*' or '**' for star.")
        if self.star in ("*", "**") and self.keyword is not None:
            raise CSTValidationError("Cannot specify a star and a keyword together.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Arg":
        return Arg(
            star=self.star,
            whitespace_after_star=visit_required(
                "whitespace_after_star", self.whitespace_after_star, visitor
            ),
            keyword=visit_optional("keyword", self.keyword, visitor),
            equal=visit_sentinel("equal", self.equal, visitor),
            value=visit_required("value", self.value, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
            whitespace_after_arg=visit_required(
                "whitespace_after_arg", self.whitespace_after_arg, visitor
            ),
        )

    def _codegen_impl(self, state: CodegenState, default_comma: bool = False) -> None:
        with state.record_syntactic_position(self):
            state.add_token(self.star)
            self.whitespace_after_star._codegen(state)
            keyword = self.keyword
            if keyword is not None:
                keyword._codegen(state)
            equal = self.equal
            if equal is MaybeSentinel.DEFAULT and self.keyword is not None:
                state.add_token(" = ")
            elif isinstance(equal, AssignEqual):
                equal._codegen(state)
            self.value._codegen(state)

        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            state.add_token(", ")
        elif isinstance(comma, Comma):
            comma._codegen(state)
        self.whitespace_after_arg._codegen(state)


class _BaseExpressionWithArgs(BaseExpression, ABC):
    """
    Arguments are complicated enough that we can't represent them easily
    in typing. So, we have common validation functions here.
    """

    # Sequence of arguments that will be passed to the function call
    args: Sequence[Arg] = ()

    def _check_kwargs_or_keywords(
        self, arg: Arg
    ) -> Optional[Callable[[Arg], Callable]]:
        """
        Validates that we only have a mix of "keyword=arg" and "**arg" expansion.
        """

        if arg.keyword is not None:
            # Valid, keyword argument
            return None
        elif arg.star == "**":
            # Valid, kwargs
            return None
        elif arg.star == "*":
            # Invalid, cannot have "*" follow "**"
            raise CSTValidationError(
                "Cannot have iterable argument unpacking after keyword argument unpacking."
            )
        else:
            # Invalid, cannot have positional argument follow **/keyword
            raise CSTValidationError(
                "Cannot have positional argument after keyword argument unpacking."
            )

    def _check_starred_or_keywords(
        self, arg: Arg
    ) -> Optional[Callable[[Arg], Callable]]:
        """
        Validates that we only have a mix of "*arg" expansion and "keyword=arg".
        """

        if arg.keyword is not None:
            # Valid, keyword argument
            return None
        elif arg.star == "**":
            # Valid, but we now no longer allow "*" args
            # pyre-fixme[7]: Expected `Optional[Callable[[Arg], Callable[...,
            #  Any]]]` but got `Callable[[Arg], Optional[Callable[[Arg], Callable[...,
            #  Any]]]]`.
            return self._check_kwargs_or_keywords
        elif arg.star == "*":
            # Valid, iterable unpacking
            return None
        else:
            # Invalid, cannot have positional argument follow **/keyword
            raise CSTValidationError(
                "Cannot have positional argument after keyword argument."
            )

    def _check_positional(self, arg: Arg) -> Optional[Callable[[Arg], Callable]]:
        """
        Validates that we only have a mix of positional args and "*arg" expansion.
        """

        if arg.keyword is not None:
            # Valid, but this puts us into starred/keyword state
            # pyre-fixme[7]: Expected `Optional[Callable[[Arg], Callable[...,
            #  Any]]]` but got `Callable[[Arg], Optional[Callable[[Arg], Callable[...,
            #  Any]]]]`.
            return self._check_starred_or_keywords
        elif arg.star == "**":
            # Valid, but we skip states to kwargs/keywords
            # pyre-fixme[7]: Expected `Optional[Callable[[Arg], Callable[...,
            #  Any]]]` but got `Callable[[Arg], Optional[Callable[[Arg], Callable[...,
            #  Any]]]]`.
            return self._check_kwargs_or_keywords
        elif arg.star == "*":
            # Valid, iterator expansion
            return None
        else:
            # Valid, allowed to have positional arguments here
            return None

    def _validate(self) -> None:
        # Validate any super-class stuff, whatever it may be.
        super()._validate()
        # Now, validate the weird intermingling rules for arguments by running
        # a small validator state machine. This works by passing each argument
        # to a validator function which can either raise an exception if it
        # detects an invalid sequence, return a new validator to be used for the
        # next arg, or return None to use the same validator. We could enforce
        # always returning ourselves instead of None but it ends up making the
        # functions themselves less readable. In this way, the current validator
        # function encodes the state we're in (positional state, iterable
        # expansion state, or dictionary expansion state).
        validator = self._check_positional
        for arg in self.args:
            # pyre-fixme[29]: `Union[Callable[[Arg], Callable[..., Any]],
            #  Callable[..., Any]]` is not a function.
            validator = validator(arg) or validator


@add_slots
@dataclass(frozen=True)
class Call(_BaseExpressionWithArgs):
    # The expression resulting in a callable that we are to call
    func: Union[BaseAtom, Attribute, Subscript, "Call"]

    # The arguments to pass to the resulting callable
    args: Sequence[Arg] = ()  # TODO This can also be a single Generator.

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace nodes
    whitespace_after_func: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_before_args: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        """
        Calls have a close paren on the right side regardless of whether they're
        parenthesized as a whole. As a result, they are safe to use directly against
        an adjacent node to the right.
        """
        if position == ExpressionPosition.LEFT:
            return True
        return super(Call, self)._safe_to_use_with_word_operator(position)

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Call":
        return Call(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            func=visit_required("func", self.func, visitor),
            whitespace_after_func=visit_required(
                "whitespace_after_func", self.whitespace_after_func, visitor
            ),
            whitespace_before_args=visit_required(
                "whitespace_before_args", self.whitespace_before_args, visitor
            ),
            args=visit_sequence("args", self.args, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            with state.record_syntactic_position(self):
                self.func._codegen(state)
                self.whitespace_after_func._codegen(state)
                state.add_token("(")
                self.whitespace_before_args._codegen(state)
                lastarg = len(self.args) - 1
                for i, arg in enumerate(self.args):
                    arg._codegen(state, default_comma=(i != lastarg))
                state.add_token(")")


@add_slots
@dataclass(frozen=True)
class Await(BaseExpression):
    # The actual expression we need to await on
    expression: BaseExpression

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace nodes
    whitespace_after_await: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        # Validate any super-class stuff, whatever it may be.
        super(Await, self)._validate()
        # Make sure we don't run identifiers together.
        if self.whitespace_after_await.empty:
            raise CSTValidationError("Must have at least one space after await")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Await":
        return Await(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            whitespace_after_await=visit_required(
                "whitespace_after_await", self.whitespace_after_await, visitor
            ),
            expression=visit_required("expression", self.expression, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token("await")
            self.whitespace_after_await._codegen(state)
            self.expression._codegen(state)


@add_slots
@dataclass(frozen=True)
class IfExp(BaseExpression):
    """
    An if expression similar to "body if test else orelse".
    """

    # The test to perform.
    test: BaseExpression

    # The expression to evaluate if the test is true.
    body: BaseExpression

    # The expression to evaluate if the test is false.
    orelse: BaseExpression

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace nodes
    whitespace_before_if: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after_if: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_before_else: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after_else: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        # Paren validation and such
        super(IfExp, self)._validate()
        # Validate spacing rules
        if (
            self.whitespace_before_if.empty
            and not self.body._safe_to_use_with_word_operator(ExpressionPosition.LEFT)
        ):
            raise CSTValidationError(
                "Must have at least one space before 'if' keyword."
            )
        if (
            self.whitespace_after_if.empty
            and not self.test._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError("Must have at least one space after 'if' keyword.")
        if (
            self.whitespace_before_else.empty
            and not self.test._safe_to_use_with_word_operator(ExpressionPosition.LEFT)
        ):
            raise CSTValidationError(
                "Must have at least one space before 'else' keyword."
            )
        if (
            self.whitespace_after_else.empty
            and not self.orelse._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            )
        ):
            raise CSTValidationError(
                "Must have at least one space after 'else' keyword."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "IfExp":
        return IfExp(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            body=visit_required("body", self.body, visitor),
            whitespace_before_if=visit_required(
                "whitespace_before_if", self.whitespace_before_if, visitor
            ),
            whitespace_after_if=visit_required(
                "whitespace_after_if", self.whitespace_after_if, visitor
            ),
            test=visit_required("test", self.test, visitor),
            whitespace_before_else=visit_required(
                "whitespace_before_else", self.whitespace_before_else, visitor
            ),
            whitespace_after_else=visit_required(
                "whitespace_after_else", self.whitespace_after_else, visitor
            ),
            orelse=visit_required("orelse", self.orelse, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.body._codegen(state)
            self.whitespace_before_if._codegen(state)
            state.add_token("if")
            self.whitespace_after_if._codegen(state)
            self.test._codegen(state)
            self.whitespace_before_else._codegen(state)
            state.add_token("else")
            self.whitespace_after_else._codegen(state)
            self.orelse._codegen(state)


@add_slots
@dataclass(frozen=True)
class From(CSTNode):
    """
    A 'from x' stanza in a Yield or Raise.
    """

    # Expression that we are yielding/raising from.
    item: BaseExpression

    whitespace_before_from: Union[
        BaseParenthesizableWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT
    whitespace_after_from: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        if (
            isinstance(self.whitespace_after_from, BaseParenthesizableWhitespace)
            and self.whitespace_after_from.empty
            and not self.item._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError(
                "Must have at least one space after 'from' keyword."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "From":
        return From(
            whitespace_before_from=visit_sentinel(
                "whitespace_before_from", self.whitespace_before_from, visitor
            ),
            whitespace_after_from=visit_required(
                "whitespace_after_from", self.whitespace_after_from, visitor
            ),
            item=visit_required("item", self.item, visitor),
        )

    def _codegen_impl(self, state: CodegenState, default_space: str = "") -> None:
        whitespace_before_from = self.whitespace_before_from
        if isinstance(whitespace_before_from, BaseParenthesizableWhitespace):
            whitespace_before_from._codegen(state)
        else:
            state.add_token(default_space)

        with state.record_syntactic_position(self):
            state.add_token("from")
            self.whitespace_after_from._codegen(state)
            self.item._codegen(state)


@add_slots
@dataclass(frozen=True)
class Yield(BaseExpression):
    """
    A yield expression similar to "yield x" or "yield from fun()"
    """

    # The test to perform.
    value: Optional[Union[BaseExpression, From]] = None

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    # Whitespace nodes
    whitespace_after_yield: Union[
        BaseParenthesizableWhitespace, MaybeSentinel
    ] = MaybeSentinel.DEFAULT

    def _validate(self) -> None:
        # Paren rules and such
        super(Yield, self)._validate()
        # Our own rules
        if (
            isinstance(self.whitespace_after_yield, BaseParenthesizableWhitespace)
            and self.whitespace_after_yield.empty
        ):
            if isinstance(self.value, From):
                raise CSTValidationError(
                    "Must have at least one space after 'yield' keyword."
                )
            if isinstance(
                self.value, BaseExpression
            ) and not self.value._safe_to_use_with_word_operator(
                ExpressionPosition.RIGHT
            ):
                raise CSTValidationError(
                    "Must have at least one space after 'yield' keyword."
                )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Yield":
        return Yield(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            whitespace_after_yield=visit_sentinel(
                "whitespace_after_yield", self.whitespace_after_yield, visitor
            ),
            value=visit_optional("value", self.value, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            state.add_token("yield")
            whitespace_after_yield = self.whitespace_after_yield
            if isinstance(whitespace_after_yield, BaseParenthesizableWhitespace):
                whitespace_after_yield._codegen(state)
            else:
                # Only need a space after yield if there is a value to yield.
                if self.value is not None:
                    state.add_token(" ")
            value = self.value
            if isinstance(value, From):
                value._codegen(state, default_space="")
            elif value is not None:
                value._codegen(state)


class BaseElement(CSTNode, ABC):
    """
    An element of a literal list, tuple, or set. For elements of a literal dict, see
    BaseMappingElement. (TODO)
    """

    # pyre-fixme[13]: Attribute `value` is never initialized.
    value: BaseExpression
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    @abstractmethod
    def _codegen_impl(
        self,
        state: CodegenState,
        default_comma: bool = False,
        default_comma_whitespace: bool = False,  # False for a single-item tuple
    ) -> None:
        ...


@add_slots
@dataclass(frozen=True)
class Element(BaseElement):
    value: BaseExpression

    # Any trailing comma
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Element":
        return Element(
            value=visit_required("value", self.value, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
        )

    def _codegen_impl(
        self,
        state: CodegenState,
        default_comma: bool = False,
        default_comma_whitespace: bool = False,
    ) -> None:
        with state.record_syntactic_position(self):
            self.value._codegen(state)
        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            if default_comma_whitespace:
                state.add_token(", ")
            else:
                state.add_token(",")
        elif isinstance(comma, Comma):
            comma._codegen(state)


@add_slots
@dataclass(frozen=True)
class StarredElement(BaseElement, _BaseParenthesizedNode):
    value: BaseExpression

    # Any trailing comma
    comma: Union[Comma, MaybeSentinel] = MaybeSentinel.DEFAULT

    # Parentheses around the leading asterisk and the value. Functionally equivalent to
    # parentheses around the value, but in a different position.
    lpar: Sequence[LeftParen] = ()
    rpar: Sequence[RightParen] = ()

    # Whitespace
    whitespace_before_value: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "StarredElement":
        return StarredElement(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            whitespace_before_value=visit_required(
                "whitespace_before_value", self.whitespace_before_value, visitor
            ),
            value=visit_required("value", self.value, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
            comma=visit_sentinel("comma", self.comma, visitor),
        )

    def _codegen_impl(
        self,
        state: CodegenState,
        default_comma: bool = False,
        default_comma_whitespace: bool = False,
    ) -> None:
        with self._parenthesize(state):
            state.add_token("*")
            self.whitespace_before_value._codegen(state)
            self.value._codegen(state)

        comma = self.comma
        if comma is MaybeSentinel.DEFAULT and default_comma:
            if default_comma_whitespace:
                state.add_token(", ")
            else:
                state.add_token(",")
        elif isinstance(comma, Comma):
            comma._codegen(state)


@add_slots
@dataclass(frozen=True)
class Tuple(BaseAtom, BaseAssignTargetExpression, BaseDelTargetExpression):
    elements: Sequence[Union[Element, StarredElement]]

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = (LeftParen(),)

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = (RightParen(),)

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        if super(Tuple, self)._safe_to_use_with_word_operator(position):
            # if we have parenthesis, we're safe.
            return True
        # elements[-1] and elements[0] must exist past this point, because
        # we're not parenthesized, meaning we must have at least one element.
        elements = self.elements
        if position == ExpressionPosition.LEFT:
            last_element = elements[-1]
            return (
                isinstance(last_element.comma, Comma)
                or (
                    isinstance(last_element, StarredElement)
                    and len(last_element.rpar) > 0
                )
                or last_element.value._safe_to_use_with_word_operator(position)
            )
        else:  # ExpressionPosition.RIGHT
            first_element = elements[0]
            # starred elements are always safe because they begin with ( or *
            return isinstance(
                first_element, StarredElement
            ) or first_element.value._safe_to_use_with_word_operator(position)

    def _validate(self) -> None:
        # Paren validation and such
        super(Tuple, self)._validate()

        if len(self.elements) == 0:
            if len(self.lpar) == 0:  # assumes len(lpar) == len(rpar), via superclass
                raise CSTValidationError(
                    "A zero-length tuple must be wrapped in parentheses."
                )
        # Invalid commas aren't possible, because MaybeSentinel will ensure that there
        # is a comma where required.

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Tuple":
        return Tuple(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            elements=visit_sequence("elements", self.elements, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            elements = self.elements
            if len(elements) == 1:
                elements[0]._codegen(
                    state, default_comma=True, default_comma_whitespace=False
                )
            else:
                for idx, el in enumerate(elements):
                    el._codegen(
                        state,
                        default_comma=(idx < len(elements) - 1),
                        default_comma_whitespace=True,
                    )


class BaseList(BaseAtom, ABC):
    """
    A Base class for List and ListComp, which both result in a list object when
    evaluated.
    """

    # Open bracket surrounding the list
    lbracket: LeftSquareBracket = LeftSquareBracket()

    # Close bracket surrounding the list
    rbracket: RightSquareBracket = RightSquareBracket()

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        return True

    @contextmanager
    def _bracketize(self, state: CodegenState) -> Generator[None, None, None]:
        self.lbracket._codegen(state)
        yield
        self.rbracket._codegen(state)


@add_slots
@dataclass(frozen=True)
class List(BaseList, BaseAssignTargetExpression, BaseDelTargetExpression):
    elements: Sequence[Union[Element, StarredElement]]
    lbracket: LeftSquareBracket = LeftSquareBracket()
    rbracket: RightSquareBracket = RightSquareBracket()
    lpar: Sequence[LeftParen] = ()
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "List":
        return List(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            lbracket=visit_required("lbracket", self.lbracket, visitor),
            elements=visit_sequence("elements", self.elements, visitor),
            rbracket=visit_required("rbracket", self.rbracket, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state), self._bracketize(state):
            elements = self.elements
            for idx, el in enumerate(elements):
                el._codegen(
                    state,
                    default_comma=(idx < len(elements) - 1),
                    default_comma_whitespace=True,
                )


class BaseSet(BaseAtom, ABC):
    # Open brace surrounding the list
    lbrace: LeftCurlyBrace = LeftCurlyBrace()

    # Close brace surrounding the list
    rbrace: RightCurlyBrace = RightCurlyBrace()

    # Sequence of open parenthesis for precedence dictation.
    lpar: Sequence[LeftParen] = ()

    # Sequence of close parenthesis for precedence dictation.
    rpar: Sequence[RightParen] = ()

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        return True

    # brace-ize seems like a very made-up word. And it is!
    @contextmanager
    def _braceize(self, state: CodegenState) -> Generator[None, None, None]:
        self.lbrace._codegen(state)
        yield
        self.rbrace._codegen(state)


@add_slots
@dataclass(frozen=True)
class Set(BaseSet):
    elements: Sequence[Union[Element, StarredElement]]
    lbrace: LeftCurlyBrace = LeftCurlyBrace()
    rbrace: RightCurlyBrace = RightCurlyBrace()
    lpar: Sequence[LeftParen] = ()
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        super(Set, self)._validate()

        if len(self.elements) == 0:
            raise CSTValidationError(
                "A literal set must have at least one element. A zero-element set "
                + "would be syntatically ambiguous with an empty dict, `{}`."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Set":
        return Set(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            lbrace=visit_required("lbrace", self.lbrace, visitor),
            elements=visit_sequence("elements", self.elements, visitor),
            rbrace=visit_required("rbrace", self.rbrace, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state), self._braceize(state):
            elements = self.elements
            for idx, el in enumerate(elements):
                el._codegen(
                    state,
                    default_comma=(idx < len(elements) - 1),
                    default_comma_whitespace=True,
                )


@add_slots
@dataclass(frozen=True)
class CompFor(CSTNode):
    """
    One `for` clause in a `BaseComprehension`, or a nested hierarchy of `for`
    clauses.

    Nested loops in comprehensions are difficult to get right, but they can be thought
    of as a flat representation of nested clauses.

    ```
    elt for a in b for c in d if e
    ```

    can be thought of as

    ```
    for a in b:
        for c in d:
            if e:
                yield elt
    ```

    And that would form the following CST:

    ```
    ListComp(
        elt=Name("elt"),
        for_in=CompFor(
            target=Name("a"),
            iter=Name("b"),
            ifs=[],
            inner_comp_for=CompFor(
                target=Name("c"),
                iter=Name("d"),
                ifs=[
                    CompIf(
                        test=Name("e"),
                    ),
                ],
            ),
        ),
    )
    ```
    """

    target: BaseAssignTargetExpression
    iter: BaseExpression
    ifs: Sequence["CompIf"] = ()
    inner_for_in: Optional["CompFor"] = None

    # Optional async modifier.
    asynchronous: Optional[Asynchronous] = None

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after_for: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_before_in: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after_in: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

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

        prev_expr = self.iter
        for if_clause in self.ifs:
            if (
                if_clause.whitespace_before.empty
                and not prev_expr._safe_to_use_with_word_operator(
                    ExpressionPosition.LEFT
                )
            ):
                raise CSTValidationError(
                    "Must have at least one space before 'if' keyword."
                )
            prev_expr = if_clause.test

        inner_for_in = self.inner_for_in
        if (
            inner_for_in is not None
            and inner_for_in.whitespace_before.empty
            and not prev_expr._safe_to_use_with_word_operator(ExpressionPosition.LEFT)
        ):
            keyword = "async" if inner_for_in.asynchronous else "for"
            raise CSTValidationError(
                f"Must have at least one space before '{keyword}' keyword."
            )

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "CompFor":
        return CompFor(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            ),
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
            ifs=visit_sequence("ifs", self.ifs, visitor),
            inner_for_in=visit_optional("inner_for_in", self.inner_for_in, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
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
        ifs = self.ifs
        for if_clause in ifs:
            if_clause._codegen(state)
        inner_for_in = self.inner_for_in
        if inner_for_in is not None:
            inner_for_in._codegen(state)


@add_slots
@dataclass(frozen=True)
class CompIf(CSTNode):
    test: BaseExpression
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_before_test: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        if (
            self.whitespace_before_test.empty
            and not self.test._safe_to_use_with_word_operator(ExpressionPosition.RIGHT)
        ):
            raise CSTValidationError("Must have at least one space after 'if' keyword.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "CompIf":
        return CompIf(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            ),
            whitespace_before_test=visit_required(
                "whitespace_before_test", self.whitespace_before_test, visitor
            ),
            test=visit_required("test", self.test, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.add_token("if")
        self.whitespace_before_test._codegen(state)
        self.test._codegen(state)


class BaseComp(BaseAtom, ABC):
    # pyre-fixme[13]: Attribute `for_in` is never initialized.
    for_in: CompFor


class BaseSimpleComp(BaseComp, ABC):
    """
    The base class for `ListComp`, `SetComp`, and `Generator`. `DictComp` is not a
    `BaseSimpleComp`, because it uses `key` and `value`.
    """

    # The expression evaluated during each iteration of the comprehension. This
    # lexically comes before the `for_in` clause, but it is semantically the inner-most
    # element, evaluated inside the `for_in` clause.
    # pyre-fixme[13]: Attribute `elt` is never initialized.
    elt: BaseAssignTargetExpression

    # The `for ... in ... if ...` clause that lexically comes after `elt`. This may be a
    # nested structure for nested comprehensions. See `ComprehensionFor` for details.
    # pyre-fixme[13]: Attribute `for_in` is never initialized.
    for_in: CompFor

    def _validate(self) -> None:
        super(BaseSimpleComp, self)._validate()

        for_in = self.for_in
        if (
            for_in.whitespace_before.empty
            and not self.elt._safe_to_use_with_word_operator(ExpressionPosition.LEFT)
        ):
            keyword = "async" if for_in.asynchronous else "for"
            raise CSTValidationError(
                f"Must have at least one space before '{keyword}' keyword."
            )


@add_slots
@dataclass(frozen=True)
class GeneratorExp(BaseSimpleComp):
    elt: BaseAssignTargetExpression
    for_in: CompFor
    lpar: Sequence[LeftParen] = (LeftParen(),)
    rpar: Sequence[RightParen] = (RightParen(),)

    def _safe_to_use_with_word_operator(self, position: ExpressionPosition) -> bool:
        # Generators are always parenthesized
        return True

    # A note about validation: Generators must always be parenthesized, but it's
    # possible that this Generator node doesn't own those parenthesis (in the case of a
    # function call with a single generator argument).
    #
    # Therefore, there's no useful validation we can do here. In theory, our parent
    # could do the validation, but there's a ton of potential parents to a Generator, so
    # it's not worth the effort.

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "GeneratorExp":
        return GeneratorExp(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            elt=visit_required("elt", self.elt, visitor),
            for_in=visit_required("for_in", self.for_in, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state):
            self.elt._codegen(state)
            self.for_in._codegen(state)


@add_slots
@dataclass(frozen=True)
class ListComp(BaseList, BaseSimpleComp):
    elt: BaseAssignTargetExpression
    for_in: CompFor
    lbracket: LeftSquareBracket = LeftSquareBracket()
    rbracket: RightSquareBracket = RightSquareBracket()
    lpar: Sequence[LeftParen] = ()
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "ListComp":
        return ListComp(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            lbracket=visit_required("lbracket", self.lbracket, visitor),
            elt=visit_required("elt", self.elt, visitor),
            for_in=visit_required("for_in", self.for_in, visitor),
            rbracket=visit_required("rbracket", self.rbracket, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state), self._bracketize(state):
            self.elt._codegen(state)
            self.for_in._codegen(state)


@add_slots
@dataclass(frozen=True)
class SetComp(BaseSet, BaseSimpleComp):
    elt: BaseAssignTargetExpression
    for_in: CompFor
    lbrace: LeftCurlyBrace = LeftCurlyBrace()
    rbrace: RightCurlyBrace = RightCurlyBrace()
    lpar: Sequence[LeftParen] = ()
    rpar: Sequence[RightParen] = ()

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "SetComp":
        return SetComp(
            lpar=visit_sequence("lpar", self.lpar, visitor),
            lbrace=visit_required("lbrace", self.lbrace, visitor),
            elt=visit_required("elt", self.elt, visitor),
            for_in=visit_required("for_in", self.for_in, visitor),
            rbrace=visit_required("rbrace", self.rbrace, visitor),
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState) -> None:
        with self._parenthesize(state), self._braceize(state):
            self.elt._codegen(state)
            self.for_in._codegen(state)
