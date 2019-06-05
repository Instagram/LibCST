# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Tuple

from libcst._add_slots import add_slots
from libcst._base_visitor import CSTVisitor
from libcst.nodes._base import BaseLeaf, CSTNode, CSTValidationError
from libcst.nodes._internal import CodegenState, visit_required
from libcst.nodes._whitespace import BaseParenthesizableWhitespace, SimpleWhitespace


class _BaseOneTokenOp(CSTNode, ABC):
    """
    Any node that has a static value and needs to own whitespace on both sides.
    """

    whitespace_before: BaseParenthesizableWhitespace
    whitespace_after: BaseParenthesizableWhitespace

    def _visit_and_replace_children(self, visitor: CSTVisitor) -> "_BaseOneTokenOp":
        return self.__class__(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            ),
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            ),
        )

    def _codegen(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.tokens.append(self._get_token())
        self.whitespace_after._codegen(state)

    @abstractmethod
    def _get_token(self) -> str:
        ...


class _BaseTwoTokenOp(CSTNode, ABC):
    """
    This node ends up as two tokens, so we must preserve the whitespace
    in beteween them.
    """

    whitespace_before: BaseParenthesizableWhitespace
    whitespace_between: BaseParenthesizableWhitespace
    whitespace_after: BaseParenthesizableWhitespace

    def _validate(self) -> None:
        if self.whitespace_between.empty:
            raise CSTValidationError("Must have at least one space between not and in.")

    def _visit_and_replace_children(self, visitor: CSTVisitor) -> "_BaseTwoTokenOp":
        return self.__class__(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            ),
            whitespace_between=visit_required(
                "whitespace_between", self.whitespace_between, visitor
            ),
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            ),
        )

    def _codegen(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.tokens.append(self._get_tokens()[0])
        self.whitespace_between._codegen(state)
        state.tokens.append(self._get_tokens()[1])
        self.whitespace_after._codegen(state)

    @abstractmethod
    def _get_tokens(self) -> Tuple[str, str]:
        ...


class BaseUnaryOp(CSTNode, ABC):
    """
    Any node that has a static value used in a Unary expression.
    """

    whitespace_after: BaseParenthesizableWhitespace

    def _visit_and_replace_children(self, visitor: CSTVisitor) -> "BaseUnaryOp":
        return self.__class__(
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            )
        )

    def _codegen(self, state: CodegenState) -> None:
        state.tokens.append(self._get_token())
        self.whitespace_after._codegen(state)

    @abstractmethod
    def _get_token(self) -> str:
        ...


class BaseBooleanOp(_BaseOneTokenOp, ABC):
    """
    Any node that has a static value used in a Binary expression. This node
    is purely for typing.
    """


class BaseBinaryOp(CSTNode, ABC):
    """
    Any node that has a static value used in a Binary expression. This node
    is purely for typing.
    """


class BaseCompOp(CSTNode, ABC):
    """
    Any node that has a static value used in a CompExpression. This node
    is purely for typing.
    """


class BaseAugOp(CSTNode, ABC):
    """
    Any node that has a static value used in an AugAssign. This node is purely
    for typing.
    """


@add_slots
@dataclass(frozen=True)
class Semicolon(_BaseOneTokenOp):
    """
    Used by SmallStatement as a separator between subsequent SmallStatements contained
    within a SimpleStatementLine or SimpleStatementSuite.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return ";"


@add_slots
@dataclass(frozen=True)
class Colon(_BaseOneTokenOp):
    """
    Used by Slice as a separator between subsequent Expressions, and in Lambda
    to separate arguments and body.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return ":"


@add_slots
@dataclass(frozen=True)
class Comma(_BaseOneTokenOp):
    """
    Syntactic trivia used as a separator between subsequent items in various parts of
    the grammar.

    Some use-cases are:
    - Import or ImportFrom
    - Function arguments
    - Tuple/list/set/dict elements
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return ","


@add_slots
@dataclass(frozen=True)
class Dot(_BaseOneTokenOp):
    """
    Used by Attribute and DottedName as a separator between subsequent
    Name nodes.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace("")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return "."


@add_slots
@dataclass(frozen=True)
class ImportStar(BaseLeaf):
    """
    Used by ImportFrom to denote a star import.
    """

    def _codegen(self, state: CodegenState) -> None:
        state.tokens.append("*")


@add_slots
@dataclass(frozen=True)
class AssignEqual(_BaseOneTokenOp):
    """
    Used by AnnAssign to denote a single equal character when doing an
    assignment on top of a type annotation.
    """

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "="


@dataclass(frozen=True)
class Plus(BaseUnaryOp):
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return "+"


@dataclass(frozen=True)
class Minus(BaseUnaryOp):
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return "-"


@dataclass(frozen=True)
class BitInvert(BaseUnaryOp):
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace("")

    def _get_token(self) -> str:
        return "~"


@dataclass(frozen=True)
class Not(BaseUnaryOp):
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "not"


@dataclass(frozen=True)
class And(BaseBooleanOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "and"


@dataclass(frozen=True)
class Or(BaseBooleanOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "or"


@dataclass(frozen=True)
class Add(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "+"


@dataclass(frozen=True)
class Subtract(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "-"


@dataclass(frozen=True)
class Multiply(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "*"


@dataclass(frozen=True)
class Divide(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "/"


@dataclass(frozen=True)
class FloorDivide(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "//"


@dataclass(frozen=True)
class Modulo(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "%"


@dataclass(frozen=True)
class Power(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "**"


@dataclass(frozen=True)
class LeftShift(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "<<"


@dataclass(frozen=True)
class RightShift(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return ">>"


@dataclass(frozen=True)
class BitOr(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "|"


@dataclass(frozen=True)
class BitAnd(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "&"


@dataclass(frozen=True)
class BitXor(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "^"


@dataclass(frozen=True)
class MatrixMultiply(BaseBinaryOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "@"


@dataclass(frozen=True)
class LessThan(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "<"


@dataclass(frozen=True)
class GreaterThan(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return ">"


@dataclass(frozen=True)
class Equal(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "=="


@dataclass(frozen=True)
class LessThanEqual(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "<="


@dataclass(frozen=True)
class GreaterThanEqual(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return ">="


@dataclass(frozen=True)
class NotEqual(BaseCompOp):
    """
    This node defines a static value for convenience, but in reality due to
    PEP 401 it can be one of two values, both of which should be a NotEqual
    CompOp.
    """

    value: str = "!="

    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _validate(self) -> None:
        if self.value not in ["!=", "<>"]:
            raise CSTValidationError("Invalid value for NotEqual node.")

    def _visit_and_replace_children(self, visitor: CSTVisitor) -> "BaseCompOp":
        return self.__class__(
            whitespace_before=visit_required(
                "whitespace_before", self.whitespace_before, visitor
            ),
            value=self.value,
            whitespace_after=visit_required(
                "whitespace_after", self.whitespace_after, visitor
            ),
        )

    def _codegen(self, state: CodegenState) -> None:
        self.whitespace_before._codegen(state)
        state.tokens.append(self.value)
        self.whitespace_after._codegen(state)


@dataclass(frozen=True)
class In(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "in"


@dataclass(frozen=True)
class NotIn(BaseCompOp, _BaseTwoTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_between: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_tokens(self) -> Tuple[str, str]:
        return ("not", "in")


@dataclass(frozen=True)
class Is(BaseCompOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "is"


@dataclass(frozen=True)
class IsNot(BaseCompOp, _BaseTwoTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_between: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_tokens(self) -> Tuple[str, str]:
        return ("is", "not")


@add_slots
@dataclass(frozen=True)
class AddAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "+="


@add_slots
@dataclass(frozen=True)
class SubtractAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "-="


@add_slots
@dataclass(frozen=True)
class MultiplyAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "*="


@add_slots
@dataclass(frozen=True)
class MatrixMultiplyAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "@="


@add_slots
@dataclass(frozen=True)
class DivideAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "/="


@add_slots
@dataclass(frozen=True)
class ModuloAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "%="


@add_slots
@dataclass(frozen=True)
class BitAndAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "&="


@add_slots
@dataclass(frozen=True)
class BitOrAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "|="


@add_slots
@dataclass(frozen=True)
class BitXorAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "^="


@add_slots
@dataclass(frozen=True)
class LeftShiftAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "<<="


@add_slots
@dataclass(frozen=True)
class RightShiftAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return ">>="


@add_slots
@dataclass(frozen=True)
class PowerAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "**="


@add_slots
@dataclass(frozen=True)
class FloorDivideAssign(BaseAugOp, _BaseOneTokenOp):
    whitespace_before: BaseParenthesizableWhitespace = SimpleWhitespace(" ")
    whitespace_after: BaseParenthesizableWhitespace = SimpleWhitespace(" ")

    def _get_token(self) -> str:
        return "//="
