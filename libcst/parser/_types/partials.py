# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from dataclasses import dataclass
from enum import Enum
from typing import Generic, Optional, Sequence, TypeVar, Union

import libcst.nodes as cst
from libcst._add_slots import add_slots
from libcst.parser._types.whitespace_state import WhitespaceState


_T = TypeVar("_T")
_V = TypeVar("_V")


@add_slots
@dataclass(frozen=True)
class WithLeadingWhitespace(Generic[_T]):
    value: _T
    whitespace_before: WhitespaceState


@add_slots
@dataclass(frozen=True)
class SimpleStatementPartial:
    body: Sequence[cst.BaseSmallStatement]
    whitespace_before: WhitespaceState
    trailing_whitespace: cst.TrailingWhitespace


@add_slots
@dataclass(frozen=True)
class SlicePartial:
    second_colon: cst.Colon
    step: Optional[cst.BaseExpression]


@add_slots
@dataclass(frozen=True)
class AttributePartial:
    dot: cst.Dot
    attr: cst.Name


@add_slots
@dataclass(frozen=True)
class ArglistPartial:
    args: Sequence[cst.Arg]


@add_slots
@dataclass(frozen=True)
class CallPartial:
    lpar: WithLeadingWhitespace[cst.LeftParen]
    args: Sequence[cst.Arg]
    rpar: cst.RightParen


@add_slots
@dataclass(frozen=True)
class SubscriptPartial:
    slice: Union[cst.Index, cst.Slice, Sequence[cst.ExtSlice]]
    lbracket: cst.LeftSquareBracket
    rbracket: cst.RightSquareBracket
    whitespace_before: WhitespaceState


@add_slots
@dataclass(frozen=True)
class AnnAssignPartial:
    annotation: cst.Annotation
    equal: Optional[cst.AssignEqual]
    value: Optional[cst.BaseExpression]


@add_slots
@dataclass(frozen=True)
class AugAssignPartial:
    operator: cst.BaseAugOp
    value: cst.BaseExpression


@add_slots
@dataclass(frozen=True)
class AssignPartial:
    equal: cst.AssignEqual
    value: cst.BaseExpression


class ParamStarPartial:
    pass


@add_slots
@dataclass(frozen=True)
class FuncdefPartial:
    lpar: cst.LeftParen
    params: cst.Parameters
    rpar: cst.RightParen


@add_slots
@dataclass(frozen=True)
class DecoratorPartial:
    decorators: Sequence[cst.Decorator]


@add_slots
@dataclass(frozen=True)
class ImportPartial:
    names: Sequence[cst.ImportAlias]


@add_slots
@dataclass(frozen=True)
class ImportRelativePartial:
    relative: Sequence[cst.Dot]
    module: Optional[Union[cst.Attribute, cst.Name]]


@add_slots
@dataclass(frozen=True)
class FormattedStringConversionPartial:
    value: str
    whitespace_before: WhitespaceState


@add_slots
@dataclass(frozen=True)
class FormattedStringFormatSpecPartial:
    values: Sequence[cst.BaseFormattedStringContent]
    whitespace_before: WhitespaceState


@add_slots
@dataclass(frozen=True)
class ExceptClausePartial:
    leading_lines: Sequence[cst.EmptyLine]
    whitespace_after_except: cst.SimpleWhitespace
    type: Optional[cst.BaseExpression] = None
    name: Optional[cst.AsName] = None
