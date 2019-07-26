# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from dataclasses import dataclass
from typing import Any, List, Sequence, Union

from libcst._add_slots import add_slots
from libcst._nodes._base import CSTNode, CSTValidationError
from libcst._nodes._expression import LeftParen, RightParen
from libcst._nodes._internal import CodegenState, visit_sequence
from libcst._nodes._whitespace import EmptyLine, TrailingWhitespace  # noqa: F401
from libcst._removal_sentinel import RemovalSentinel
from libcst._visitors import CSTVisitorT


@add_slots
@dataclass(frozen=True)
class DummyNode(CSTNode):

    children: List[Union[CSTNode, str]]

    # HACK: So that we can support being used as an expression
    lpar: Sequence[LeftParen] = ()

    # HACK: So that we can support being used as an expression
    rpar: Sequence[RightParen] = ()

    def _validate(self) -> None:
        if self.lpar and not self.rpar:
            raise CSTValidationError("Cannot have left paren without right paren.")
        if not self.lpar and self.rpar:
            raise CSTValidationError("Cannot have right paren without left paren.")
        if len(self.lpar) != len(self.rpar):
            raise CSTValidationError("Cannot have unbalanced parens.")
        if len(self.children) < 1:
            raise CSTValidationError("Must have at least one child for dummy node.")

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "DummyNode":
        # Preserve traversal order
        lpar = visit_sequence("lpar", self.lpar, visitor)

        new_children: List[Union[CSTNode, str]] = []
        for child in self.children:
            if isinstance(child, CSTNode):
                new_child = child._visit_impl(visitor)
                if not isinstance(new_child, RemovalSentinel):
                    new_children.append(new_child)
            else:
                new_children.append(child)
        return DummyNode(
            lpar=lpar,
            children=new_children,
            rpar=visit_sequence("rpar", self.rpar, visitor),
        )

    def _codegen_impl(self, state: CodegenState, **kwargs: Any) -> None:
        for lpar in self.lpar:
            lpar._codegen(state)
        for child in self.children:
            if isinstance(child, CSTNode):
                child._codegen(state)
            else:
                state.add_token(child)
        for rpar in self.rpar:
            rpar._codegen(state)
