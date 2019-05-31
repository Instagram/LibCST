# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass
from typing import Sequence, TypeVar, Union

from libcst._add_slots import add_slots
from libcst._base_visitor import CSTVisitor
from libcst._removal_sentinel import RemovalSentinel
from libcst.nodes._base import CSTNode
from libcst.nodes._internal import CodegenState, visit_sequence
from libcst.nodes._statement import BaseCompoundStatement, SimpleStatementLine
from libcst.nodes._whitespace import EmptyLine


_ModuleSelfT = TypeVar("_ModuleSelfT", bound="Module")

# type alias needed for scope overlap in type definition
builtin_bytes = bytes


@add_slots
@dataclass(frozen=True)
class Module(CSTNode):
    """
    Contains some top-level information inferred from the file letting us set correct
    defaults when printing the tree about global formatting rules.
    """

    body: Sequence[Union[SimpleStatementLine, BaseCompoundStatement]]
    # Normally any whitespace/comments are assigned to the next node visited, but Module
    # is a special case, and comments at the top of the file tend to refer to the module
    # itself, so we assign them to the Module.
    header: Sequence[EmptyLine] = ()
    footer: Sequence[EmptyLine] = ()

    encoding: str = "utf-8"
    default_indent: str = " " * 4
    default_newline: str = "\n"
    has_trailing_newline: bool = True

    def _visit_and_replace_children(self, visitor: CSTVisitor) -> "Module":
        return Module(
            header=visit_sequence("header", self.header, visitor),
            body=visit_sequence("body", self.body, visitor),
            footer=visit_sequence("footer", self.footer, visitor),
            encoding=self.encoding,
            default_indent=self.default_indent,
            default_newline=self.default_newline,
            has_trailing_newline=self.has_trailing_newline,
        )

    def visit(self: _ModuleSelfT, visitor: CSTVisitor) -> _ModuleSelfT:
        result = CSTNode.visit(self, visitor)
        if isinstance(result, RemovalSentinel):
            return self.with_changes(body=(), header=(), footer=())
        else:  # is a Module
            return result

    def _codegen(self, state: CodegenState) -> None:
        for h in self.header:
            h._codegen(state)
        for stmt in self.body:
            stmt._codegen(state)
        for f in self.footer:
            f._codegen(state)
        if self.has_trailing_newline:
            if len(state.tokens) == 0:
                # There was nothing in the header, footer, or body. Just add a newline
                # to preserve the trailing newline.
                state.tokens.append(state.default_newline)
        else:  # has_trailing_newline is false
            if len(state.tokens) > 0:
                # EmptyLine and all statements generate newlines, so we can be sure that
                # the last token (if we're not an empty file) is a newline.
                state.tokens.pop()

    @property
    def code(self) -> str:
        return self.code_for_node(self)

    @property
    def bytes(self) -> builtin_bytes:
        return self.code.encode(self.encoding)

    def code_for_node(self, node: CSTNode) -> str:
        """
        Generates the code for the given node in the context of this module. This is a
        method of Module, not CSTNode, because we need to know the module's default
        indentation and newline formats.
        """
        state = CodegenState(
            default_indent=self.default_indent, default_newline=self.default_newline
        )
        node._codegen(state)
        return "".join(state.tokens)
