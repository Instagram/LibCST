# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass
from typing import TYPE_CHECKING, Optional, Sequence, TypeVar, Union, cast

from libcst._add_slots import add_slots
from libcst._nodes._base import CSTNode
from libcst._nodes._internal import (
    BasicCodegenState,
    CodegenState,
    SyntacticCodegenState,
    visit_body_sequence,
    visit_sequence,
)
from libcst._nodes._statement import BaseCompoundStatement, SimpleStatementLine
from libcst._nodes._whitespace import EmptyLine
from libcst._removal_sentinel import RemovalSentinel
from libcst._visitors import CSTVisitorT


if TYPE_CHECKING:
    # These are circular dependencies only used for typing purposes
    from libcst.metadata.position_provider import (  # noqa: F401
        BasicPositionProvider,
        SyntacticPositionProvider,
        PositionProvider,
    )


_ModuleSelfT = TypeVar("_ModuleSelfT", bound="Module")

# type alias needed for scope overlap in type definition
builtin_bytes = bytes


@add_slots
@dataclass(frozen=True)
class Module(CSTNode):
    """
    Contains some top-level information inferred from the file letting us set correct
    defaults when printing the tree about global formatting rules. All code parsed
    with :func:`parse_module` will be encapsulated in a module.
    """

    #: A list of zero or more statements that make up this module.
    body: Sequence[Union[SimpleStatementLine, BaseCompoundStatement]]

    #: Normally any whitespace/comments are assigned to the next node visited, but
    #: :class:`Module` is a special case, and comments at the top of the file tend
    #: to refer to the module itself, so we assign them to the :class:`Module`
    #: instead of the first statement in the body.
    header: Sequence[EmptyLine] = ()

    #: Any trailing whitespace/comments found after the last statement.
    footer: Sequence[EmptyLine] = ()

    #: The inferred file encoding.
    encoding: str = "utf-8"

    #: The inferred indentation of the file, expressed as a series of tabs or spaces.
    default_indent: str = " " * 4

    #: The inferred newline of the file, expressed as either ``\n`` or ``\r\n``.
    default_newline: str = "\n"

    #: Whether the module has a trailing newline or not.
    has_trailing_newline: bool = True

    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "Module":
        return Module(
            header=visit_sequence("header", self.header, visitor),
            body=visit_body_sequence("body", self.body, visitor),
            footer=visit_sequence("footer", self.footer, visitor),
            encoding=self.encoding,
            default_indent=self.default_indent,
            default_newline=self.default_newline,
            has_trailing_newline=self.has_trailing_newline,
        )

    def visit(
        self: _ModuleSelfT, visitor: CSTVisitorT, use_compatible: bool = True
    ) -> _ModuleSelfT:
        """
        Returns the result of running a visitor over this module.

        :class:`Module` overrides the default visitor entry point to resolve metadata
        dependencies declared by 'visitor'.
        """
        # TODO: remove compatibility hack
        if use_compatible:
            from libcst.metadata.wrapper import MetadataWrapper

            wrapper = MetadataWrapper(self)
            result = wrapper.visit(visitor)
        else:
            result = super(Module, self).visit(visitor)

        if isinstance(result, RemovalSentinel):
            return self.with_changes(body=(), header=(), footer=())
        else:  # is a Module
            return cast(_ModuleSelfT, result)

    def _codegen_impl(self, state: CodegenState) -> None:
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
                state.add_token(state.default_newline)
        else:  # has_trailing_newline is false
            if len(state.tokens) > 0:
                # EmptyLine and all statements generate newlines, so we can be sure that
                # the last token (if we're not an empty file) is a newline.
                state.tokens.pop()

    @property
    def code(self) -> str:
        """
        The string representation of this module, respecting the inferred indentation
        and newline type.
        """
        return self.code_for_node(self)

    @property
    def bytes(self) -> builtin_bytes:
        """
        The bytes representation of this module, respecting the inferred indentation
        and newline type, using the current encoding.
        """
        return self.code.encode(self.encoding)

    def code_for_node(
        self, node: CSTNode, provider: Optional["PositionProvider"] = None
    ) -> str:
        """
        Generates the code for the given node in the context of this module. This is a
        method of Module, not CSTNode, because we need to know the module's default
        indentation and newline formats.

        By default, this also generates syntactic line and column metadata for each
        node. Passing BasicPositionProvider will generate basic line and column
        metadata instead.
        """

        from libcst.metadata.position_provider import SyntacticPositionProvider

        if provider is None:
            state = CodegenState(
                default_indent=self.default_indent,
                default_newline=self.default_newline,
                provider=provider,
            )
        elif isinstance(provider, SyntacticPositionProvider):
            state = SyntacticCodegenState(
                default_indent=self.default_indent,
                default_newline=self.default_newline,
                provider=provider,
            )
        else:
            state = BasicCodegenState(
                default_indent=self.default_indent,
                default_newline=self.default_newline,
                provider=provider,
            )

        node._codegen(state)

        return "".join(state.tokens)
