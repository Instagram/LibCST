# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import builtins
import functools
import sys
from typing import Optional, Set, Union

import libcst as cst
from libcst.codemod import CodemodContext, VisitorBasedCodemodCommand


@functools.lru_cache()
def _empty_module() -> cst.Module:
    return cst.parse_module("")


def _code_for_node(node: cst.CSTNode) -> str:
    return _empty_module().code_for_node(node)


def _ast_for_node(node: cst.CSTNode) -> ast.Module:
    code = _code_for_node(node)
    return ast.parse(code, type_comments=True)


def _simple_statement_type_comment(
    node: cst.SimpleStatementLine,
) -> Optional[str]:
    return _ast_for_node(node).body[-1].type_comment


@functools.lru_cache()
def _builtins() -> Set[str]:
    return set(dir(builtins))


def _is_builtin(annotation: str) -> bool:
    return annotation in _builtins()


def _convert_annotation(raw: str) -> cst.Annotation:
    # Convert annotation comments to string annotations to be safe,
    # otherwise runtime errors would be common.
    #
    # Special-case builtins to reduce the amount of quoting noise.
    #
    # NOTE: we could potentially detect more cases for skipping quotes
    # using ScopeProvider, which would make the output prettier.
    if _is_builtin(raw):
        return cst.Annotation(annotation=cst.Name(value=raw))
    else:
        return cst.Annotation(annotation=cst.SimpleString(f'"{raw}"'))


class ConvertTypeComments(VisitorBasedCodemodCommand):
    """
    Codemod that converts type comments, as described in
    https://www.python.org/dev/peps/pep-0484/#type-comments,
    into PEP 526 annotated assignments.

    This is a work in progress: the codemod only currently handles
    single-annotation assigns, but it will preserve any type comments
    that it does not consume.
    """

    def __init__(self, context: CodemodContext) -> None:
        if (sys.version_info.major, sys.version_info.minor) < (3, 8):
            # The ast module did not get `type_comments` until Python 3.7.
            # In 3.6, we should error than silently running a nonsense codemod.
            #
            # NOTE: it is possible to use the typed_ast library for 3.6, but
            # this is not a high priority right now. See, e.g., the
            # mypy.fastparse module.
            raise NotImplementedError(
                "You are trying to run ConvertTypeComments on a "
                + "python version without type comment support. Please "
                + "try using python 3.8+ to run your codemod."
            )
        super().__init__(context)

    def _strip_TrailingWhitespace(
        self,
        node: cst.TrailingWhitespace,
    ) -> cst.TrailingWhitespace:
        return node.with_changes(
            whitespace=cst.SimpleWhitespace(
                ""
            ),  # any whitespace came before the comment, so strip it.
            comment=None,
        )

    def _convert_Assign(
        self,
        assign: cst.Assign,
        type_comment: str,
    ) -> Union[cst.AnnAssign, cst.Assign]:
        if len(assign.targets) != 1:
            # this case is not yet implemented, and we short-circuit
            # it when handling SimpleStatementLine.
            raise RuntimeError("Should not convert multi-target assign")
        return cst.AnnAssign(
            target=assign.targets[0].target,
            annotation=_convert_annotation(raw=type_comment),
            value=assign.value,
            semicolon=assign.semicolon,
        )

    def leave_SimpleStatementLine(
        self,
        original_node: cst.SimpleStatementLine,
        updated_node: cst.SimpleStatementLine,
    ) -> cst.SimpleStatementLine:
        """
        Convert any SimpleStatementLine containing an Assign with a
        type comment into a one that uses a PEP 526 AnnAssign.
        """
        # determine whether to apply an annotation
        assign = updated_node.body[-1]
        if not isinstance(assign, cst.Assign):  # only Assign matters
            return updated_node
        type_comment = _simple_statement_type_comment(original_node)
        if type_comment is None:
            return updated_node
        if len(assign.targets) != 1:  # multi-target Assign isn't used
            return updated_node
        target = assign.targets[0].target
        if isinstance(target, cst.Tuple):  # multi-element Assign isn't handled
            return updated_node
        # At this point have a single-line Assign with a type comment.
        # Convert it to an AnnAssign and strip the comment.
        return updated_node.with_changes(
            body=[
                *updated_node.body[:-1],
                self._convert_Assign(
                    assign=assign,
                    type_comment=type_comment,
                ),
            ],
            trailing_whitespace=self._strip_TrailingWhitespace(
                updated_node.trailing_whitespace
            ),
        )
