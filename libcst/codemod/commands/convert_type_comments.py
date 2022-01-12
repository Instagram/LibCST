# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import builtins
import functools
import re
import sys
from typing import Optional, Pattern, Set, Union

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


def _assign_type_comment(node: cst.SimpleStatementLine) -> Optional[str]:
    return _ast_for_node(node).body[0].type_comment


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
    """

    TYPE_COMMENT_REGEX: Pattern[str] = re.compile("# *type: *[^ ]+")
    TYPE_IGNORE_PREFIX: str = "# type: ignore"

    _should_strip_type_comments: bool = False

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

    def _is_type_comment(self, comment: Optional[cst.Comment]) -> bool:
        return (
            comment is not None
            and self.TYPE_COMMENT_REGEX.match(comment.value) is not None
            and not comment.value.startswith(self.TYPE_IGNORE_PREFIX)
        )

    def _should_strip_comment(self, comment: Optional[cst.Comment]) -> bool:
        return self._should_strip_type_comments and self._is_type_comment(comment)

    def _convert_Assign(
        self,
        assign: cst.Assign,
        type_comment: str,
    ) -> Union[cst.AnnAssign, cst.Assign]:
        if len(assign.targets) != 1:
            return assign
        return cst.AnnAssign(
            target=assign.targets[0].target,
            annotation=_convert_annotation(raw=type_comment),
            value=assign.value,
            semicolon=assign.semicolon,
        )

    def visit_SimpleStatementLine(
        self,
        node: cst.SimpleStatementLine,
    ) -> None:
        # manage the state used for determining comments to delete
        self._should_strip_type_comments = True

    def leave_SimpleStatementLine(
        self,
        original_node: cst.SimpleStatementLine,
        updated_node: cst.SimpleStatementLine,
    ) -> cst.SimpleStatementLine:
        """
        Convert any SimpleStatementLine containing an Assign with a
        type comment into a one that uses a PEP 526 AnnAssign.
        """
        # manage the state used for determining comments to delete
        self._should_strip_type_comments = False
        # determine whether to apply an annotation
        body = updated_node.body
        if len(body) != 1:
            return updated_node
        statement = body[0]
        if not isinstance(statement, cst.Assign):
            return updated_node
        type_comment = _assign_type_comment(original_node)
        if type_comment is None:
            return updated_node
        # At this point have a single-line Assign with a type comment.
        # Convert it to an AnnAssign.
        return updated_node.with_changes(
            body=[
                self._convert_Assign(
                    assign=statement,
                    type_comment=type_comment,
                )
            ]
        )

    def leave_TrailingWhitespace(
        self,
        original_node: cst.TrailingWhitespace,
        updated_node: cst.TrailingWhitespace,
    ) -> cst.TrailingWhitespace:
        """
        Remove type comments from trailing whitespace.
        """
        if self._should_strip_comment(updated_node.comment):
            return updated_node.with_changes(
                whitespace=cst.SimpleWhitespace(
                    ""
                ),  # whitespace came before the comment, so strip it.
                comment=None,
            )
        return updated_node
