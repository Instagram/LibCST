# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import builtins
import functools
import sys
from typing import List, Optional, Set, Tuple, Union

from typing_extensions import TypeAlias

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


class _FailedToApplyAnnotation:
    pass


class _ArityError(Exception):
    pass


UnpackedTargets: TypeAlias = Union[cst.BaseExpression, List["UnpackedTargets"]]
UnpackedAnnotations: TypeAlias = Union[str, List["UnpackedAnnotations"]]
TargetAnnotationPair: TypeAlias = Tuple[cst.BaseExpression, str]


class AnnotationSpreader:
    """
    Utilities to help with lining up tuples of types from type comments with
    the tuples of values with which they should be associated.
    """

    @staticmethod
    def _unparse_annotation(
        expression: ast.expr,
    ) -> UnpackedAnnotations:
        if isinstance(expression, ast.Tuple):
            return [
                AnnotationSpreader._unparse_annotation(elt) for elt in expression.elts
            ]
        else:
            return ast.unparse(expression)

    @staticmethod
    def unpack_type_comment(
        type_comment: str,
    ) -> UnpackedAnnotations:
        """
        Unpack an ast module expression and unparse it into a recursive
        list of strings matching the tuple structure of the type comment.
        """
        # pyre-ignore[16]: the ast module stubs do not have full details
        annotation_ast = ast.parse(type_comment, "<type_comment>", "eval").body
        return AnnotationSpreader._unparse_annotation(annotation_ast)

    @staticmethod
    def unpack_target(
        target: cst.BaseExpression,
    ) -> UnpackedTargets:
        """
        Take a (non-function-type) type comment and split it into
        components. A type comment body should always be either a single
        type or a tuple of types.

        We work with strings for annotations because without detailed scope
        analysis that is the safest option for codemods.
        """
        if isinstance(target, cst.Tuple):
            return [
                AnnotationSpreader.unpack_target(element.value)
                for element in target.elements
            ]
        else:
            return target

    @staticmethod
    def zip_and_flatten(
        target: UnpackedTargets,
        type_info: UnpackedAnnotations,
    ) -> List[Tuple[cst.BaseAssignTargetExpression, str]]:
        if isinstance(type_info, list):
            if isinstance(target, list) and len(target) == len(type_info):
                # The arities match, so we return the flattened result of
                # mapping zip_and_flatten over each pair.
                out: List[Tuple[cst.BaseAssignTargetExpression, str]] = []
                for target_, type_info_ in zip(target, type_info):
                    out.extend(AnnotationSpreader.zip_and_flatten(target_, type_info_))
                return out
            else:
                # Either mismatched lengths, or multi-type and one-target
                raise _ArityError()
        elif isinstance(target, list):
            # multi-target and one-type
            raise _ArityError()
        else:
            assert isinstance(target, cst.BaseAssignTargetExpression)
            return [(target, type_info)]


def convert_Assign(
    node: cst.Assign,
    type_comment: str,
) -> Union[
    _FailedToApplyAnnotation,
    cst.AnnAssign,
    List[Union[cst.AnnAssign, cst.Assign]],
]:
    type_info = AnnotationSpreader.unpack_type_comment(type_comment)
    targets = [
        AnnotationSpreader.unpack_target(target.target) for target in node.targets
    ]
    # zip the type and target information tother. If there are mismatched
    # arities, this is a PEP 484 violation (technically we could use
    # logic beyond the PEP to recover some cases as typing.Tuple, but this
    # should be rare) so we give up.
    try:
        zipped_targets = [
            AnnotationSpreader.zip_and_flatten(target, type_info) for target in targets
        ]
    except _ArityError:
        return _FailedToApplyAnnotation()
    if len(zipped_targets) == 1 and len(zipped_targets[0]) == 1:
        # We can convert simple one-target assignments into a single AnnAssign
        target, raw_annotation = zipped_targets[0][0]
        return cst.AnnAssign(
            target=target,
            annotation=_convert_annotation(raw=raw_annotation),
            value=node.value,
            semicolon=node.semicolon,
        )
    else:
        # For multi-target assigns (regardless of whether they are using tuples
        # on the LHS or multiple `=` tokens or both), we need to add a type
        # declaration per individual LHS target.
        type_declarations = [
            cst.AnnAssign(
                target=target,
                annotation=_convert_annotation(raw=raw_annotation),
                value=None,
            )
            for zipped_target in zipped_targets
            for target, raw_annotation in zipped_target
        ]
        return [
            *type_declarations,
            node,
        ]


class ConvertTypeComments(VisitorBasedCodemodCommand):
    """
    Codemod that converts type comments, as described in
    https://www.python.org/dev/peps/pep-0484/#type-comments,
    into PEP 526 annotated assignments.

    This is a work in progress: we intend to also support
    function type comments, with statements, and for statements
    but those are not yet implemented.
    """

    def __init__(self, context: CodemodContext) -> None:
        if (sys.version_info.major, sys.version_info.minor) < (3, 9):
            # The ast module did not get `unparse` until Python 3.9,
            # or `type_comments` until Python 3.8
            #
            # For earlier versions of python, raise early instead of failing
            # later. It might be possible to use libcst parsing and the typed_ast
            # library to support earlier python versions, but this is not a
            # high priority.
            raise NotImplementedError(
                "You are trying to run ConvertTypeComments on a "
                + "python version without type comment support. Please "
                + "try using Python 3.9+ to run your codemod."
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

    def leave_SimpleStatementLine(
        self,
        original_node: cst.SimpleStatementLine,
        updated_node: cst.SimpleStatementLine,
    ) -> Union[cst.SimpleStatementLine, cst.FlattenSentinel]:
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
        # At this point have a single-line Assign with a type comment.
        # Convert it to an AnnAssign and strip the comment.
        converted = convert_Assign(
            node=assign,
            type_comment=type_comment,
        )
        if isinstance(converted, _FailedToApplyAnnotation):
            # We were unable to consume the type comment, so return the
            # original code unchanged.
            # TODO: allow stripping the invalid type comments via a flag
            return updated_node
        elif isinstance(converted, cst.AnnAssign):
            # We were able to convert the Assign into an AnnAssign, so
            # we can update the node.
            return updated_node.with_changes(
                body=[*updated_node.body[:-1], converted],
                trailing_whitespace=self._strip_TrailingWhitespace(
                    updated_node.trailing_whitespace,
                ),
            )
        elif isinstance(converted, list):
            # We need to inject two or more type declarations.
            #
            # In this case, we need to split across multiple lines, and
            # this also means we'll spread any multi-statement lines out
            # (multi-statement lines are PEP 8 violating anyway).
            #
            # We still preserve leading lines from before our transform.
            new_statements = [
                *(
                    statement.with_changes(
                        semicolon=cst.MaybeSentinel.DEFAULT,
                    )
                    for statement in updated_node.body[:-1]
                ),
                *converted,
            ]
            if len(new_statements) < 2:
                raise RuntimeError("Unreachable code.")
            return cst.FlattenSentinel(
                [
                    updated_node.with_changes(
                        body=[new_statements[0]],
                        trailing_whitespace=self._strip_TrailingWhitespace(
                            updated_node.trailing_whitespace,
                        ),
                    ),
                    *(
                        cst.SimpleStatementLine(body=[statement])
                        for statement in new_statements[1:]
                    ),
                ]
            )
        else:
            raise RuntimeError(f"Unhandled value {converted}")
