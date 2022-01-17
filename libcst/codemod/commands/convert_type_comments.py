# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import builtins
import functools
import sys
from typing import List, Optional, Sequence, Set, Tuple, Union

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


def _statement_type_comment(
    node: Union[cst.SimpleStatementLine, cst.For, cst.With],
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


UnpackedBindings: TypeAlias = Union[cst.BaseExpression, List["UnpackedBindings"]]
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
    ) -> UnpackedBindings:
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
    def annotated_bindings(
        bindings: UnpackedBindings,
        annotations: UnpackedAnnotations,
    ) -> List[Tuple[cst.BaseAssignTargetExpression, str]]:
        if isinstance(annotations, list):
            if isinstance(bindings, list) and len(bindings) == len(annotations):
                # The arities match, so we return the flattened result of
                # mapping annotated_bindings over each pair.
                out: List[Tuple[cst.BaseAssignTargetExpression, str]] = []
                for binding, annotation in zip(bindings, annotations):
                    out.extend(
                        AnnotationSpreader.annotated_bindings(binding, annotation)
                    )
                return out
            else:
                # Either mismatched lengths, or multi-type and one-target
                raise _ArityError()
        elif isinstance(bindings, list):
            # multi-target and one-type
            raise _ArityError()
        else:
            assert isinstance(bindings, cst.BaseAssignTargetExpression)
            return [(bindings, annotations)]

    @staticmethod
    def type_declaration(
        binding: cst.BaseAssignTargetExpression,
        raw_annotation: str,
    ) -> cst.AnnAssign:
        return cst.AnnAssign(
            target=binding,
            annotation=_convert_annotation(raw=raw_annotation),
            value=None,
        )

    @staticmethod
    def type_declaration_statements(
        bindings: UnpackedBindings,
        annotations: UnpackedAnnotations,
        leading_lines: Sequence[cst.EmptyLine],
    ) -> List[cst.SimpleStatementLine]:
        return [
            cst.SimpleStatementLine(
                body=[
                    AnnotationSpreader.type_declaration(
                        binding=binding,
                        raw_annotation=raw_annotation,
                    )
                ],
                leading_lines=leading_lines if i == 0 else [],
            )
            for i, (binding, raw_annotation) in enumerate(
                AnnotationSpreader.annotated_bindings(
                    bindings=bindings,
                    annotations=annotations,
                )
            )
        ]


def convert_Assign(
    node: cst.Assign,
    type_comment: str,
) -> Union[
    _FailedToApplyAnnotation,
    cst.AnnAssign,
    List[Union[cst.AnnAssign, cst.Assign]],
]:
    # zip the type and target information tother. If there are mismatched
    # arities, this is a PEP 484 violation (technically we could use
    # logic beyond the PEP to recover some cases as typing.Tuple, but this
    # should be rare) so we give up.
    try:
        annotations = AnnotationSpreader.unpack_type_comment(type_comment)
        annotated_targets = [
            AnnotationSpreader.annotated_bindings(
                bindings=AnnotationSpreader.unpack_target(target.target),
                annotations=annotations,
            )
            for target in node.targets
        ]
    except _ArityError:
        return _FailedToApplyAnnotation()
    if len(annotated_targets) == 1 and len(annotated_targets[0]) == 1:
        # We can convert simple one-target assignments into a single AnnAssign
        binding, raw_annotation = annotated_targets[0][0]
        return cst.AnnAssign(
            target=binding,
            annotation=_convert_annotation(raw=raw_annotation),
            value=node.value,
            semicolon=node.semicolon,
        )
    else:
        # For multi-target assigns (regardless of whether they are using tuples
        # on the LHS or multiple `=` tokens or both), we need to add a type
        # declaration per individual LHS target.
        type_declarations = [
            AnnotationSpreader.type_declaration(binding, raw_annotation)
            for annotated_bindings in annotated_targets
            for binding, raw_annotation in annotated_bindings
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
            # later. It might be possible to use libcst parsing and the
            # typed_ast library to support earlier python versions, but this is
            # not a high priority.
            raise NotImplementedError(
                "You are trying to run ConvertTypeComments, but libcst "
                + "needs to be running with Python 3.9+ in order to "
                + "do this. Try using Python 3.9+ to run your codemod. "
                + "Note that the target code can be using Python 3.6+, "
                + "it is only libcst that needs a new Python version."
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
        type_comment = _statement_type_comment(original_node)
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

    def leave_For(
        self,
        original_node: cst.For,
        updated_node: cst.For,
    ) -> Union[cst.For, cst.FlattenSentinel]:
        """
        Convert a For with a type hint on the bound variable(s) to
        use type declarations.
        """
        # Type comments are only possible when the body is an indented
        # block, and we need this refinement to work with the header,
        # so we check and only then extract the type comment.
        body = updated_node.body
        if not isinstance(body, cst.IndentedBlock):
            return updated_node
        type_comment = _statement_type_comment(original_node)
        if type_comment is None:
            return updated_node
        # Zip up the type hint and the bindings. If we hit an arity
        # error, abort.
        try:
            type_declarations = AnnotationSpreader.type_declaration_statements(
                bindings=AnnotationSpreader.unpack_target(updated_node.target),
                annotations=AnnotationSpreader.unpack_type_comment(type_comment),
                leading_lines=updated_node.leading_lines,
            )
        except _ArityError:
            return updated_node
        # There is no arity error, so we can add the type delaration(s)
        return cst.FlattenSentinel(
            [
                *type_declarations,
                updated_node.with_changes(
                    body=body.with_changes(
                        header=self._strip_TrailingWhitespace(body.header)
                    ),
                    leading_lines=[],
                ),
            ]
        )

    def leave_With(
        self,
        original_node: cst.With,
        updated_node: cst.With,
    ) -> Union[cst.With, cst.FlattenSentinel]:
        """
        Convert a With with a type hint on the bound variable(s) to
        use type declarations.
        """
        # Type comments are only possible when the body is an indented
        # block, and we need this refinement to work with the header,
        # so we check and only then extract the type comment.
        body = updated_node.body
        if not isinstance(body, cst.IndentedBlock):
            return updated_node
        type_comment = _statement_type_comment(original_node)
        if type_comment is None:
            return updated_node
        # PEP 484 does not attempt to specify type comment semantics for
        # multiple with bindings (there's more than one sensible way to
        # do it), so we make no attempt to handle this
        targets = [
            item.asname.name for item in updated_node.items if item.asname is not None
        ]
        if len(targets) != 1:
            return updated_node
        target = targets[0]
        # Zip up the type hint and the bindings. If we hit an arity
        # error, abort.
        try:
            type_declarations = AnnotationSpreader.type_declaration_statements(
                bindings=AnnotationSpreader.unpack_target(target),
                annotations=AnnotationSpreader.unpack_type_comment(type_comment),
                leading_lines=updated_node.leading_lines,
            )
        except _ArityError:
            return updated_node
        # There is no arity error, so we can add the type delaration(s)
        return cst.FlattenSentinel(
            [
                *type_declarations,
                updated_node.with_changes(
                    body=body.with_changes(
                        header=self._strip_TrailingWhitespace(body.header)
                    ),
                    leading_lines=[],
                ),
            ]
        )
