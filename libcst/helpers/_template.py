# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict

from typing import Dict, Mapping, Optional, Set, Union

import libcst as cst
from libcst.helpers.common import ensure_type


TEMPLATE_PREFIX: str = "__LIBCST_MANGLED_NAME_"
TEMPLATE_SUFFIX: str = "_EMAN_DELGNAM_TSCBIL__"


ValidReplacementType = Union[
    cst.BaseExpression,
    cst.Annotation,
    cst.AssignTarget,
    cst.Param,
    cst.Parameters,
    cst.Arg,
]


def mangled_name(var: str) -> str:
    return f"{TEMPLATE_PREFIX}{var}{TEMPLATE_SUFFIX}"


def unmangled_name(var: str) -> Optional[str]:
    if TEMPLATE_PREFIX in var and TEMPLATE_SUFFIX in var:
        prefix, name_and_suffix = var.split(TEMPLATE_PREFIX, 1)
        name, suffix = name_and_suffix.split(TEMPLATE_SUFFIX, 1)
        if not prefix and not suffix:
            return name
    # This is not a valid mangled name
    return None


def mangle_template(template: str, template_vars: Set[str]) -> str:
    if TEMPLATE_PREFIX in template or TEMPLATE_SUFFIX in template:
        raise Exception("Cannot parse a template containing reserved strings")

    for var in template_vars:
        original = f"{{{var}}}"
        if original not in template:
            raise Exception(
                f'Template string is missing a reference to "{var}" referred to in kwargs'
            )
        template = template.replace(original, mangled_name(var))
    return template


class TemplateTransformer(cst.CSTTransformer):
    def __init__(
        self, template_replacements: Mapping[str, ValidReplacementType]
    ) -> None:
        self.simple_replacements: Dict[str, cst.BaseExpression] = {
            name: value
            for name, value in template_replacements.items()
            if isinstance(value, cst.BaseExpression)
        }
        self.annotation_replacements: Dict[str, cst.Annotation] = {
            name: value
            for name, value in template_replacements.items()
            if isinstance(value, cst.Annotation)
        }
        self.assignment_replacements: Dict[str, cst.AssignTarget] = {
            name: value
            for name, value in template_replacements.items()
            if isinstance(value, cst.AssignTarget)
        }
        self.param_replacements: Dict[str, cst.Param] = {
            name: value
            for name, value in template_replacements.items()
            if isinstance(value, cst.Param)
        }
        self.parameters_replacements: Dict[str, cst.Parameters] = {
            name: value
            for name, value in template_replacements.items()
            if isinstance(value, cst.Parameters)
        }
        self.arg_replacements: Dict[str, cst.Arg] = {
            name: value
            for name, value in template_replacements.items()
            if isinstance(value, cst.Arg)
        }

        # Figure out if there are any variables that we can't support
        # inserting into templates.
        supported_vars = {
            *[name for name in self.simple_replacements],
            *[name for name in self.annotation_replacements],
            *[name for name in self.assignment_replacements],
            *[name for name in self.param_replacements],
            *[name for name in self.parameters_replacements],
            *[name for name in self.arg_replacements],
        }
        unsupported_vars = {
            name for name in template_replacements if name not in supported_vars
        }
        if unsupported_vars:
            raise Exception(
                f'Template replacement for "{next(iter(unsupported_vars))}" is unsupported'
            )

    def leave_Name(
        self, original_node: cst.Name, updated_node: cst.Name
    ) -> cst.BaseExpression:
        var_name = unmangled_name(updated_node.value)
        if var_name is None or var_name not in self.simple_replacements:
            # This is not a valid name, don't modify it
            return updated_node
        return self.simple_replacements[var_name].deep_clone()

    def leave_Annotation(
        self, original_node: cst.Annotation, updated_node: cst.Annotation,
    ) -> cst.Annotation:
        # We can't use matchers here due to circular imports
        annotation = updated_node.annotation
        if isinstance(annotation, cst.Name):
            var_name = unmangled_name(annotation.value)
            if var_name in self.annotation_replacements:
                return self.annotation_replacements[var_name].deep_clone()
        return updated_node

    def leave_AssignTarget(
        self, original_node: cst.AssignTarget, updated_node: cst.AssignTarget,
    ) -> cst.AssignTarget:
        # We can't use matchers here due to circular imports
        target = updated_node.target
        if isinstance(target, cst.Name):
            var_name = unmangled_name(target.value)
            if var_name in self.assignment_replacements:
                return self.assignment_replacements[var_name].deep_clone()
        return updated_node

    def leave_Param(
        self, original_node: cst.Param, updated_node: cst.Param,
    ) -> cst.Param:
        var_name = unmangled_name(updated_node.name.value)
        if var_name in self.param_replacements:
            return self.param_replacements[var_name].deep_clone()
        return updated_node

    def leave_Parameters(
        self, original_node: cst.Parameters, updated_node: cst.Parameters,
    ) -> cst.Parameters:
        # A very special case for when we use a template variable for all
        # function parameters.
        if (
            len(updated_node.params) == 1
            and updated_node.star_arg == cst.MaybeSentinel.DEFAULT
            and len(updated_node.kwonly_params) == 0
            and updated_node.star_kwarg is None
            and len(updated_node.posonly_params) == 0
            and updated_node.posonly_ind == cst.MaybeSentinel.DEFAULT
        ):
            # This parameters node has only one argument, which is possibly
            # a replacement.
            var_name = unmangled_name(updated_node.params[0].name.value)
            if var_name in self.parameters_replacements:
                return self.parameters_replacements[var_name].deep_clone()
        return updated_node

    def leave_Arg(self, original_node: cst.Arg, updated_node: cst.Arg,) -> cst.Arg:
        # We can't use matchers here due to circular imports
        arg = updated_node.value
        if isinstance(arg, cst.Name):
            var_name = unmangled_name(arg.value)
            if var_name in self.arg_replacements:
                return self.arg_replacements[var_name].deep_clone()
        return updated_node


class TemplateChecker(cst.CSTVisitor):
    def __init__(self, template_vars: Set[str]) -> None:
        self.template_vars = template_vars

    def visit_Name(self, node: cst.Name) -> None:
        for var in self.template_vars:
            if node.value == mangled_name(var):
                raise Exception(f'Template variable "{var}" was not replaced properly')


def unmangle_nodes(
    tree: cst.CSTNode, template_replacements: Mapping[str, ValidReplacementType],
) -> cst.CSTNode:
    unmangler = TemplateTransformer(template_replacements)
    return ensure_type(tree.visit(unmangler), cst.CSTNode)


_DEFAULT_PARTIAL_PARSER_CONFIG: cst.PartialParserConfig = cst.PartialParserConfig()


def parse_template_module(
    template: str,
    config: cst.PartialParserConfig = _DEFAULT_PARTIAL_PARSER_CONFIG,
    **template_replacements: ValidReplacementType,
) -> cst.Module:
    """
    Accepts an entire python module template, including all leading and trailing
    whitespace. Any :class:`~libcst.CSTNode` provided as a keyword argument to
    this function will be inserted into the template at the appropriate location
    similar to an f-string expansion. For example::

      module = parse_template_module("from {mod} import Foo\\n", mod=Name("bar"))

    The above code will parse to a module containing a single
    :class:`~libcst.FromImport` statement, referencing module ``bar`` and importing
    object ``Foo`` from it. Remember that if you are parsing a template as part
    of a substitution inside a transform, its considered
    :ref:`best practice <libcst-config_best_practice>` to pass in a ``config``
    from the current module under transformation.

    Note that unlike :func:`~libcst.parse_module`, this function does not support
    bytes as an input. This is due to the fact that it is processed as a template
    before parsing as a module.
    """

    source = mangle_template(template, {name for name in template_replacements})
    module = cst.parse_module(source, config)
    new_module = ensure_type(unmangle_nodes(module, template_replacements), cst.Module)
    new_module.visit(TemplateChecker({name for name in template_replacements}))
    return new_module


def parse_template_statement(
    template: str,
    config: cst.PartialParserConfig = _DEFAULT_PARTIAL_PARSER_CONFIG,
    **template_replacements: ValidReplacementType,
) -> Union[cst.SimpleStatementLine, cst.BaseCompoundStatement]:
    """
    Accepts a statement template followed by a trailing newline. If a trailing
    newline is not provided, one will be added. Any :class:`~libcst.CSTNode`
    provided as a keyword argument to this function will be inserted into the
    template at the appropriate location similar to an f-string expansion. For
    example::

      statement = parse_template_statement("assert x > 0, {msg}", msg=SimpleString('"Uh oh!"'))

    The above code will parse to an assert statement checking that some variable
    ``x`` is greater than zero, or providing the assert message ``"Uh oh!"``.

    Remember that if you are parsing a template as part of a substitution inside
    a transform, its considered :ref:`best practice <libcst-config_best_practice>`
    to pass in a ``config`` from the current module under transformation.
    """

    source = mangle_template(template, {name for name in template_replacements})
    statement = cst.parse_statement(source, config)
    new_statement = unmangle_nodes(statement, template_replacements)
    if not isinstance(
        new_statement, (cst.SimpleStatementLine, cst.BaseCompoundStatement)
    ):
        raise Exception(
            f"Expected a statement bot got a {new_statement.__class__.__name__}!"
        )
    new_statement.visit(TemplateChecker({name for name in template_replacements}))
    return new_statement


def parse_template_expression(
    template: str,
    config: cst.PartialParserConfig = _DEFAULT_PARTIAL_PARSER_CONFIG,
    **template_replacements: ValidReplacementType,
) -> cst.BaseExpression:
    """
    Accepts an expression template on a single line. Leading and trailing whitespace
    is not valid (there’s nowhere to store it on the expression node). Any
    :class:`~libcst.CSTNode` provided as a keyword argument to this function will
    be inserted into the template at the appropriate location similar to an
    f-string expansion. For example::

      expression = parse_template_expression("x + {foo}", foo=Name("y")))

    The above code will parse to a :class:`~libcst.BinaryOperation` expression
    adding two names (``x`` and ``y``) together.

    Remember that if you are parsing a template as part of a substitution inside
    a transform, its considered :ref:`best practice <libcst-config_best_practice>`
    to pass in a ``config`` from the current module under transformation.
    """

    source = mangle_template(template, {name for name in template_replacements})
    expression = cst.parse_expression(source, config)
    new_expression = ensure_type(
        unmangle_nodes(expression, template_replacements), cst.BaseExpression
    )
    new_expression.visit(TemplateChecker({name for name in template_replacements}))
    return new_expression
