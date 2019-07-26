# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import re
from functools import lru_cache
from typing import Iterator, Mapping, Tuple

from parso.pgen2.generator import Grammar, generate_grammar
from parso.python.token import PythonTokenTypes, TokenType

from libcst._parser._conversions.expression import (
    convert_arg_assign_comp_for,
    convert_arglist,
    convert_argument,
    convert_atom,
    convert_atom_basic,
    convert_atom_curlybraces,
    convert_atom_ellipses,
    convert_atom_expr,
    convert_atom_expr_await,
    convert_atom_expr_trailer,
    convert_atom_parens,
    convert_atom_squarebrackets,
    convert_atom_string,
    convert_binop,
    convert_boolop,
    convert_comp_for,
    convert_comp_if,
    convert_comp_op,
    convert_comparison,
    convert_dictorsetmaker,
    convert_expression_input,
    convert_factor,
    convert_fstring,
    convert_fstring_content,
    convert_fstring_conversion,
    convert_fstring_expr,
    convert_fstring_format_spec,
    convert_lambda,
    convert_not_test,
    convert_power,
    convert_sliceop,
    convert_star_arg,
    convert_star_expr,
    convert_subscript,
    convert_subscriptlist,
    convert_sync_comp_for,
    convert_test,
    convert_test_nocond,
    convert_test_or_expr_list,
    convert_testlist_comp_list,
    convert_testlist_comp_tuple,
    convert_trailer,
    convert_trailer_arglist,
    convert_trailer_attribute,
    convert_trailer_subscriptlist,
    convert_yield_arg,
    convert_yield_expr,
)
from libcst._parser._conversions.module import convert_file_input
from libcst._parser._conversions.params import (
    convert_argslist,
    convert_fpdef,
    convert_fpdef_assign,
    convert_fpdef_star,
    convert_fpdef_starstar,
)
from libcst._parser._conversions.statement import (
    convert_annassign,
    convert_assert_stmt,
    convert_assign,
    convert_asyncable_funcdef,
    convert_asyncable_stmt,
    convert_augassign,
    convert_break_stmt,
    convert_classdef,
    convert_compound_stmt,
    convert_continue_stmt,
    convert_decorated,
    convert_decorator,
    convert_decorators,
    convert_del_stmt,
    convert_dotted_as_name,
    convert_dotted_as_names,
    convert_dotted_name,
    convert_except_clause,
    convert_expr_stmt,
    convert_for_stmt,
    convert_funcdef,
    convert_funcdef_annotation,
    convert_global_stmt,
    convert_if_stmt,
    convert_if_stmt_elif,
    convert_if_stmt_else,
    convert_import_as_name,
    convert_import_as_names,
    convert_import_from,
    convert_import_name,
    convert_import_relative,
    convert_import_stmt,
    convert_indented_suite,
    convert_nonlocal_stmt,
    convert_parameters,
    convert_pass_stmt,
    convert_raise_stmt,
    convert_return_stmt,
    convert_simple_stmt_line,
    convert_simple_stmt_partial,
    convert_simple_stmt_suite,
    convert_small_stmt,
    convert_stmt,
    convert_stmt_input,
    convert_suite,
    convert_try_stmt,
    convert_while_stmt,
    convert_with_item,
    convert_with_stmt,
)
from libcst._parser._conversions.terminals import (
    convert_DEDENT,
    convert_ENDMARKER,
    convert_FSTRING_END,
    convert_FSTRING_START,
    convert_FSTRING_STRING,
    convert_INDENT,
    convert_NAME,
    convert_NEWLINE,
    convert_NUMBER,
    convert_OP,
    convert_STRING,
)
from libcst._parser._production_decorator import get_productions
from libcst._parser._types.conversions import NonterminalConversion, TerminalConversion
from libcst._parser._types.production import Production


# Keep this sorted alphabetically
_TERMINAL_CONVERSIONS_SEQUENCE: Tuple[TerminalConversion, ...] = (
    convert_DEDENT,
    convert_ENDMARKER,
    convert_INDENT,
    convert_NAME,
    convert_NEWLINE,
    convert_NUMBER,
    convert_OP,
    convert_STRING,
    convert_FSTRING_START,
    convert_FSTRING_END,
    convert_FSTRING_STRING,
)

# Try to match the order of https://docs.python.org/3/reference/grammar.html
_NONTERMINAL_CONVERSIONS_SEQUENCE: Tuple[NonterminalConversion, ...] = (
    convert_file_input,
    convert_stmt_input,  # roughly equivalent to single_input
    convert_expression_input,  # roughly equivalent to eval_input
    convert_stmt,
    convert_simple_stmt_partial,
    convert_simple_stmt_line,
    convert_simple_stmt_suite,
    convert_small_stmt,
    convert_expr_stmt,
    convert_annassign,
    convert_augassign,
    convert_assign,
    convert_pass_stmt,
    convert_continue_stmt,
    convert_break_stmt,
    convert_del_stmt,
    convert_import_stmt,
    convert_import_name,
    convert_import_relative,
    convert_import_from,
    convert_import_as_name,
    convert_dotted_as_name,
    convert_import_as_names,
    convert_dotted_as_names,
    convert_dotted_name,
    convert_return_stmt,
    convert_raise_stmt,
    convert_global_stmt,
    convert_nonlocal_stmt,
    convert_assert_stmt,
    convert_compound_stmt,
    convert_if_stmt,
    convert_if_stmt_elif,
    convert_if_stmt_else,
    convert_while_stmt,
    convert_for_stmt,
    convert_try_stmt,
    convert_except_clause,
    convert_with_stmt,
    convert_with_item,
    convert_asyncable_funcdef,
    convert_funcdef,
    convert_classdef,
    convert_decorator,
    convert_decorators,
    convert_decorated,
    convert_asyncable_stmt,
    convert_parameters,
    convert_argslist,
    convert_fpdef_star,
    convert_fpdef_starstar,
    convert_fpdef_assign,
    convert_fpdef,
    convert_funcdef_annotation,
    convert_suite,
    convert_indented_suite,
    convert_test,
    convert_test_nocond,
    convert_lambda,
    convert_boolop,
    convert_not_test,
    convert_comparison,
    convert_comp_op,
    convert_star_expr,
    convert_binop,
    convert_factor,
    convert_power,
    convert_atom_expr,
    convert_atom_expr_await,
    convert_atom_expr_trailer,
    convert_trailer,
    convert_trailer_attribute,
    convert_trailer_subscriptlist,
    convert_subscriptlist,
    convert_subscript,
    convert_sliceop,
    convert_trailer_arglist,
    convert_atom,
    convert_atom_basic,
    convert_atom_parens,
    convert_atom_squarebrackets,
    convert_atom_curlybraces,
    convert_atom_string,
    convert_fstring,
    convert_fstring_content,
    convert_fstring_conversion,
    convert_fstring_expr,
    convert_fstring_format_spec,
    convert_atom_ellipses,
    convert_testlist_comp_tuple,
    convert_testlist_comp_list,
    convert_test_or_expr_list,
    convert_dictorsetmaker,
    convert_arglist,
    convert_argument,
    convert_arg_assign_comp_for,
    convert_star_arg,
    convert_sync_comp_for,
    convert_comp_for,
    convert_comp_if,
    convert_yield_expr,
    convert_yield_arg,
)


def get_grammar_str() -> str:
    """
    Returns an BNF-like grammar text that `parso.pgen2.generator.generate_grammar` can
    handle.

    While you should generally use `get_grammar` instead, this can be useful for
    debugging the grammar.
    """
    lines = []
    for p in get_nonterminal_productions():
        lines.append(str(p))
    return "\n".join(lines) + "\n"


# TODO: We should probably provide an on-disk cache like parso and lib2to3 do. Because
# of how we're defining our grammar, efficient cache invalidation is harder, though not
# impossible.
@lru_cache()
def get_grammar() -> "Grammar[TokenType]":
    return generate_grammar(get_grammar_str(), PythonTokenTypes)


@lru_cache()
def get_terminal_conversions() -> Mapping[str, TerminalConversion]:
    """
    Returns a mapping from terminal type name to the conversion function that should be
    called by the parser.
    """
    return {
        # pyre-fixme[16]: Optional type has no attribute `group`.
        re.match("convert_(.*)", fn.__name__).group(1): fn
        for fn in _TERMINAL_CONVERSIONS_SEQUENCE
    }


@lru_cache()
def validate_grammar() -> None:
    for fn in _NONTERMINAL_CONVERSIONS_SEQUENCE:
        fn_productions = get_productions(fn)
        if all(p.name == fn_productions[0].name for p in fn_productions):
            # all the production names are the same, ensure that the `convert_` function
            # is named correctly
            production_name = fn_productions[0].name
            expected_name = f"convert_{production_name}"
            if fn.__name__ != expected_name:
                raise Exception(
                    f"The conversion function for '{production_name}' "
                    + f"must be called '{expected_name}', not '{fn.__name__}'."
                )


def get_nonterminal_productions() -> Iterator[Production]:
    for conversion in _NONTERMINAL_CONVERSIONS_SEQUENCE:
        # TODO: Filter out productions used by other python versions here
        yield from get_productions(conversion)


@lru_cache()
def get_nonterminal_conversions() -> Mapping[str, NonterminalConversion]:
    """
    Returns a mapping from nonterminal production name to the conversion function that
    should be called by the parser.
    """
    conversions = {}
    for fn in _NONTERMINAL_CONVERSIONS_SEQUENCE:
        for fn_production in get_productions(fn):
            # TODO: Filter out productions used by other python versions here
            if fn_production.name in conversions:
                raise Exception(
                    f"Found duplicate '{fn_production.name}' production in grammar"
                )
            conversions[fn_production.name] = fn

    return conversions
