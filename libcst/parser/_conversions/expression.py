# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
from tokenize import (
    Floatnumber as FLOATNUMBER_RE,
    Imagnumber as IMAGNUMBER_RE,
    Intnumber as INTNUMBER_RE,
)
from typing import Any, Dict, List, Sequence, Type

import libcst.nodes as cst
from libcst._maybe_sentinel import MaybeSentinel
from libcst.parser._conversions.dummy import make_dummy_node
from libcst.parser._custom_itertools import grouper
from libcst.parser._production_decorator import with_production
from libcst.parser._types.config import ParserConfig
from libcst.parser._types.partials import (
    ArglistPartial,
    AttributePartial,
    CallPartial,
    FormattedStringConversionPartial,
    FormattedStringFormatSpecPartial,
    SlicePartial,
    SubscriptPartial,
    WithLeadingWhitespace,
)
from libcst.parser._types.token import Token
from libcst.parser._whitespace_parser import parse_parenthesizable_whitespace


BINOP_TOKEN_LUT: Dict[str, Type[cst.BaseBinaryOp]] = {
    "*": cst.Multiply,
    "@": cst.MatrixMultiply,
    "/": cst.Divide,
    "%": cst.Modulo,
    "//": cst.FloorDivide,
    "+": cst.Add,
    "-": cst.Subtract,
    "<<": cst.LeftShift,
    ">>": cst.RightShift,
    "&": cst.BitAnd,
    "^": cst.BitXor,
    "|": cst.BitOr,
}


BOOLOP_TOKEN_LUT: Dict[str, Type[cst.BaseBooleanOp]] = {"and": cst.And, "or": cst.Or}


COMPOP_TOKEN_LUT: Dict[str, Type[cst.BaseCompOp]] = {
    "<": cst.LessThan,
    ">": cst.GreaterThan,
    "==": cst.Equal,
    "<=": cst.LessThanEqual,
    ">=": cst.GreaterThanEqual,
    "in": cst.In,
    "is": cst.Is,
}


# N.B. This uses a `testlist | star_expr`, not a `testlist_star_expr` because
# `testlist_star_expr` may not always be representable by a non-partial node, since it's
# only used as part of `expr_stmt`.
@with_production("expression_input", "(testlist | star_expr) ENDMARKER")
def convert_expression_input(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child, endmarker) = children
    # HACK: UGLY! REMOVE THIS SOON!
    # Unwrap WithLeadingWhitespace if it exists. It shouldn't exist by this point, but
    # testlist isn't fully implemented, and we currently leak these partial objects.
    if isinstance(child, WithLeadingWhitespace):
        child = child.value
    return child


@with_production("testlist_star_expr", "(test|star_expr) (',' (test|star_expr))* [',']")
def convert_testlist_star_expr(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child
    else:
        return make_dummy_node(config, children)


@with_production("test", "or_test ['if' or_test 'else' test] | lambdef")
def convert_test(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child
    else:
        (body, if_token, test, else_token, orelse) = children
        return WithLeadingWhitespace(
            cst.IfExp(
                body=body.value,
                test=test.value,
                orelse=orelse.value,
                whitespace_before_if=parse_parenthesizable_whitespace(
                    config, if_token.whitespace_before
                ),
                whitespace_after_if=parse_parenthesizable_whitespace(
                    config, if_token.whitespace_after
                ),
                whitespace_before_else=parse_parenthesizable_whitespace(
                    config, else_token.whitespace_before
                ),
                whitespace_after_else=parse_parenthesizable_whitespace(
                    config, else_token.whitespace_after
                ),
            ),
            body.whitespace_before,
        )


@with_production("test_nocond", "or_test | lambdef_nocond")
def convert_test_nocond(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("lambdef", "'lambda' [varargslist] ':' test")
@with_production("lambdef_nocond", "'lambda' [varargslist] ':' test_nocond")
def convert_lambda(config: ParserConfig, children: Sequence[Any]) -> Any:
    lambdatoken, *params, colontoken, test = children

    # Grab the whitespace around the colon. If there are no params, then
    # the colon owns the whitespace before and after it. If there are
    # any params, then the last param owns the whitespace before the colon.
    # We handle the parameter movement below.
    colon = cst.Colon(
        whitespace_before=parse_parenthesizable_whitespace(
            config, colontoken.whitespace_before
        ),
        whitespace_after=parse_parenthesizable_whitespace(
            config, colontoken.whitespace_after
        ),
    )

    # Unpack optional parameters
    if len(params) == 0:
        parameters = cst.Parameters()
        whitespace_after_lambda = MaybeSentinel.DEFAULT
    else:
        (parameters,) = params
        whitespace_after_lambda = parse_parenthesizable_whitespace(
            config, lambdatoken.whitespace_after
        )

        # Handle pre-colon whitespace
        if parameters.star_kwarg is not None:
            if parameters.star_kwarg.comma == MaybeSentinel.DEFAULT:
                parameters = parameters.with_changes(
                    star_kwarg=parameters.star_kwarg.with_changes(
                        whitespace_after_param=colon.whitespace_before
                    )
                )
        elif parameters.kwonly_params:
            if parameters.kwonly_params[-1].comma == MaybeSentinel.DEFAULT:
                parameters = parameters.with_changes(
                    kwonly_params=(
                        *parameters.kwonly_params[:-1],
                        parameters.kwonly_params[-1].with_changes(
                            whitespace_after_param=colon.whitespace_before
                        ),
                    )
                )
        elif isinstance(parameters.star_arg, cst.Param):
            if parameters.star_arg.comma == MaybeSentinel.DEFAULT:
                parameters = parameters.with_changes(
                    star_arg=parameters.star_arg.with_changes(
                        whitespace_after_param=colon.whitespace_before
                    )
                )
        elif parameters.default_params:
            if parameters.default_params[-1].comma == MaybeSentinel.DEFAULT:
                parameters = parameters.with_changes(
                    default_params=(
                        *parameters.default_params[:-1],
                        parameters.default_params[-1].with_changes(
                            whitespace_after_param=colon.whitespace_before
                        ),
                    )
                )
        elif parameters.params:
            if parameters.params[-1].comma == MaybeSentinel.DEFAULT:
                parameters = parameters.with_changes(
                    params=(
                        *parameters.params[:-1],
                        parameters.params[-1].with_changes(
                            whitespace_after_param=colon.whitespace_before
                        ),
                    )
                )

        # Colon doesn't own its own pre-whitespace now.
        colon = colon.with_changes(whitespace_before=cst.SimpleWhitespace(""))

    # Return a lambda
    return WithLeadingWhitespace(
        cst.Lambda(
            whitespace_after_lambda=whitespace_after_lambda,
            params=parameters,
            body=test.value,
            colon=colon,
        ),
        lambdatoken.whitespace_before,
    )


@with_production("or_test", "and_test ('or' and_test)*")
@with_production("and_test", "not_test ('and' not_test)*")
def convert_boolop(config: ParserConfig, children: Sequence[Any]) -> Any:
    leftexpr, *rightexprs = children
    if len(rightexprs) == 0:
        return leftexpr

    whitespace_before = leftexpr.whitespace_before
    leftexpr = leftexpr.value

    # Convert all of the operations that have no precedence in a loop
    for op, rightexpr in grouper(rightexprs, 2):
        if op.string not in BOOLOP_TOKEN_LUT:
            raise Exception(f"Unexpected token '{op.string}'!")
        leftexpr = cst.BooleanOperation(
            left=leftexpr,
            operator=BOOLOP_TOKEN_LUT[op.string](
                whitespace_before=parse_parenthesizable_whitespace(
                    config, op.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, op.whitespace_after
                ),
            ),
            right=rightexpr.value,
        )
    return WithLeadingWhitespace(leftexpr, whitespace_before)


@with_production("not_test", "'not' not_test | comparison")
def convert_not_test(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child
    else:
        nottoken, nottest = children
        return WithLeadingWhitespace(
            cst.UnaryOperation(
                operator=cst.Not(
                    whitespace_after=parse_parenthesizable_whitespace(
                        config, nottoken.whitespace_after
                    )
                ),
                expression=nottest.value,
            ),
            nottoken.whitespace_before,
        )


@with_production("comparison", "expr (comp_op expr)*")
def convert_comparison(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child

    lhs, *rest = children

    comparisons: List[cst.ComparisonTarget] = []
    for operator, comparator in grouper(rest, 2):
        comparisons.append(
            cst.ComparisonTarget(operator=operator, comparator=comparator.value)
        )

    return WithLeadingWhitespace(
        cst.Comparison(left=lhs.value, comparisons=tuple(comparisons)),
        lhs.whitespace_before,
    )


@with_production(
    "comp_op", "('<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not')"
)
def convert_comp_op(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (op,) = children
        if op.string in COMPOP_TOKEN_LUT:
            # A regular comparison containing one token
            return COMPOP_TOKEN_LUT[op.string](
                whitespace_before=parse_parenthesizable_whitespace(
                    config, op.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, op.whitespace_after
                ),
            )
        elif op.string in ["!=", "<>"]:
            # Not equal, which can take two forms in some cases
            return cst.NotEqual(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, op.whitespace_before
                ),
                value=op.string,
                whitespace_after=parse_parenthesizable_whitespace(
                    config, op.whitespace_after
                ),
            )
        else:
            # TODO: Make this a ParserSyntaxError
            raise Exception(f"Unexpected token '{op.string}'!")
    else:
        # A two-token comparison
        leftcomp, rightcomp = children

        if leftcomp.string == "not" and rightcomp.string == "in":
            return cst.NotIn(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, leftcomp.whitespace_before
                ),
                whitespace_between=parse_parenthesizable_whitespace(
                    config, leftcomp.whitespace_after
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, rightcomp.whitespace_after
                ),
            )
        elif leftcomp.string == "is" and rightcomp.string == "not":
            return cst.IsNot(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, leftcomp.whitespace_before
                ),
                whitespace_between=parse_parenthesizable_whitespace(
                    config, leftcomp.whitespace_after
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, rightcomp.whitespace_after
                ),
            )
        else:
            # TODO: Make this a ParserSyntaxError
            raise Exception(f"Unexpected token '{leftcomp.string} {rightcomp.string}'!")


@with_production("star_expr", "'*' expr")
def convert_star_expr(config: ParserConfig, children: Sequence[Any]) -> Any:
    star, expr = children
    return WithLeadingWhitespace(
        cst.Starred(
            expr.value,
            whitespace_after_star=parse_parenthesizable_whitespace(
                config, star.whitespace_after
            ),
        ),
        star.whitespace_before,
    )


@with_production("expr", "xor_expr ('|' xor_expr)*")
@with_production("xor_expr", "and_expr ('^' and_expr)*")
@with_production("and_expr", "shift_expr ('&' shift_expr)*")
@with_production("shift_expr", "arith_expr (('<<'|'>>') arith_expr)*")
@with_production("arith_expr", "term (('+'|'-') term)*")
@with_production("term", "factor (('*'|'@'|'/'|'%'|'//') factor)*")
def convert_binop(config: ParserConfig, children: Sequence[Any]) -> Any:
    leftexpr, *rightexprs = children
    if len(rightexprs) == 0:
        return leftexpr

    whitespace_before = leftexpr.whitespace_before
    leftexpr = leftexpr.value

    # Convert all of the operations that have no precedence in a loop
    for op, rightexpr in grouper(rightexprs, 2):
        if op.string not in BINOP_TOKEN_LUT:
            raise Exception(f"Unexpected token '{op.string}'!")
        leftexpr = cst.BinaryOperation(
            left=leftexpr,
            operator=BINOP_TOKEN_LUT[op.string](
                whitespace_before=parse_parenthesizable_whitespace(
                    config, op.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, op.whitespace_after
                ),
            ),
            right=rightexpr.value,
        )
    return WithLeadingWhitespace(leftexpr, whitespace_before)


@with_production("factor", "('+'|'-'|'~') factor | power")
def convert_factor(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child

    op, factor = children

    # First, tokenize the unary operator
    if op.string == "+":
        opnode = cst.Plus(
            whitespace_after=parse_parenthesizable_whitespace(
                config, op.whitespace_after
            )
        )
    elif op.string == "-":
        opnode = cst.Minus(
            whitespace_after=parse_parenthesizable_whitespace(
                config, op.whitespace_after
            )
        )
    elif op.string == "~":
        opnode = cst.BitInvert(
            whitespace_after=parse_parenthesizable_whitespace(
                config, op.whitespace_after
            )
        )
    else:
        raise Exception(f"Unexpected token '{op.string}'!")

    # Second, bump the operator into a number node if that's what the
    # factor is. Otherwise, return a unary operator node.
    if (
        isinstance(factor.value, cst.Number)
        and isinstance(opnode, (cst.Plus, cst.Minus))
        and factor.value.operator is None
    ):
        return WithLeadingWhitespace(
            cst.Number(operator=opnode, number=factor.value.number),
            op.whitespace_before,
        )
    else:
        return WithLeadingWhitespace(
            cst.UnaryOperation(operator=opnode, expression=factor.value),
            op.whitespace_before,
        )


@with_production("power", "atom_expr ['**' factor]")
def convert_power(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child

    left, power, right = children
    return WithLeadingWhitespace(
        cst.BinaryOperation(
            left=left.value,
            operator=cst.Power(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, power.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, power.whitespace_after
                ),
            ),
            right=right.value,
        ),
        left.whitespace_before,
    )


@with_production("atom_expr", "atom_expr_await | atom_expr_trailer")
def convert_atom_expr(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("atom_expr_await", "'await' atom_expr_trailer")
def convert_atom_expr_await(config: ParserConfig, children: Sequence[Any]) -> Any:
    keyword, expr = children
    return WithLeadingWhitespace(
        cst.Await(
            whitespace_after_await=parse_parenthesizable_whitespace(
                config, keyword.whitespace_after
            ),
            expression=expr.value,
        ),
        keyword.whitespace_before,
    )


@with_production("atom_expr_trailer", "atom trailer*")
def convert_atom_expr_trailer(config: ParserConfig, children: Sequence[Any]) -> Any:
    atom, *trailers = children
    whitespace_before = atom.whitespace_before
    atom = atom.value

    # Need to walk through all trailers from left to right and construct
    # a series of nodes based on each partial type. We can't do this with
    # left recursion due to limits in the parser.
    for trailer in trailers:
        if isinstance(trailer, SubscriptPartial):
            atom = cst.Subscript(
                value=atom,
                whitespace_after_value=parse_parenthesizable_whitespace(
                    config, trailer.whitespace_before
                ),
                lbracket=trailer.lbracket,
                slice=trailer.slice,
                rbracket=trailer.rbracket,
            )
        elif isinstance(trailer, AttributePartial):
            atom = cst.Attribute(value=atom, dot=trailer.dot, attr=trailer.attr)
        elif isinstance(trailer, CallPartial):
            # If the trailing argument doesn't have a comma, then it owns the
            # trailing whitespace before the rpar. Otherwise, the comma owns
            # it.
            if (
                len(trailer.args) > 0
                and trailer.args[-1].comma == MaybeSentinel.DEFAULT
            ):
                args = (
                    *trailer.args[:-1],
                    trailer.args[-1].with_changes(
                        whitespace_after_arg=trailer.rpar.whitespace_before
                    ),
                )
            else:
                args = trailer.args
            atom = cst.Call(
                func=atom,
                whitespace_after_func=parse_parenthesizable_whitespace(
                    config, trailer.lpar.whitespace_before
                ),
                whitespace_before_args=trailer.lpar.value.whitespace_after,
                args=tuple(args),
            )
        else:
            # This is an invalid trailer, so lets give up
            raise Exception("Logic error!")
    return WithLeadingWhitespace(atom, whitespace_before)


@with_production(
    "trailer", "trailer_arglist | trailer_subscriptlist | trailer_attribute"
)
def convert_trailer(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("trailer_arglist", "'(' [arglist] ')'")
def convert_trailer_arglist(config: ParserConfig, children: Sequence[Any]) -> Any:
    lpar, *arglist, rpar = children
    return CallPartial(
        lpar=WithLeadingWhitespace(
            cst.LeftParen(
                whitespace_after=parse_parenthesizable_whitespace(
                    config, lpar.whitespace_after
                )
            ),
            lpar.whitespace_before,
        ),
        args=() if not arglist else arglist[0].args,
        rpar=cst.RightParen(
            whitespace_before=parse_parenthesizable_whitespace(
                config, rpar.whitespace_before
            )
        ),
    )


@with_production("trailer_subscriptlist", "'[' subscriptlist ']'")
def convert_trailer_subscriptlist(config: ParserConfig, children: Sequence[Any]) -> Any:
    (lbracket, subscriptlist, rbracket) = children
    return SubscriptPartial(
        lbracket=cst.LeftSquareBracket(
            whitespace_after=parse_parenthesizable_whitespace(
                config, lbracket.whitespace_after
            )
        ),
        slice=subscriptlist.value,
        rbracket=cst.RightSquareBracket(
            whitespace_before=parse_parenthesizable_whitespace(
                config, rbracket.whitespace_before
            )
        ),
        whitespace_before=lbracket.whitespace_before,
    )


@with_production("subscriptlist", "subscript (',' subscript)* [',']")
def convert_subscriptlist(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) > 1:
        # This is a list of ExtSlice, so construct as such by grouping every
        # subscript with an optional comma and adding to a list.
        extslices = []
        for slice, comma in grouper(children, 2):
            if comma is None:
                extslices.append(cst.ExtSlice(slice=slice.value))
            else:
                extslices.append(
                    cst.ExtSlice(
                        slice=slice.value,
                        comma=cst.Comma(
                            whitespace_before=parse_parenthesizable_whitespace(
                                config, comma.whitespace_before
                            ),
                            whitespace_after=parse_parenthesizable_whitespace(
                                config, comma.whitespace_after
                            ),
                        ),
                    )
                )
        return WithLeadingWhitespace(extslices, children[0].whitespace_before)
    else:
        # This is an Index or Slice, as parsed in the child.
        (index_or_slice,) = children
        return index_or_slice


@with_production("subscript", "test | [test] ':' [test] [sliceop]")
def convert_subscript(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1 and not isinstance(children[0], Token):
        # This is just an index node
        (test,) = children
        return WithLeadingWhitespace(cst.Index(test.value), test.whitespace_before)

    if isinstance(children[-1], SlicePartial):
        # We got a partial slice as the final param. Extract the final
        # bits of the full subscript.
        *others, sliceop = children
        whitespace_before = others[0].whitespace_before
        second_colon = sliceop.second_colon
        step = sliceop.step
    else:
        # We can just parse this below, without taking extras from the
        # partial child.
        others = children
        whitespace_before = others[0].whitespace_before
        second_colon = MaybeSentinel.DEFAULT
        step = None

    # We need to create a partial slice to pass up. So, align so we have
    # a list that's always [Optional[Test], Colon, Optional[Test]].
    if isinstance(others[0], Token):
        # First token is a colon, so insert an empty test on the LHS. We
        # know the RHS is a test since it's not a sliceop.
        slicechildren = [None, *others]
    else:
        # First token is non-colon, so its a test.
        slicechildren = [*others]

    if len(slicechildren) < 3:
        # Now, we have to fill in the RHS. We know its two long
        # at this point if its not already 3.
        slicechildren = [*slicechildren, None]

    lower, first_colon, upper = slicechildren
    return WithLeadingWhitespace(
        cst.Slice(
            lower=lower.value if lower is not None else None,
            first_colon=cst.Colon(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, first_colon.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, first_colon.whitespace_after
                ),
            ),
            upper=upper.value if upper is not None else None,
            second_colon=second_colon,
            step=step,
        ),
        whitespace_before=whitespace_before,
    )


@with_production("sliceop", "':' [test]")
def convert_sliceop(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 2:
        colon, test = children
        step = test.value
    else:
        (colon,) = children
        step = None
    return SlicePartial(
        second_colon=cst.Colon(
            whitespace_before=parse_parenthesizable_whitespace(
                config, colon.whitespace_before
            ),
            whitespace_after=parse_parenthesizable_whitespace(
                config, colon.whitespace_after
            ),
        ),
        step=step,
    )


@with_production("trailer_attribute", "'.' NAME")
def convert_trailer_attribute(config: ParserConfig, children: Sequence[Any]) -> Any:
    dot, name = children
    return AttributePartial(
        dot=cst.Dot(
            whitespace_before=parse_parenthesizable_whitespace(
                config, dot.whitespace_before
            ),
            whitespace_after=parse_parenthesizable_whitespace(
                config, dot.whitespace_after
            ),
        ),
        attr=cst.Name(name.string),
    )


@with_production(
    "atom",
    "atom_parens | atom_squarebrackets | atom_curlybrackets | atom_string | atom_fstring | atom_basic | atom_ellipses",
)
def convert_atom(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("atom_basic", "NAME | NUMBER | 'None' | 'True' | 'False'")
def convert_atom_basic(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    if child.type.name == "NAME":
        # This also handles 'None', 'True', and 'False' directly, but we
        # keep it in the grammar to be more correct.
        return WithLeadingWhitespace(cst.Name(child.string), child.whitespace_before)
    elif child.type.name == "NUMBER":
        # We must determine what type of number it is since we split node
        # types up this way.
        if re.fullmatch(INTNUMBER_RE, child.string):
            return WithLeadingWhitespace(
                cst.Number(cst.Integer(child.string)), child.whitespace_before
            )
        elif re.fullmatch(FLOATNUMBER_RE, child.string):
            return WithLeadingWhitespace(
                cst.Number(cst.Float(child.string)), child.whitespace_before
            )
        elif re.fullmatch(IMAGNUMBER_RE, child.string):
            return WithLeadingWhitespace(
                cst.Number(cst.Imaginary(child.string)), child.whitespace_before
            )
        else:
            raise Exception("Unparseable number {child.string}")
    else:
        raise Exception(f"Logic error, unexpected token {child.type.name}")


@with_production("atom_squarebrackets", "'[' [testlist_comp] ']'")
def convert_atom_squarebrackets(config: ParserConfig, children: Sequence[Any]) -> Any:
    return make_dummy_node(config, children)


@with_production("atom_curlybrackets", "'{' [dictorsetmaker] '}'")
def convert_atom_curlybrackets(config: ParserConfig, children: Sequence[Any]) -> Any:
    return make_dummy_node(config, children)


@with_production("atom_parens", "'(' [yield_expr|testlist_comp] ')'")
def convert_atom_parens(config: ParserConfig, children: Sequence[Any]) -> Any:
    lpar, *atoms, rpar = children

    if len(atoms) == 1:
        inner_atom = atoms[0].value
        # With numbers, we bubble up the parens to the innermost node since
        # Number() is just a wrapper to match on any valid number. The only
        # instance where we don't do this is in the case that a number has
        # a unary operator associated with it. In this case, the outer parens
        # are owned by the Number node instead of the inner Integer/Float/Imaginary.
        if isinstance(inner_atom, cst.Number) and inner_atom.operator is None:
            return WithLeadingWhitespace(
                inner_atom.with_changes(
                    number=inner_atom.number.with_changes(
                        lpar=(
                            (
                                cst.LeftParen(
                                    whitespace_after=parse_parenthesizable_whitespace(
                                        config, lpar.whitespace_after
                                    )
                                ),
                            )
                            + tuple(inner_atom.lpar)
                        ),
                        rpar=(
                            tuple(inner_atom.rpar)
                            + (
                                cst.RightParen(
                                    whitespace_before=parse_parenthesizable_whitespace(
                                        config, rpar.whitespace_before
                                    )
                                ),
                            )
                        ),
                    )
                ),
                lpar.whitespace_before,
            )
        else:
            return WithLeadingWhitespace(
                inner_atom.with_changes(
                    lpar=(
                        (
                            cst.LeftParen(
                                whitespace_after=parse_parenthesizable_whitespace(
                                    config, lpar.whitespace_after
                                )
                            ),
                        )
                        + tuple(inner_atom.lpar)
                    ),
                    rpar=(
                        tuple(inner_atom.rpar)
                        + (
                            cst.RightParen(
                                whitespace_before=parse_parenthesizable_whitespace(
                                    config, rpar.whitespace_before
                                )
                            ),
                        )
                    ),
                ),
                lpar.whitespace_before,
            )
    else:
        # We don't support tuples yet
        return make_dummy_node(config, children)


@with_production("atom_ellipses", "'...'")
def convert_atom_ellipses(config: ParserConfig, children: Sequence[Any]) -> Any:
    (token,) = children
    return WithLeadingWhitespace(cst.Ellipses(), token.whitespace_before)


@with_production("atom_string", "STRING atom_string | STRING STRING | STRING")
def convert_atom_string(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        return WithLeadingWhitespace(
            cst.SimpleString(children[0].string), children[0].whitespace_before
        )
    else:
        left, right = children
        if isinstance(right, Token):
            return WithLeadingWhitespace(
                cst.ConcatenatedString(
                    left=cst.SimpleString(left.string),
                    whitespace_between=parse_parenthesizable_whitespace(
                        config, right.whitespace_before
                    ),
                    right=cst.SimpleString(right.string),
                ),
                left.whitespace_before,
            )
        else:
            return WithLeadingWhitespace(
                cst.ConcatenatedString(
                    left=cst.SimpleString(left.string),
                    whitespace_between=parse_parenthesizable_whitespace(
                        config, right.whitespace_before
                    ),
                    right=right.value,
                ),
                left.whitespace_before,
            )


@with_production("atom_fstring", "fstring [ atom_fstring | fstring ]")
def convert_atom_fstring(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        # Return the already-parsed f-string object
        (child,) = children
        return child
    else:
        left, right = children
        # Return a concatenated version of these two f-strings
        return WithLeadingWhitespace(
            cst.ConcatenatedString(
                left=left.value,
                whitespace_between=parse_parenthesizable_whitespace(
                    config, right.whitespace_before
                ),
                right=right.value,
            ),
            left.whitespace_before,
        )


@with_production("fstring", "FSTRING_START fstring_content* FSTRING_END")
def convert_fstring(config: ParserConfig, children: Sequence[Any]) -> Any:
    start, *content, end = children
    return WithLeadingWhitespace(
        cst.FormattedString(start=start.string, parts=tuple(content), end=end.string),
        start.whitespace_before,
    )


@with_production("fstring_content", "FSTRING_STRING | fstring_expr")
def convert_fstring_content(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    if isinstance(child, Token):
        # Construct and return a raw string portion.
        return cst.FormattedStringText(child.string)
    else:
        # Pass the expression up one production.
        return child


@with_production("fstring_conversion", "'!' NAME")
def convert_fstring_conversion(config: ParserConfig, children: Sequence[Any]) -> Any:
    exclaim, name = children
    # There cannot be a space between the two tokens, so no need to preserve this.
    return FormattedStringConversionPartial(name.string, exclaim.whitespace_before)


@with_production(
    "fstring_expr", "'{' testlist [ fstring_conversion ] [ fstring_format_spec ] '}'"
)
def convert_fstring_expr(config: ParserConfig, children: Sequence[Any]) -> Any:
    openbrkt, testlist, *conversions, closebrkt = children

    # Extract any optional conversion
    if len(conversions) > 0 and isinstance(
        conversions[0], FormattedStringConversionPartial
    ):
        conversion = conversions[0].value
        conversions = conversions[1:]
    else:
        conversion = None

    # Extract any optional format spec
    if len(conversions) > 0:
        format_spec = conversions[0].values
    else:
        format_spec = None

    return cst.FormattedStringExpression(
        whitespace_before_expression=parse_parenthesizable_whitespace(
            config, testlist.whitespace_before
        ),
        expression=testlist.value,
        whitespace_after_expression=parse_parenthesizable_whitespace(
            config, children[2].whitespace_before
        ),
        conversion=conversion,
        format_spec=format_spec,
    )


@with_production("fstring_format_spec", "':' fstring_content*")
def convert_fstring_format_spec(config: ParserConfig, children: Sequence[Any]) -> Any:
    colon, *content = children
    return FormattedStringFormatSpecPartial(tuple(content), colon.whitespace_before)


@with_production(
    "testlist_comp", "(test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )"
)
def convert_testlist_comp(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child
    else:
        return make_dummy_node(config, children)


@with_production("testlist", "test (',' test)* [',']")
def convert_testlist(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child
    else:
        return make_dummy_node(config, children)


@with_production(
    "dictorsetmaker",
    (
        "( ((test ':' test | '**' expr)"
        + "(comp_for | (',' (test ':' test | '**' expr))* [','])) |"
        + "((test | star_expr) "
        + "(comp_for | (',' (test | star_expr))* [','])) )"
    ),
)
def convert_dictorsetmaker(config: ParserConfig, children: Sequence[Any]) -> Any:
    return make_dummy_node(config, children)


@with_production("exprlist", "(expr|star_expr) (',' (expr|star_expr))* [',']")
def convert_exprlist(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child
    else:
        return make_dummy_node(config, children)


@with_production("arglist", "argument (',' argument)* [',']")
def convert_arglist(config: ParserConfig, children: Sequence[Any]) -> Any:
    args = []
    for argument, comma in grouper(children, 2):
        if comma is None:
            args.append(argument)
        else:
            args.append(
                argument.with_changes(
                    comma=cst.Comma(
                        whitespace_before=parse_parenthesizable_whitespace(
                            config, comma.whitespace_before
                        ),
                        whitespace_after=parse_parenthesizable_whitespace(
                            config, comma.whitespace_after
                        ),
                    )
                )
            )
    return ArglistPartial(args)


@with_production("argument", "arg_assign_comp_for | star_arg")
def convert_argument(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("arg_assign_comp_for", "test [comp_for] | test '=' test")
def convert_arg_assign_comp_for(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        # Simple test
        (child,) = children
        return cst.Arg(value=child.value)
    elif len(children) == 2:
        # Comprehension, but we don't support comprehensions yet, so
        # just set the value to a dummy node.
        return cst.Arg(value=make_dummy_node(config, children).value)
    else:
        # "key = value" assignment argument
        lhs, equal, rhs = children
        return cst.Arg(
            keyword=lhs.value,
            equal=cst.AssignEqual(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, equal.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, equal.whitespace_after
                ),
            ),
            value=rhs.value,
        )


@with_production("star_arg", "'**' test | '*' test")
def convert_star_arg(config: ParserConfig, children: Sequence[Any]) -> Any:
    star, test = children
    return cst.Arg(
        star=star.string,
        whitespace_after_star=parse_parenthesizable_whitespace(
            config, star.whitespace_after
        ),
        value=test.value,
    )


@with_production("comp_iter", "comp_for | comp_if")
def convert_comp_iter(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("sync_comp_for", "'for' exprlist 'in' or_test [comp_iter]")
def convert_sync_comp_for(config: ParserConfig, children: Sequence[Any]) -> Any:
    return make_dummy_node(config, children)


@with_production("comp_for", "['async'] sync_comp_for")
def convert_comp_for(config: ParserConfig, children: Sequence[Any]) -> Any:
    return make_dummy_node(config, children)


@with_production("comp_if", "'if' test_nocond [comp_iter]")
def convert_comp_if(config: ParserConfig, children: Sequence[Any]) -> Any:
    return make_dummy_node(config, children)


@with_production("yield_expr", "'yield' [yield_arg]")
def convert_yield_expr(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        # Yielding implicit none
        (yield_token,) = children
        yield_node = cst.Yield(value=None)
    else:
        # Yielding explicit value
        (yield_token, yield_arg) = children
        yield_node = cst.Yield(
            value=yield_arg.value,
            whitespace_after_yield=parse_parenthesizable_whitespace(
                config, yield_arg.whitespace_before
            ),
        )

    return WithLeadingWhitespace(yield_node, yield_token.whitespace_before)


@with_production("yield_arg", "'from' test | testlist")
def convert_yield_arg(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        # Just a regular testlist, pass it up
        (child,) = children
        return child
    else:
        # Its a yield from
        (from_token, test) = children

        return WithLeadingWhitespace(
            cst.From(
                item=test.value,
                whitespace_after_from=parse_parenthesizable_whitespace(
                    config, test.whitespace_before
                ),
            ),
            from_token.whitespace_before,
        )
