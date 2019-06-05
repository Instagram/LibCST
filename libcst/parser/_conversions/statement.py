# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict, List, Optional, Sequence, Tuple, Type

import libcst.nodes as cst
from libcst._maybe_sentinel import MaybeSentinel
from libcst.parser._custom_itertools import grouper
from libcst.parser._production_decorator import with_production
from libcst.parser._types.config import ParserConfig
from libcst.parser._types.partials import (
    AnnAssignPartial,
    AssignPartial,
    AugAssignPartial,
    DecoratorPartial,
    ExceptClausePartial,
    FuncdefPartial,
    ImportPartial,
    ImportRelativePartial,
    SimpleStatementPartial,
    WithLeadingWhitespace,
)
from libcst.parser._types.token import Token
from libcst.parser._whitespace_parser import (
    parse_empty_lines,
    parse_parenthesizable_whitespace,
    parse_simple_whitespace,
)


AUGOP_TOKEN_LUT: Dict[str, Type[cst.BaseAugOp]] = {
    "+=": cst.AddAssign,
    "-=": cst.SubtractAssign,
    "*=": cst.MultiplyAssign,
    "@=": cst.MatrixMultiplyAssign,
    "/=": cst.DivideAssign,
    "%=": cst.ModuloAssign,
    "&=": cst.BitAndAssign,
    "|=": cst.BitOrAssign,
    "^=": cst.BitXorAssign,
    "<<=": cst.LeftShiftAssign,
    ">>=": cst.RightShiftAssign,
    "**=": cst.PowerAssign,
    "//=": cst.FloorDivideAssign,
}


@with_production("stmt_input", "stmt ENDMARKER")
def convert_stmt_input(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child, endmarker) = children
    return child


@with_production("stmt", "simple_stmt_line | compound_stmt")
def convert_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("simple_stmt_partial", "small_stmt (';' small_stmt)* [';'] NEWLINE")
def convert_simple_stmt_partial(config: ParserConfig, children: Sequence[Any]) -> Any:
    *statements, trailing_whitespace = children

    last_stmt = len(statements) / 2
    body = []
    for i, (stmt_body, semi) in enumerate(grouper(statements, 2)):
        if semi is not None:
            if i == (last_stmt - 1):
                # Trailing semicolons only own the whitespace before.
                semi = cst.Semicolon(
                    whitespace_before=parse_simple_whitespace(
                        config, semi.whitespace_before
                    ),
                    whitespace_after=cst.SimpleWhitespace(""),
                )
            else:
                # Middle semicolons own the whitespace before and after.
                semi = cst.Semicolon(
                    whitespace_before=parse_simple_whitespace(
                        config, semi.whitespace_before
                    ),
                    whitespace_after=parse_simple_whitespace(
                        config, semi.whitespace_after
                    ),
                )
        else:
            semi = MaybeSentinel.DEFAULT
        body.append(stmt_body.value.with_changes(semicolon=semi))
    return SimpleStatementPartial(
        body,
        whitespace_before=statements[0].whitespace_before,
        trailing_whitespace=trailing_whitespace,
    )


@with_production("simple_stmt_line", "simple_stmt_partial")
def convert_simple_stmt_line(config: ParserConfig, children: Sequence[Any]) -> Any:
    """
    This function is similar to convert_simple_stmt_suite, but yields a different type
    """
    (partial,) = children
    return cst.SimpleStatementLine(
        partial.body,
        leading_lines=parse_empty_lines(config, partial.whitespace_before),
        trailing_whitespace=partial.trailing_whitespace,
    )


@with_production("simple_stmt_suite", "simple_stmt_partial")
def convert_simple_stmt_suite(config: ParserConfig, children: Sequence[Any]) -> Any:
    """
    This function is similar to convert_simple_stmt_line, but yields a different type
    """
    (partial,) = children
    return cst.SimpleStatementSuite(
        partial.body,
        leading_whitespace=parse_simple_whitespace(config, partial.whitespace_before),
        trailing_whitespace=partial.trailing_whitespace,
    )


@with_production(
    "small_stmt",
    (
        "expr_stmt | del_stmt | pass_stmt | break_stmt | continue_stmt | return_stmt"
        + "| raise_stmt | yield_stmt | import_stmt | global_stmt | nonlocal_stmt"
        + "| assert_stmt"
    ),
)
def convert_small_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    # Doesn't construct SmallStatement, because we don't know about semicolons yet.
    # convert_simple_stmt will construct the SmallStatement nodes.
    (small_stmt_body,) = children
    return small_stmt_body


@with_production("expr_stmt", "testlist_star_expr (annassign | augassign | assign* )")
@with_production("yield_stmt", "yield_expr")
def convert_expr_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        # This is an unassigned expr statement (like a function call)
        (test_node,) = children
        return WithLeadingWhitespace(
            cst.Expr(value=test_node.value), test_node.whitespace_before
        )
    elif len(children) == 2:
        lhs, rhs = children
        if isinstance(rhs, AnnAssignPartial):
            return WithLeadingWhitespace(
                cst.AnnAssign(
                    target=lhs.value,
                    annotation=rhs.annotation,
                    equal=MaybeSentinel.DEFAULT if rhs.equal is None else rhs.equal,
                    value=rhs.value,
                ),
                lhs.whitespace_before,
            )
        elif isinstance(rhs, AugAssignPartial):
            return WithLeadingWhitespace(
                cst.AugAssign(target=lhs.value, operator=rhs.operator, value=rhs.value),
                lhs.whitespace_before,
            )
    # The only thing it could be at this point is an assign with one or more targets.
    # So, walk the children moving the equals ownership back one and constructing a
    # list of AssignTargets.
    targets = []
    for i in range(len(children) - 1):
        target = children[i].value
        equal = children[i + 1].equal

        targets.append(
            cst.AssignTarget(
                target=target,
                whitespace_before_equal=equal.whitespace_before,
                whitespace_after_equal=equal.whitespace_after,
            )
        )

    return WithLeadingWhitespace(
        cst.Assign(targets=tuple(targets), value=children[-1].value),
        children[0].whitespace_before,
    )


@with_production("annassign", "':' test ['=' test]")
def convert_annassign(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 2:
        # Variable annotation only
        colon, annotation = children
        annotation = annotation.value
        equal = None
        value = None
    elif len(children) == 4:
        # Variable annotation and assignment
        colon, annotation, equal, value = children
        annotation = annotation.value
        value = value.value
        equal = cst.AssignEqual(
            whitespace_before=parse_simple_whitespace(config, equal.whitespace_before),
            whitespace_after=parse_simple_whitespace(config, equal.whitespace_after),
        )
    else:
        raise Exception("Invalid parser state!")

    return AnnAssignPartial(
        annotation=cst.Annotation(
            whitespace_before_indicator=parse_simple_whitespace(
                config, colon.whitespace_before
            ),
            indicator=colon.string,
            whitespace_after_indicator=parse_simple_whitespace(
                config, colon.whitespace_after
            ),
            annotation=annotation,
        ),
        equal=equal,
        value=value,
    )


@with_production(
    "augassign",
    (
        "('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | "
        + "'>>=' | '**=' | '//=') (yield_expr | testlist)"
    ),
)
def convert_augassign(config: ParserConfig, children: Sequence[Any]) -> Any:
    op, expr = children
    if op.string not in AUGOP_TOKEN_LUT:
        raise Exception(f"Unexpected token '{op.string}'!")
    return AugAssignPartial(
        operator=AUGOP_TOKEN_LUT[op.string](
            whitespace_before=parse_simple_whitespace(config, op.whitespace_before),
            whitespace_after=parse_simple_whitespace(config, op.whitespace_after),
        ),
        value=expr.value,
    )


@with_production("assign", "'=' (yield_expr|testlist_star_expr)")
def convert_assign(config: ParserConfig, children: Sequence[Any]) -> Any:
    equal, expr = children
    return AssignPartial(
        equal=cst.AssignEqual(
            whitespace_before=parse_simple_whitespace(config, equal.whitespace_before),
            whitespace_after=parse_simple_whitespace(config, equal.whitespace_after),
        ),
        value=expr.value,
    )


@with_production("pass_stmt", "'pass'")
def convert_pass_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (name,) = children
    return WithLeadingWhitespace(cst.Pass(), name.whitespace_before)


@with_production("del_stmt", "'del' exprlist")
def convert_del_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (del_name, exprlist) = children
    return WithLeadingWhitespace(
        cst.Del(
            target=exprlist.value,
            whitespace_after_del=parse_simple_whitespace(
                config, del_name.whitespace_after
            ),
        ),
        del_name.whitespace_before,
    )


@with_production("continue_stmt", "'continue'")
def convert_continue_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (name,) = children
    return WithLeadingWhitespace(cst.Continue(), name.whitespace_before)


@with_production("break_stmt", "'break'")
def convert_break_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (name,) = children
    return WithLeadingWhitespace(cst.Break(), name.whitespace_before)


@with_production("return_stmt", "'return' [testlist]")
def convert_return_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (keyword,) = children
        return WithLeadingWhitespace(
            cst.Return(whitespace_after_return=cst.SimpleWhitespace("")),
            keyword.whitespace_before,
        )
    else:
        (keyword, testlist) = children
        return WithLeadingWhitespace(
            cst.Return(
                value=testlist.value,
                whitespace_after_return=parse_simple_whitespace(
                    config, keyword.whitespace_after
                ),
            ),
            keyword.whitespace_before,
        )


@with_production("import_stmt", "import_name | import_from")
def convert_import_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (child,) = children
    return child


@with_production("import_name", "'import' dotted_as_names")
def convert_import_name(config: ParserConfig, children: Sequence[Any]) -> Any:
    importtoken, names = children
    return WithLeadingWhitespace(
        cst.Import(
            names=names.names,
            whitespace_after_import=parse_simple_whitespace(
                config, importtoken.whitespace_after
            ),
        ),
        importtoken.whitespace_before,
    )


@with_production("import_relative", "('.' | '...')* dotted_name | ('.' | '...')+")
def convert_import_relative(config: ParserConfig, children: Sequence[Any]) -> Any:
    dots = []
    dotted_name = None
    for child in children:
        if isinstance(child, Token):
            # Special case for "...", which is part of the grammar
            if child.string == "...":
                dots.extend(
                    [
                        cst.Dot(),
                        cst.Dot(),
                        cst.Dot(
                            whitespace_after=parse_simple_whitespace(
                                config, child.whitespace_after
                            )
                        ),
                    ]
                )
            else:
                dots.append(
                    cst.Dot(
                        whitespace_after=parse_simple_whitespace(
                            config, child.whitespace_after
                        )
                    )
                )
        else:
            # This should be the dotted name, and we can't get more than
            # one, but lets be sure anyway
            if dotted_name is not None:
                raise Exception("Logic error!")
            dotted_name = child

    return ImportRelativePartial(relative=tuple(dots), module=dotted_name)


@with_production(
    "import_from",
    "'from' import_relative 'import' ('*' | '(' import_as_names ')' | import_as_names)",
)
def convert_import_from(config: ParserConfig, children: Sequence[Any]) -> Any:
    fromtoken, import_relative, importtoken, *importlist = children

    if len(importlist) == 1:
        (possible_star,) = importlist
        if isinstance(possible_star, Token):
            # Its a "*" import, so we must construct this node.
            names = cst.ImportStar()
        else:
            # Its an import as names partial, grab the names from that.
            names = possible_star.names
        lpar = None
        rpar = None
    else:
        # Its an import as names partial with parens
        lpartoken, namespartial, rpartoken = importlist
        lpar = cst.LeftParen(
            whitespace_after=parse_parenthesizable_whitespace(
                config, lpartoken.whitespace_after
            )
        )
        names = namespartial.names
        rpar = cst.RightParen(
            whitespace_before=parse_parenthesizable_whitespace(
                config, rpartoken.whitespace_before
            )
        )

    # If we have a relative-only import, then we need to relocate the space
    # after the final dot to be owned by the import token.
    if len(import_relative.relative) > 0 and import_relative.module is None:
        whitespace_before_import = import_relative.relative[-1].whitespace_after
        relative = (
            *import_relative.relative[:-1],
            import_relative.relative[-1].with_changes(
                whitespace_after=cst.SimpleWhitespace("")
            ),
        )
    else:
        whitespace_before_import = parse_simple_whitespace(
            config, importtoken.whitespace_before
        )
        relative = import_relative.relative

    return WithLeadingWhitespace(
        cst.ImportFrom(
            whitespace_after_from=parse_simple_whitespace(
                config, fromtoken.whitespace_after
            ),
            relative=relative,
            module=import_relative.module,
            whitespace_before_import=whitespace_before_import,
            whitespace_after_import=parse_simple_whitespace(
                config, importtoken.whitespace_after
            ),
            lpar=lpar,
            names=names,
            rpar=rpar,
        ),
        fromtoken.whitespace_before,
    )


@with_production("import_as_name", "NAME ['as' NAME]")
def convert_import_as_name(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (dotted_name,) = children
        return cst.ImportAlias(name=cst.Name(dotted_name.string), asname=None)
    else:
        dotted_name, astoken, name = children
        return cst.ImportAlias(
            name=cst.Name(dotted_name.string),
            asname=cst.AsName(
                whitespace_before_as=parse_simple_whitespace(
                    config, astoken.whitespace_before
                ),
                whitespace_after_as=parse_simple_whitespace(
                    config, astoken.whitespace_after
                ),
                name=cst.Name(name.string),
            ),
        )


@with_production("dotted_as_name", "dotted_name ['as' NAME]")
def convert_dotted_as_name(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (dotted_name,) = children
        return cst.ImportAlias(name=dotted_name, asname=None)
    else:
        dotted_name, astoken, name = children
        return cst.ImportAlias(
            name=dotted_name,
            asname=cst.AsName(
                whitespace_before_as=parse_parenthesizable_whitespace(
                    config, astoken.whitespace_before
                ),
                whitespace_after_as=parse_parenthesizable_whitespace(
                    config, astoken.whitespace_after
                ),
                name=cst.Name(name.string),
            ),
        )


@with_production("import_as_names", "import_as_name (',' import_as_name)* [',']")
def convert_import_as_names(config: ParserConfig, children: Sequence[Any]) -> Any:
    return _gather_import_names(config, children)


@with_production("dotted_as_names", "dotted_as_name (',' dotted_as_name)*")
def convert_dotted_as_names(config: ParserConfig, children: Sequence[Any]) -> Any:
    return _gather_import_names(config, children)


def _gather_import_names(
    config: ParserConfig, children: Sequence[Any]
) -> ImportPartial:
    names = []
    for name, comma in grouper(children, 2):
        if comma is None:
            names.append(name)
        else:
            names.append(
                name.with_changes(
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

    return ImportPartial(names=names)


@with_production("dotted_name", "NAME ('.' NAME)*")
def convert_dotted_name(config: ParserConfig, children: Sequence[Any]) -> Any:
    left, *rest = children
    node = cst.Name(left.string)

    for dot, right in grouper(rest, 2):
        node = cst.Attribute(
            value=node,
            dot=cst.Dot(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, dot.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, dot.whitespace_after
                ),
            ),
            attr=cst.Name(right.string),
        )

    return node


@with_production("raise_stmt", "'raise' [test ['from' test]]")
def convert_raise_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (raise_token,) = children
        whitespace_after_raise = MaybeSentinel.DEFAULT
        exc = None
        cause = None
    elif len(children) == 2:
        (raise_token, test) = children
        whitespace_after_raise = parse_simple_whitespace(config, test.whitespace_before)
        exc = test.value
        cause = None
    elif len(children) == 4:
        (raise_token, test, from_token, source) = children
        whitespace_after_raise = parse_simple_whitespace(config, test.whitespace_before)
        exc = test.value
        cause = cst.From(
            whitespace_before_from=parse_simple_whitespace(
                config, from_token.whitespace_before
            ),
            whitespace_after_from=parse_simple_whitespace(
                config, source.whitespace_before
            ),
            item=source.value,
        )
    else:
        raise Exception("Logic error!")

    return WithLeadingWhitespace(
        cst.Raise(whitespace_after_raise=whitespace_after_raise, exc=exc, cause=cause),
        raise_token.whitespace_before,
    )


def _construct_nameitems(
    config: ParserConfig, names: Sequence[Any]
) -> List[cst.NameItem]:
    nameitems: List[cst.NameItem] = []
    for name, maybe_comma in grouper(names, 2):
        if maybe_comma is None:
            nameitems.append(cst.NameItem(cst.Name(name.string)))
        else:
            nameitems.append(
                cst.NameItem(
                    cst.Name(name.string),
                    comma=cst.Comma(
                        whitespace_before=parse_simple_whitespace(
                            config, maybe_comma.whitespace_before
                        ),
                        whitespace_after=parse_simple_whitespace(
                            config, maybe_comma.whitespace_after
                        ),
                    ),
                )
            )
    return nameitems


@with_production("global_stmt", "'global' NAME (',' NAME)*")
def convert_global_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (global_token, *names) = children
    return WithLeadingWhitespace(
        cst.Global(
            names=tuple(_construct_nameitems(config, names)),
            whitespace_after_global=parse_simple_whitespace(
                config, names[0].whitespace_before
            ),
        ),
        global_token.whitespace_before,
    )


@with_production("nonlocal_stmt", "'nonlocal' NAME (',' NAME)*")
def convert_nonlocal_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (nonlocal_token, *names) = children
    return WithLeadingWhitespace(
        cst.Nonlocal(
            names=tuple(_construct_nameitems(config, names)),
            whitespace_after_nonlocal=parse_simple_whitespace(
                config, names[0].whitespace_before
            ),
        ),
        nonlocal_token.whitespace_before,
    )


@with_production("assert_stmt", "'assert' test [',' test]")
def convert_assert_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 2:
        (assert_token, test) = children
        assert_node = cst.Assert(
            whitespace_after_assert=parse_simple_whitespace(
                config, test.whitespace_before
            ),
            test=test.value,
            msg=None,
        )
    else:
        (assert_token, test, comma_token, msg) = children
        assert_node = cst.Assert(
            whitespace_after_assert=parse_simple_whitespace(
                config, test.whitespace_before
            ),
            test=test.value,
            comma=cst.Comma(
                whitespace_before=parse_simple_whitespace(
                    config, comma_token.whitespace_before
                ),
                whitespace_after=parse_simple_whitespace(config, msg.whitespace_before),
            ),
            msg=msg.value,
        )

    return WithLeadingWhitespace(assert_node, assert_token.whitespace_before)


@with_production(
    "compound_stmt",
    ("if_stmt | while_stmt | asyncable_stmt | try_stmt | classdef | decorated"),
)
def convert_compound_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (stmt,) = children
    return stmt


@with_production("if_stmt", "'if' test ':' suite [if_stmt_elif|if_stmt_else]")
def convert_if_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    if_tok, test, colon_tok, suite, *tail = children

    if len(tail) > 0:
        (orelse,) = tail
    else:
        orelse = None

    return cst.If(
        leading_lines=parse_empty_lines(config, if_tok.whitespace_before),
        whitespace_before_test=parse_simple_whitespace(config, if_tok.whitespace_after),
        test=test.value,
        whitespace_after_test=parse_simple_whitespace(
            config, colon_tok.whitespace_before
        ),
        body=suite,
        orelse=orelse,
    )


@with_production("if_stmt_elif", "'elif' test ':' suite [if_stmt_elif|if_stmt_else]")
def convert_if_stmt_elif(config: ParserConfig, children: Sequence[Any]) -> Any:
    # this behaves exactly the same as `convert_if_stmt`, except that the leading token
    # has a different string value.
    return convert_if_stmt(config, children)


@with_production("if_stmt_else", "'else' ':' suite")
def convert_if_stmt_else(config: ParserConfig, children: Sequence[Any]) -> Any:
    else_tok, colon_tok, suite = children
    return cst.Else(
        leading_lines=parse_empty_lines(config, else_tok.whitespace_before),
        whitespace_before_colon=parse_simple_whitespace(
            config, colon_tok.whitespace_before
        ),
        body=suite,
    )


@with_production("while_stmt", "'while' test ':' suite ['else' ':' suite]")
def convert_while_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    while_token, test, while_colon_token, while_suite, *else_block = children

    if len(else_block) > 0:
        (else_token, else_colon_token, else_suite) = else_block
        orelse = cst.Else(
            leading_lines=parse_empty_lines(config, else_token.whitespace_before),
            whitespace_before_colon=parse_simple_whitespace(
                config, else_colon_token.whitespace_before
            ),
            body=else_suite,
        )
    else:
        orelse = None

    return cst.While(
        leading_lines=parse_empty_lines(config, while_token.whitespace_before),
        whitespace_after_while=parse_simple_whitespace(
            config, while_token.whitespace_after
        ),
        test=test.value,
        whitespace_before_colon=parse_simple_whitespace(
            config, while_colon_token.whitespace_before
        ),
        body=while_suite,
        orelse=orelse,
    )


@with_production(
    "for_stmt", "'for' exprlist 'in' testlist ':' suite ['else' ':' suite]"
)
def convert_for_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (
        for_token,
        expr,
        in_token,
        test,
        for_colon_token,
        for_suite,
        *else_block,
    ) = children

    if len(else_block) > 0:
        (else_token, else_colon_token, else_suite) = else_block
        orelse = cst.Else(
            leading_lines=parse_empty_lines(config, else_token.whitespace_before),
            whitespace_before_colon=parse_simple_whitespace(
                config, else_colon_token.whitespace_before
            ),
            body=else_suite,
        )
    else:
        orelse = None

    return WithLeadingWhitespace(
        cst.For(
            whitespace_after_for=parse_simple_whitespace(
                config, for_token.whitespace_after
            ),
            target=expr.value,
            whitespace_before_in=parse_simple_whitespace(
                config, in_token.whitespace_before
            ),
            whitespace_after_in=parse_simple_whitespace(
                config, in_token.whitespace_after
            ),
            iter=test.value,
            whitespace_before_colon=parse_simple_whitespace(
                config, for_colon_token.whitespace_before
            ),
            body=for_suite,
            orelse=orelse,
        ),
        for_token.whitespace_before,
    )


@with_production(
    "try_stmt",
    "('try' ':' suite ((except_clause ':' suite)+ ['else' ':' suite] ['finally' ':' suite] | 'finally' ':' suite))",
)
def convert_try_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    trytoken, try_colon_token, try_suite, *rest = children
    handlers: List[cst.ExceptHandler] = []
    orelse: Optional[cst.Else] = None
    finalbody: Optional[cst.Finally] = None

    for clause, colon_token, suite in grouper(rest, 3):
        if isinstance(clause, Token):
            if clause.string == "else":
                if orelse is not None:
                    raise Exception("Logic error!")
                orelse = cst.Else(
                    leading_lines=parse_empty_lines(config, clause.whitespace_before),
                    whitespace_before_colon=parse_simple_whitespace(
                        config, colon_token.whitespace_before
                    ),
                    body=suite,
                )
            elif clause.string == "finally":
                if finalbody is not None:
                    raise Exception("Logic error!")
                finalbody = cst.Finally(
                    leading_lines=parse_empty_lines(config, clause.whitespace_before),
                    whitespace_before_colon=parse_simple_whitespace(
                        config, colon_token.whitespace_before
                    ),
                    body=suite,
                )
            else:
                raise Exception("Logic error!")
        elif isinstance(clause, ExceptClausePartial):
            handlers.append(
                cst.ExceptHandler(
                    body=suite,
                    type=clause.type,
                    name=clause.name,
                    leading_lines=clause.leading_lines,
                    whitespace_after_except=clause.whitespace_after_except,
                    whitespace_before_colon=parse_simple_whitespace(
                        config, colon_token.whitespace_before
                    ),
                )
            )
        else:
            raise Exception("Logic error!")

    return cst.Try(
        leading_lines=parse_empty_lines(config, trytoken.whitespace_before),
        whitespace_before_colon=parse_simple_whitespace(
            config, try_colon_token.whitespace_before
        ),
        body=try_suite,
        handlers=tuple(handlers),
        orelse=orelse,
        finalbody=finalbody,
    )


@with_production("except_clause", "'except' [test ['as' NAME]]")
def convert_except_clause(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (except_token,) = children
        whitespace_after_except = cst.SimpleWhitespace("")
        test = None
        name = None
    elif len(children) == 2:
        (except_token, test_node) = children
        whitespace_after_except = parse_simple_whitespace(
            config, except_token.whitespace_after
        )
        test = test_node.value
        name = None
    else:
        (except_token, test_node, as_token, name_token) = children
        whitespace_after_except = parse_simple_whitespace(
            config, except_token.whitespace_after
        )
        test = test_node.value
        name = cst.AsName(
            whitespace_before_as=parse_simple_whitespace(
                config, as_token.whitespace_before
            ),
            whitespace_after_as=parse_simple_whitespace(
                config, as_token.whitespace_after
            ),
            name=cst.Name(name_token.string),
        )

    return ExceptClausePartial(
        leading_lines=parse_empty_lines(config, except_token.whitespace_before),
        whitespace_after_except=whitespace_after_except,
        type=test,
        name=name,
    )


@with_production("with_stmt", "'with' with_item (',' with_item)*  ':' suite")
def convert_with_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    (with_token, *items, colon_token, suite) = children
    item_nodes: List[cst.WithItem] = []

    for with_item, maybe_comma in grouper(items, 2):
        if maybe_comma is not None:
            item_nodes.append(
                with_item.with_changes(
                    comma=cst.Comma(
                        whitespace_before=parse_parenthesizable_whitespace(
                            config, maybe_comma.whitespace_before
                        ),
                        whitespace_after=parse_parenthesizable_whitespace(
                            config, maybe_comma.whitespace_after
                        ),
                    )
                )
            )
        else:
            item_nodes.append(with_item)

    return WithLeadingWhitespace(
        cst.With(
            whitespace_after_with=parse_simple_whitespace(
                config, with_token.whitespace_after
            ),
            items=tuple(item_nodes),
            whitespace_before_colon=parse_simple_whitespace(
                config, colon_token.whitespace_before
            ),
            body=suite,
        ),
        with_token.whitespace_before,
    )


@with_production("with_item", "test ['as' expr]")
def convert_with_item(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 3:
        (test, as_token, expr_node) = children
        test_node = test.value
        asname = cst.AsName(
            whitespace_before_as=parse_simple_whitespace(
                config, as_token.whitespace_before
            ),
            whitespace_after_as=parse_simple_whitespace(
                config, as_token.whitespace_after
            ),
            name=expr_node.value,
        )
    else:
        (test,) = children
        test_node = test.value
        asname = None

    return cst.WithItem(item=test_node, asname=asname)


def _extract_async(
    config: ParserConfig, children: Sequence[Any]
) -> Tuple[List[cst.EmptyLine], Optional[cst.Asynchronous], Any]:
    if len(children) == 1:
        (stmt,) = children

        whitespace_before = stmt.whitespace_before
        asyncnode = None
    else:
        asynctoken, stmt = children

        whitespace_before = asynctoken.whitespace_before
        asyncnode = cst.Asynchronous(
            whitespace_after=parse_simple_whitespace(
                config, asynctoken.whitespace_after
            )
        )

    return (parse_empty_lines(config, whitespace_before), asyncnode, stmt.value)


@with_production("asyncable_funcdef", "['async'] funcdef")
def convert_asyncable_funcdef(config: ParserConfig, children: Sequence[Any]) -> Any:
    leading_lines, asyncnode, funcdef = _extract_async(config, children)

    return funcdef.with_changes(
        asynchronous=asyncnode, leading_lines=leading_lines, lines_after_decorators=()
    )


@with_production("funcdef", "'def' NAME parameters [funcdef_annotation] ':' suite")
def convert_funcdef(config: ParserConfig, children: Sequence[Any]) -> Any:
    defnode, namenode, param_partial, *annotation, colon, suite = children

    # If the trailing paremeter doesn't have a comma, then it owns the trailing
    # whitespace before the rpar. Otherwise, the comma owns it (and will have
    # already parsed it). We don't check/update ParamStar because if it exists
    # then we are guaranteed have at least one kwonly_param.
    parameters = param_partial.params
    if parameters.star_kwarg is not None:
        if parameters.star_kwarg.comma == MaybeSentinel.DEFAULT:
            parameters = parameters.with_changes(
                star_kwarg=parameters.star_kwarg.with_changes(
                    whitespace_after_param=param_partial.rpar.whitespace_before
                )
            )
    elif parameters.kwonly_params:
        if parameters.kwonly_params[-1].comma == MaybeSentinel.DEFAULT:
            parameters = parameters.with_changes(
                kwonly_params=(
                    *parameters.kwonly_params[:-1],
                    parameters.kwonly_params[-1].with_changes(
                        whitespace_after_param=param_partial.rpar.whitespace_before
                    ),
                )
            )
    elif isinstance(parameters.star_arg, cst.Param):
        if parameters.star_arg.comma == MaybeSentinel.DEFAULT:
            parameters = parameters.with_changes(
                star_arg=parameters.star_arg.with_changes(
                    whitespace_after_param=param_partial.rpar.whitespace_before
                )
            )
    elif parameters.default_params:
        if parameters.default_params[-1].comma == MaybeSentinel.DEFAULT:
            parameters = parameters.with_changes(
                default_params=(
                    *parameters.default_params[:-1],
                    parameters.default_params[-1].with_changes(
                        whitespace_after_param=param_partial.rpar.whitespace_before
                    ),
                )
            )
    elif parameters.params:
        if parameters.params[-1].comma == MaybeSentinel.DEFAULT:
            parameters = parameters.with_changes(
                params=(
                    *parameters.params[:-1],
                    parameters.params[-1].with_changes(
                        whitespace_after_param=param_partial.rpar.whitespace_before
                    ),
                )
            )

    return WithLeadingWhitespace(
        cst.FunctionDef(
            whitespace_after_def=parse_simple_whitespace(
                config, defnode.whitespace_after
            ),
            name=cst.Name(namenode.string),
            whitespace_after_name=parse_simple_whitespace(
                config, namenode.whitespace_after
            ),
            whitespace_before_params=param_partial.lpar.whitespace_after,
            params=parameters,
            returns=None if not annotation else annotation[0],
            whitespace_before_colon=parse_simple_whitespace(
                config, colon.whitespace_before
            ),
            body=suite,
        ),
        defnode.whitespace_before,
    )


@with_production("parameters", "'(' [typedargslist] ')'")
def convert_parameters(config: ParserConfig, children: Sequence[Any]) -> Any:
    lpar, *paramlist, rpar = children
    return FuncdefPartial(
        lpar=cst.LeftParen(
            whitespace_after=parse_parenthesizable_whitespace(
                config, lpar.whitespace_after
            )
        ),
        params=cst.Parameters() if not paramlist else paramlist[0],
        rpar=cst.RightParen(
            whitespace_before=parse_parenthesizable_whitespace(
                config, rpar.whitespace_before
            )
        ),
    )


@with_production("funcdef_annotation", "'->' test")
def convert_funcdef_annotation(config: ParserConfig, children: Sequence[Any]) -> Any:
    arrow, typehint = children
    return cst.Annotation(
        whitespace_before_indicator=parse_parenthesizable_whitespace(
            config, arrow.whitespace_before
        ),
        indicator="->",
        whitespace_after_indicator=parse_parenthesizable_whitespace(
            config, arrow.whitespace_after
        ),
        annotation=typehint.value,
    )


@with_production("classdef", "'class' NAME ['(' [arglist] ')'] ':' suite")
def convert_classdef(config: ParserConfig, children: Sequence[Any]) -> Any:
    classdef, name, *arglist, colon, suite = children

    # First, parse out the comments and empty lines before the statement.
    leading_lines = parse_empty_lines(config, classdef.whitespace_before)

    # Compute common whitespace and nodes
    whitespace_after_class = parse_simple_whitespace(config, classdef.whitespace_after)
    namenode = cst.Name(name.string)
    whitespace_after_name = parse_simple_whitespace(config, name.whitespace_after)

    # Now, construct the classdef node itself
    if not arglist:
        # No arglist, so no arguments to this class
        return cst.ClassDef(
            leading_lines=leading_lines,
            lines_after_decorators=(),
            whitespace_after_class=whitespace_after_class,
            name=namenode,
            whitespace_after_name=whitespace_after_name,
            body=suite,
        )
    else:
        # Unwrap arglist partial, because its valid to not have any
        lpar, *args, rpar = arglist
        args = args[0].args if args else []

        bases: List[cst.Arg] = []
        keywords: List[cst.Arg] = []

        current_arg = bases
        for arg in args:
            if arg.star == "**" or arg.keyword is not None:
                current_arg = keywords
            # Some quick validation
            if current_arg is keywords and (
                arg.star == "*" or (arg.star == "" and arg.keyword is None)
            ):
                # TODO: Need a real syntax error here
                raise Exception("Syntax error!")
            current_arg.append(arg)

        return cst.ClassDef(
            leading_lines=leading_lines,
            lines_after_decorators=(),
            whitespace_after_class=whitespace_after_class,
            name=namenode,
            whitespace_after_name=whitespace_after_name,
            lpar=cst.LeftParen(
                whitespace_after=parse_parenthesizable_whitespace(
                    config, lpar.whitespace_after
                )
            ),
            bases=bases,
            keywords=keywords,
            rpar=cst.RightParen(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, rpar.whitespace_before
                )
            ),
            whitespace_before_colon=parse_simple_whitespace(
                config, colon.whitespace_before
            ),
            body=suite,
        )


@with_production("decorator", "'@' dotted_name [ '(' [arglist] ')' ] NEWLINE")
def convert_decorator(config: ParserConfig, children: Sequence[Any]) -> Any:
    atsign, name, *arglist, newline = children
    if not arglist:
        # This is either a name or an attribute node, so just extract it.
        decoratornode = name
    else:
        # This needs to be converted into a call node, and we have the
        # arglist partial.
        lpar, *args, rpar = arglist
        args = args[0].args if args else []

        # If the trailing argument doesn't have a comma, then it owns the
        # trailing whitespace before the rpar. Otherwise, the comma owns
        # it.
        if len(args) > 0 and args[-1].comma == MaybeSentinel.DEFAULT:
            args[-1] = args[-1].with_changes(
                whitespace_after_arg=parse_parenthesizable_whitespace(
                    config, rpar.whitespace_before
                )
            )

        decoratornode = cst.Call(
            func=name,
            whitespace_after_func=parse_simple_whitespace(
                config, lpar.whitespace_before
            ),
            whitespace_before_args=parse_parenthesizable_whitespace(
                config, lpar.whitespace_after
            ),
            args=tuple(args),
        )

    return cst.Decorator(
        leading_lines=parse_empty_lines(config, atsign.whitespace_before),
        whitespace_after_at=parse_simple_whitespace(config, atsign.whitespace_after),
        decorator=decoratornode,
        trailing_whitespace=newline,
    )


@with_production("decorators", "decorator+")
def convert_decorators(config: ParserConfig, children: Sequence[Any]) -> Any:
    return DecoratorPartial(decorators=children)


@with_production("decorated", "decorators (classdef | asyncable_funcdef)")
def convert_decorated(config: ParserConfig, children: Sequence[Any]) -> Any:
    partial, class_or_func = children

    # First, split up the spacing on the first decorator
    leading_lines = partial.decorators[0].leading_lines

    # Now, redistribute ownership of the whitespace
    decorators = (
        partial.decorators[0].with_changes(leading_lines=()),
        *partial.decorators[1:],
    )

    # Now, modify the original function or class to add the decorators.
    return class_or_func.with_changes(
        leading_lines=leading_lines,
        lines_after_decorators=(
            *class_or_func.leading_lines,
            *class_or_func.lines_after_decorators,
        ),
        decorators=decorators,
    )


@with_production("asyncable_stmt", "['async'] (funcdef | with_stmt | for_stmt)")
def convert_asyncable_stmt(config: ParserConfig, children: Sequence[Any]) -> Any:
    leading_lines, asyncnode, stmtnode = _extract_async(config, children)
    if isinstance(stmtnode, cst.FunctionDef):
        return stmtnode.with_changes(
            asynchronous=asyncnode,
            leading_lines=leading_lines,
            lines_after_decorators=(),
        )
    elif isinstance(stmtnode, cst.With):
        return stmtnode.with_changes(
            asynchronous=asyncnode, leading_lines=leading_lines
        )
    elif isinstance(stmtnode, cst.For):
        return stmtnode.with_changes(
            asynchronous=asyncnode, leading_lines=leading_lines
        )
    else:
        raise Exception("Logic error!")


@with_production("suite", "simple_stmt_suite | indented_suite")
def convert_suite(config: ParserConfig, children: Sequence[Any]) -> Any:
    (suite,) = children
    return suite


@with_production("indented_suite", "NEWLINE INDENT stmt+ DEDENT")
def convert_indented_suite(config: ParserConfig, children: Sequence[Any]) -> Any:
    newline, indent, *stmts, dedent = children
    return cst.IndentedBlock(
        header=newline,
        indent=(
            None
            if indent.relative_indent == config.default_indent
            else indent.relative_indent
        ),
        body=stmts,
        # We want to be able to only keep comments in the footer that are actually for
        # this IndentedBlock. We do so by assuming that lines which are indented to the
        # same level as the block itself are comments that go at the footer of the
        # block. Comments that are indented to less than this indent are assumed to
        # belong to the next line of code. We override the indent here because the
        # dedent node's absolute indent is the resulting indentation after the dedent
        # is performed. Its this way because the whitespace state for both the dedent's
        # whitespace_after and the next BaseCompoundStatement's whitespace_before is
        # shared. This allows us to partially parse here and parse the rest of the
        # whitespace and comments on the next line, effectively making sure that
        # comments are attached to the correct node.
        footer=parse_empty_lines(
            config,
            dedent.whitespace_after,
            override_absolute_indent=indent.whitespace_before.absolute_indent,
        ),
    )
