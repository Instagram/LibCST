// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use libcst_nodes::*;
use libcst_tokenize::whitespace_parser::{
    parse_empty_lines, parse_parenthesizable_whitespace, parse_simple_whitespace,
    parse_trailing_whitespace, Config, WhitespaceError,
};
use libcst_tokenize::{TokError, TokType, Token};
use peg::str::LineCol;
use peg::{parser, Parse, ParseElem, ParseLiteral, RuleResult};
use std::mem::swap;
use thiserror::Error;
use TokType::{Async, Dedent, EndMarker, Indent, Name as NameTok, Newline as NL, Number, String};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParserError<'a> {
    #[error("tokenizer error")]
    TokenizerError(TokError<'a>),
    #[error(transparent)]
    ParserError(#[from] peg::error::ParseError<<TokVec<'a> as Parse>::PositionRepr>),
    #[error(transparent)]
    WhitespaceError(#[from] WhitespaceError),
    #[error("invalid operator")]
    OperatorError,
}

pub type Result<'a, T> = std::result::Result<T, ParserError<'a>>;

#[derive(Debug)]
pub struct TokVec<'a>(Vec<Token<'a>>);

impl<'a> From<Vec<Token<'a>>> for TokVec<'a> {
    fn from(vec: Vec<Token<'a>>) -> Self {
        TokVec(vec)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseLoc {
    pub start_pos: LineCol,
    pub end_pos: LineCol,
}

impl std::fmt::Display for ParseLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.start_pos.fmt(f)
    }
}

impl<'a> Parse for TokVec<'a> {
    type PositionRepr = ParseLoc;

    fn start(&self) -> usize {
        0
    }

    fn is_eof(&self, pos: usize) -> bool {
        pos >= self.0.len()
    }

    fn position_repr(&self, pos: usize) -> Self::PositionRepr {
        let tok = self.0.get(pos).unwrap_or_else(|| self.0.last().unwrap());
        ParseLoc {
            start_pos: LineCol {
                line: tok.start_pos.line_number(),
                column: tok.start_pos.char_column_number(),
                offset: tok.start_pos.byte_idx(),
            },
            end_pos: LineCol {
                line: tok.end_pos.line_number(),
                column: tok.end_pos.char_column_number(),
                offset: tok.end_pos.byte_idx(),
            },
        }
    }
}

impl<'a> ParseElem for TokVec<'a> {
    type Element = Token<'a>;

    fn parse_elem(&self, pos: usize) -> RuleResult<Self::Element> {
        match self.0.get(pos) {
            Some(tok) => RuleResult::Matched(pos + 1, tok.clone()),
            None => RuleResult::Failed,
        }
    }
}

impl<'a> ParseLiteral for TokVec<'a> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> RuleResult<()> {
        match self.parse_elem(pos) {
            RuleResult::Matched(p, Token { string: lit, .. }) if lit == literal => {
                RuleResult::Matched(p, ())
            }
            _ => RuleResult::Failed,
        }
    }
}

parser! {
    pub grammar python<'a>(config: &Config<'a>) for TokVec<'a> {
        pub rule file() -> Module<'a>
            = traced(<_file()>)

        rule _file() -> Module<'a>
            = s:statements() eof:tok(EndMarker, "EOF") {?
                make_module(config, s, eof)
                    .map_err(|e| "module")
            }

        pub rule statements() -> Vec<Statement<'a>>
            = statement()+

        pub rule statement() -> Statement<'a>
            = c:compound_stmt() { Statement::Compound(c) }
            / s:simple_stmt() {?
                    Ok(Statement::Simple(make_simple_statement_line(config, s)
                        .map_err(|e| "simple_stmt")?))
            }

        rule simple_stmt() -> SimpleStatementParts<'a>
            = first:&_ statements:(s:small_stmt() semi:lit(";") { (s, semi) })*
            last_statement:small_stmt() last_semi:lit(";")? nl:tok(NL, "NEWLINE") {
                SimpleStatementParts {first, statements, last_statement, last_semi, nl}
            }

        rule compound_stmt() -> CompoundStatement<'a>
            = &("def" / "@" / tok(Async, "ASYNC")) f:function_def() {
                CompoundStatement::FunctionDef(f)
            }
            / &"if" f:if_stmt() { CompoundStatement::If(f) }

        rule small_stmt() -> SmallStatement<'a>
            = a:assignment() { SmallStatement::Assign(a) }
            / e:star_expressions() { SmallStatement::Expr { value: e, semicolon: None } }
            // this is expanded from the original grammar's import_stmt rule
            / &"import" i:import_name() { SmallStatement::Import(i) }
            / &"from" i:import_from() { SmallStatement::ImportFrom(i) }
            / "pass" { SmallStatement::Pass { semicolon: None } }

        rule assignment() -> Assign<'a>
            = lhs:(t:star_targets() eq:lit("=") {(t, eq)})+ rhs:(yield_expr() / star_expressions()) !lit("=") {?
                make_assignment(config, lhs, rhs)
                    .map_err(|e| "assignment")
            }

        rule block() -> Suite<'a>
            = n:tok(NL, "NEWLINE") ind:tok(Indent, "INDENT") s:statements() ded:tok(Dedent, "DEDENT") {?
                make_indented_block(config, n, ind, s, ded)
                    .map_err(|e| "indented block")
            }
            / s:simple_stmt() {?
                make_simple_statement_suite(config, s)
                    .map_err(|e| "simple_stmt suite")
            }

        rule star_expressions() -> Expression<'a>
            = first:star_expression()
                rest:(comma:comma() e:star_expression() { (comma, expr_to_element(e)) })+
                comma:comma()? {?
                    make_tuple(config, expr_to_element(first), rest, comma, None, None)
                        .map(Expression::Tuple)
                        .map_err(|e| "star_expressions")
            }
            / e:star_expression() comma:comma() {?
                make_tuple(config, expr_to_element(e), vec![], Some(comma), None, None)
                    .map(Expression::Tuple)
                    .map_err(|e| "star_expressions")
            }
            / star_expression()

        #[cache]
        rule star_expression() -> Expression<'a>
            = star:lit("*") e:bitwise_or() {?
                make_starred_element(config, star, expr_to_element(e))
                    .map(Expression::StarredElement)
                    .map_err(|_| "star_expression")
            }
            / expression()

        rule star_named_expressions() -> Vec<Element<'a>>
            = first:star_named_expression()
                rest:(c:comma() e:star_named_expression() { (c, e) })*
                trail:comma()? {
                    make_star_named_expressions(first, rest, trail)
            }

        rule star_named_expression() -> Element<'a>
            = star:lit("*") e:bitwise_or() {?
                make_starred_element(config, star, expr_to_element(e))
                    .map(Element::Starred)
                    .map_err(|e| "star_named_expression")
            }
            / e:named_expression() { expr_to_element(e) }

        #[cache]
        rule expression() -> Expression<'a>
            = disjunction()

        #[cache]
        rule disjunction() -> Expression<'a>
            = a:conjunction() b:(or:lit("or") inner:conjunction() { (or, inner) })+ {?
                make_boolean_op(config, a, b).map_err(|e| "expected disjunction")
            }
            / conjunction()

        #[cache]
        rule conjunction() -> Expression<'a>
            = a:inversion() b:(and:lit("and") inner:inversion() { (and, inner) })+ {?
                make_boolean_op(config, a, b).map_err(|e| "expected conjunction")
            }
            / inversion()

        #[cache]
        rule inversion() -> Expression<'a>
            = not:lit("not") a:inversion() {?
                make_unary_op(config, not, a).map_err(|e| "expected inversion")
            }
            / comparison()

        #[cache]
        rule comparison() -> Expression<'a>
            = a:bitwise_or() b:compare_op_bitwise_or_pair()+ { make_comparison(a, b) }
            / bitwise_or()

        #[cache]
        rule compare_op_bitwise_or_pair() -> (CompOp<'a>, Expression<'a>)
            = _op_bitwise_or("==")
            / _op_bitwise_or("!=") // TODO: support barry_as_flufl
            / _op_bitwise_or("<=")
            / _op_bitwise_or("<")
            / _op_bitwise_or(">=")
            / _op_bitwise_or(">")
            / _op_bitwise_or2("not", "in")
            / _op_bitwise_or("in")
            / _op_bitwise_or2("is", "not")
            / _op_bitwise_or("is")

        rule _op_bitwise_or(o: &'static str) -> (CompOp<'a>, Expression<'a>)
            = op:lit(o) e:bitwise_or() {?
                make_comparison_operator(config, op)
                    .map(|op| (op, e))
                    .map_err(|_| "comparison")
            }

        rule _op_bitwise_or2(first: &'static str, second: &'static str) -> (CompOp<'a>, Expression<'a>)
            = f:lit(first) s:lit(second) e:bitwise_or() {?
                make_comparison_operator_2(config, f, s)
                    .map(|op| (op, e))
                    .map_err(|_| "comparison")
            }

        rule star_targets() -> AssignTargetExpression<'a>
            = a:star_target() !lit(",") {a}
            / first:(t:star_target() {assign_target_to_element(t)})
                rest:(comma:comma() t:star_target() {(comma, assign_target_to_element(t))})*
                comma:comma()? {?
                    make_tuple(config, first, rest, comma, None, None)
                        .map(AssignTargetExpression::Tuple)
                        .map_err(|e| "star_targets")
            }

        rule star_target() -> AssignTargetExpression<'a>
            = star:lit("*") !lit("*") t:star_target() {?
                make_starred_element(config, star, assign_target_to_element(t))
                    .map(AssignTargetExpression::StarredElement)
                    .map_err(|e| "star_target")
            }
            / target_with_star_atom()

        // This differs from star_targets above because it requires at least two items
        // in the tuple
        rule star_targets_tuple_seq() -> Tuple<'a>
            = first:(t:star_target() {assign_target_to_element(t)})
                rest:(c:comma() t:star_target() {(c, assign_target_to_element(t))})+
                trail:comma()? {?
                    make_tuple(config, first, rest, trail, None, None)
                        .map_err(|_| "star_target_tuple_seq")
            }
            / t:star_target() trail:comma()? {?
                make_tuple(config, assign_target_to_element(t), vec![], trail, None, None)
                    .map_err(|_| "star_target_tuple_seq")
            }

        rule target_with_star_atom() -> AssignTargetExpression<'a>
            = a:t_primary() dot:lit(".") n:name() !t_lookahead() {?
                make_attribute(config, a, dot, n)
                    .map(AssignTargetExpression::Attribute)
                    .map_err(|e| "target_with_star_atom")
            }
            / a:t_primary() lbrak:lit("[") s:slices() rbrak:lit("]") {?
                make_subscript(config, a, lbrak, s, rbrak)
                    .map(AssignTargetExpression::Subscript)
                    .map_err(|_| "target_with_star_atom")
            }
            / a:star_atom() {a}

        rule star_atom() -> AssignTargetExpression<'a>
            = a:name() { AssignTargetExpression::Name(a) }
            / lpar:lpar() a:target_with_star_atom() rpar:rpar() { a.with_parens(lpar, rpar) }
            / lpar:lpar() a:star_targets_tuple_seq()? rpar:rpar() {
               AssignTargetExpression::Tuple(
                   a.unwrap_or_default().with_parens(lpar, rpar)
               )
            }
            // TODO: List

        rule lpar() -> LeftParen<'a>
            = a:lit("(") {? make_lpar(config, a).map_err(|_| "lpar")}

        rule rpar() -> RightParen<'a>
            = a:lit(")") {? make_rpar(config, a).map_err(|_| "rpar")}

        #[cache_left_rec]
        rule t_primary() -> Expression<'a>
            = value:t_primary() dot:lit(".") attr:name() &t_lookahead() {?
                make_attribute(config, value, dot, attr)
                    .map(Expression::Attribute)
                    .map_err(|e| "t_primary")
            }
            / v:t_primary() lpar:lit("[") s:slices() rpar:lit("]") !t_lookahead() {?
                make_subscript(config, v, lpar, s, rpar)
                    .map(Expression::Subscript)
                    .map_err(|_| "list")
            }
            / f:t_primary() lpar:&lit("(") gen:genexp() &t_lookahead() {
                Expression::Call(make_genexp_call(config, f, lpar, gen))
            }
            / f:t_primary() lpar:lit("(") arg:arguments()? rpar:lit(")") &t_lookahead() {?
                make_call(config, f, lpar, arg.unwrap_or_default(), rpar)
                    .map(Expression::Call)
                    .map_err(|_| "call")
            }
            / a:atom() &t_lookahead() {a}

        rule t_lookahead() -> ()
            = "(" / "[" / "."

        rule yield_expr() -> Expression<'a>
            = lit("yield") lit("from") a:expression() { panic!("yield from not implemented") }
            / lit("yield") a:star_expressions()? { panic!("yield not implemented") }

        #[cache_left_rec]
        rule bitwise_or() -> Expression<'a>
            = a:bitwise_or() op:lit("|") b:bitwise_xor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected bitwise_or")
            }
            / bitwise_xor()

        #[cache_left_rec]
        rule bitwise_xor() -> Expression<'a>
            = a:bitwise_xor() op:lit("^") b:bitwise_and() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected bitwise_xor")
            }
            / bitwise_and()

        #[cache_left_rec]
        rule bitwise_and() -> Expression<'a>
            = a:bitwise_and() op:lit("&") b:shift_expr() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected bitwise_and")
            }
            / shift_expr()

        #[cache_left_rec]
        rule shift_expr() -> Expression<'a>
            = a:shift_expr() op:lit("<<") b:sum() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected shift_expr")
            }
            / a:shift_expr() op:lit(">>") b:sum() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected shift_expr")
            }
            / sum()

        #[cache_left_rec]
        rule sum() -> Expression<'a>
            = a:sum() op:lit("+") b:term() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected sum")
            }
            / a:sum() op:lit("-") b:term() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected sum")
            }
            / term()

        #[cache_left_rec]
        rule term() -> Expression<'a>
            = a:term() op:lit("*") b:factor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("/") b:factor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("//") b:factor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("%") b:factor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("@") b:factor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected term")
            }
            / factor()

        #[cache]
        rule factor() -> Expression<'a>
            = op:lit("+") a:factor() {?
                make_unary_op(config, op, a).map_err(|e| "expected factor")
            }
            / op:lit("-") a:factor() {?
                make_unary_op(config, op, a).map_err(|e| "expected factor")
            }
            / op:lit("~") a:factor() {?
                make_unary_op(config, op, a).map_err(|e| "expected factor")
            }
            / power()

        rule power() -> Expression<'a>
            = a:await_primary() op:lit("**") b:factor() {?
                make_binary_op(config, a, op, b).map_err(|e| "expected power")
            }
            / await_primary()

        rule await_primary() -> Expression<'a>
            // TODO: await expressions
            = primary()

        #[cache_left_rec]
        rule primary() -> Expression<'a>
            = v:primary() dot:lit(".") attr:name() {?
                make_attribute(config, v, dot, attr)
                    .map(Expression::Attribute)
                    .map_err(|_| "attribute")
            }
            / a:primary() lpar:&lit("(") b:genexp() {
                Expression::Call(make_genexp_call(config, a, lpar, b))
            }
            / f:primary() lpar:lit("(") arg:arguments()? rpar:lit(")") {?
                make_call(config, f, lpar, arg.unwrap_or_default(), rpar)
                    .map(Expression::Call)
                    .map_err(|_| "call")
            }
            / v:primary() lbrak:lit("[") s:slices() rbrak:lit("]") {?
                make_subscript(config, v, lbrak, s, rbrak)
                    .map(Expression::Subscript)
                    .map_err(|_| "subscript")
            }
            / atom()

        rule list() -> Expression<'a>
            = lbrak:lit("[") e:star_named_expressions()? rbrak:lit("]") {?
                make_list(config, lbrak, e.unwrap_or_default(), rbrak)
                    .map(Expression::List)
                    .map_err(|_| "list")
            }

        rule slices() -> Vec<SubscriptElement<'a>>
            = s:slice() !"," { vec![SubscriptElement { slice: s, comma: None }] }
            / first:slice() rest:(c:comma() s:slice() {(c, s)})* trail:comma()? {
                make_slices(first, rest, trail)
            }

        rule slice() -> BaseSlice<'a>
            = l:expression()? col:lit(":") u:expression()?
                rest:(c:lit(":") s:expression()? {(c, s)})? {?
                    make_slice(config, l, col, u, rest)
                        .map_err(|_| "slice")
            }
            / v:expression() { make_index(v) }

        rule listcomp() -> Expression<'a>
            = lbrak:lit("[") elt:named_expression() comp:for_if_clauses() rbrak:lit("]") {?
                make_list_comp(config, lbrak, elt, comp, rbrak)
                    .map(Expression::ListComp)
                    .map_err(|_| "listcomp")
            }

        rule set() -> Expression<'a>
            = lbrace:lit("{") e:star_named_expressions()? rbrace:lit("}") {?
                make_set(config, lbrace, e.unwrap_or_default(), rbrace)
                    .map(Expression::Set)
                    .map_err(|_| "set")
            }

        rule setcomp() -> Expression<'a>
            = lbrace:lit("{") elt:named_expression() comp:for_if_clauses() rbrace:lit("}") {?
                make_set_comp(config, lbrace, elt, comp, rbrace)
                    .map(Expression::SetComp)
                    .map_err(|_| "setcomp")
            }

        rule dict() -> Expression<'a>
            = lbrace:lit("{") els:double_starred_keypairs()? rbrace:lit("}") {?
                make_dict(config, lbrace, els.unwrap_or_default(), rbrace)
                    .map(Expression::Dict)
                    .map_err(|_| "dict")
            }

        rule dictcomp() -> Expression<'a>
            = lbrace:lit("{") elt:kvpair() comp:for_if_clauses() rbrace:lit("}") {?
                make_dict_comp(config, lbrace, elt, comp, rbrace)
                    .map(Expression::DictComp)
                    .map_err(|_| "dictcomp")
            }

        rule double_starred_keypairs() -> Vec<DictElement<'a>>
            = first:double_starred_kvpair()
                rest:(c:comma() e:double_starred_kvpair() {(c, e)})*
                trail:comma()? {
                    make_double_starred_keypairs(first, rest, trail)
            }

        rule double_starred_kvpair() -> DictElement<'a>
            = s:lit("**") e:bitwise_or() {?
                make_double_starred_element(config, s, e)
                    .map(DictElement::Starred)
                    .map_err(|_| "double_starred_element")
            }
            / k:kvpair() {? make_dict_element(config, k).map_err(|_| "dict_element")}

        rule kvpair() -> (Expression<'a>, Token<'a>, Expression<'a>)
            = k:expression() colon:lit(":") v:expression() { (k, colon, v) }

        rule genexp() -> GeneratorExp<'a>
            = lpar:lit("(") elt:named_expression() comp:for_if_clauses() rpar:lit(")") {?
                make_genexp(config, lpar, elt, comp, rpar).map_err(|_| "genexp")
            }

        rule for_if_clauses() -> CompFor<'a>
            = c:for_if_clause()+ { merge_comp_fors(c) }

        rule for_if_clause() -> CompFor<'a>
            = asy:_async() f:lit("for") tgt:star_targets() i:lit("in")
                iter:disjunction() ifs:_comp_if()* {?
                    make_for_if(config, Some(asy), f, tgt, i, iter, ifs)
                        .map_err(|_| "for_in")
            }
            / f:lit("for") tgt:star_targets() i:lit("in")
            iter:disjunction() ifs:_comp_if()* {?
                make_for_if(config, None, f, tgt, i, iter, ifs)
                    .map_err(|_| "for_in")
            }

        rule _comp_if() -> CompIf<'a>
            = kw:lit("if") cond:disjunction() {?
                make_comp_if(config, kw, cond).map_err(|_| "comp_if")
            }

        rule arguments() -> Vec<Arg<'a>>
            = a:args() &")" {a} // trailing comma already included

        rule args() -> Vec<Arg<'a>>
            = pos:(a:_posarg() c:comma() {a.with_comma(c)})+ kw:kwargs()? { concat(pos, kw.unwrap_or_default()) }
            / pos:(a:_posarg() c:comma() {a.with_comma(c)})* last:_posarg() {concat(pos, vec![last])}
            / kwargs()

        rule _posarg() -> Arg<'a>
            = a:(starred_expression() / e:named_expression() { make_arg(e) })
                !"=" { a }

        rule starred_expression() -> Arg<'a>
            = star:lit("*") e:expression() {? make_star_arg(config, star, e).map_err(|_| "star_arg") }

        rule kwargs() -> Vec<Arg<'a>>
            = s:(a:kwarg_or_starred() c:comma() { a.with_comma(c) })+
                d:(a:kwarg_or_double_starred() c:comma() { a.with_comma(c)})*
                last:kwarg_or_double_starred()? { make_kwargs(s, d, last) }
            / s:(a:kwarg_or_starred() c:comma() { a.with_comma(c) })* last:kwarg_or_starred() { make_kwargs(s, vec![], Some(last)) }
            / d:(a:kwarg_or_double_starred() c:comma() { a.with_comma(c) })*
                last:kwarg_or_double_starred()? { make_kwargs(vec![], d, last) }

        rule kwarg_or_starred() -> Arg<'a>
            = _kwarg()
            / starred_expression()

        rule kwarg_or_double_starred() -> Arg<'a>
            = _kwarg()
            / star:lit("**") e:expression() {? make_star_arg(config, star, e).map_err(|_| "star_arg") }

        rule _kwarg() -> Arg<'a>
            = n:name() eq:lit("=") v:expression() {?
                make_kwarg(config, n, eq, v).map_err(|_| "kwarg")
            }

        rule atom() -> Expression<'a>
            = n:name() { Expression::Name(n) }
            / n:lit("True") { Expression::Name(make_name(n)) }
            / n:lit("False") { Expression::Name(make_name(n)) }
            / n:lit("None") { Expression::Name(make_name(n)) }
            / &tok(String, "STRING") s:strings() {s}
            / n:tok(Number, "NUMBER") {? make_number(config, n).map_err(|e| "expected number")}
            / &"(" e:(tuple() / group() / (g:genexp() {Expression::GeneratorExp(g)})) {e}
            / &"[" e:(list() / listcomp()) {e}
            / &"{" e:(dict() / set() / dictcomp() / setcomp()) {e}
            / lit("...") { Expression::Ellipsis {lpar: vec![], rpar: vec![]}}

        rule strings() -> Expression<'a>
            = s:tok(String, "STRING") {
                Expression::SimpleString { value: s.string, lpar: vec![], rpar: vec![]}
            }

        rule tuple() -> Expression<'a>
            = lpar:lit("(") first:star_named_expression() &","
                rest:(c:comma() e:star_named_expression() {(c, e)})*
                trailing_comma:comma()? rpar:lit(")") {?
                    make_tuple(config, first, rest, trailing_comma, Some(lpar), Some(rpar))
                        .map(Expression::Tuple)
                        .map_err(|e| "tuple")
            }
            / lpar:lpar() lit(")") {
                Expression::Tuple(Tuple::default().with_parens(
                    lpar, RightParen { whitespace_before: Default::default() }
                ))}

        rule group() -> Expression<'a>
            = lpar:lit("(") e:(yield_expr() / named_expression()) rpar:lit(")") {?
                add_expr_parens(config, e, lpar, rpar)
                    .map_err(|e| "group")
            }

        rule function_def() -> FunctionDef<'a>
            = d:decorators() f:function_def_raw() {f.with_decorators(d)}
            / function_def_raw()

        rule decorators() -> Vec<Decorator<'a>>
            = (at:lit("@") name:name() tok(NL, "NEWLINE") {? make_decorator(config, at, name).map_err(|e| "expected decorator")} )+

        rule function_def_raw() -> FunctionDef<'a>
            = def:lit("def") n:name() op:lit("(") params:params()? cp:lit(")") c:lit(":") b:block() {?
                make_function_def(config, def, n, op, params, cp, c, b).map_err(|e| "function def" )
            }

        rule params() -> Parameters<'a>
            = parameters()

        rule parameters() -> Parameters<'a>
            = a:slash_no_default() b:param_no_default()* c:param_with_default()*  d:star_etc()?
            {? make_parameters(config, Some(a), concat(b, c), d).map_err(|e| "parameters") }
            / a:slash_with_default() b:param_with_default()* d:star_etc()? {?
                make_parameters(config, Some(a), b, d)
                    .map_err(|e| "parameters")
            }
            / a:param_no_default()+ b:param_with_default()* d:star_etc()? {?
                make_parameters(config, None, concat(a, b), d)
                    .map_err(|e| "parameters")
            }
            / a:param_with_default()+ d:star_etc()? {?
                make_parameters(config, None, a, d)
                    .map_err(|e| "parameters")
            }
            / d:star_etc() {?
                make_parameters(config, None, vec![], Some(d))
                    .map_err(|e| "parameters")
            }

        rule slash_no_default() -> (Vec<Param<'a>>, ParamSlash<'a>)
            = a:param_no_default()+ slash:lit("/") com:lit(",") {?
                make_comma(config, com)
                    .map(|c| (a, ParamSlash { comma: Some(c)}))
                    .map_err(|e| "slash_no_default")
            }
            / a:param_no_default()+ slash:lit("/") &")" {
                (a, ParamSlash { comma: None })
            }

        rule slash_with_default() -> (Vec<Param<'a>>, ParamSlash<'a>)
            = a:param_no_default()* b:param_with_default()+ slash:lit("/") com:lit(",") {?
                make_comma(config, com)
                    .map(|c| (concat(a, b), ParamSlash { comma: Some(c) }))
                    .map_err(|e| "slash_with_default")
            }
            / a:param_no_default()* b:param_with_default()+ slash:lit("/") &")" {
                (concat(a, b), ParamSlash { comma: None })
            }

        rule star_etc() -> StarEtc<'a>
            = star:lit("*") a:param_no_default() b:param_maybe_default()* kw:kwds()? {?
                add_param_star(config, a, star)
                    .map(|p| StarEtc(Some(StarArg::Param(Box::new(p))), b, kw))
                    .map_err(|e| "star_etc")
            }
            / star:lit("*") com:lit(",") b:param_maybe_default()+ kw:kwds()? {?
                make_comma(config, com)
                    .map(|comma| StarEtc(Some(StarArg::Star(ParamStar {comma})), b, kw))
                    .map_err(|e| "star_etc")
            }
            / kw:kwds() { StarEtc(None, vec![], Some(kw)) }

        rule kwds() -> Param<'a>
            = star:lit("**") a:param_no_default() {?
                add_param_star(config, a, star)
                    .map_err(|e| "kwds")
            }

        rule param_no_default() -> Param<'a>
            = a:param() c:lit(",") {? add_param_default(config, a, None, Some(c)).map_err(|e| "param_no_default") }
            / a:param() &")" {a}

        rule param_with_default() -> Param<'a>
            = a:param() def:default() c:lit(",") {?
                add_param_default(config, a, Some(def), Some(c))
                    .map_err(|e| "param_with_default")
            }
            / a:param() def:default() &")" {?
                add_param_default(config, a, Some(def), None)
                    .map_err(|e| "param_with_default")
            }

        rule param_maybe_default() -> Param<'a>
            = a:param() def:default()? c:lit(",") {?
                add_param_default(config, a, def, Some(c))
                    .map_err(|e| "param_maybe_default")
            }
            / a:param() def:default()? &")" {?
                add_param_default(config, a, def, None)
                .map_err(|e| "param_maybe_default")
            }

        rule param() -> Param<'a>
            = n:name() { Param {name: n, ..Default::default() } }

        rule default() -> (AssignEqual<'a>, Expression<'a>)
            = eq:lit("=") ex:expression() {?
                Ok((make_assign_equal(config, eq).map_err(|e| "=")?, ex))
            }

        rule if_stmt() -> If<'a>
            = i:lit("if") a:named_expression() col:lit(":") b:block() elif:elif_stmt() {?
                make_if(config, i, a, col, b, Some(OrElse::Elif(elif)), false)
                    .map_err(|e| "if statement")
            }
            / i:lit("if") a:named_expression() col:lit(":") b:block() el:else_block()? {?
                make_if(config, i, a, col, b, el.map(OrElse::Else), false)
                    .map_err(|e| "if statement")
            }

        rule elif_stmt() -> If<'a>
            = i:lit("elif") a:named_expression() col:lit(":") b:block() elif:elif_stmt() {?
                make_if(config, i, a, col, b, Some(OrElse::Elif(elif)), true)
                    .map_err(|e| "elif statement")
            }
            / i:lit("elif") a:named_expression() col:lit(":") b:block() el:else_block()? {?
                make_if(config, i, a, col, b, el.map(OrElse::Else), true)
                    .map_err(|e| "elif statement")
            }

        rule else_block() -> Else<'a>
            = el:lit("else") col:lit(":") b:block() {?
                make_else(config, el, col, b)
                    .map_err(|e| "else block")
            }

        rule named_expression() -> Expression<'a>
            = a:name() op:lit(":=") b:expression() { todo!() }
            / e:expression() !lit(":=") { e }

        rule import_name() -> Import<'a>
            = kw:lit("import") a:dotted_as_names() {?
                make_import(config, kw, a)
                    .map_err(|e| "import")
            }

        rule import_from() -> ImportFrom<'a>
            = from:lit("from") dots:lit(".")* m:dotted_name()
                import:lit("import") als:import_from_targets() {?
                    make_import_from(config, from, dots, Some(m), import, als)
                        .map_err(|e| "import_from")
            }
            / from:lit("from") dots:lit(".")+
                import:lit("import") als:import_from_targets() {?
                    make_import_from(config, from, dots, None, import, als)
                        .map_err(|e| "import_from")
            }

        rule import_from_targets() -> ParenthesizedImportNames<'a>
            = lpar:lit("(") als:import_from_as_names() c:comma()? rpar:lit(")") {
                let mut als = als;
                if let (comma@Some(_), Some(mut last)) = (c, als.last_mut()) {
                    last.comma = comma;
                }
                (Some(lpar), ImportNames::Aliases(als), Some(rpar))
            }
            / als:import_from_as_names() !lit(",") { (None, ImportNames::Aliases(als), None)}
            / star:lit("*") { (None, ImportNames::Star(ImportStar {}), None) }

        rule import_from_as_names() -> Vec<ImportAlias<'a>>
            = first:import_from_as_name() tail:(c:comma() al:import_from_as_name() {(c, al)})* {
                make_import_from_as_names(first, tail)
            }

        rule import_from_as_name() -> ImportAlias<'a>
            = n:name() asname:(kw:lit("as") z:name() {(kw, z)})? {?
                make_import_alias(config, NameOrAttribute::N(n), asname)
                    .map_err(|e| "import_from_as_name")
            }

        rule dotted_as_names() -> Vec<ImportAlias<'a>>
            = init:(d:dotted_as_name() c:comma() {d.with_comma(c)})*
                last:dotted_as_name() {
                    concat(init, vec![last])
            }

        rule dotted_as_name() -> ImportAlias<'a>
            = n:dotted_name() asname:(kw:lit("as") z:name() {(kw, z)})? {?
                make_import_alias(config, n, asname)
                    .map_err(|e| "dotted_as_name")
            }

        rule dotted_name() -> NameOrAttribute<'a>
            = first:name() tail:(dot:lit(".") n:name() {(dot, n)})* {?
                make_name_or_attr(config, first, tail)
                    .map_err(|e| "dotted_name")
            }

        rule mb_lit(lit: &str) -> Option<Token<'a>>
            = ([t@Token {..}] {? if t.string == lit {
                                    Ok(t)
                                } else { Err("optional semicolon") }
                              })?

        rule comma() -> Comma<'a>
            = c:lit(",") {? make_comma(config, c).map_err(|e| ",") }

        /// matches any token, not just whitespace
        rule _() -> Token<'a>
            = [t] { t }

        rule lit(lit: &'static str) -> Token<'a>
            = [t@Token {..}] {? if t.string == lit { Ok(t) } else { Err(lit) } }

        rule tok(tok: TokType, err: &'static str) -> Token<'a>
            = [t@Token {..}] {? if t.r#type == tok { Ok(t) } else { Err(err) } }

        rule name() -> Name<'a>
            = !("False" / "None" / "True" / "and" / "as" / "assert" / "async" / "await"
                / "break" / "class" / "continue" / "def" / "del" / "elif" / "else"
                / "except" / "finally" / "for" / "from" / "global" / "if" / "import"
                / "in" / "is" / "lambda" / "nonlocal" / "not" / "or" / "pass" / "raise"
                / "return" / "try" / "while" / "with" / "yield"
            )
            t:tok(NameTok, "NAME") {make_name(t)}

        rule _async() -> Token<'a>
            = tok(Async, "ASYNC")

        rule traced<T>(e: rule<T>) -> T =
            &(input:(_)* {
                #[cfg(feature = "trace")]
                {
                    println!("[PEG_INPUT_START]");
                    println!("{}", config.input);
                    println!("[PEG_TRACE_START]");
                }
            })
            e:e()? {?
                #[cfg(feature = "trace")]
                println!("[PEG_TRACE_STOP]");
                e.ok_or("")
            }

    }
}

#[allow(clippy::too_many_arguments)]
fn make_function_def<'a>(
    config: &Config<'a>,
    mut def: Token<'a>,
    name: Name<'a>,
    mut open_paren: Token<'a>,
    params: Option<Parameters<'a>>,
    _close_paren: Token<'a>,
    mut colon: Token<'a>,
    body: Suite<'a>,
) -> Result<'a, FunctionDef<'a>> {
    Ok(FunctionDef {
        name,
        params: params.unwrap_or_default(),
        decorators: Default::default(),
        body,
        leading_lines: parse_empty_lines(config, &mut def.whitespace_before, None)?,
        lines_after_decorators: vec![],
        whitespace_after_def: parse_simple_whitespace(config, &mut def.whitespace_after)?,
        whitespace_after_name: parse_simple_whitespace(config, &mut open_paren.whitespace_before)?,
        whitespace_before_colon: parse_simple_whitespace(config, &mut colon.whitespace_before)?,
        whitespace_before_params: parse_parenthesizable_whitespace(
            config,
            &mut open_paren.whitespace_after,
        )?,
    })
}

fn make_decorator<'a>(
    config: &Config<'a>,
    mut at: Token<'a>,
    name: Name<'a>,
    // mut newline: Token<'a>,
) -> Result<'a, Decorator<'a>> {
    Ok(Decorator {
        decorator: name,
        leading_lines: parse_empty_lines(config, &mut at.whitespace_before, None)?,
        whitespace_after_at: parse_simple_whitespace(config, &mut at.whitespace_after)?,
        trailing_whitespace: Default::default(), //parse_trailing_whitespace(config, &mut newline.whitespace_before)?,
    })
}

fn make_comparison<'a>(
    head: Expression<'a>,
    tail: Vec<(CompOp<'a>, Expression<'a>)>,
) -> Expression<'a> {
    let mut comparisons = vec![];
    for (operator, e) in tail {
        comparisons.push(ComparisonTarget {
            operator,
            comparator: e,
        });
    }
    Expression::Comparison {
        left: Box::new(head),
        comparisons,
        lpar: vec![],
        rpar: vec![],
    }
}

fn make_comparison_operator<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, CompOp<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;

    match tok.string {
        "<" => Ok(CompOp::LessThan {
            whitespace_after,
            whitespace_before,
        }),
        ">" => Ok(CompOp::GreaterThan {
            whitespace_after,
            whitespace_before,
        }),
        "<=" => Ok(CompOp::LessThanEqual {
            whitespace_after,
            whitespace_before,
        }),
        ">=" => Ok(CompOp::GreaterThanEqual {
            whitespace_after,
            whitespace_before,
        }),
        "==" => Ok(CompOp::Equal {
            whitespace_after,
            whitespace_before,
        }),
        "!=" => Ok(CompOp::NotEqual {
            whitespace_after,
            whitespace_before,
        }),
        "in" => Ok(CompOp::In {
            whitespace_after,
            whitespace_before,
        }),
        "is" => Ok(CompOp::Is {
            whitespace_after,
            whitespace_before,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_comparison_operator_2<'a>(
    config: &Config<'a>,
    mut first: Token<'a>,
    mut second: Token<'a>,
) -> Result<'a, CompOp<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut first.whitespace_before)?;
    let whitespace_between = parse_parenthesizable_whitespace(config, &mut first.whitespace_after)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut second.whitespace_after)?;

    match (first.string, second.string) {
        ("is", "not") => Ok(CompOp::IsNot {
            whitespace_before,
            whitespace_between,
            whitespace_after,
        }),
        ("not", "in") => Ok(CompOp::NotIn {
            whitespace_before,
            whitespace_between,
            whitespace_after,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_boolean_op<'a>(
    config: &Config<'a>,
    head: Expression<'a>,
    tail: Vec<(Token<'a>, Expression<'a>)>,
) -> Result<'a, Expression<'a>> {
    if tail.is_empty() {
        return Ok(head);
    }

    let mut expr = head;
    for (tok, right) in tail {
        expr = Expression::BooleanOperation {
            left: Box::new(expr),
            operator: make_boolean_operator(config, tok)?,
            right: Box::new(right),
            lpar: vec![],
            rpar: vec![],
        }
    }
    Ok(expr)
}

fn make_boolean_operator<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, BooleanOp<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
    match tok.string {
        "and" => Ok(BooleanOp::And {
            whitespace_after,
            whitespace_before,
        }),
        "or" => Ok(BooleanOp::Or {
            whitespace_after,
            whitespace_before,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_binary_op<'a>(
    config: &Config<'a>,
    left: Expression<'a>,
    op: Token<'a>,
    right: Expression<'a>,
) -> Result<'a, Expression<'a>> {
    let operator = make_binary_operator(config, op)?;
    Ok(Expression::BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
        lpar: vec![],
        rpar: vec![],
    })
}

fn make_binary_operator<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, BinaryOp<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;

    match tok.string {
        "+" => Ok(BinaryOp::Add {
            whitespace_after,
            whitespace_before,
        }),
        "-" => Ok(BinaryOp::Subtract {
            whitespace_after,
            whitespace_before,
        }),
        "*" => Ok(BinaryOp::Multiply {
            whitespace_after,
            whitespace_before,
        }),
        "/" => Ok(BinaryOp::Divide {
            whitespace_after,
            whitespace_before,
        }),
        "//" => Ok(BinaryOp::FloorDivide {
            whitespace_after,
            whitespace_before,
        }),
        "%" => Ok(BinaryOp::Modulo {
            whitespace_after,
            whitespace_before,
        }),
        "**" => Ok(BinaryOp::Power {
            whitespace_after,
            whitespace_before,
        }),
        "<<" => Ok(BinaryOp::LeftShift {
            whitespace_after,
            whitespace_before,
        }),
        ">>" => Ok(BinaryOp::RightShift {
            whitespace_after,
            whitespace_before,
        }),
        "|" => Ok(BinaryOp::BitOr {
            whitespace_after,
            whitespace_before,
        }),
        "&" => Ok(BinaryOp::BitAnd {
            whitespace_after,
            whitespace_before,
        }),
        "^" => Ok(BinaryOp::BitXor {
            whitespace_after,
            whitespace_before,
        }),
        "@" => Ok(BinaryOp::MatrixMultiply {
            whitespace_after,
            whitespace_before,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_unary_op<'a>(
    config: &Config<'a>,
    op: Token<'a>,
    tail: Expression<'a>,
) -> Result<'a, Expression<'a>> {
    let operator = make_unary_operator(config, op)?;
    Ok(Expression::UnaryOperation {
        operator,
        expression: Box::new(tail),
        lpar: vec![],
        rpar: vec![],
    })
}

fn make_unary_operator<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, UnaryOp<'a>> {
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
    match tok.string {
        "+" => Ok(UnaryOp::Plus(whitespace_after)),
        "-" => Ok(UnaryOp::Minus(whitespace_after)),
        "~" => Ok(UnaryOp::BitInvert(whitespace_after)),
        "not" => Ok(UnaryOp::Not(whitespace_after)),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_number<'a>(_config: &Config<'a>, num: Token<'a>) -> Result<'a, Expression<'a>> {
    Ok(Expression::Integer {
        value: num.string,
        lpar: vec![],
        rpar: vec![],
    })
}

fn make_indented_block<'a>(
    config: &Config<'a>,
    mut nl: Token<'a>,
    indent: Token<'a>,
    statements: Vec<Statement<'a>>,
    mut dedent: Token<'a>,
) -> Result<'a, Suite<'a>> {
    // We want to be able to only keep comments in the footer that are actually for
    // this IndentedBlock. We do so by assuming that lines which are indented to the
    // same level as the block itself are comments that go at the footer of the
    // block. Comments that are indented to less than this indent are assumed to
    // belong to the next line of code. We override the indent here because the
    // dedent node's absolute indent is the resulting indentation after the dedent
    // is performed. Its this way because the whitespace state for both the dedent's
    // whitespace_after and the next BaseCompoundStatement's whitespace_before is
    // shared. This allows us to partially parse here and parse the rest of the
    // whitespace and comments on the next line, effectively making sure that
    // comments are attached to the correct node.
    let footer = parse_empty_lines(
        config,
        &mut dedent.whitespace_after,
        Some(indent.whitespace_before.absolute_indent),
    )?;
    let header = parse_trailing_whitespace(config, &mut nl.whitespace_before)?;
    Ok(Suite::IndentedBlock(IndentedBlock {
        body: statements,
        header,
        indent: indent.relative_indent,
        footer,
    }))
}

struct SimpleStatementParts<'a> {
    first: Token<'a>, // The first token of the first statement. Used for its whitespace
    statements: Vec<(SmallStatement<'a>, Token<'a>)>, // statement, semicolon pairs
    last_statement: SmallStatement<'a>,
    #[allow(dead_code)]
    last_semi: Option<Token<'a>>,
    nl: Token<'a>,
}

fn _make_simple_statement<'a>(
    config: &Config<'a>,
    mut parts: SimpleStatementParts<'a>,
) -> Result<'a, (Token<'a>, Vec<SmallStatement<'a>>, TrailingWhitespace<'a>)> {
    let mut body = vec![];
    for (statement, _semi) in parts.statements {
        // TODO: parse whitespace before and after semi and attach it to a semicolon
        // inside statement
        body.push(statement);
    }
    // TODO: parse whitespace before last semi and attach it to a semicolon inside
    // last_statement
    body.push(parts.last_statement);

    // TODO: is this correct?
    let trailing_whitespace = parse_trailing_whitespace(config, &mut parts.nl.whitespace_before)?;

    Ok((parts.first, body, trailing_whitespace))
}

fn make_simple_statement_suite<'a>(
    config: &Config<'a>,
    parts: SimpleStatementParts<'a>,
) -> Result<'a, Suite<'a>> {
    let (mut first, body, trailing_whitespace) = _make_simple_statement(config, parts)?;
    let leading_whitespace = parse_simple_whitespace(config, &mut first.whitespace_before)?;
    Ok(Suite::SimpleStatementSuite(SimpleStatementSuite {
        body,
        leading_whitespace,
        trailing_whitespace,
    }))
}

fn make_simple_statement_line<'a>(
    config: &Config<'a>,
    parts: SimpleStatementParts<'a>,
) -> Result<'a, SimpleStatementLine<'a>> {
    let (mut first, body, trailing_whitespace) = _make_simple_statement(config, parts)?;
    let leading_lines = parse_empty_lines(config, &mut first.whitespace_before, None)?;
    Ok(SimpleStatementLine {
        body,
        leading_lines,
        trailing_whitespace,
    })
}

fn make_if<'a>(
    config: &Config<'a>,
    mut keyword: Token<'a>,
    cond: Expression<'a>,
    mut colon: Token<'a>,
    block: Suite<'a>,
    orelse: Option<OrElse<'a>>,
    is_elif: bool,
) -> Result<'a, If<'a>> {
    let leading_lines = parse_empty_lines(config, &mut keyword.whitespace_before, None)?;
    let whitespace_before_test = parse_simple_whitespace(config, &mut keyword.whitespace_after)?;
    let whitespace_after_test = parse_simple_whitespace(config, &mut colon.whitespace_before)?;
    Ok(If {
        leading_lines,
        whitespace_before_test,
        test: cond,
        whitespace_after_test,
        body: block,
        orelse: orelse.map(Box::new),
        is_elif,
    })
}

fn make_else<'a>(
    config: &Config<'a>,
    mut keyword: Token<'a>,
    mut colon: Token<'a>,
    block: Suite<'a>,
) -> Result<'a, Else<'a>> {
    let leading_lines = parse_empty_lines(config, &mut keyword.whitespace_before, None)?;
    let whitespace_before_colon = parse_simple_whitespace(config, &mut colon.whitespace_before)?;
    Ok(Else {
        leading_lines,
        whitespace_before_colon,
        body: block,
    })
}

struct StarEtc<'a>(Option<StarArg<'a>>, Vec<Param<'a>>, Option<Param<'a>>);

fn make_parameters<'a>(
    _config: &Config<'a>,
    posonly: Option<(Vec<Param<'a>>, ParamSlash<'a>)>,
    params: Vec<Param<'a>>,
    star_etc: Option<StarEtc<'a>>,
) -> Result<'a, Parameters<'a>> {
    let (posonly_params, posonly_ind) = match posonly {
        Some((a, b)) => (a, Some(b)),
        None => (vec![], None),
    };
    let (star_arg, kwonly_params, star_kwarg) = match star_etc {
        None => (None, vec![], None),
        Some(StarEtc(a, b, c)) => (a, b, c),
    };
    Ok(Parameters {
        params,
        star_arg,
        kwonly_params,
        star_kwarg,
        posonly_params,
        posonly_ind,
    })
}

fn add_param_default<'a>(
    config: &Config<'a>,
    param: Param<'a>,
    def: Option<(AssignEqual<'a>, Expression<'a>)>,
    comma_tok: Option<Token<'a>>,
) -> Result<'a, Param<'a>> {
    let comma = match comma_tok {
        None => None,
        Some(c) => Some(make_comma(config, c)?),
    };

    let (equal, default) = match def {
        Some((a, b)) => (Some(a), Some(b)),
        None => (None, None),
    };
    Ok(Param {
        equal,
        default,
        comma,
        ..param
    })
}

fn add_param_star<'a>(
    config: &Config<'a>,
    param: Param<'a>,
    mut star: Token<'a>,
) -> Result<'a, Param<'a>> {
    let whitespace_after_star =
        parse_parenthesizable_whitespace(config, &mut star.whitespace_after)?;
    Ok(Param {
        star: Some(star.string),
        whitespace_after_star,
        ..param
    })
}

fn make_assign_equal<'a>(config: &Config<'a>, mut eq: Token<'a>) -> Result<'a, AssignEqual<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut eq.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut eq.whitespace_after)?;
    Ok(AssignEqual {
        whitespace_before,
        whitespace_after,
    })
}

fn make_comma<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, Comma<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
    Ok(Comma {
        whitespace_before,
        whitespace_after,
    })
}

fn concat<T>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.into_iter().chain(b.into_iter()).collect()
}

fn make_name_or_attr<'a>(
    config: &Config<'a>,
    first_tok: Name<'a>,
    mut tail: Vec<(Token<'a>, Name<'a>)>,
) -> Result<'a, NameOrAttribute<'a>> {
    if let Some((dot, name)) = tail.pop() {
        let dot = make_dot(config, dot)?;
        return Ok(NameOrAttribute::A(Attribute {
            attr: name,
            dot,
            lpar: Default::default(),
            rpar: Default::default(),
            value: Box::new(make_name_or_attr(config, first_tok, tail)?.into()),
        }));
    } else {
        Ok(NameOrAttribute::N(first_tok))
    }
}

fn make_name(tok: Token) -> Name {
    Name {
        value: tok.string,
        ..Default::default()
    }
}

fn make_dot<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, Dot<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
    Ok(Dot {
        whitespace_before,
        whitespace_after,
    })
}

fn make_import_alias<'a>(
    config: &Config<'a>,
    name: NameOrAttribute<'a>,
    asname: Option<(Token<'a>, Name<'a>)>,
) -> Result<'a, ImportAlias<'a>> {
    Ok(ImportAlias {
        name,
        asname: match asname {
            None => None,
            Some((mut kw, n)) => {
                let whitespace_before_as =
                    parse_parenthesizable_whitespace(config, &mut kw.whitespace_before)?;
                let whitespace_after_as =
                    parse_parenthesizable_whitespace(config, &mut kw.whitespace_after)?;
                Some(AsName {
                    name: NameOrAttribute::N(n),
                    whitespace_after_as,
                    whitespace_before_as,
                })
            }
        },
        comma: None,
    })
}

type ParenthesizedImportNames<'a> = (Option<Token<'a>>, ImportNames<'a>, Option<Token<'a>>);

fn make_import_from<'a>(
    config: &Config<'a>,
    mut from: Token<'a>,
    dots: Vec<Token<'a>>,
    module: Option<NameOrAttribute<'a>>,
    mut import: Token<'a>,
    aliases: ParenthesizedImportNames<'a>,
) -> Result<'a, ImportFrom<'a>> {
    let whitespace_after_from = parse_simple_whitespace(config, &mut from.whitespace_after)?;
    let whitespace_after_import = parse_simple_whitespace(config, &mut import.whitespace_after)?;
    let (lpar_tok, names, rpar_tok) = aliases;

    let lpar = match lpar_tok {
        None => None,
        Some(tok) => Some(make_lpar(config, tok)?),
    };
    let rpar = match rpar_tok {
        None => None,
        Some(tok) => Some(make_rpar(config, tok)?),
    };

    let mut relative = vec![];
    for mut dot_tok in dots {
        let dot = Dot {
            whitespace_after: ParenthesizableWhitespace::SimpleWhitespace(parse_simple_whitespace(
                config,
                &mut dot_tok.whitespace_after,
            )?),
            whitespace_before: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace("")),
        };
        relative.push(dot);
    }
    let mut whitespace_before_import = SimpleWhitespace("");
    if !relative.is_empty() && module.is_none() {
        // For relative-only imports relocate the space after the final dot to be owned
        // by the import token.
        if let Some(Dot {
            whitespace_after: ParenthesizableWhitespace::SimpleWhitespace(dot_ws),
            ..
        }) = relative.last_mut()
        {
            swap(dot_ws, &mut whitespace_before_import);
        }
    } else {
        whitespace_before_import = parse_simple_whitespace(config, &mut import.whitespace_before)?;
    }

    Ok(ImportFrom {
        module,
        names,
        relative,
        lpar,
        rpar,
        semicolon: None,
        whitespace_after_from,
        whitespace_after_import,
        whitespace_before_import,
    })
}

fn make_import<'a>(
    config: &Config<'a>,
    mut import: Token<'a>,
    names: Vec<ImportAlias<'a>>,
) -> Result<'a, Import<'a>> {
    let whitespace_after_import = parse_simple_whitespace(config, &mut import.whitespace_after)?;
    Ok(Import {
        names,
        whitespace_after_import,
        semicolon: None,
    })
}

fn make_import_from_as_names<'a>(
    first: ImportAlias<'a>,
    tail: Vec<(Comma<'a>, ImportAlias<'a>)>,
) -> Vec<ImportAlias<'a>> {
    let mut ret = vec![];
    let mut cur = first;
    for (comma, alias) in tail {
        ret.push(cur.with_comma(comma));
        cur = alias;
    }
    ret.push(cur);
    ret
}

fn make_lpar<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, LeftParen<'a>> {
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
    Ok(LeftParen { whitespace_after })
}

fn make_rpar<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, RightParen<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    Ok(RightParen { whitespace_before })
}

fn make_module<'a>(
    config: &Config<'a>,
    body: Vec<Statement<'a>>,
    mut tok: Token<'a>,
) -> Result<'a, Module<'a>> {
    let mut footer = parse_empty_lines(config, &mut tok.whitespace_before, Some(""))?;
    let mut last_indented = None;
    for (num, line) in footer.iter().enumerate() {
        if !line.whitespace.0.is_empty() {
            last_indented = Some(num);
        } else if line.comment.is_some() {
            // This is a non-indented comment. Everything from here should belong in the
            // footer.
            break;
        }
    }
    if let Some(num) = last_indented {
        if num + 1 == footer.len() {
            footer = vec![];
        } else {
            let (_, rest) = footer.split_at(num + 1);
            footer = rest.to_vec();
        }
    }
    Ok(Module { body, footer })
}

fn make_attribute<'a>(
    config: &Config<'a>,
    value: Expression<'a>,
    dot: Token<'a>,
    attr: Name<'a>,
) -> Result<'a, Attribute<'a>> {
    let dot = make_dot(config, dot)?;
    Ok(Attribute {
        attr,
        dot,
        lpar: Default::default(),
        rpar: Default::default(),
        value: Box::new(value),
    })
}

fn make_starred_element<'a>(
    config: &Config<'a>,
    mut star: Token<'a>,
    rest: Element<'a>,
) -> Result<'a, StarredElement<'a>> {
    let value = match rest {
        Element::Simple { value, .. } => value,
        _ => panic!("Internal error while making starred element"),
    };
    let whitespace_before_value =
        parse_parenthesizable_whitespace(config, &mut star.whitespace_after)?;
    Ok(StarredElement {
        value: Box::new(value),
        whitespace_before_value,
        lpar: Default::default(),
        rpar: Default::default(),
        comma: Default::default(),
    })
}

fn assign_target_to_element(expr: AssignTargetExpression) -> Element {
    match expr {
        AssignTargetExpression::Attribute(a) => Element::Simple {
            value: Expression::Attribute(a),
            comma: Default::default(),
        },
        AssignTargetExpression::Name(a) => Element::Simple {
            value: Expression::Name(a),
            comma: Default::default(),
        },
        AssignTargetExpression::Tuple(a) => Element::Simple {
            value: Expression::Tuple(a),
            comma: Default::default(),
        },
        AssignTargetExpression::StarredElement(s) => Element::Starred(s),
        AssignTargetExpression::List(l) => Element::Simple {
            value: Expression::List(l),
            comma: Default::default(),
        },
        AssignTargetExpression::Subscript(s) => Element::Simple {
            value: Expression::Subscript(s),
            comma: Default::default(),
        },
    }
}

fn make_assignment<'a>(
    config: &Config<'a>,
    lhs: Vec<(AssignTargetExpression<'a>, Token<'a>)>,
    rhs: Expression<'a>,
) -> Result<'a, Assign<'a>> {
    let mut targets = vec![];
    for (target, mut equal) in lhs {
        let whitespace_before_equal =
            parse_simple_whitespace(config, &mut equal.whitespace_before)?;
        let whitespace_after_equal = parse_simple_whitespace(config, &mut equal.whitespace_after)?;
        targets.push(AssignTarget {
            target,
            whitespace_before_equal,
            whitespace_after_equal,
        });
    }
    Ok(Assign {
        targets,
        value: rhs,
        semicolon: Default::default(),
    })
}

fn expr_to_element(expr: Expression) -> Element {
    Element::Simple {
        value: expr,
        comma: Default::default(),
    }
}

fn make_tuple<'a>(
    config: &Config<'a>,
    first: Element<'a>,
    rest: Vec<(Comma<'a>, Element<'a>)>,
    trailing_comma: Option<Comma<'a>>,
    lpar_tok: Option<Token<'a>>,
    rpar_tok: Option<Token<'a>>,
) -> Result<'a, Tuple<'a>> {
    let mut lpar: Vec<LeftParen<'a>> = Default::default();
    let mut rpar: Vec<RightParen<'a>> = Default::default();
    let elements = make_star_named_expressions(first, rest, trailing_comma);

    if let Some(lpar_tok) = lpar_tok {
        lpar.push(make_lpar(config, lpar_tok)?);
    }

    if let Some(rpar_tok) = rpar_tok {
        rpar.push(make_rpar(config, rpar_tok)?);
    }

    Ok(Tuple {
        elements,
        lpar,
        rpar,
    })
}

fn add_expr_parens<'a>(
    config: &Config<'a>,
    e: Expression<'a>,
    lpar: Token<'a>,
    rpar: Token<'a>,
) -> Result<'a, Expression<'a>> {
    Ok(e.with_parens(make_lpar(config, lpar)?, make_rpar(config, rpar)?))
}

fn make_kwarg<'a>(
    config: &Config<'a>,
    name: Name<'a>,
    eq: Token<'a>,
    value: Expression<'a>,
) -> Result<'a, Arg<'a>> {
    let equal = Some(make_assign_equal(config, eq)?);
    let keyword = Some(name);
    Ok(Arg {
        value,
        keyword,
        equal,
        comma: None,
        star: "",
        whitespace_after_star: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace("")),
        whitespace_after_arg: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace("")),
    })
}

fn make_kwargs<'a>(
    starred: Vec<Arg<'a>>,
    mut double_starred: Vec<Arg<'a>>,
    last: Option<Arg<'a>>,
) -> Vec<Arg<'a>> {
    if let Some(last) = last {
        double_starred.push(last);
    }
    concat(starred, double_starred)
}

fn make_star_arg<'a>(
    config: &Config<'a>,
    mut star: Token<'a>,
    expr: Expression<'a>,
) -> Result<'a, Arg<'a>> {
    let whitespace_after_star =
        parse_parenthesizable_whitespace(config, &mut star.whitespace_after)?;
    Ok(Arg {
        value: expr,
        keyword: None,
        equal: None,
        comma: None,
        star: star.string,
        whitespace_after_star,
        whitespace_after_arg: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace("")),
    })
}

fn make_call<'a>(
    config: &Config<'a>,
    func: Expression<'a>,
    mut lpar: Token<'a>,
    mut args: Vec<Arg<'a>>,
    mut rpar: Token<'a>,
) -> Result<'a, Call<'a>> {
    let whitespace_after_func =
        parse_parenthesizable_whitespace(config, &mut lpar.whitespace_before)?;
    let whitespace_before_args =
        parse_parenthesizable_whitespace(config, &mut lpar.whitespace_after)?;
    if let Some(arg) = args.last_mut() {
        if arg.comma.is_none() {
            arg.whitespace_after_arg =
                parse_parenthesizable_whitespace(config, &mut rpar.whitespace_before)?;
        }
    }

    let lpar = vec![];
    let rpar = vec![];
    let func = Box::new(func);

    Ok(Call {
        func,
        args,
        lpar,
        rpar,
        whitespace_after_func,
        whitespace_before_args,
    })
}

fn make_genexp_call<'a>(
    config: &Config<'a>,
    func: Expression<'a>,
    mut lpar_tok: Token<'a>,
    mut genexp: GeneratorExp<'a>,
) -> Call<'a> {
    // func ( (genexp) )
    //      ^
    //   lpar_tok

    // lpar_tok is the same token that was used to parse genexp's first lpar.
    // Nothing owns the whitespace before lpar_tok, so the same token is passed in here
    // again, to be converted into whitespace_after_func. We then split off a pair of
    // parenthesis from genexp, since now Call will own them, and shuffle around
    // whitespace accordingly.

    let mut lpars = genexp.lpar.into_iter();
    let first_lpar = lpars.next().expect("genexp without lpar");
    genexp.lpar = lpars.collect();
    let last_rpar = genexp.rpar.pop().expect("genexp without rpar");

    let whitespace_before_args = first_lpar.whitespace_after;
    let whitespace_after_func =
        parse_parenthesizable_whitespace(config, &mut lpar_tok.whitespace_before)
            .expect("This whitespace was parsed once already, it should never fail");

    Call {
        func: Box::new(func),
        args: vec![Arg {
            value: Expression::GeneratorExp(genexp),
            keyword: None,
            equal: None,
            comma: None,
            star: "",
            whitespace_after_star: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace(
                "",
            )),
            whitespace_after_arg: last_rpar.whitespace_before,
        }],
        lpar: vec![],
        rpar: vec![],
        whitespace_after_func,
        whitespace_before_args,
    }
}

fn make_arg(expr: Expression) -> Arg {
    Arg {
        value: expr,
        keyword: Default::default(),
        equal: Default::default(),
        comma: Default::default(),
        star: Default::default(),
        whitespace_after_star: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace("")),
        whitespace_after_arg: ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace("")),
    }
}

fn make_comp_if<'a>(
    config: &Config<'a>,
    mut kw: Token<'a>,
    test: Expression<'a>,
) -> Result<'a, CompIf<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut kw.whitespace_before)?;
    let whitespace_before_test =
        parse_parenthesizable_whitespace(config, &mut kw.whitespace_after)?;
    Ok(CompIf {
        test,
        whitespace_before,
        whitespace_before_test,
    })
}

fn make_for_if<'a>(
    config: &Config<'a>,
    mut async_tok: Option<Token<'a>>,
    mut for_: Token<'a>,
    target: AssignTargetExpression<'a>,
    mut in_: Token<'a>,
    iter: Expression<'a>,
    ifs: Vec<CompIf<'a>>,
) -> Result<'a, CompFor<'a>> {
    let mut whitespace_before =
        parse_parenthesizable_whitespace(config, &mut for_.whitespace_before)?;
    let whitespace_after_for =
        parse_parenthesizable_whitespace(config, &mut for_.whitespace_after)?;
    let whitespace_before_in =
        parse_parenthesizable_whitespace(config, &mut in_.whitespace_before)?;
    let whitespace_after_in = parse_parenthesizable_whitespace(config, &mut in_.whitespace_after)?;
    let inner_for_in = None;

    // If there is an async keyword, the start of the CompFor expression is considered
    // to be this keyword, so whitespace_before needs to adjust but Asynchronous will
    // own the whitespace before the for token.
    let asynchronous = if let Some(asy) = async_tok.as_mut() {
        let whitespace_after = whitespace_before;
        whitespace_before = parse_parenthesizable_whitespace(config, &mut asy.whitespace_before)?;
        Some(Asynchronous { whitespace_after })
    } else {
        None
    };

    Ok(CompFor {
        target,
        iter,
        ifs,
        inner_for_in,
        asynchronous,
        whitespace_before,
        whitespace_after_for,
        whitespace_before_in,
        whitespace_after_in,
    })
}

fn make_genexp<'a>(
    config: &Config<'a>,
    lpar: Token<'a>,
    elt: Expression<'a>,
    for_in: CompFor<'a>,
    rpar: Token<'a>,
) -> Result<'a, GeneratorExp<'a>> {
    let lpar = vec![make_lpar(config, lpar)?];
    let rpar = vec![make_rpar(config, rpar)?];

    Ok(GeneratorExp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lpar,
        rpar,
    })
}

fn merge_comp_fors(comp_fors: Vec<CompFor>) -> CompFor {
    let mut it = comp_fors.into_iter().rev();
    let first = it.next().expect("cant merge empty comp_fors");

    it.fold(first, |acc, curr| CompFor {
        inner_for_in: Some(Box::new(acc)),
        ..curr
    })
}

fn make_list_comp<'a>(
    config: &Config<'a>,
    mut lbrak: Token<'a>,
    elt: Expression<'a>,
    for_in: CompFor<'a>,
    mut rbrak: Token<'a>,
) -> Result<'a, ListComp<'a>> {
    let lbracket =
        parse_parenthesizable_whitespace(config, &mut lbrak.whitespace_after).map(|ws| {
            LeftSquareBracket {
                whitespace_after: ws,
            }
        })?;
    let rbracket =
        parse_parenthesizable_whitespace(config, &mut rbrak.whitespace_before).map(|ws| {
            RightSquareBracket {
                whitespace_before: ws,
            }
        })?;

    Ok(ListComp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    })
}

fn make_set_comp<'a>(
    config: &Config<'a>,
    mut lbrace: Token<'a>,
    elt: Expression<'a>,
    for_in: CompFor<'a>,
    mut rbrace: Token<'a>,
) -> Result<'a, SetComp<'a>> {
    let lbrace =
        parse_parenthesizable_whitespace(config, &mut lbrace.whitespace_after).map(|ws| {
            LeftCurlyBrace {
                whitespace_after: ws,
            }
        })?;
    let rbrace =
        parse_parenthesizable_whitespace(config, &mut rbrace.whitespace_before).map(|ws| {
            RightCurlyBrace {
                whitespace_before: ws,
            }
        })?;

    Ok(SetComp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    })
}

fn make_dict_comp<'a>(
    config: &Config<'a>,
    mut lbrace: Token<'a>,
    kvpair: (Expression<'a>, Token<'a>, Expression<'a>),
    for_in: CompFor<'a>,
    mut rbrace: Token<'a>,
) -> Result<'a, DictComp<'a>> {
    let lbrace =
        parse_parenthesizable_whitespace(config, &mut lbrace.whitespace_after).map(|ws| {
            LeftCurlyBrace {
                whitespace_after: ws,
            }
        })?;
    let rbrace =
        parse_parenthesizable_whitespace(config, &mut rbrace.whitespace_before).map(|ws| {
            RightCurlyBrace {
                whitespace_before: ws,
            }
        })?;

    let (key, mut colon, value) = kvpair;
    let whitespace_before_colon =
        parse_parenthesizable_whitespace(config, &mut colon.whitespace_before)?;
    let whitespace_after_colon =
        parse_parenthesizable_whitespace(config, &mut colon.whitespace_after)?;

    Ok(DictComp {
        key: Box::new(key),
        value: Box::new(value),
        for_in: Box::new(for_in),
        lbrace,
        rbrace,
        lpar: vec![],
        rpar: vec![],
        whitespace_before_colon,
        whitespace_after_colon,
    })
}

fn make_list<'a>(
    config: &Config<'a>,
    mut lbrak: Token<'a>,
    elements: Vec<Element<'a>>,
    mut rbrak: Token<'a>,
) -> Result<'a, List<'a>> {
    let lbracket =
        parse_parenthesizable_whitespace(config, &mut lbrak.whitespace_after).map(|ws| {
            LeftSquareBracket {
                whitespace_after: ws,
            }
        })?;
    let rbracket =
        parse_parenthesizable_whitespace(config, &mut rbrak.whitespace_before).map(|ws| {
            RightSquareBracket {
                whitespace_before: ws,
            }
        })?;
    Ok(List {
        elements,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    })
}

fn make_set<'a>(
    config: &Config<'a>,
    mut lbrace: Token<'a>,
    elements: Vec<Element<'a>>,
    mut rbrace: Token<'a>,
) -> Result<'a, Set<'a>> {
    let lbrace =
        parse_parenthesizable_whitespace(config, &mut lbrace.whitespace_after).map(|ws| {
            LeftCurlyBrace {
                whitespace_after: ws,
            }
        })?;
    let rbrace =
        parse_parenthesizable_whitespace(config, &mut rbrace.whitespace_before).map(|ws| {
            RightCurlyBrace {
                whitespace_before: ws,
            }
        })?;
    Ok(Set {
        elements,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    })
}

fn comma_separate<'a, T>(
    first: T,
    rest: Vec<(Comma<'a>, T)>,
    trailing_comma: Option<Comma<'a>>,
) -> Vec<T>
where
    T: WithComma<'a>,
{
    let mut elements = vec![];
    let mut current = first;
    for (comma, next) in rest {
        elements.push(current.with_comma(comma));
        current = next;
    }
    if let Some(mut comma) = trailing_comma {
        // don't consume trailing whitespace for trailing comma
        comma.whitespace_after = ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace(""));
        current = current.with_comma(comma);
    }
    elements.push(current);
    elements
}

fn make_star_named_expressions<'a>(
    first: Element<'a>,
    rest: Vec<(Comma<'a>, Element<'a>)>,
    trailing_comma: Option<Comma<'a>>,
) -> Vec<Element<'a>> {
    comma_separate(first, rest, trailing_comma)
}

fn make_dict<'a>(
    config: &Config<'a>,
    mut lbrace: Token<'a>,
    elements: Vec<DictElement<'a>>,
    mut rbrace: Token<'a>,
) -> Result<'a, Dict<'a>> {
    let lbrace =
        parse_parenthesizable_whitespace(config, &mut lbrace.whitespace_after).map(|ws| {
            LeftCurlyBrace {
                whitespace_after: ws,
            }
        })?;
    let rbrace =
        parse_parenthesizable_whitespace(config, &mut rbrace.whitespace_before).map(|ws| {
            RightCurlyBrace {
                whitespace_before: ws,
            }
        })?;
    Ok(Dict {
        elements,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    })
}

fn make_double_starred_keypairs<'a>(
    first: DictElement<'a>,
    rest: Vec<(Comma<'a>, DictElement<'a>)>,
    trailing_comma: Option<Comma<'a>>,
) -> Vec<DictElement<'a>> {
    let mut elements = vec![];
    let mut current = first;
    for (comma, next) in rest {
        elements.push(current.with_comma(comma));
        current = next;
    }
    if let Some(mut comma) = trailing_comma {
        // don't consume trailing whitespace for trailing comma
        comma.whitespace_after = ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace(""));
        current = current.with_comma(comma);
    }
    elements.push(current);
    elements
}

fn make_dict_element<'a>(
    config: &Config<'a>,
    el: (Expression<'a>, Token<'a>, Expression<'a>),
) -> Result<'a, DictElement<'a>> {
    let (key, mut colon, value) = el;
    let whitespace_before_colon =
        parse_parenthesizable_whitespace(config, &mut colon.whitespace_before)?;
    let whitespace_after_colon =
        parse_parenthesizable_whitespace(config, &mut colon.whitespace_after)?;

    Ok(DictElement::Simple {
        key,
        value,
        comma: Default::default(),
        whitespace_before_colon,
        whitespace_after_colon,
    })
}

fn make_double_starred_element<'a>(
    config: &Config<'a>,
    mut star: Token<'a>,
    value: Expression<'a>,
) -> Result<'a, DoubleStarredElement<'a>> {
    let whitespace_before_value =
        parse_parenthesizable_whitespace(config, &mut star.whitespace_after)?;
    Ok(DoubleStarredElement {
        value,
        comma: Default::default(),
        whitespace_before_value,
    })
}

fn make_index(value: Expression) -> BaseSlice {
    BaseSlice::Index(Index { value })
}

fn make_colon<'a>(config: &Config<'a>, mut tok: Token<'a>) -> Result<'a, Colon<'a>> {
    let whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
    let whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
    Ok(Colon {
        whitespace_before,
        whitespace_after,
    })
}

fn make_slice<'a>(
    config: &Config<'a>,
    lower: Option<Expression<'a>>,
    first_colon: Token<'a>,
    upper: Option<Expression<'a>>,
    rest: Option<(Token<'a>, Option<Expression<'a>>)>,
) -> Result<'a, BaseSlice<'a>> {
    let first_colon = make_colon(config, first_colon)?;
    let (second_colon, step) = if let Some((tok, step)) = rest {
        (Some(make_colon(config, tok)?), step)
    } else {
        (None, None)
    };
    Ok(BaseSlice::Slice(Slice {
        lower,
        upper,
        step,
        first_colon,
        second_colon,
    }))
}

// HACK: in slices if there is a colon directly before or after the comma, the colon
// owns the whitespace. Not sure if this is by design or accident.
fn ltrim_comma(before: &BaseSlice, comma: &mut Comma) {
    if let BaseSlice::Slice(s) = &before {
        let trailing_second_colon = s.step.is_none() && s.second_colon.is_some();
        let trailing_first_colon = s.upper.is_none() && s.step.is_none();
        if trailing_first_colon || trailing_second_colon {
            comma.whitespace_before =
                ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace(""));
        }
    }
}

fn rtrim_comma(comma: &mut Comma, after: &BaseSlice) {
    if let BaseSlice::Slice(s) = &after {
        let leading_first_colon = s.lower.is_none();
        if leading_first_colon {
            comma.whitespace_after =
                ParenthesizableWhitespace::SimpleWhitespace(SimpleWhitespace(""));
        }
    }
}

fn make_slices<'a>(
    first: BaseSlice<'a>,
    rest: Vec<(Comma<'a>, BaseSlice<'a>)>,
    mut trailing_comma: Option<Comma<'a>>,
) -> Vec<SubscriptElement<'a>> {
    let mut elements = vec![];
    let mut current = first;
    for (mut comma, next) in rest {
        ltrim_comma(&current, &mut comma);
        rtrim_comma(&mut comma, &next);
        elements.push(SubscriptElement {
            slice: current,
            comma: Some(comma),
        });
        current = next;
    }
    if let Some(comma) = trailing_comma.as_mut() {
        ltrim_comma(&current, comma);
    }
    elements.push(SubscriptElement {
        slice: current,
        comma: trailing_comma,
    });
    elements
}

fn make_subscript<'a>(
    config: &Config<'a>,
    value: Expression<'a>,
    mut lbrak: Token<'a>,
    slice: Vec<SubscriptElement<'a>>,
    mut rbrak: Token<'a>,
) -> Result<'a, Subscript<'a>> {
    let lbracket =
        parse_parenthesizable_whitespace(config, &mut lbrak.whitespace_after).map(|ws| {
            LeftSquareBracket {
                whitespace_after: ws,
            }
        })?;

    // if there is a trailing comma, it owns the whitespace before right bracket
    let rbracket = if let Some(SubscriptElement { comma: Some(_), .. }) = slice.last() {
        Ok(ParenthesizableWhitespace::SimpleWhitespace(
            SimpleWhitespace(""),
        ))
    } else {
        parse_parenthesizable_whitespace(config, &mut rbrak.whitespace_before)
    }
    .map(|ws| RightSquareBracket {
        whitespace_before: ws,
    })?;

    let whitespace_after_value =
        parse_parenthesizable_whitespace(config, &mut lbrak.whitespace_before)?;
    Ok(Subscript {
        value: Box::new(value),
        slice,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
        whitespace_after_value,
    })
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use libcst_tokenize::{TokConfig, TokenIterator};

    use super::*;

    #[test]
    fn make_module_strips_whitespace() {
        let input = "  # no\n\n  # no\n# yes\n  # yes\n# yes\n";
        let c = Config {
            input,
            lines: input.split_inclusive('\n').collect(),
            default_newline: "\n",
        };
        let toks: Vec<_> = TokenIterator::new(
            input,
            &TokConfig {
                async_hacks: false,
                split_fstring: true,
            },
        )
        .try_collect()
        .expect("tokenization error");
        let last_tok = toks.last().unwrap().clone();
        let m = make_module(&c, vec![], last_tok).expect("parse error");
        assert_eq!(m.footer.len(), 3);
    }
}
