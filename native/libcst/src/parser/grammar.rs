// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::cell::RefCell;
use std::rc::Rc;

use crate::nodes::*;
use crate::tokenizer::whitespace_parser::{Config, WhitespaceError};
use crate::tokenizer::{TokError, TokType, Token};
use peg::str::LineCol;
use peg::{parser, Parse, ParseElem, RuleResult};
use thiserror::Error;
use TokType::{
    Async, Await as AWAIT, Dedent, EndMarker, FStringEnd, FStringStart, FStringString, Indent,
    Name as NameTok, Newline as NL, Number, String as STRING,
};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParserError<'a> {
    #[error("tokenizer error: {0}")]
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
pub struct TokVec<'a>(Vec<Rc<RefCell<Token<'a>>>>);

impl<'a> std::convert::From<Vec<Token<'a>>> for TokVec<'a> {
    fn from(vec: Vec<Token<'a>>) -> Self {
        TokVec(vec.into_iter().map(|x| Rc::new(RefCell::new(x))).collect())
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
        let tok = self
            .0
            .get(pos)
            .unwrap_or_else(|| self.0.last().unwrap())
            .borrow();
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

type TokenRef<'a> = Rc<RefCell<Token<'a>>>;

impl<'a> ParseElem for TokVec<'a> {
    type Element = TokenRef<'a>;

    fn parse_elem(&self, pos: usize) -> RuleResult<Self::Element> {
        match self.0.get(pos) {
            Some(tok) => RuleResult::Matched(pos + 1, tok.clone()),
            None => RuleResult::Failed,
        }
    }
}

// impl<'a> ParseLiteral for TokVec<'a> {
//     fn parse_string_literal(&self, pos: usize, literal: &str) -> RuleResult<()> {
//         match self.parse_elem(pos) {
//             RuleResult::Matched(p, tok) if tok.borrow().string == literal => {
//                 RuleResult::Matched(p, ())
//             }
//             _ => RuleResult::Failed,
//         }
//     }
// }

parser! {
    pub grammar toy<'a>(config: &Config<'a>) for TokVec<'a> {
        pub rule lpar() -> LeftParen<'a>
            = a:lit("(") { make_lpar(a) }

        rule lit(lit: &'static str) -> TokenRef<'a>
            = [t] {? if t.borrow().string == lit { Ok(t) } else { Err(lit) } }
    }
}

parser! {
    pub grammar python<'a>(config: &Config<'a>) for TokVec<'a> {
        pub rule file() -> Module<'a>
            = traced(<_file()>)

        rule _file() -> Module<'a>
            = s:statements()? eof:tok(EndMarker, "EOF") {
                make_module(s.unwrap_or_default(), eof)
            }

        pub rule statements() -> Vec<Statement<'a>>
            = statement()+

        pub rule statement() -> Statement<'a>
            = c:compound_stmt() { Statement::Compound(c) }
            / s:simple_stmt() {
                    Statement::Simple(make_simple_statement_line(s))
            }

        rule simple_stmt() -> SimpleStatementParts<'a>
            = first:&_ statements:(s:small_stmt() semi:lit(";") { (s, semi) })*
            last_statement:small_stmt() last_semi:lit(";")? nl:tok(NL, "NEWLINE") {
                SimpleStatementParts {first, statements, last_statement, last_semi, nl}
            }

        rule compound_stmt() -> CompoundStatement<'a>
            = &(lit("def") / lit("@") / tok(Async, "ASYNC")) f:function_def() {
                CompoundStatement::FunctionDef(f)
            }
            / &lit("if") f:if_stmt() { CompoundStatement::If(f) }
            / &(lit("class") / lit("@")) c:class_def() { CompoundStatement::ClassDef(c) }
            / &(lit("with") / tok(Async, "ASYNC")) w:with_stmt() { CompoundStatement::With(w) }
            / &(lit("for") / tok(Async, "ASYNC")) f:for_stmt() { CompoundStatement::For(f) }
            / &lit("try") t:try_stmt() { CompoundStatement::Try(t) }
            / &lit("while") w:while_stmt() { CompoundStatement::While(w) }

        #[cache]
        rule small_stmt() -> SmallStatement<'a>
            = assignment()
            / e:star_expressions() { SmallStatement::Expr(Expr { value: e, semicolon: None }) }
            / &lit("return") s:return_stmt() { SmallStatement::Return(s) }
            // this is expanded from the original grammar's import_stmt rule
            / &lit("import") i:import_name() { SmallStatement::Import(i) }
            / &lit("from") i:import_from() { SmallStatement::ImportFrom(i) }
            / &lit("raise") r:raise_stmt() { SmallStatement::Raise(r) }
            / lit("pass") { SmallStatement::Pass(Pass { semicolon: None }) }
            / &lit("del") s:del_stmt() { SmallStatement::Del(s) }
            / &lit("yield") s:yield_stmt() { SmallStatement::Expr(Expr { value: s, semicolon: None }) }
            / &lit("assert") s:assert_stmt() {SmallStatement::Assert(s)}
            / lit("break") { SmallStatement::Break(Break { semicolon: None })}
            / lit("continue") { SmallStatement::Continue(Continue { semicolon: None })}
            / &lit("global") s:global_stmt() {SmallStatement::Global(s)}
            / &lit("nonlocal") s:nonlocal_stmt() {SmallStatement::Nonlocal(s)}

        rule assignment() -> SmallStatement<'a>
            = a:name() col:lit(":") ann:expression()
                rhs:(eq:lit("=") d:annotated_rhs() {(eq, d)})? {
                    SmallStatement::AnnAssign(
                        make_ann_assignment(AssignTargetExpression::Name(a), col, ann, rhs))
            }
            // TODO: there's an extra '(' single_target ')' clause here in upstream
            / a:single_subscript_attribute_target() col:lit(":") ann:expression()
                rhs:(eq:lit("=") d:annotated_rhs() {(eq, d)})? {
                    SmallStatement::AnnAssign(make_ann_assignment(a, col, ann, rhs))
            }
            / lhs:(t:star_targets() eq:lit("=") {(t, eq)})+ rhs:(yield_expr() / star_expressions()) !lit("=") {
                SmallStatement::Assign(make_assignment(lhs, rhs))
            }
            / t:single_target() op:augassign() rhs:(yield_expr() / star_expressions()) {
                SmallStatement::AugAssign(make_aug_assign(t, op, rhs))
            }

        rule augassign() -> AugOp<'a>
            = &(lit("+=") / lit("-=") / lit("*=") / lit("@=") /  lit("/=") / lit("%=") / lit("&=") / lit("|=") / lit("^=") / lit("<<=")
                / lit(">>=") / lit("**=") / lit("//=")) tok:_ {?
                    make_aug_op(tok).map_err(|_| "aug_op")
            }

        rule class_def() -> ClassDef<'a>
            = d:decorators() c:class_def_raw() { c.with_decorators(d) }
            / class_def_raw()

        rule class_def_raw() -> ClassDef<'a>
            = kw:lit("class") n:name() arg:(l:lpar() a:arguments()? r:rpar() {(l, a, r)})?
                col:lit(":") b:block() {
                    make_class_def(kw, n, arg, col, b)
            }

        #[cache]
        rule block() -> Suite<'a>
            = n:tok(NL, "NEWLINE") ind:tok(Indent, "INDENT") s:statements() ded:tok(Dedent, "DEDENT") {
                make_indented_block(n, ind, s, ded)
            }
            / s:simple_stmt() {
                make_simple_statement_suite(s)
            }

        rule star_expressions() -> Expression<'a>
            = first:star_expression()
                rest:(comma:comma() e:star_expression() { (comma, expr_to_element(e)) })+
                comma:comma()? {
                    Expression::Tuple(make_tuple(expr_to_element(first), rest, comma, None, None))
            }
            / e:star_expression() comma:comma() {
                Expression::Tuple(make_tuple(expr_to_element(e), vec![], Some(comma), None, None))
            }
            / star_expression()

        #[cache]
        rule star_expression() -> Expression<'a>
            = star:lit("*") e:bitwise_or() {
                Expression::StarredElement(make_starred_element(star, expr_to_element(e)))
            }
            / expression()

        rule star_named_expressions() -> Vec<Element<'a>>
            = first:star_named_expression()
                rest:(c:comma() e:star_named_expression() { (c, e) })*
                trail:comma()? {
                    comma_separate(first, rest, trail)
            }

        rule star_named_expression() -> Element<'a>
            = star:lit("*") e:bitwise_or() {
                Element::Starred(make_starred_element(star, expr_to_element(e)))
            }
            / e:named_expression() { expr_to_element(e) }

        #[cache]
        rule expression() -> Expression<'a>
            = _conditional_expression()
            / lambdef()

        rule _conditional_expression() -> Expression<'a>
            = body:disjunction() i:lit("if") test:disjunction() e:lit("else") oe:expression() {
                Expression::IfExp(make_ifexp(body, i, test, e, oe))
            }
            / disjunction()

        rule lambdef() -> Expression<'a>
            = kw:lit("lambda") p:lambda_params()? c:lit(":") b:expression() {
                Expression::Lambda(make_lambda(kw, p.unwrap_or_default(), c, b))
            }

        rule lambda_params() -> Parameters<'a>
            = lambda_parameters()

        // lambda_parameters etc. duplicates parameters but without annotations or type
        // comments, and if there's no comma after a parameter, we expect a colon, not a
        // close parenthesis.

        rule lambda_parameters() -> Parameters<'a>
            = a:lambda_slash_no_default() b:lambda_param_no_default()*
                c:lambda_param_with_default()* d:lambda_star_etc()? {
                    make_parameters(Some(a), concat(b, c), d)
            }
            / a:lambda_slash_with_default() b:lambda_param_with_default()*
                d:lambda_star_etc()? {
                    make_parameters(Some(a), b, d)
            }
            / a:lambda_param_no_default()+ b:lambda_param_with_default()*
                d:lambda_star_etc()? {
                    make_parameters(None, concat(a, b), d)
            }
            / a:lambda_param_with_default()+ d:lambda_star_etc()? {
                make_parameters(None, a, d)
            }
            / d:lambda_star_etc() {
                make_parameters(None, vec![], Some(d))
            }

        rule lambda_slash_no_default() -> (Vec<Param<'a>>, ParamSlash<'a>)
            = a:lambda_param_no_default()+ slash:lit("/") com:comma() {
                (a, ParamSlash { comma: Some(com) } )
            }
            / a:lambda_param_no_default()+ slash:lit("/") &lit(":") {
                (a, ParamSlash { comma: None })
            }

        rule lambda_slash_with_default() -> (Vec<Param<'a>>, ParamSlash<'a>)
            = a:lambda_param_no_default()* b:lambda_param_with_default()+ slash:lit("/") c:comma(){
                (concat(a, b), ParamSlash { comma: Some(c) })
            }
            / a:lambda_param_no_default()* b:lambda_param_with_default()+ slash:lit("/") &lit(":") {
                (concat(a, b), ParamSlash { comma: None })
            }

        rule lambda_star_etc() -> StarEtc<'a>
            = star:lit("*") a:lambda_param_no_default()
                b:lambda_param_maybe_default()* kw:lambda_kwds()? {
                    StarEtc(Some(StarArg::Param(
                        Box::new(add_param_star(a, star))
                    )), b, kw)
            }
            / lit("*") c:comma() b:lambda_param_maybe_default()+ kw:lambda_kwds()? {
                StarEtc(Some(StarArg::Star(ParamStar {comma: c})), b, kw)
            }
            / kw:lambda_kwds() { StarEtc(None, vec![], Some(kw)) }

        rule lambda_kwds() -> Param<'a>
            = star:lit("**") a:lambda_param_no_default() {
                add_param_star(a, star)
            }

        rule lambda_param_no_default() -> Param<'a>
            = a:lambda_param() c:lit(",") {
                add_param_default(a, None, Some(c))
            }
            / a:lambda_param() &lit(":") {a}

        rule lambda_param_with_default() -> Param<'a>
            = a:lambda_param() def:default() c:lit(",") {
                add_param_default(a, Some(def), Some(c))
            }
            / a:lambda_param() def:default() &lit(":") {
                add_param_default(a, Some(def), None)
            }

        rule lambda_param_maybe_default() -> Param<'a>
            = a:lambda_param() def:default()? c:lit(",") {
                add_param_default(a, def, Some(c))
            }
            / a:lambda_param() def:default()? &lit(":") {
                add_param_default(a, def, None)
            }

        rule lambda_param() -> Param<'a>
            = name:name() { Param { name, ..Default::default() } }

        #[cache]
        rule disjunction() -> Expression<'a>
            = a:conjunction() b:(or:lit("or") inner:conjunction() { (or, inner) })+ {?
                make_boolean_op(a, b).map_err(|e| "expected disjunction")
            }
            / conjunction()

        #[cache]
        rule conjunction() -> Expression<'a>
            = a:inversion() b:(and:lit("and") inner:inversion() { (and, inner) })+ {?
                make_boolean_op(a, b).map_err(|e| "expected conjunction")
            }
            / inversion()

        #[cache]
        rule inversion() -> Expression<'a>
            = not:lit("not") a:inversion() {?
                make_unary_op(not, a).map_err(|e| "expected inversion")
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
                make_comparison_operator(op)
                    .map(|op| (op, e))
                    .map_err(|_| "comparison")
            }

        rule _op_bitwise_or2(first: &'static str, second: &'static str) -> (CompOp<'a>, Expression<'a>)
            = f:lit(first) s:lit(second) e:bitwise_or() {?
                make_comparison_operator_2(f, s)
                    .map(|op| (op, e))
                    .map_err(|_| "comparison")
            }

        rule star_targets() -> AssignTargetExpression<'a>
            = a:star_target() !lit(",") {a}
            / first:(t:star_target() {assign_target_to_element(t)})
                rest:(comma:comma() t:star_target() {(comma, assign_target_to_element(t))})*
                comma:comma()? {
                    AssignTargetExpression::Tuple(make_tuple(first, rest, comma, None, None))
            }

        #[cache]
        rule star_target() -> AssignTargetExpression<'a>
            = star:lit("*") !lit("*") t:star_target() {
                AssignTargetExpression::StarredElement(
                    make_starred_element(star, assign_target_to_element(t))
                )
            }
            / target_with_star_atom()

        // This differs from star_targets above because it requires at least two items
        // in the tuple
        rule star_targets_tuple_seq() -> Tuple<'a>
            = first:(t:star_target() {assign_target_to_element(t)})
                rest:(c:comma() t:star_target() {(c, assign_target_to_element(t))})+
                trail:comma()? {
                    make_tuple(first, rest, trail, None, None)
            }
            / t:star_target() trail:comma()? {
                make_tuple(assign_target_to_element(t), vec![], trail, None, None)
            }

        rule star_targets_list_seq() -> Vec<Element<'a>>
            = first:(t:star_target() { assign_target_to_element(t) })
                rest:(c:comma() t:star_target() {(c, assign_target_to_element(t))})*
                trail:comma()? {
                    comma_separate(first, rest, trail)
            }

        #[cache]
        rule target_with_star_atom() -> AssignTargetExpression<'a>
            = a:t_primary() dot:lit(".") n:name() !t_lookahead() {
                AssignTargetExpression::Attribute(make_attribute(a, dot, n))
            }
            / a:t_primary() lbrak:lbrak() s:slices() rbrak:rbrak() !t_lookahead() {
                AssignTargetExpression::Subscript(
                    make_subscript(a, lbrak, s, rbrak)
                )
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
            / lbrak:lbrak() a:star_targets_list_seq()? rbrak:rbrak() {
                AssignTargetExpression::List(
                    make_list(lbrak, a.unwrap_or_default(), rbrak)
                )
            }

        rule single_target() -> AssignTargetExpression<'a>
            = single_subscript_attribute_target()
            / n:name() { AssignTargetExpression::Name(n) }
            / lpar:lpar() t:single_target() rpar:rpar() { t.with_parens(lpar, rpar) }

        rule single_subscript_attribute_target() -> AssignTargetExpression<'a>
            = a:t_primary() dot:lit(".") n:name() !t_lookahead() {
                AssignTargetExpression::Attribute(make_attribute(a, dot, n))
            }
            / a:t_primary() lbrak:lbrak() s:slices() rbrak:rbrak() !t_lookahead() {
                AssignTargetExpression::Subscript(
                    make_subscript(a, lbrak, s, rbrak)
                )
            }

        rule del_targets() -> Vec<Element<'a>>
            = first:del_target() rest:(c:comma() t:del_target() {(c,t.into())})* trail:comma()? {
                comma_separate(first.into(), rest, trail)
            }

        rule del_target() -> DelTargetExpression<'a>
            = a:t_primary() d:lit(".") n:name() !t_lookahead() {
                DelTargetExpression::Attribute(make_attribute(a, d, n))
            }
            / a:t_primary() lbrak:lbrak() s:slices() rbrak:rbrak() !t_lookahead() {
                DelTargetExpression::Subscript(
                    make_subscript(a, lbrak, s, rbrak)
                )
            }
            / del_t_atom()

        rule del_t_atom() -> DelTargetExpression<'a>
            = n:name() { DelTargetExpression::Name(n) }
            / l:lpar() d:del_target() r:rpar() { d.with_parens(l, r) }
            / l:lpar() d:del_targets()? r:rpar() {
                make_del_tuple(Some(l), d.unwrap_or_default(), Some(r))
            }
            / l:lbrak() d:del_targets()? r:rbrak() {
                DelTargetExpression::List(
                    make_list(l, d.unwrap_or_default(), r)
                )
            }

        #[cache_left_rec]
        rule t_primary() -> Expression<'a>
            = value:t_primary() dot:lit(".") attr:name() &t_lookahead() {
                Expression::Attribute(make_attribute(value, dot, attr))
            }
            / v:t_primary() l:lbrak() s:slices() r:rbrak() &t_lookahead() {
                Expression::Subscript(make_subscript(v, l, s, r))
            }
            / f:t_primary() gen:genexp() &t_lookahead() {
                Expression::Call(make_genexp_call(f, gen))
            }
            / f:t_primary() lpar:lit("(") arg:arguments()? rpar:lit(")") &t_lookahead() {
                Expression::Call(make_call(f, lpar, arg.unwrap_or_default(), rpar))
            }
            / a:atom() &t_lookahead() {a}

        rule t_lookahead() -> ()
            = (lit("(") / lit("[") / lit(".")) {}

        rule yield_expr() -> Expression<'a>
            = y:lit("yield") f:lit("from") a:expression() {
                Expression::Yield(make_yield(y, Some(f), Some(a)))
            }
            / y:lit("yield") a:star_expressions()? {
                Expression::Yield(make_yield(y, None, a))
            }

        #[cache_left_rec]
        rule bitwise_or() -> Expression<'a>
            = a:bitwise_or() op:lit("|") b:bitwise_xor() {?
                make_binary_op(a, op, b).map_err(|e| "expected bitwise_or")
            }
            / bitwise_xor()

        #[cache_left_rec]
        rule bitwise_xor() -> Expression<'a>
            = a:bitwise_xor() op:lit("^") b:bitwise_and() {?
                make_binary_op(a, op, b).map_err(|e| "expected bitwise_xor")
            }
            / bitwise_and()

        #[cache_left_rec]
        rule bitwise_and() -> Expression<'a>
            = a:bitwise_and() op:lit("&") b:shift_expr() {?
                make_binary_op(a, op, b).map_err(|e| "expected bitwise_and")
            }
            / shift_expr()

        #[cache_left_rec]
        rule shift_expr() -> Expression<'a>
            = a:shift_expr() op:lit("<<") b:sum() {?
                make_binary_op(a, op, b).map_err(|e| "expected shift_expr")
            }
            / a:shift_expr() op:lit(">>") b:sum() {?
                make_binary_op(a, op, b).map_err(|e| "expected shift_expr")
            }
            / sum()

        #[cache_left_rec]
        rule sum() -> Expression<'a>
            = a:sum() op:lit("+") b:term() {?
                make_binary_op(a, op, b).map_err(|e| "expected sum")
            }
            / a:sum() op:lit("-") b:term() {?
                make_binary_op(a, op, b).map_err(|e| "expected sum")
            }
            / term()

        #[cache_left_rec]
        rule term() -> Expression<'a>
            = a:term() op:lit("*") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("/") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("//") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("%") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected term")
            }
            / a:term() op:lit("@") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected term")
            }
            / factor()

        #[cache]
        rule factor() -> Expression<'a>
            = op:lit("+") a:factor() {?
                make_unary_op(op, a).map_err(|e| "expected factor")
            }
            / op:lit("-") a:factor() {?
                make_unary_op(op, a).map_err(|e| "expected factor")
            }
            / op:lit("~") a:factor() {?
                make_unary_op(op, a).map_err(|e| "expected factor")
            }
            / power()

        rule power() -> Expression<'a>
            = a:await_primary() op:lit("**") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected power")
            }
            / await_primary()

        rule await_primary() -> Expression<'a>
            = aw:tok(AWAIT, "AWAIT") e:primary() {
                Expression::Await(make_await(aw, e))
            }
            / primary()

        #[cache_left_rec]
        rule primary() -> Expression<'a>
            = v:primary() dot:lit(".") attr:name() {
                Expression::Attribute(make_attribute(v, dot, attr))
            }
            / a:primary() b:genexp() {
                Expression::Call(make_genexp_call(a, b))
            }
            / f:primary() lpar:lit("(") arg:arguments()? rpar:lit(")") {
                Expression::Call(make_call(f, lpar, arg.unwrap_or_default(), rpar))
            }
            / v:primary() lbrak:lbrak() s:slices() rbrak:rbrak() {
                Expression::Subscript(make_subscript(v, lbrak, s, rbrak))
            }
            / atom()

        rule list() -> Expression<'a>
            = lbrak:lbrak() e:star_named_expressions()? rbrak:rbrak() {
                Expression::List(
                    make_list(lbrak, e.unwrap_or_default(), rbrak)
                )
            }

        rule slices() -> Vec<SubscriptElement<'a>>
            = s:slice() !lit(",") { vec![SubscriptElement { slice: s, comma: None }] }
            / first:slice() rest:(c:comma() s:slice() {(c, s)})* trail:comma()? {
                make_slices(first, rest, trail)
            }

        rule slice() -> BaseSlice<'a>
            = l:expression()? col:lit(":") u:expression()?
                rest:(c:lit(":") s:expression()? {(c, s)})? {
                    make_slice(l, col, u, rest)
            }
            / v:expression() { make_index(v) }

        rule listcomp() -> Expression<'a>
            = lbrak:lbrak() elt:named_expression() comp:for_if_clauses() rbrak:rbrak() {
                Expression::ListComp(make_list_comp(lbrak, elt, comp, rbrak))
            }

        rule set() -> Expression<'a>
            = lbrace:lbrace() e:star_named_expressions()? rbrace:rbrace() {
                Expression::Set(make_set(lbrace, e.unwrap_or_default(), rbrace))
            }

        rule setcomp() -> Expression<'a>
            = l:lbrace() elt:named_expression() comp:for_if_clauses() r:rbrace() {
                Expression::SetComp(make_set_comp(l, elt, comp, r))
            }

        rule dict() -> Expression<'a>
            = lbrace:lbrace() els:double_starred_keypairs()? rbrace:rbrace() {
                Expression::Dict(make_dict(lbrace, els.unwrap_or_default(), rbrace))
            }

        rule dictcomp() -> Expression<'a>
            = lbrace:lbrace() elt:kvpair() comp:for_if_clauses() rbrace:rbrace() {
                Expression::DictComp(make_dict_comp(lbrace, elt, comp, rbrace))
            }

        rule double_starred_keypairs() -> Vec<DictElement<'a>>
            = first:double_starred_kvpair()
                rest:(c:comma() e:double_starred_kvpair() {(c, e)})*
                trail:comma()? {
                    make_double_starred_keypairs(first, rest, trail)
            }

        rule double_starred_kvpair() -> DictElement<'a>
            = s:lit("**") e:bitwise_or() {
                DictElement::Starred(make_double_starred_element(s, e))
            }
            / k:kvpair() { make_dict_element(k) }

        rule kvpair() -> (Expression<'a>, TokenRef<'a>, Expression<'a>)
            = k:expression() colon:lit(":") v:expression() { (k, colon, v) }

        rule genexp() -> GeneratorExp<'a>
            = lpar:lpar() elt:named_expression() comp:for_if_clauses() rpar:rpar() {
                make_genexp(lpar, elt, comp, rpar)
            }

        rule for_if_clauses() -> CompFor<'a>
            = c:for_if_clause()+ { merge_comp_fors(c) }

        rule for_if_clause() -> CompFor<'a>
            = asy:_async() f:lit("for") tgt:star_targets() i:lit("in")
                iter:disjunction() ifs:_comp_if()* {
                    make_for_if(Some(asy), f, tgt, i, iter, ifs)
            }
            / f:lit("for") tgt:star_targets() i:lit("in")
            iter:disjunction() ifs:_comp_if()* {
                make_for_if(None, f, tgt, i, iter, ifs)
            }

        rule _comp_if() -> CompIf<'a>
            = kw:lit("if") cond:disjunction() {
                make_comp_if(kw, cond)
            }

        rule arguments() -> Vec<Arg<'a>>
            = a:args() trail:comma()? &lit(")") {add_arguments_trailing_comma(a, trail)}

        rule args() -> Vec<Arg<'a>>
            = first:_posarg()
                rest:(c:comma() a:_posarg() {(c, a)})*
                kw:(c:comma() k:kwargs() {(c, k)})? {
                    let (trail, kw) = kw.map(|(x,y)| (Some(x), Some(y))).unwrap_or((None, None));
                    concat(
                        comma_separate(first, rest, trail),
                        kw.unwrap_or_default(),
                    )
            }
            / kwargs()

        rule _posarg() -> Arg<'a>
            = a:(starred_expression() / e:named_expression() { make_arg(e) })
                !lit("=") { a }

        rule starred_expression() -> Arg<'a>
            = star:lit("*") e:expression() { make_star_arg(star, e) }

        rule kwargs() -> Vec<Arg<'a>>
            = sfirst:kwarg_or_starred() srest:(c:comma() a:kwarg_or_starred() {(c,a)})*
                scomma:comma()
                dfirst:kwarg_or_double_starred()
                drest:(c:comma() a:kwarg_or_double_starred() {(c,a)})* {
                    concat(
                        comma_separate(sfirst, srest, Some(scomma)),
                        comma_separate(dfirst, drest, None),
                    )
            }
            / first:kwarg_or_starred()
                rest:(c:comma() a:kwarg_or_starred() {(c, a)})* {
                    comma_separate(first, rest, None)
            }
            / first:kwarg_or_double_starred()
                rest:(c:comma() a:kwarg_or_double_starred() {(c,a)})* {
                    comma_separate(first, rest, None)
            }

        rule kwarg_or_starred() -> Arg<'a>
            = _kwarg()
            / starred_expression()

        rule kwarg_or_double_starred() -> Arg<'a>
            = _kwarg()
            / star:lit("**") e:expression() { make_star_arg(star, e) }

        rule _kwarg() -> Arg<'a>
            = n:name() eq:lit("=") v:expression() {
                make_kwarg(n, eq, v)
            }

        rule atom() -> Expression<'a>
            = n:name() { Expression::Name(n) }
            / n:lit("True") { Expression::Name(make_name(n)) }
            / n:lit("False") { Expression::Name(make_name(n)) }
            / n:lit("None") { Expression::Name(make_name(n)) }
            / &(tok(STRING, "") / tok(FStringStart, "")) s:strings() {s.into()}
            / n:tok(Number, "NUMBER") { make_number(n) }
            / &lit("(") e:(tuple() / group() / (g:genexp() {Expression::GeneratorExp(g)})) {e}
            / &lit("[") e:(list() / listcomp()) {e}
            / &lit("{") e:(dict() / set() / dictcomp() / setcomp()) {e}
            / lit("...") { Expression::Ellipsis(Ellipsis {lpar: vec![], rpar: vec![]})}

        rule strings() -> String<'a>
            = s:(str:tok(STRING, "STRING") t:&_ {(make_string(str), t)}
                / str:fstring() t:&_ {(String::Formatted(str), t)})+ {
                make_strings(s)
            }

        rule tuple() -> Expression<'a>
            = lpar:lpar() first:star_named_expression() &lit(",")
                rest:(c:comma() e:star_named_expression() {(c, e)})*
                trailing_comma:comma()? rpar:rpar() {
                    Expression::Tuple(
                        make_tuple(first, rest, trailing_comma, Some(lpar), Some(rpar))
                    )
            }
            / lpar:lpar() rpar:lit(")") {
                Expression::Tuple(Tuple::default().with_parens(
                    lpar, RightParen { whitespace_before: Default::default(), rpar_tok: rpar }
                ))}

        rule group() -> Expression<'a>
            = lpar:lpar() e:(yield_expr() / named_expression()) rpar:rpar() {
                e.with_parens(lpar, rpar)
            }

        rule try_stmt() -> Try<'a>
            = kw:lit("try") lit(":") b:block() f:finally_block() {
                make_try(kw, b, vec![], None, Some(f))
            }
            / kw:lit("try") lit(":") b:block() ex:except_block()+ el:else_block()?
                f:finally_block()? {
                    make_try(kw, b, ex, el, f)
            }

        rule except_block() -> ExceptHandler<'a>
            = kw:lit("except") e:expression() a:(k:lit("as") n:name() {(k, n)})?
                col:lit(":") b:block() {
                    make_except(kw, Some(e), a, col, b)
            }
            / kw:lit("except") col:lit(":") b:block() {
                make_except(kw, None, None, col, b)
            }

        rule finally_block() -> Finally<'a>
            = kw:lit("finally") col:lit(":") b:block() {
                make_finally(kw, col, b)
            }

        rule return_stmt() -> Return<'a>
            = kw:lit("return") a:star_expressions()? {
                make_return(kw, a)
            }

        rule raise_stmt() -> Raise<'a>
            = kw:lit("raise") exc:expression()
                rest:(f:lit("from") cau:expression() {(f, cau)})? {
                    make_raise(kw, Some(exc), rest)
            }
            / kw:lit("raise") {
                make_raise(kw, None, None)
            }

        rule function_def() -> FunctionDef<'a>
            = d:decorators() f:function_def_raw() {f.with_decorators(d)}
            / function_def_raw()

        rule decorators() -> Vec<Decorator<'a>>
            = (at:lit("@") e:named_expression() nl:tok(NL, "NEWLINE") {
                make_decorator(at, e, nl)
                } )+

        rule _returns() -> Annotation<'a>
            = l:lit("->") e:expression() {
                make_annotation(l, e)
            }

        rule function_def_raw() -> FunctionDef<'a>
            = def:lit("def") n:name() op:lit("(") params:params()?
                cp:lit(")") ty:_returns()? c:lit(":") b:block() {
                    make_function_def(None, def, n, op, params, cp, ty, c, b)
            }
            / asy:tok(Async, "ASYNC") def:lit("def") n:name() op:lit("(") params:params()?
                cp:lit(")") ty:_returns()? c:lit(":") b:block() {
                    make_function_def(Some(asy), def, n, op, params, cp, ty, c, b)
            }

        rule params() -> Parameters<'a>
            = parameters()

        rule parameters() -> Parameters<'a>
            = a:slash_no_default() b:param_no_default()* c:param_with_default()*  d:star_etc()? {
                make_parameters(Some(a), concat(b, c), d)
            }
            / a:slash_with_default() b:param_with_default()* d:star_etc()? {
                make_parameters(Some(a), b, d)
            }
            / a:param_no_default()+ b:param_with_default()* d:star_etc()? {
                make_parameters(None, concat(a, b), d)
            }
            / a:param_with_default()+ d:star_etc()? {
                make_parameters(None, a, d)
            }
            / d:star_etc() {
                make_parameters(None, vec![], Some(d))
            }

        rule slash_no_default() -> (Vec<Param<'a>>, ParamSlash<'a>)
            = a:param_no_default()+ slash:lit("/") com:comma() {
                    (a, ParamSlash { comma: Some(com)})
            }
            / a:param_no_default()+ slash:lit("/") &lit(")") {
                (a, ParamSlash { comma: None })
            }

        rule slash_with_default() -> (Vec<Param<'a>>, ParamSlash<'a>)
            = a:param_no_default()* b:param_with_default()+ slash:lit("/") c:comma() {
                (concat(a, b), ParamSlash { comma: Some(c) })
            }
            / a:param_no_default()* b:param_with_default()+ slash:lit("/") &lit(")") {
                (concat(a, b), ParamSlash { comma: None })
            }

        rule star_etc() -> StarEtc<'a>
            = star:lit("*") a:param_no_default() b:param_maybe_default()* kw:kwds()? {
                StarEtc(Some(StarArg::Param(Box::new(
                    add_param_star(a, star)))), b, kw)
            }
            / lit("*") c:comma() b:param_maybe_default()+ kw:kwds()? {
                    StarEtc(Some(StarArg::Star(ParamStar {comma:c })), b, kw)
            }
            / kw:kwds() { StarEtc(None, vec![], Some(kw)) }

        rule kwds() -> Param<'a>
            = star:lit("**") a:param_no_default() {
                add_param_star(a, star)
            }

        rule param_no_default() -> Param<'a>
            = a:param() c:lit(",") { add_param_default(a, None, Some(c)) }
            / a:param() &lit(")") {a}

        rule param_with_default() -> Param<'a>
            = a:param() def:default() c:lit(",") {
                add_param_default(a, Some(def), Some(c))
            }
            / a:param() def:default() &lit(")") {
                add_param_default(a, Some(def), None)
            }

        rule param_maybe_default() -> Param<'a>
            = a:param() def:default()? c:lit(",") {
                add_param_default(a, def, Some(c))
            }
            / a:param() def:default()? &lit(")") {
                add_param_default(a, def, None)
            }

        rule param() -> Param<'a>
            = n:name() a:annotation()? {
                Param {name: n, annotation: a, ..Default::default() }
            }

        rule annotation() -> Annotation<'a>
            = col:lit(":") e:expression() {
                make_annotation(col, e)
            }

        rule default() -> (AssignEqual<'a>, Expression<'a>)
            = eq:lit("=") ex:expression() {
                (make_assign_equal(eq), ex)
            }

        rule if_stmt() -> If<'a>
            = i:lit("if") a:named_expression() col:lit(":") b:block() elif:elif_stmt() {
                make_if(i, a, col, b, Some(OrElse::Elif(elif)), false)
            }
            / i:lit("if") a:named_expression() col:lit(":") b:block() el:else_block()? {
                make_if(i, a, col, b, el.map(OrElse::Else), false)
            }

        rule elif_stmt() -> If<'a>
            = i:lit("elif") a:named_expression() col:lit(":") b:block() elif:elif_stmt() {
                make_if(i, a, col, b, Some(OrElse::Elif(elif)), true)
            }
            / i:lit("elif") a:named_expression() col:lit(":") b:block() el:else_block()? {
                make_if(i, a, col, b, el.map(OrElse::Else), true)
            }

        rule else_block() -> Else<'a>
            = el:lit("else") col:lit(":") b:block() {
                make_else(el, col, b)
            }

        rule while_stmt() -> While<'a>
            = kw:lit("while") test:named_expression() col:lit(":") b:block() el:else_block()? {
                make_while(kw, test, col, b, el)
            }

        rule for_stmt() -> For<'a>
            = f:lit("for") t:star_targets() i:lit("in") it:star_expressions()
                c:lit(":") b:block() el:else_block()? {
                    make_for(None, f, t, i, it, c, b, el)
            }
            / asy:tok(Async, "ASYNC") f:lit("for") t:star_targets() i:lit("in")
                it:star_expressions()
                c:lit(":") b:block() el:else_block()? {
                    make_for(Some(asy), f, t, i, it, c, b, el)
            }

        rule with_stmt() -> With<'a>
            = kw:lit("with") first:with_item() rest:(c:comma() i:with_item() {(c,i)})*
                col:lit(":") b:block() {
                    make_with(None, kw, comma_separate(first, rest, None), col, b)
            }
            / asy:tok(Async, "ASYNC") kw:lit("with") first:with_item()
                rest:(c:comma() i:with_item() {(c,i)})* col:lit(":") b:block() {
                    make_with(Some(asy), kw, comma_separate(first, rest, None), col, b)
            }

        rule with_item() -> WithItem<'a>
            = e:expression() a:lit("as") t:star_target() &(lit(",") / lit(":")) {
                make_with_item(e, Some(a), Some(t))
            }
            / e:expression() {
                make_with_item(e, None, None)
            }

        rule named_expression() -> Expression<'a>
            = a:name() op:lit(":=") b:expression() { todo!() }
            / e:expression() !lit(":=") { e }

        rule annotated_rhs() -> Expression<'a>
            = yield_expr() / star_expressions()

        rule global_stmt() -> Global<'a>
            = kw:lit("global") init:(n:name() c:comma() {(n, c)})* last:name() {
                make_global(kw, init, last)
            }

        rule nonlocal_stmt() -> Nonlocal<'a>
            = kw:lit("nonlocal") init:(n:name() c:comma() {(n, c)})* last:name() {
                make_nonlocal(kw, init, last)
            }

        rule yield_stmt() -> Expression<'a>
            = yield_expr()

        rule assert_stmt() -> Assert<'a>
            = kw:lit("assert") test:expression() rest:(c:comma() msg:expression() {(c, msg)})? {
                make_assert(kw, test, rest)
            }

        rule del_stmt() -> Del<'a>
            = kw:lit("del") t:del_target() &(lit(";") / tok(NL, "NEWLINE")) {
                make_del(kw, t)
            }
            / kw:lit("del") t:del_targets() &(lit(";") / tok(NL, "NEWLINE")) {
                make_del(kw, make_del_tuple(None, t, None))
            }

        rule import_name() -> Import<'a>
            = kw:lit("import") a:dotted_as_names() {
                make_import(kw, a)
            }

        rule import_from() -> ImportFrom<'a>
            = from:lit("from") dots:dot()* m:dotted_name()
                import:lit("import") als:import_from_targets() {
                    make_import_from(from, dots, Some(m), import, als)
            }
            / from:lit("from") dots:dot()+
                import:lit("import") als:import_from_targets() {
                    make_import_from(from, dots, None, import, als)
            }

        rule import_from_targets() -> ParenthesizedImportNames<'a>
            = lpar:lpar() als:import_from_as_names() c:comma()? rpar:rpar() {
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
            = n:name() asname:(kw:lit("as") z:name() {(kw, z)})? {
                make_import_alias(NameOrAttribute::N(n), asname)
            }

        rule dotted_as_names() -> Vec<ImportAlias<'a>>
            = init:(d:dotted_as_name() c:comma() {d.with_comma(c)})*
                last:dotted_as_name() {
                    concat(init, vec![last])
            }

        rule dotted_as_name() -> ImportAlias<'a>
            = n:dotted_name() asname:(kw:lit("as") z:name() {(kw, z)})? {
                make_import_alias(n, asname)
            }

        rule dotted_name() -> NameOrAttribute<'a>
            = first:name() tail:(dot:lit(".") n:name() {(dot, n)})* {
                make_name_or_attr(first, tail)
            }

        rule comma() -> Comma<'a>
            = c:lit(",") { make_comma(c) }

        rule dot() -> Dot<'a>
            = tok:lit(".") { make_dot(tok) }

        rule lpar() -> LeftParen<'a>
            = a:lit("(") { make_lpar(a) }

        rule rpar() -> RightParen<'a>
            = a:lit(")") { make_rpar(a) }

        rule lbrak() -> LeftSquareBracket<'a>
            = tok:lit("[") { make_left_bracket(tok) }

        rule rbrak() -> RightSquareBracket<'a>
            = tok:lit("]") { make_right_bracket(tok) }

        rule lbrace() -> LeftCurlyBrace<'a>
            = tok:lit("{") { make_left_brace(tok) }

        rule rbrace() -> RightCurlyBrace<'a>
            = tok:lit("}") { make_right_brace(tok) }

        /// matches any token, not just whitespace
        rule _() -> TokenRef<'a>
            = [t] { t }

        rule lit(lit: &'static str) -> TokenRef<'a>
            = [t] {? if t.borrow().string == lit { Ok(t) } else { Err(lit) } }

        rule tok(tok: TokType, err: &'static str) -> TokenRef<'a>
            = [t] {? if t.borrow().r#type == tok { Ok(t) } else { Err(err) } }

        rule name() -> Name<'a>
            = !( lit("False") / lit("None") / lit("True") / lit("and") / lit("as") / lit("assert") / lit("async") / lit("await")
                / lit("break") / lit("class") / lit("continue") / lit("def") / lit("del") / lit("elif") / lit("else")
                / lit("except") / lit("finally") / lit("for") / lit("from") / lit("global") / lit("if") / lit("import")
                / lit("in") / lit("is") / lit("lambda") / lit("nonlocal") / lit("not") / lit("or") / lit("pass") / lit("raise")
                / lit("return") / lit("try") / lit("while") / lit("with") / lit("yield")
            )
            t:tok(NameTok, "NAME") {make_name(t)}

        rule _async() -> TokenRef<'a>
            = tok(Async, "ASYNC")

        rule fstring() -> FormattedString<'a>
            = start:tok(FStringStart, "f\"")
                parts:(_f_string() / _f_replacement())*
                end:tok(FStringEnd, "\"") {
                    make_fstring(start.borrow().string, parts, end.borrow().string)
            }

        rule _f_string() -> FormattedStringContent<'a>
            = t:tok(FStringString, "f-string contents") {
                FormattedStringContent::Text(FormattedStringText { value: t.borrow().string })
            }

        rule _f_replacement() -> FormattedStringContent<'a>
            = lb:lit("{") e:_f_expr() eq:lit("=")?
                conv:(t:lit("!") c:_f_conversion() {(t,c)})?
                spec:(t:lit(":") s:_f_spec() {(t,s)})?
                rb:lit("}") {
                    FormattedStringContent::Expression(
                        make_fstring_expression(lb, e, eq, conv, spec, rb)
                    )
            }

        rule _f_expr() -> Expression<'a>
            = annotated_rhs()

        rule _f_conversion() -> &'a str
            = lit("r") {"r"} / lit("s") {"s"} / lit("a") {"a"}

        rule _f_spec() -> Vec<FormattedStringContent<'a>>
            = (_f_string() / _f_replacement())+

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
    async_tok: Option<TokenRef<'a>>,
    def_tok: TokenRef<'a>,
    name: Name<'a>,
    open_paren_tok: TokenRef<'a>,
    params: Option<Parameters<'a>>,
    close_paren_tok: TokenRef<'a>,
    returns: Option<Annotation<'a>>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
) -> FunctionDef<'a> {
    let asynchronous = async_tok.as_ref().map(|_| Asynchronous {
        whitespace_after: Default::default(),
    });
    FunctionDef {
        name,
        params: params.unwrap_or_default(),
        body,
        decorators: Default::default(),
        returns,
        asynchronous,
        leading_lines: Default::default(),
        lines_after_decorators: vec![],
        whitespace_after_def: Default::default(),
        whitespace_after_name: Default::default(),
        whitespace_before_colon: Default::default(),
        whitespace_before_params: Default::default(),
        async_tok,
        def_tok,
        open_paren_tok,
        close_paren_tok,
        colon_tok,
    }
}

fn make_decorator<'a>(
    at_tok: TokenRef<'a>,
    name: Expression<'a>,
    newline_tok: TokenRef<'a>,
) -> Decorator<'a> {
    Decorator {
        decorator: name,
        leading_lines: Default::default(),
        whitespace_after_at: Default::default(),
        trailing_whitespace: Default::default(),
        newline_tok,
        at_tok,
    }
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
    Expression::Comparison(Comparison {
        left: Box::new(head),
        comparisons,
        lpar: vec![],
        rpar: vec![],
    })
}

fn make_comparison_operator(tok: TokenRef) -> Result<CompOp> {
    let whitespace_before = Default::default();
    let whitespace_after = Default::default();
    let op = match tok.borrow().string {
        "<" => "<",
        ">" => ">",
        "<=" => "<=",
        ">=" => ">=",
        "==" => "==",
        "!=" => "!=",
        "in" => "in",
        "is" => "is",
        _ => return Err(ParserError::OperatorError),
    };

    match op {
        "<" => Ok(CompOp::LessThan {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        ">" => Ok(CompOp::GreaterThan {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "<=" => Ok(CompOp::LessThanEqual {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        ">=" => Ok(CompOp::GreaterThanEqual {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "==" => Ok(CompOp::Equal {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "!=" => Ok(CompOp::NotEqual {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "in" => Ok(CompOp::In {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "is" => Ok(CompOp::Is {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_comparison_operator_2<'a>(
    first: TokenRef<'a>,
    second: TokenRef<'a>,
) -> Result<'a, CompOp<'a>> {
    let whitespace_before = Default::default();
    let whitespace_between = Default::default();
    let whitespace_after = Default::default();

    let strs = match (first.borrow().string, second.borrow().string) {
        ("is", "not") => ("is", "not"),
        ("not", "in") => ("not", "in"),
        _ => return Err(ParserError::OperatorError),
    };

    match strs {
        ("is", "not") => Ok(CompOp::IsNot {
            whitespace_before,
            whitespace_between,
            whitespace_after,
            is_tok: first,
            not_tok: second,
        }),
        ("not", "in") => Ok(CompOp::NotIn {
            whitespace_before,
            whitespace_between,
            whitespace_after,
            not_tok: first,
            in_tok: second,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_boolean_op<'a>(
    head: Expression<'a>,
    tail: Vec<(TokenRef<'a>, Expression<'a>)>,
) -> Result<'a, Expression<'a>> {
    if tail.is_empty() {
        return Ok(head);
    }

    let mut expr = head;
    for (tok, right) in tail {
        expr = Expression::BooleanOperation(BooleanOperation {
            left: Box::new(expr),
            operator: make_boolean_operator(tok)?,
            right: Box::new(right),
            lpar: vec![],
            rpar: vec![],
        })
    }
    Ok(expr)
}

fn make_boolean_operator(tok: TokenRef) -> Result<BooleanOp> {
    let whitespace_before = Default::default();
    let whitespace_after = Default::default();
    let str = match tok.borrow().string {
        "and" => "and",
        "or" => "or",
        _ => return Err(ParserError::OperatorError),
    };
    match str {
        "and" => Ok(BooleanOp::And {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "or" => Ok(BooleanOp::Or {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_binary_op<'a>(
    left: Expression<'a>,
    op: TokenRef<'a>,
    right: Expression<'a>,
) -> Result<'a, Expression<'a>> {
    let operator = make_binary_operator(op)?;
    Ok(Expression::BinaryOperation(BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
        lpar: vec![],
        rpar: vec![],
    }))
}

fn make_binary_operator(tok: TokenRef) -> Result<BinaryOp> {
    let whitespace_before = Default::default();
    let whitespace_after = Default::default();

    let str = match tok.borrow().string {
        "+" => "+",
        "-" => "-",
        "*" => "*",
        "/" => "/",
        "//" => "//",
        "%" => "%",
        "**" => "**",
        "<<" => "<<",
        ">>" => ">>",
        "|" => "|",
        "&" => "&",
        "^" => "^",
        "@" => "@",
        _ => return Err(ParserError::OperatorError),
    };

    match str {
        "+" => Ok(BinaryOp::Add {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "-" => Ok(BinaryOp::Subtract {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "*" => Ok(BinaryOp::Multiply {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "/" => Ok(BinaryOp::Divide {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "//" => Ok(BinaryOp::FloorDivide {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "%" => Ok(BinaryOp::Modulo {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "**" => Ok(BinaryOp::Power {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "<<" => Ok(BinaryOp::LeftShift {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        ">>" => Ok(BinaryOp::RightShift {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "|" => Ok(BinaryOp::BitOr {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "&" => Ok(BinaryOp::BitAnd {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "^" => Ok(BinaryOp::BitXor {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        "@" => Ok(BinaryOp::MatrixMultiply {
            whitespace_after,
            whitespace_before,
            tok,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_unary_op<'a>(op: TokenRef<'a>, tail: Expression<'a>) -> Result<'a, Expression<'a>> {
    let operator = make_unary_operator(op)?;
    Ok(Expression::UnaryOperation(UnaryOperation {
        operator,
        expression: Box::new(tail),
        lpar: vec![],
        rpar: vec![],
    }))
}

fn make_unary_operator(tok: TokenRef) -> Result<UnaryOp> {
    let str = match tok.borrow().string {
        "+" => "+",
        "-" => "-",
        "~" => "~",
        "not" => "not",
        _ => return Err(ParserError::OperatorError),
    };

    match str {
        "+" => Ok(UnaryOp::Plus(Default::default(), tok)),
        "-" => Ok(UnaryOp::Minus(Default::default(), tok)),
        "~" => Ok(UnaryOp::BitInvert(Default::default(), tok)),
        "not" => Ok(UnaryOp::Not(Default::default(), tok)),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_number(num: TokenRef) -> Expression {
    Expression::Integer(Integer {
        value: num.borrow().string,
        lpar: vec![],
        rpar: vec![],
    })
}

fn make_indented_block<'a>(
    nl: TokenRef<'a>,
    indent: TokenRef<'a>,
    statements: Vec<Statement<'a>>,
    dedent: TokenRef<'a>,
) -> Suite<'a> {
    Suite::IndentedBlock(IndentedBlock {
        body: statements,
        header: Default::default(),
        indent: Default::default(),
        footer: Default::default(),
        newline_tok: nl,
        indent_tok: indent,
        dedent_tok: dedent,
    })
}

struct SimpleStatementParts<'a> {
    first: TokenRef<'a>, // The first token of the first statement. Used for its whitespace
    statements: Vec<(SmallStatement<'a>, TokenRef<'a>)>, // statement, semicolon pairs
    last_statement: SmallStatement<'a>,
    #[allow(dead_code)]
    last_semi: Option<TokenRef<'a>>,
    nl: TokenRef<'a>,
}

fn make_semicolon(tok: TokenRef) -> Semicolon {
    Semicolon {
        whitespace_before: Default::default(),
        whitespace_after: Default::default(),
        tok,
    }
}

fn _make_simple_statement(
    parts: SimpleStatementParts,
) -> (TokenRef, Vec<SmallStatement>, TokenRef) {
    let mut body = vec![];
    for (statement, semi) in parts.statements {
        body.push(statement.with_semicolon(Some(make_semicolon(semi))));
    }
    let mut last_statement = parts.last_statement;
    if let Some(semi) = parts.last_semi {
        // TODO: last semi only owns whitespace before
        last_statement = last_statement.with_semicolon(Some(make_semicolon(semi)));
    }
    body.push(last_statement);

    (parts.first, body, parts.nl)
}

fn make_simple_statement_suite(parts: SimpleStatementParts) -> Suite {
    let (first_tok, body, newline_tok) = _make_simple_statement(parts);

    Suite::SimpleStatementSuite(SimpleStatementSuite {
        body,
        leading_whitespace: Default::default(),
        trailing_whitespace: Default::default(),
        first_tok,
        newline_tok,
    })
}

fn make_simple_statement_line(parts: SimpleStatementParts) -> SimpleStatementLine {
    let (first_tok, body, newline_tok) = _make_simple_statement(parts);
    SimpleStatementLine {
        body,
        leading_lines: Default::default(),
        trailing_whitespace: Default::default(),
        first_tok,
        newline_tok,
    }
}

fn make_if<'a>(
    if_tok: TokenRef<'a>,
    cond: Expression<'a>,
    colon_tok: TokenRef<'a>,
    block: Suite<'a>,
    orelse: Option<OrElse<'a>>,
    is_elif: bool,
) -> If<'a> {
    If {
        leading_lines: Default::default(),
        whitespace_before_test: Default::default(),
        test: cond,
        whitespace_after_test: Default::default(),
        body: block,
        orelse: orelse.map(Box::new),
        is_elif,
        if_tok,
        colon_tok,
    }
}

fn make_else<'a>(else_tok: TokenRef<'a>, colon_tok: TokenRef<'a>, block: Suite<'a>) -> Else<'a> {
    Else {
        leading_lines: Default::default(),
        whitespace_before_colon: Default::default(),
        body: block,
        else_tok,
        colon_tok,
    }
}

struct StarEtc<'a>(Option<StarArg<'a>>, Vec<Param<'a>>, Option<Param<'a>>);

fn make_parameters<'a>(
    posonly: Option<(Vec<Param<'a>>, ParamSlash<'a>)>,
    params: Vec<Param<'a>>,
    star_etc: Option<StarEtc<'a>>,
) -> Parameters<'a> {
    let (posonly_params, posonly_ind) = match posonly {
        Some((a, b)) => (a, Some(b)),
        None => (vec![], None),
    };
    let (star_arg, kwonly_params, star_kwarg) = match star_etc {
        None => (None, vec![], None),
        Some(StarEtc(a, b, c)) => (a, b, c),
    };
    Parameters {
        params,
        star_arg,
        kwonly_params,
        star_kwarg,
        posonly_params,
        posonly_ind,
    }
}

fn add_param_default<'a>(
    param: Param<'a>,
    def: Option<(AssignEqual<'a>, Expression<'a>)>,
    comma_tok: Option<TokenRef<'a>>,
) -> Param<'a> {
    let comma = comma_tok.map(make_comma);

    let (equal, default) = match def {
        Some((a, b)) => (Some(a), Some(b)),
        None => (None, None),
    };
    Param {
        equal,
        default,
        comma,
        ..param
    }
}

fn add_param_star<'a>(param: Param<'a>, star: TokenRef<'a>) -> Param<'a> {
    let str = star.borrow().string;
    Param {
        star: Some(str),
        star_tok: Some(star),
        ..param
    }
}

fn make_assign_equal(tok: TokenRef) -> AssignEqual {
    AssignEqual {
        whitespace_before: Default::default(),
        whitespace_after: Default::default(),
        tok,
    }
}

fn make_comma(tok: TokenRef) -> Comma {
    Comma {
        whitespace_before: Default::default(),
        whitespace_after: Default::default(),
        tok,
    }
}

fn concat<T>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.into_iter().chain(b.into_iter()).collect()
}

fn make_name_or_attr<'a>(
    first_tok: Name<'a>,
    mut tail: Vec<(TokenRef<'a>, Name<'a>)>,
) -> NameOrAttribute<'a> {
    if let Some((dot, name)) = tail.pop() {
        let dot = make_dot(dot);
        return NameOrAttribute::A(Attribute {
            attr: name,
            dot,
            lpar: Default::default(),
            rpar: Default::default(),
            value: Box::new(make_name_or_attr(first_tok, tail).into()),
        });
    } else {
        NameOrAttribute::N(first_tok)
    }
}

fn make_name(tok: TokenRef) -> Name {
    Name {
        value: tok.borrow().string,
        ..Default::default()
    }
}

fn make_dot(tok: TokenRef) -> Dot {
    Dot {
        whitespace_before: Default::default(),
        whitespace_after: Default::default(),
        tok,
    }
}

fn make_import_alias<'a>(
    name: NameOrAttribute<'a>,
    asname: Option<(TokenRef<'a>, Name<'a>)>,
) -> ImportAlias<'a> {
    ImportAlias {
        name,
        asname: asname.map(|(x, y)| make_as_name(x, AssignTargetExpression::Name(y))),
        comma: None,
    }
}

fn make_as_name<'a>(as_tok: TokenRef<'a>, name: AssignTargetExpression<'a>) -> AsName<'a> {
    AsName {
        name,
        whitespace_before_as: Default::default(),
        whitespace_after_as: Default::default(),
        as_tok,
    }
}

type ParenthesizedImportNames<'a> = (
    Option<LeftParen<'a>>,
    ImportNames<'a>,
    Option<RightParen<'a>>,
);

fn make_import_from<'a>(
    from_tok: TokenRef<'a>,
    dots: Vec<Dot<'a>>,
    module: Option<NameOrAttribute<'a>>,
    import_tok: TokenRef<'a>,
    aliases: ParenthesizedImportNames<'a>,
) -> ImportFrom<'a> {
    let (lpar, names, rpar) = aliases;

    ImportFrom {
        module,
        names,
        relative: dots,
        lpar,
        rpar,
        semicolon: None,
        whitespace_after_from: Default::default(),
        whitespace_after_import: Default::default(),
        whitespace_before_import: Default::default(),
        from_tok,
        import_tok,
    }
}

fn make_import<'a>(import_tok: TokenRef<'a>, names: Vec<ImportAlias<'a>>) -> Import<'a> {
    Import {
        names,
        whitespace_after_import: Default::default(),
        semicolon: None,
        import_tok,
    }
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

fn make_lpar(tok: TokenRef) -> LeftParen {
    LeftParen {
        whitespace_after: Default::default(),
        lpar_tok: tok,
    }
}

fn make_rpar(tok: TokenRef) -> RightParen {
    RightParen {
        whitespace_before: Default::default(),
        rpar_tok: tok,
    }
}

fn make_module<'a>(body: Vec<Statement<'a>>, tok: TokenRef<'a>) -> Module<'a> {
    Module {
        body,
        header: Default::default(),
        footer: Default::default(),
        eof_tok: tok,
    }
}

fn make_attribute<'a>(value: Expression<'a>, dot: TokenRef<'a>, attr: Name<'a>) -> Attribute<'a> {
    let dot = make_dot(dot);
    Attribute {
        attr,
        dot,
        lpar: Default::default(),
        rpar: Default::default(),
        value: Box::new(value),
    }
}

fn make_starred_element<'a>(star_tok: TokenRef<'a>, rest: Element<'a>) -> StarredElement<'a> {
    let value = match rest {
        Element::Simple { value, .. } => value,
        _ => panic!("Internal error while making starred element"),
    };
    StarredElement {
        value: Box::new(value),
        whitespace_before_value: Default::default(),
        lpar: Default::default(),
        rpar: Default::default(),
        comma: Default::default(),
        star_tok,
    }
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
    lhs: Vec<(AssignTargetExpression<'a>, TokenRef<'a>)>,
    rhs: Expression<'a>,
) -> Assign<'a> {
    let mut targets = vec![];
    for (target, equal_tok) in lhs {
        targets.push(AssignTarget {
            target,
            whitespace_before_equal: Default::default(),
            whitespace_after_equal: Default::default(),
            equal_tok,
        });
    }
    Assign {
        targets,
        value: rhs,
        semicolon: Default::default(),
    }
}

fn expr_to_element(expr: Expression) -> Element {
    Element::Simple {
        value: expr,
        comma: Default::default(),
    }
}

fn make_tuple<'a>(
    first: Element<'a>,
    rest: Vec<(Comma<'a>, Element<'a>)>,
    trailing_comma: Option<Comma<'a>>,
    lpar: Option<LeftParen<'a>>,
    rpar: Option<RightParen<'a>>,
) -> Tuple<'a> {
    let elements = comma_separate(first, rest, trailing_comma);

    let lpar = lpar.map(|l| vec![l]).unwrap_or_default();
    let rpar = rpar.map(|r| vec![r]).unwrap_or_default();

    Tuple {
        elements,
        lpar,
        rpar,
    }
}

fn make_kwarg<'a>(name: Name<'a>, eq: TokenRef<'a>, value: Expression<'a>) -> Arg<'a> {
    let equal = Some(make_assign_equal(eq));
    let keyword = Some(name);
    Arg {
        value,
        keyword,
        equal,
        comma: None,
        star: "",
        whitespace_after_star: Default::default(),
        whitespace_after_arg: Default::default(),
        star_tok: None,
    }
}

fn make_star_arg<'a>(star: TokenRef<'a>, expr: Expression<'a>) -> Arg<'a> {
    let str = star.borrow().string;
    Arg {
        value: expr,
        keyword: None,
        equal: None,
        comma: None,
        star: str,
        whitespace_after_star: Default::default(),
        whitespace_after_arg: Default::default(),
        star_tok: Some(star),
    }
}

fn make_call<'a>(
    func: Expression<'a>,
    lpar_tok: TokenRef<'a>,
    args: Vec<Arg<'a>>,
    rpar_tok: TokenRef<'a>,
) -> Call<'a> {
    let lpar = vec![];
    let rpar = vec![];
    let func = Box::new(func);

    Call {
        func,
        args,
        lpar,
        rpar,
        whitespace_after_func: Default::default(),
        whitespace_before_args: Default::default(),
        lpar_tok,
        rpar_tok,
    }
}

fn make_genexp_call<'a>(func: Expression<'a>, mut genexp: GeneratorExp<'a>) -> Call<'a> {
    // func ( (genexp) )
    //      ^
    //   lpar_tok

    // lpar_tok is the same token that was used to parse genexp's first lpar.
    // Nothing owns the whitespace before lpar_tok, so the same token is passed in here
    // again, to be converted into whitespace_after_func. We then split off a pair of
    // parenthesis from genexp, since now Call will own them.

    let mut lpars = genexp.lpar.into_iter();
    let lpar_tok = lpars.next().expect("genexp without lpar").lpar_tok;
    genexp.lpar = lpars.collect();
    let rpar_tok = genexp.rpar.pop().expect("genexp without rpar").rpar_tok;

    Call {
        func: Box::new(func),
        args: vec![Arg {
            value: Expression::GeneratorExp(genexp),
            keyword: None,
            equal: None,
            comma: None,
            star: "",
            whitespace_after_star: Default::default(),
            whitespace_after_arg: Default::default(),
            star_tok: None,
        }],
        lpar: vec![],
        rpar: vec![],
        whitespace_after_func: Default::default(),
        whitespace_before_args: Default::default(),
        lpar_tok,
        rpar_tok,
    }
}

fn make_arg(expr: Expression) -> Arg {
    Arg {
        value: expr,
        keyword: Default::default(),
        equal: Default::default(),
        comma: Default::default(),
        star: Default::default(),
        whitespace_after_star: Default::default(),
        whitespace_after_arg: Default::default(),
        star_tok: None,
    }
}

fn make_comp_if<'a>(if_tok: TokenRef<'a>, test: Expression<'a>) -> CompIf<'a> {
    CompIf {
        test,
        whitespace_before: Default::default(),
        whitespace_before_test: Default::default(),
        if_tok,
    }
}

fn make_for_if<'a>(
    async_tok: Option<TokenRef<'a>>,
    for_tok: TokenRef<'a>,
    target: AssignTargetExpression<'a>,
    in_tok: TokenRef<'a>,
    iter: Expression<'a>,
    ifs: Vec<CompIf<'a>>,
) -> CompFor<'a> {
    let inner_for_in = None;
    let asynchronous = async_tok.as_ref().map(|_| Asynchronous {
        whitespace_after: Default::default(),
    });

    CompFor {
        target,
        iter,
        ifs,
        inner_for_in,
        asynchronous,
        whitespace_before: Default::default(),
        whitespace_after_for: Default::default(),
        whitespace_before_in: Default::default(),
        whitespace_after_in: Default::default(),
        async_tok,
        for_tok,
        in_tok,
    }
}

fn make_genexp<'a>(
    lpar: LeftParen<'a>,
    elt: Expression<'a>,
    for_in: CompFor<'a>,
    rpar: RightParen<'a>,
) -> GeneratorExp<'a> {
    let lpar = vec![lpar];
    let rpar = vec![rpar];

    GeneratorExp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lpar,
        rpar,
    }
}

fn merge_comp_fors(comp_fors: Vec<CompFor>) -> CompFor {
    let mut it = comp_fors.into_iter().rev();
    let first = it.next().expect("cant merge empty comp_fors");

    it.fold(first, |acc, curr| CompFor {
        inner_for_in: Some(Box::new(acc)),
        ..curr
    })
}

fn make_left_bracket(tok: TokenRef) -> LeftSquareBracket {
    LeftSquareBracket {
        whitespace_after: Default::default(),
        tok,
    }
}

fn make_right_bracket(tok: TokenRef) -> RightSquareBracket {
    RightSquareBracket {
        whitespace_before: Default::default(),
        tok,
    }
}

fn make_left_brace(tok: TokenRef) -> LeftCurlyBrace {
    LeftCurlyBrace {
        whitespace_after: Default::default(),
        tok,
    }
}

fn make_right_brace(tok: TokenRef) -> RightCurlyBrace {
    RightCurlyBrace {
        whitespace_before: Default::default(),
        tok,
    }
}

fn make_list_comp<'a>(
    lbracket: LeftSquareBracket<'a>,
    elt: Expression<'a>,
    for_in: CompFor<'a>,
    rbracket: RightSquareBracket<'a>,
) -> ListComp<'a> {
    ListComp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_set_comp<'a>(
    lbrace: LeftCurlyBrace<'a>,
    elt: Expression<'a>,
    for_in: CompFor<'a>,
    rbrace: RightCurlyBrace<'a>,
) -> SetComp<'a> {
    SetComp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_dict_comp<'a>(
    lbrace: LeftCurlyBrace<'a>,
    kvpair: (Expression<'a>, TokenRef<'a>, Expression<'a>),
    for_in: CompFor<'a>,
    rbrace: RightCurlyBrace<'a>,
) -> DictComp<'a> {
    let (key, colon_tok, value) = kvpair;

    DictComp {
        key: Box::new(key),
        value: Box::new(value),
        for_in: Box::new(for_in),
        lbrace,
        rbrace,
        lpar: vec![],
        rpar: vec![],
        whitespace_before_colon: Default::default(),
        whitespace_after_colon: Default::default(),
        colon_tok,
    }
}

fn make_list<'a>(
    lbracket: LeftSquareBracket<'a>,
    elements: Vec<Element<'a>>,
    rbracket: RightSquareBracket<'a>,
) -> List<'a> {
    List {
        elements,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_set<'a>(
    lbrace: LeftCurlyBrace<'a>,
    elements: Vec<Element<'a>>,
    rbrace: RightCurlyBrace<'a>,
) -> Set<'a> {
    Set {
        elements,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    }
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
    if let Some(comma) = trailing_comma {
        current = current.with_comma(comma);
    }
    elements.push(current);
    elements
}

fn make_dict<'a>(
    lbrace: LeftCurlyBrace<'a>,
    elements: Vec<DictElement<'a>>,
    rbrace: RightCurlyBrace<'a>,
) -> Dict<'a> {
    Dict {
        elements,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    }
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

fn make_dict_element<'a>(el: (Expression<'a>, TokenRef<'a>, Expression<'a>)) -> DictElement<'a> {
    let (key, colon_tok, value) = el;
    DictElement::Simple {
        key,
        value,
        comma: Default::default(),
        whitespace_before_colon: Default::default(),
        whitespace_after_colon: Default::default(),
        colon_tok,
    }
}

fn make_double_starred_element<'a>(
    star_tok: TokenRef<'a>,
    value: Expression<'a>,
) -> DoubleStarredElement<'a> {
    DoubleStarredElement {
        value,
        comma: Default::default(),
        whitespace_before_value: Default::default(),
        star_tok,
    }
}

fn make_index(value: Expression) -> BaseSlice {
    BaseSlice::Index(Index { value })
}

fn make_colon(tok: TokenRef) -> Colon {
    let whitespace_before = Default::default();
    let whitespace_after = Default::default();
    Colon {
        whitespace_before,
        whitespace_after,
        tok,
    }
}

fn make_slice<'a>(
    lower: Option<Expression<'a>>,
    first_colon: TokenRef<'a>,
    upper: Option<Expression<'a>>,
    rest: Option<(TokenRef<'a>, Option<Expression<'a>>)>,
) -> BaseSlice<'a> {
    let first_colon = make_colon(first_colon);
    let (second_colon, step) = if let Some((tok, step)) = rest {
        (Some(make_colon(tok)), step)
    } else {
        (None, None)
    };
    BaseSlice::Slice(Slice {
        lower,
        upper,
        step,
        first_colon,
        second_colon,
    })
}

fn make_slices<'a>(
    first: BaseSlice<'a>,
    rest: Vec<(Comma<'a>, BaseSlice<'a>)>,
    trailing_comma: Option<Comma<'a>>,
) -> Vec<SubscriptElement<'a>> {
    let mut elements = vec![];
    let mut current = first;
    for (comma, next) in rest {
        elements.push(SubscriptElement {
            slice: current,
            comma: Some(comma),
        });
        current = next;
    }
    elements.push(SubscriptElement {
        slice: current,
        comma: trailing_comma,
    });
    elements
}

fn make_subscript<'a>(
    value: Expression<'a>,
    lbracket: LeftSquareBracket<'a>,
    slice: Vec<SubscriptElement<'a>>,
    rbracket: RightSquareBracket<'a>,
) -> Subscript<'a> {
    let lbracket_tok = lbracket.tok.clone();
    Subscript {
        value: Box::new(value),
        slice,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
        whitespace_after_value: Default::default(),
        lbracket_tok,
    }
}

fn make_ifexp<'a>(
    body: Expression<'a>,
    if_tok: TokenRef<'a>,
    test: Expression<'a>,
    else_tok: TokenRef<'a>,
    orelse: Expression<'a>,
) -> IfExp<'a> {
    IfExp {
        test: Box::new(test),
        body: Box::new(body),
        orelse: Box::new(orelse),
        lpar: Default::default(),
        rpar: Default::default(),
        whitespace_before_if: Default::default(),
        whitespace_after_if: Default::default(),
        whitespace_before_else: Default::default(),
        whitespace_after_else: Default::default(),
        if_tok,
        else_tok,
    }
}

fn add_arguments_trailing_comma<'a>(
    mut args: Vec<Arg<'a>>,
    trailing_comma: Option<Comma<'a>>,
) -> Vec<Arg<'a>> {
    if let Some(comma) = trailing_comma {
        let last = args.pop().unwrap();
        args.push(last.with_comma(comma));
    }
    args
}

fn make_lambda<'a>(
    lambda_tok: TokenRef<'a>,
    params: Parameters<'a>,
    colon_tok: TokenRef<'a>,
    expr: Expression<'a>,
) -> Lambda<'a> {
    let colon = make_colon(colon_tok);
    Lambda {
        params: Box::new(params),
        body: Box::new(expr),
        colon,
        lpar: Default::default(),
        rpar: Default::default(),
        whitespace_after_lambda: Default::default(),
        lambda_tok,
    }
}

fn make_annotation<'a>(tok: TokenRef<'a>, ann: Expression<'a>) -> Annotation<'a> {
    Annotation {
        annotation: ann,
        whitespace_before_indicator: Default::default(),
        whitespace_after_indicator: Default::default(),
        tok,
    }
}

fn make_ann_assignment<'a>(
    target: AssignTargetExpression<'a>,
    col: TokenRef<'a>,
    ann: Expression<'a>,
    rhs: Option<(TokenRef<'a>, Expression<'a>)>,
) -> AnnAssign<'a> {
    let annotation = make_annotation(col, ann);
    let (eq, value) = rhs.map(|(x, y)| (Some(x), Some(y))).unwrap_or((None, None));
    let equal = eq.map(make_assign_equal);
    AnnAssign {
        target,
        annotation,
        value,
        equal,
        semicolon: None,
    }
}

fn make_yield<'a>(
    yield_tok: TokenRef<'a>,
    f: Option<TokenRef<'a>>,
    e: Option<Expression<'a>>,
) -> Yield<'a> {
    let value = match (f, e) {
        (None, None) => None,
        (Some(f), Some(e)) => Some(YieldValue::From(make_from(f, e))),
        (None, Some(e)) => Some(YieldValue::Expression(e)),
        _ => panic!("yield from without expression"),
    };
    Yield {
        value: value.map(Box::new),
        lpar: Default::default(),
        rpar: Default::default(),
        whitespace_after_yield: Default::default(),
        yield_tok,
    }
}

fn make_from<'a>(tok: TokenRef<'a>, e: Expression<'a>) -> From<'a> {
    From {
        item: e,
        whitespace_before_from: Default::default(),
        whitespace_after_from: Default::default(),
        tok,
    }
}

fn make_return<'a>(return_tok: TokenRef<'a>, value: Option<Expression<'a>>) -> Return<'a> {
    Return {
        value,
        whitespace_after_return: Default::default(),
        semicolon: Default::default(),
        return_tok,
    }
}

fn make_assert<'a>(
    assert_tok: TokenRef<'a>,
    test: Expression<'a>,
    rest: Option<(Comma<'a>, Expression<'a>)>,
) -> Assert<'a> {
    let (comma, msg) = if let Some((c, msg)) = rest {
        (Some(c), Some(msg))
    } else {
        (None, None)
    };

    Assert {
        test,
        msg,
        comma,
        whitespace_after_assert: Default::default(),
        semicolon: Default::default(),
        assert_tok,
    }
}

fn make_raise<'a>(
    raise_tok: TokenRef<'a>,
    exc: Option<Expression<'a>>,
    rest: Option<(TokenRef<'a>, Expression<'a>)>,
) -> Raise<'a> {
    let cause = rest.map(|(t, e)| make_from(t, e));

    Raise {
        exc,
        cause,
        whitespace_after_raise: Default::default(),
        semicolon: Default::default(),
        raise_tok,
    }
}

fn make_global<'a>(
    tok: TokenRef<'a>,
    init: Vec<(Name<'a>, Comma<'a>)>,
    last: Name<'a>,
) -> Global<'a> {
    let mut names: Vec<NameItem<'a>> = init
        .into_iter()
        .map(|(name, c)| NameItem {
            name,
            comma: Some(c),
        })
        .collect();
    names.push(NameItem {
        name: last,
        comma: None,
    });
    Global {
        names,
        whitespace_after_global: Default::default(),
        semicolon: Default::default(),
        tok,
    }
}

fn make_nonlocal<'a>(
    tok: TokenRef<'a>,
    init: Vec<(Name<'a>, Comma<'a>)>,
    last: Name<'a>,
) -> Nonlocal<'a> {
    let mut names: Vec<NameItem<'a>> = init
        .into_iter()
        .map(|(name, c)| NameItem {
            name,
            comma: Some(c),
        })
        .collect();
    names.push(NameItem {
        name: last,
        comma: None,
    });
    Nonlocal {
        names,
        whitespace_after_nonlocal: Default::default(),
        semicolon: Default::default(),
        tok,
    }
}

#[allow(clippy::too_many_arguments)]
fn make_for<'a>(
    async_tok: Option<TokenRef<'a>>,
    for_tok: TokenRef<'a>,
    target: AssignTargetExpression<'a>,
    in_tok: TokenRef<'a>,
    iter: Expression<'a>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
    orelse: Option<Else<'a>>,
) -> For<'a> {
    let asynchronous = async_tok.as_ref().map(|_| Asynchronous {
        whitespace_after: Default::default(),
    });

    For {
        target,
        iter,
        body,
        orelse,
        asynchronous,
        leading_lines: Default::default(),
        whitespace_after_for: Default::default(),
        whitespace_before_in: Default::default(),
        whitespace_after_in: Default::default(),
        whitespace_before_colon: Default::default(),
        async_tok,
        for_tok,
        in_tok,
        colon_tok,
    }
}

fn make_while<'a>(
    while_tok: TokenRef<'a>,
    test: Expression<'a>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
    orelse: Option<Else<'a>>,
) -> While<'a> {
    While {
        test,
        body,
        orelse,
        leading_lines: Default::default(),
        whitespace_after_while: Default::default(),
        whitespace_before_colon: Default::default(),
        while_tok,
        colon_tok,
    }
}

fn make_await<'a>(await_tok: TokenRef<'a>, expression: Expression<'a>) -> Await<'a> {
    Await {
        expression: Box::new(expression),
        lpar: Default::default(),
        rpar: Default::default(),
        whitespace_after_await: Default::default(),
        await_tok,
    }
}

fn make_class_def<'a>(
    class_tok: TokenRef<'a>,
    name: Name<'a>,
    args: Option<(LeftParen<'a>, Option<Vec<Arg<'a>>>, RightParen<'a>)>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
) -> ClassDef<'a> {
    let mut bases = vec![];
    let mut keywords = vec![];
    let mut parens_tok = None;
    let mut lpar = None;
    let mut rpar = None;

    if let Some((lpar_, args, rpar_)) = args {
        parens_tok = Some((lpar_.lpar_tok.clone(), rpar_.rpar_tok.clone()));
        lpar = Some(lpar_);
        rpar = Some(rpar_);
        if let Some(args) = args {
            let mut current_arg = &mut bases;
            for arg in args {
                if arg.star == "**" || arg.keyword.is_some() {
                    current_arg = &mut keywords;
                }
                // TODO: libcst-python does validation here
                current_arg.push(arg);
            }
        }
    }
    ClassDef {
        name,
        body,
        bases,
        keywords,
        decorators: vec![],
        lpar,
        rpar,
        leading_lines: Default::default(),
        lines_after_decorators: Default::default(),
        whitespace_after_class: Default::default(),
        whitespace_after_name: Default::default(),
        whitespace_before_colon: Default::default(),
        class_tok,
        parens_tok,
        colon_tok,
    }
}

fn make_string(tok: TokenRef) -> String {
    String::Simple(SimpleString {
        value: tok.borrow().string,
        ..Default::default()
    })
}

fn make_strings<'a>(s: Vec<(String<'a>, TokenRef<'a>)>) -> String<'a> {
    let mut strings = s.into_iter().rev();
    let (first, _) = strings.next().expect("no strings to make a string of");
    strings.fold(first, |acc, (str, tok)| {
        let ret: String<'a> = String::Concatenated(ConcatenatedString {
            left: Box::new(str),
            right: Box::new(acc),
            whitespace_between: Default::default(),
            lpar: Default::default(),
            rpar: Default::default(),
            right_tok: tok,
        });
        ret
    })
}

fn make_fstring_expression<'a>(
    lbrace_tok: TokenRef<'a>,
    expression: Expression<'a>,
    eq: Option<TokenRef<'a>>,
    conversion_pair: Option<(TokenRef<'a>, &'a str)>,
    format_pair: Option<(TokenRef<'a>, Vec<FormattedStringContent<'a>>)>,
    rbrace_tok: TokenRef<'a>,
) -> FormattedStringExpression<'a> {
    let equal = eq.map(make_assign_equal);
    let (conversion_tok, conversion) = if let Some((t, c)) = conversion_pair {
        (Some(t), Some(c))
    } else {
        (None, None)
    };
    let (format_tok, format_spec) = if let Some((t, f)) = format_pair {
        (Some(t), Some(f))
    } else {
        (None, None)
    };
    let after_expr_tok = if equal.is_some() {
        None
    } else if let Some(tok) = conversion_tok {
        Some(tok)
    } else if let Some(tok) = format_tok {
        Some(tok)
    } else {
        Some(rbrace_tok)
    };

    FormattedStringExpression {
        expression,
        conversion,
        format_spec,
        whitespace_before_expression: Default::default(),
        whitespace_after_expression: Default::default(),
        equal,
        lbrace_tok,
        after_expr_tok,
    }
}

fn make_fstring<'a>(
    start: &'a str,
    parts: Vec<FormattedStringContent<'a>>,
    end: &'a str,
) -> FormattedString<'a> {
    FormattedString {
        start,
        parts,
        end,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_finally<'a>(
    finally_tok: TokenRef<'a>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
) -> Finally<'a> {
    Finally {
        body,
        leading_lines: Default::default(),
        whitespace_before_colon: Default::default(),
        finally_tok,
        colon_tok,
    }
}

fn make_except<'a>(
    except_tok: TokenRef<'a>,
    exp: Option<Expression<'a>>,
    as_: Option<(TokenRef<'a>, Name<'a>)>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
) -> ExceptHandler<'a> {
    // TODO: AsName should come from outside
    let name = as_.map(|(x, y)| make_as_name(x, AssignTargetExpression::Name(y)));
    ExceptHandler {
        body,
        r#type: exp,
        name,
        leading_lines: Default::default(),
        whitespace_after_except: Default::default(),
        whitespace_before_colon: Default::default(),
        except_tok,
        colon_tok,
    }
}

fn make_try<'a>(
    try_tok: TokenRef<'a>,
    body: Suite<'a>,
    handlers: Vec<ExceptHandler<'a>>,
    orelse: Option<Else<'a>>,
    finalbody: Option<Finally<'a>>,
) -> Try<'a> {
    Try {
        body,
        handlers,
        orelse,
        finalbody,
        leading_lines: Default::default(),
        whitespace_before_colon: Default::default(),
        try_tok,
    }
}

fn make_aug_op(tok: TokenRef) -> Result<AugOp> {
    let whitespace_before = Default::default();
    let whitespace_after = Default::default();
    let str = tok.borrow().string;

    Ok(match str {
        "+=" => AugOp::AddAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "-=" => AugOp::SubtractAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "*=" => AugOp::MultiplyAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "@=" => AugOp::MatrixMultiplyAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "/=" => AugOp::DivideAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "%=" => AugOp::ModuloAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "&=" => AugOp::BitAndAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "|=" => AugOp::BitOrAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "^=" => AugOp::BitXorAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "<<=" => AugOp::LeftShiftAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        ">>=" => AugOp::RightShiftAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "**=" => AugOp::PowerAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        "//=" => AugOp::FloorDivideAssign {
            whitespace_before,
            whitespace_after,
            tok,
        },
        _ => return Err(ParserError::OperatorError),
    })
}

fn make_aug_assign<'a>(
    target: AssignTargetExpression<'a>,
    operator: AugOp<'a>,
    value: Expression<'a>,
) -> AugAssign<'a> {
    AugAssign {
        target,
        operator,
        value,
        semicolon: Default::default(),
    }
}

fn make_with_item<'a>(
    item: Expression<'a>,
    as_: Option<TokenRef<'a>>,
    n: Option<AssignTargetExpression<'a>>,
) -> WithItem<'a> {
    let asname = match (as_, n) {
        (Some(as_), Some(n)) => Some(make_as_name(as_, n)),
        (None, None) => None,
        _ => panic!("as and name should be present or missing together"),
    };
    WithItem {
        item,
        asname,
        comma: Default::default(),
    }
}

fn make_with<'a>(
    async_tok: Option<TokenRef<'a>>,
    with_tok: TokenRef<'a>,
    items: Vec<WithItem<'a>>,
    colon_tok: TokenRef<'a>,
    body: Suite<'a>,
) -> With<'a> {
    let asynchronous = async_tok.as_ref().map(|_| Asynchronous {
        whitespace_after: Default::default(),
    });
    With {
        items,
        body,
        asynchronous,
        leading_lines: Default::default(),
        whitespace_after_with: Default::default(),
        whitespace_before_colon: Default::default(),
        async_tok,
        with_tok,
        colon_tok,
    }
}

fn make_del<'a>(tok: TokenRef<'a>, target: DelTargetExpression<'a>) -> Del<'a> {
    Del {
        target,
        whitespace_after_del: Default::default(),
        semicolon: Default::default(),
        tok,
    }
}

fn make_del_tuple<'a>(
    lpar: Option<LeftParen<'a>>,
    elements: Vec<Element<'a>>,
    rpar: Option<RightParen<'a>>,
) -> DelTargetExpression<'a> {
    DelTargetExpression::Tuple(Tuple {
        elements,
        lpar: lpar.map(|x| vec![x]).unwrap_or_default(),
        rpar: rpar.map(|x| vec![x]).unwrap_or_default(),
    })
}
