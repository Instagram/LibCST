// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use std::iter;

macro_rules! lit {
    ($e:literal) => {
        Token { string: $e, .. }
    };
}

macro_rules! tok {
    ($e:ident) => {
        Token {
            r#type: TokType::$e,
            ..
        }
    };
}

#[derive(Debug)]
pub struct TokVec<'a>(Vec<Token<'a>>);

impl<'a> Into<TokVec<'a>> for Vec<Token<'a>> {
    fn into(self) -> TokVec<'a> {
        TokVec(self)
    }
}

impl<'a> Parse for TokVec<'a> {
    type PositionRepr = usize;

    fn start(&self) -> usize {
        0
    }

    fn is_eof(&self, pos: usize) -> bool {
        pos >= self.0.len()
    }

    fn position_repr(&self, pos: usize) -> Self::PositionRepr {
        pos
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

peg::parser! {
    pub grammar python<'a>(config: &Config<'a>) for TokVec<'a> {
        pub rule file() -> Module<'a>
            = s:statements() [tok!(EndMarker)] { Module { body: s } }

        pub rule statements() -> Vec<Statement<'a>>
            = s:statement()+ {s.into_iter().flatten().collect()}

        pub rule statement() -> Vec<Statement<'a>>
            = c:compound_stmt() {vec![c]}
            / simple_stmt()


        rule simple_stmt() -> Vec<Statement<'a>>
            = init:(s:small_stmt() [semi@lit!(";")] { Statement::SmallStatement(s) })+
            s:small_stmt() semi:mb_lit(";") {
                init.into_iter().chain(
                    iter::once(Statement::SmallStatement(s))
                ).collect()
            }

        rule compound_stmt() -> Statement<'a>
            = &("def" / "@" / [Async]) f:function_def() { Statement::FunctionDef(f) }

        rule small_stmt() -> SmallStatement<'a>
            = e:star_expressions() { SmallStatement::Expr { value: e, semicolon: None } }
            / "pass" { SmallStatement::Pass { semicolon: None } }

        rule star_expressions() -> Expression<'a>
            = star_expression()

        rule star_expression() -> Expression<'a>
            = // TODO lit!("*") bitwise_or()
            expression()

        rule expression() -> Expression<'a>
            = disjunction()

        rule disjunction() -> Expression<'a>
            = a:conjunction() b:([or@lit!("or")] inner:conjunction() { (or, inner) })+ {?
                make_binary_op(&config, a, b).map_err(|e| "expected disjunction")
            }
            / conjunction()

        rule conjunction() -> Expression<'a>
            = a:inversion() b:([and@lit!("and")] inner:inversion() { (and, inner) })+ {?
                make_binary_op(&config, a, b).map_err(|e| "expected conjunction")
            }
            / inversion()

        rule inversion() -> Expression<'a>
            = [not@lit!("not")] a:inversion() {?
                make_unary_op(&config, not, a).map_err(|e| "expected inversion")
            }
            / comparison()

        rule comparison() -> Expression<'a>
            = a:bitwise_or() b:compare_op_bitwise_or_pair()+ {?
                make_comparison(&config, a, b).map_err(|e| "expected comparison")
            }
            / bitwise_or()

        rule compare_op_bitwise_or_pair() -> (Token<'a>, Expression<'a>)
            = eq_bitwise_or()
            / noteq_bitwise_or()

        rule eq_bitwise_or() -> (Token<'a>, Expression<'a>)
            = [op@lit!("==")] e:bitwise_or() { (op, e) }

        rule noteq_bitwise_or() -> (Token<'a>, Expression<'a>)
            = [op@lit!("!=")] e:bitwise_or() { (op, e) } // TODO: support barry_as_flufl

        rule bitwise_or() -> Expression<'a>
            // TODO left-recursive grammar
            = a:bitwise_xor() tail:([op@lit!("|")] b:bitwise_xor() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected bitwise_or")
            }
            / bitwise_xor()

        rule bitwise_xor() -> Expression<'a>
            // TODO left-recursive grammar
            = a:bitwise_and() tail:([op@lit!("^")] b:bitwise_and() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected bitwise_xor")
            }
            / bitwise_and()

        rule bitwise_and() -> Expression<'a>
            // TODO left-recursive grammar
            = a:shift_expr() tail:([op@lit!("&")] b:shift_expr() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected bitwise_and")
            }
            / shift_expr()

        rule shift_expr() -> Expression<'a>
            // TODO left-recursive grammar
            = a:sum() tail:([op@lit!("<<")] b:shift_expr() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected shift_expr")
            }
            / a:sum() tail:([op@lit!(">>")] b:shift_expr() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected shift_expr")
            }
            / sum()

        rule sum() -> Expression<'a>
            // TODO left-recursive grammar
            = a:term() tail:([op@lit!("+")] b:term() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected sum")
            }
            / a:term() tail:([op@lit!("-")] b:term() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected sum")
            }
            / term()

        rule term() -> Expression<'a>
            // TODO left-recursive grammar
            = a:factor() tail:([op@lit!("*")] b:factor() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected term")
            }
            / a:factor() tail:([op@lit!("/")] b:factor() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected term")
            }
            / a:factor() tail:([op@lit!("//")] b:factor() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected term")
            }
            / a:factor() tail:([op@lit!("%")] b:factor() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected term")
            }
            / a:factor() tail:([op@lit!("@")] b:factor() {(op, b)})+ {?
                make_binary_op(&config, a, tail).map_err(|e| "expected term")
            }
            / factor()

        rule factor() -> Expression<'a>
            = [op@lit!("+")] a:factor() {?
                make_unary_op(&config, op, a).map_err(|e| "expected factor")
            }
            / [op@lit!("-")] a:factor() {?
                make_unary_op(&config, op, a).map_err(|e| "expected factor")
            }
            / [op@lit!("~")] a:factor() {?
                make_unary_op(&config, op, a).map_err(|e| "expected factor")
            }
            / power()

        rule power() -> Expression<'a>
            = a:await_primary() [op@lit!("**")] b:factor() {?
                make_binary_op(&config, a, vec![(op, b)]).map_err(|e| "expected power")
            }
            / await_primary()

        rule await_primary() -> Expression<'a>
            // TODO: await expressions
            = primary()

        rule primary() -> Expression<'a>
            = atom()
            // TODO: missing left-recursive branches here

        rule atom() -> Expression<'a>
            = [n@lit!("name")] { Expression::Name(Name { value: n.string, lpar: vec![], rpar: vec![] }) }
            / &[tok!(String)] s:strings() {s}
            / [n@tok!(Number)] {? make_number(&config, n).map_err(|e| "expected number")}
            / [lit!("...")] { Expression::Ellipsis {lpar: vec![], rpar: vec![]}}

        rule strings() -> Expression<'a>
            = [s@tok!(String)] {
                Expression::SimpleString { value: s.string, lpar: vec![], rpar: vec![]}
            }

        rule function_def() -> FunctionDef<'a>
            = d:decorators() f:function_def_raw() {f.with_decorators(d)}
            / function_def_raw()

        rule decorators() -> Vec<Decorator<'a>>
            = ([at@lit!("@")] [name@tok!(Name)] [tok!(Newline)] {? make_decorator(&config, at, name).map_err(|e| "expected decorator")} )+

        rule function_def_raw() -> FunctionDef<'a>
            = [def@lit!("def")] [n@tok!(Name)] [op@lit!("(")] params:params()? [cp@lit!(")")] [c@lit!(":")] "..." {? make_function_def(&config, def, n, op, params, cp, c).map_err(|e| "ohno" )}

        rule params() -> Parameters<'a>
            = parameters()

        rule parameters() -> Parameters<'a>
            = a:slash_no_default() b:param_no_default()* // c:param_with_default()* d:star_etc()? {}
            { Parameters { params: {a.into_iter().chain(b.into_iter()).collect()}} }
            / a:param_no_default()+ { Parameters {params: a}}

        rule slash_no_default() -> Vec<Param<'a>>
            = a:param_no_default()+ "/" "," { a }
            / a:param_no_default()+ "/" &")" { a }

        rule param_no_default() -> Param<'a>
            = a:param() "," {a}
            / a:param() &")" {a}

        rule param() -> Param<'a>
            = [n@tok!(Name)] { Param {name: Name {value: n.string, ..Default::default()}, whitespace_after_param: SimpleWhitespace(""), whitespace_after_star: SimpleWhitespace("")}}


        rule mb_lit(lit: &str) -> Option<Token<'a>>
            = [t@Token {..}] {? match &t {
                                Token { string: lit, ..} => Ok(Some(t)),
                                _ => Err("no")
                              } }
            / &[_]? { None }

    }
}

fn make_function_def<'a>(
    config: &Config<'a>,
    mut def: Token<'a>,
    mut name: Token<'a>,
    open_paren: Token<'a>,
    params: Option<Parameters<'a>>,
    close_paren: Token<'a>,
    mut colon: Token<'a>,
) -> Result<'a, FunctionDef<'a>> {
    Ok(FunctionDef {
        name: Name {
            value: name.string,
            ..Default::default()
        },
        params: params.unwrap_or_default(),
        decorators: Default::default(),
        whitespace_after_def: parse_simple_whitespace(config, &mut def.whitespace_after)?,
        whitespace_after_name: parse_simple_whitespace(config, &mut name.whitespace_after)?,
        whitespace_before_colon: parse_simple_whitespace(config, &mut colon.whitespace_before)?,
    })
}

fn make_decorator<'a>(
    config: &Config<'a>,
    mut at: Token<'a>,
    mut name: Token<'a>,
    // mut newline: Token<'a>,
) -> Result<'a, Decorator<'a>> {
    Ok(Decorator {
        decorator: Name {
            value: name.string,
            ..Default::default()
        },
        leading_lines: parse_empty_lines(config, &mut at.whitespace_before, None)?,
        whitespace_after_at: parse_simple_whitespace(config, &mut at.whitespace_after)?,
        trailing_whitespace: Default::default(), //parse_trailing_whitespace(config, &mut newline.whitespace_before)?,
    })
}

fn make_comparison<'a>(
    config: &Config<'a>,
    head: Expression<'a>,
    tail: Vec<(Token<'a>, Expression<'a>)>,
) -> Result<'a, Expression<'a>> {
    todo!()
}

fn make_binary_op<'a>(
    config: &Config<'a>,
    head: Expression<'a>,
    tail: Vec<(Token<'a>, Expression<'a>)>,
) -> Result<'a, Expression<'a>> {
    if tail.is_empty() {
        return Ok(head);
    }

    let mut expr = head;
    for (tok, right) in tail {
        expr = Expression::BinaryOperation {
            left: Box::new(expr),
            operator: tok.string,
            right: Box::new(right),
            lpar: vec![],
            rpar: vec![],
        }
    }
    Ok(expr)
}

fn make_unary_op<'a>(
    config: &Config<'a>,
    op: Token<'a>,
    tail: Expression<'a>,
) -> Result<'a, Expression<'a>> {
    Ok(Expression::UnaryOperation {
        operator: op.string,
        expression: Box::new(tail),
        lpar: vec![],
        rpar: vec![],
    })
}

fn make_number<'a>(config: &Config<'a>, num: Token<'a>) -> Result<'a, Expression<'a>> {
    Ok(Expression::Integer {
        value: num.string,
        lpar: vec![],
        rpar: vec![],
    })
}
