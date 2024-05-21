// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::rc::Rc;

use crate::expression::make_async;
use crate::nodes::deflated::*;
use crate::nodes::expression::make_fstringtext;
use crate::nodes::op::make_importstar;
use crate::nodes::traits::ParenthesizedDeflatedNode;
use crate::parser::ParserError;
use crate::tokenizer::{TokType, Token};
use crate::WithComma;
use peg::str::LineCol;
use peg::{parser, Parse, ParseElem, RuleResult};
use TokType::{
    Async, Await as AWAIT, Dedent, EndMarker, FStringEnd, FStringStart, FStringString, Indent,
    Name as NameTok, Newline as NL, Number, String as STRING,
};

pub type Result<'a, T> = std::result::Result<T, ParserError<'a>>;
type GrammarResult<T> = std::result::Result<T, &'static str>;

#[derive(Debug)]
pub struct TokVec<'a>(Vec<Rc<Token<'a>>>);

impl<'a> std::convert::From<Vec<Token<'a>>> for TokVec<'a> {
    fn from(vec: Vec<Token<'a>>) -> Self {
        TokVec(vec.into_iter().map(Rc::new).collect())
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

type TokenRef<'input, 'a> = &'input Token<'a>;

impl<'input, 'a: 'input> ParseElem<'input> for TokVec<'a> {
    type Element = TokenRef<'input, 'a>;

    fn parse_elem(&'input self, pos: usize) -> RuleResult<Self::Element> {
        match self.0.get(pos) {
            Some(tok) => RuleResult::Matched(pos + 1, tok),
            None => RuleResult::Failed,
        }
    }
}

const MAX_RECURSION_DEPTH: usize = 3000;

parser! {
    pub grammar python<'a>(input: &'a str) for TokVec<'a> {

        // Starting Rules

        pub rule file(encoding: Option<&str>) -> Module<'input, 'a>
            = traced(<_file(encoding.unwrap_or("utf-8"))>)

        pub rule expression_input() -> Expression<'input, 'a>
            = traced(<e:star_expressions() tok(NL, "NEWLINE") tok(EndMarker, "EOF") {e}>)

        pub rule statement_input() -> Statement<'input, 'a>
            = traced(<s:statement() tok(EndMarker, "EOF") {s}>)

        rule _file(encoding: &str) -> Module<'input, 'a>
            = s:statements()? eof:tok(EndMarker, "EOF") {
                make_module(s.unwrap_or_default(), eof, encoding)
            }

        // General statements

        rule statements() -> Vec<Statement<'input, 'a>>
            = statement()+

        rule statement() -> Statement<'input, 'a>
            = c:compound_stmt() { Statement::Compound(c) }
            / s:simple_stmts() {
                    Statement::Simple(make_simple_statement_line(s))
            }

        rule simple_stmts() -> SimpleStatementParts<'input, 'a>
            = first_tok:&_ stmts:separated_trailer(<simple_stmt()>, <lit(";")>) nl:tok(NL, "NEWLINE") {
                SimpleStatementParts {
                    first_tok,
                    first_statement: stmts.0,
                    rest: stmts.1,
                    last_semi: stmts.2,
                    nl,
                }
            }

        #[cache]
        rule simple_stmt() -> SmallStatement<'input, 'a>
            = assignment()
            / &lit("type") s: type_stmt() {SmallStatement::TypeAlias(s)}
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


        rule compound_stmt() -> CompoundStatement<'input, 'a>
            = &(lit("def") / lit("@") / tok(Async, "ASYNC")) f:function_def() {
                CompoundStatement::FunctionDef(f)
            }
            / &lit("if") f:if_stmt() { CompoundStatement::If(f) }
            / &(lit("class") / lit("@")) c:class_def() { CompoundStatement::ClassDef(c) }
            / &(lit("with") / tok(Async, "ASYNC")) w:with_stmt() { CompoundStatement::With(w) }
            / &(lit("for") / tok(Async, "ASYNC")) f:for_stmt() { CompoundStatement::For(f) }
            / &lit("try") t:try_stmt() { CompoundStatement::Try(t) }
            / &lit("try") t:try_star_stmt() { CompoundStatement::TryStar(t) }
            / &lit("while") w:while_stmt() { CompoundStatement::While(w) }
            / m:match_stmt() { CompoundStatement::Match(m) }

        // Simple statements

        rule assignment() -> SmallStatement<'input, 'a>
            = a:name() col:lit(":") ann:expression()
                rhs:(eq:lit("=") d:annotated_rhs() {(eq, d)})? {
                    SmallStatement::AnnAssign(make_ann_assignment(
                        AssignTargetExpression::Name(Box::new(a)), col, ann, rhs))
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

        rule annotated_rhs() -> Expression<'input, 'a>
            = yield_expr() / star_expressions()

        rule augassign() -> AugOp<'input, 'a>
            = &(lit("+=")
                / lit("-=")
                / lit("*=")
                / lit("@=")
                /  lit("/=")
                / lit("%=")
                / lit("&=")
                / lit("|=")
                / lit("^=")
                / lit("<<=")
                / lit(">>=")
                / lit("**=")
                / lit("//=")) tok:_ {?
                    make_aug_op(tok).map_err(|_| "aug_op")
            }

        rule return_stmt() -> Return<'input, 'a>
            = kw:lit("return") a:star_expressions()? {
                make_return(kw, a)
            }

        rule raise_stmt() -> Raise<'input, 'a>
            = kw:lit("raise") exc:expression()
                rest:(f:lit("from") cau:expression() {(f, cau)})? {
                    make_raise(kw, Some(exc), rest)
            }
            / kw:lit("raise") {
                make_raise(kw, None, None)
            }

        rule global_stmt() -> Global<'input, 'a>
            = kw:lit("global") init:(n:name() c:comma() {(n, c)})* last:name() {
                make_global(kw, init, last)
            }

        rule nonlocal_stmt() -> Nonlocal<'input, 'a>
            = kw:lit("nonlocal") init:(n:name() c:comma() {(n, c)})* last:name() {
                make_nonlocal(kw, init, last)
            }

        rule del_stmt() -> Del<'input, 'a>
            = kw:lit("del") t:del_target() &(lit(";") / tok(NL, "NEWLINE")) {
                make_del(kw, t)
            }
            / kw:lit("del") t:del_targets() &(lit(";") / tok(NL, "NEWLINE")) {
                make_del(kw, make_del_tuple(None, t, None))
            }

        rule yield_stmt() -> Expression<'input, 'a>
            = yield_expr()

        rule assert_stmt() -> Assert<'input, 'a>
            = kw:lit("assert") test:expression() rest:(c:comma() msg:expression() {(c, msg)})? {
                make_assert(kw, test, rest)
            }

        // Import statements

        rule import_name() -> Import<'input, 'a>
            = kw:lit("import") a:dotted_as_names() {
                make_import(kw, a)
            }

        rule import_from() -> ImportFrom<'input, 'a>
            = from:lit("from") dots:dots()? m:dotted_name()
                import:lit("import") als:import_from_targets() {
                    make_import_from(from, dots.unwrap_or_default(), Some(m), import, als)
            }
            / from:lit("from") dots:dots()
                import:lit("import") als:import_from_targets() {
                    make_import_from(from, dots, None, import, als)
            }

        rule import_from_targets() -> ParenthesizedImportNames<'input, 'a>
            = lpar:lpar() als:import_from_as_names() c:comma()? rpar:rpar() {
                let mut als = als;
                if let (comma@Some(_), Some(mut last)) = (c, als.last_mut()) {
                    last.comma = comma;
                }
                (Some(lpar), ImportNames::Aliases(als), Some(rpar))
            }
            / als:import_from_as_names() !lit(",") { (None, ImportNames::Aliases(als), None)}
            / star:lit("*") { (None, ImportNames::Star(make_importstar()), None) }

        rule import_from_as_names() -> Vec<ImportAlias<'input, 'a>>
            = items:separated(<import_from_as_name()>, <comma()>) {
                make_import_from_as_names(items.0, items.1)
            }

        rule import_from_as_name() -> ImportAlias<'input, 'a>
            = n:name() asname:(kw:lit("as") z:name() {(kw, z)})? {
                make_import_alias(NameOrAttribute::N(Box::new(n)), asname)
            }

        rule dotted_as_names() -> Vec<ImportAlias<'input, 'a>>
            = init:(d:dotted_as_name() c:comma() {d.with_comma(c)})*
                last:dotted_as_name() {
                    concat(init, vec![last])
            }

        rule dotted_as_name() -> ImportAlias<'input, 'a>
            = n:dotted_name() asname:(kw:lit("as") z:name() {(kw, z)})? {
                make_import_alias(n, asname)
            }

        // TODO: why does this diverge from CPython?
        rule dotted_name() -> NameOrAttribute<'input, 'a>
            = first:name() tail:(dot:lit(".") n:name() {(dot, n)})* {
                make_name_or_attr(first, tail)
            }

        // Compound statements

        // Common elements

        #[cache]
        rule block() -> Suite<'input, 'a>
            = n:tok(NL, "NEWLINE") ind:tok(Indent, "INDENT") s:statements() ded:tok(Dedent, "DEDENT") {
                make_indented_block(n, ind, s, ded)
            }
            / s:simple_stmts() {
                make_simple_statement_suite(s)
            }

        rule decorators() -> Vec<Decorator<'input, 'a>>
            = (at:lit("@") e:named_expression() nl:tok(NL, "NEWLINE") {
                make_decorator(at, e, nl)
            } )+

        // Class definitions

        rule class_def() -> ClassDef<'input, 'a>
            = d:decorators() c:class_def_raw() { c.with_decorators(d) }
            / class_def_raw()

        rule class_def_raw() -> ClassDef<'input, 'a>
            = kw:lit("class") n:name() t:type_params()? arg:(l:lpar() a:arguments()? r:rpar() {(l, a, r)})?
                col:lit(":") b:block() {?
                    make_class_def(kw, n, t, arg, col, b)
            }

        // Function definitions

        rule function_def() -> FunctionDef<'input, 'a>
            = d:decorators() f:function_def_raw() {f.with_decorators(d)}
            / function_def_raw()

        rule _returns() -> Annotation<'input, 'a>
            = l:lit("->") e:expression() {
                make_annotation(l, e)
            }

        rule function_def_raw() -> FunctionDef<'input, 'a>
            = def:lit("def") n:name() t:type_params()? op:lit("(") params:params()?
                cp:lit(")") ty:_returns()? c:lit(":") b:block() {
                    make_function_def(None, def, n, t, op, params, cp, ty, c, b)
            }
            / asy:tok(Async, "ASYNC") def:lit("def") n:name() t:type_params()? op:lit("(") params:params()?
                cp:lit(")") ty:_returns()? c:lit(":") b:block() {
                    make_function_def(Some(asy), def, n, t, op, params, cp, ty, c, b)
            }

        // Function parameters

        rule params() -> Parameters<'input, 'a>
            = parameters()

        rule parameters() -> Parameters<'input, 'a>
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

        rule slash_no_default() -> (Vec<Param<'input, 'a>>, ParamSlash<'input, 'a>)
            = a:param_no_default()+ tok:lit("/") com:comma() {
                    (a, ParamSlash { comma: Some(com), tok })
            }
            / a:param_no_default()+ tok:lit("/") &lit(")") {
                (a, ParamSlash { comma: None, tok })
            }

        rule slash_with_default() -> (Vec<Param<'input, 'a>>, ParamSlash<'input, 'a>)
            = a:param_no_default()* b:param_with_default()+ tok:lit("/") c:comma() {
                (concat(a, b), ParamSlash { comma: Some(c), tok })
            }
            / a:param_no_default()* b:param_with_default()+ tok:lit("/") &lit(")") {
                (concat(a, b), ParamSlash { comma: None, tok })
            }

        rule star_etc() -> StarEtc<'input, 'a>
            = star:lit("*") a:param_no_default() b:param_maybe_default()* kw:kwds()? {
                StarEtc(Some(StarArg::Param(Box::new(
                    add_param_star(a, star)))), b, kw)
            }
            / star:lit("*") a:param_no_default_star_annotation() b:param_maybe_default()* kw:kwds()? {
                StarEtc(Some(StarArg::Param(Box::new(
                    add_param_star(a, star)))), b, kw)
            }
            / lit("*") c:comma() b:param_maybe_default()+ kw:kwds()? {
                StarEtc(Some(StarArg::Star(Box::new(ParamStar {comma:c }))), b, kw)
            }
            / kw:kwds() { StarEtc(None, vec![], Some(kw)) }

        rule kwds() -> Param<'input, 'a>
            = star:lit("**") a:param_no_default() {
                add_param_star(a, star)
            }

        rule param_no_default() -> Param<'input, 'a>
            = a:param() c:lit(",") { add_param_default(a, None, Some(c)) }
            / a:param() &lit(")") {a}

        rule param_no_default_star_annotation() -> Param<'input, 'a>
            = a:param_star_annotation() c:lit(",") { add_param_default(a, None, Some(c))}
            / a:param_star_annotation() &lit(")") {a}

        rule param_with_default() -> Param<'input, 'a>
            = a:param() def:default() c:lit(",") {
                add_param_default(a, Some(def), Some(c))
            }
            / a:param() def:default() &lit(")") {
                add_param_default(a, Some(def), None)
            }

        rule param_maybe_default() -> Param<'input, 'a>
            = a:param() def:default()? c:lit(",") {
                add_param_default(a, def, Some(c))
            }
            / a:param() def:default()? &lit(")") {
                add_param_default(a, def, None)
            }

        rule param() -> Param<'input, 'a>
            = n:name() a:annotation()? {
                Param {name: n, annotation: a, ..Default::default() }
            }

        rule param_star_annotation() -> Param<'input, 'a>
            = n:name() a:star_annotation() {
                Param {name: n, annotation: Some(a), ..Default::default() }
            }

        rule annotation() -> Annotation<'input, 'a>
            = col:lit(":") e:expression() {
                make_annotation(col, e)
            }

        rule star_annotation() -> Annotation<'input, 'a>
            = col:lit(":") e:star_expression() {
                make_annotation(col, e)
            }

        rule default() -> (AssignEqual<'input, 'a>,  Expression<'input, 'a>)
            = eq:lit("=") ex:expression() {
                (make_assign_equal(eq), ex)
            }

        rule default_or_starred() -> (AssignEqual<'input, 'a>,Option<TokenRef<'input, 'a>>,  Expression<'input, 'a>)
            = eq:lit("=") ex:expression() {
                (make_assign_equal(eq), None , ex)
            }
            / eq:lit("=") star:lit("*") ex:expression() {
                // make_star_default(eq, star, ex)
                (make_assign_equal(eq), Some(star) , ex)
            }

        // If statement

        rule if_stmt() -> If<'input, 'a>
            = i:lit("if") a:named_expression() col:lit(":") b:block() elif:elif_stmt() {
                make_if(i, a, col, b, Some(OrElse::Elif(elif)), false)
            }
            / i:lit("if") a:named_expression() col:lit(":") b:block() el:else_block()? {
                make_if(i, a, col, b, el.map(OrElse::Else), false)
            }

        rule elif_stmt() -> If<'input, 'a>
            = i:lit("elif") a:named_expression() col:lit(":") b:block() elif:elif_stmt() {
                make_if(i, a, col, b, Some(OrElse::Elif(elif)), true)
            }
            / i:lit("elif") a:named_expression() col:lit(":") b:block() el:else_block()? {
                make_if(i, a, col, b, el.map(OrElse::Else), true)
            }

        rule else_block() -> Else<'input, 'a>
            = el:lit("else") col:lit(":") b:block() {
                make_else(el, col, b)
            }

        // While statement

        rule while_stmt() -> While<'input, 'a>
            = kw:lit("while") test:named_expression() col:lit(":") b:block() el:else_block()? {
                make_while(kw, test, col, b, el)
            }

        // For statement

        rule for_stmt() -> For<'input, 'a>
            = f:lit("for") t:star_targets() i:lit("in") it:star_expressions()
                c:lit(":") b:block() el:else_block()? {
                    make_for(None, f, t, i, it, c, b, el)
            }
            / asy:tok(Async, "ASYNC") f:lit("for") t:star_targets() i:lit("in")
                it:star_expressions()
                c:lit(":") b:block() el:else_block()? {
                    make_for(Some(asy), f, t, i, it, c, b, el)
            }

        // With statement

        rule with_stmt() -> With<'input, 'a>
            = kw:lit("with") l:lpar() items:separated_trailer(<with_item()>, <comma()>) r:rpar()
                col:lit(":") b:block() {
                    make_with(None, kw, Some(l), comma_separate(items.0, items.1, items.2), Some(r), col, b)
            }
            / kw:lit("with") items:separated(<with_item()>, <comma()>)
                col:lit(":") b:block() {
                    make_with(None, kw, None, comma_separate(items.0, items.1, None), None, col, b)
            }
            / asy:tok(Async, "ASYNC") kw:lit("with") l:lpar() items:separated_trailer(<with_item()>, <comma()>) r:rpar()
                col:lit(":") b:block() {
                    make_with(Some(asy), kw, Some(l), comma_separate(items.0, items.1, items.2), Some(r), col, b)
            }
            / asy:tok(Async, "ASYNC") kw:lit("with") items:separated(<with_item()>, <comma()>)
                col:lit(":") b:block() {
                    make_with(Some(asy), kw, None, comma_separate(items.0, items.1, None), None, col, b)
            }

        rule with_item() -> WithItem<'input, 'a>
            = e:expression() a:lit("as") t:star_target() &(lit(",") / lit(":") / rpar()) {
                make_with_item(e, Some(a), Some(t))
            }
            / e:expression() {
                make_with_item(e, None, None)
            }

        // Try statement

        rule try_stmt() -> Try<'input, 'a>
            = kw:lit("try") lit(":") b:block() f:finally_block() {
                make_try(kw, b, vec![], None, Some(f))
            }
            / kw:lit("try") lit(":") b:block() ex:except_block()+ el:else_block()?
                f:finally_block()? {
                    make_try(kw, b, ex, el, f)
            }

        // Note: this is separate because TryStar is a different type in LibCST
        rule try_star_stmt() -> TryStar<'input, 'a>
            = kw:lit("try") lit(":") b:block() ex:except_star_block()+
                el:else_block()? f:finally_block()? {
                    make_try_star(kw, b, ex, el, f)
            }

        // Except statement

        rule except_block() -> ExceptHandler<'input, 'a>
            = kw:lit("except") e:expression() a:(k:lit("as") n:name() {(k, n)})?
                col:lit(":") b:block() {
                    make_except(kw, Some(e), a, col, b)
            }
            / kw:lit("except") col:lit(":") b:block() {
                make_except(kw, None, None, col, b)
            }

        rule except_star_block() -> ExceptStarHandler<'input, 'a>
            = kw:lit("except") star:lit("*") e:expression()
                a:(k:lit("as") n:name() {(k, n)})? col:lit(":") b:block() {
                    make_except_star(kw, star, e, a, col, b)
            }

        rule finally_block() -> Finally<'input, 'a>
            = kw:lit("finally") col:lit(":") b:block() {
                make_finally(kw, col, b)
            }


        // Match statement

        rule match_stmt() -> Match<'input, 'a>
            = kw:lit("match") subject:subject_expr() col:lit(":") tok(NL, "NEWLINE")
                i:tok(Indent, "INDENT") cases:case_block()+ d:tok(Dedent, "DEDENT") {
                    make_match(kw, subject, col, i, cases, d)
            }

        rule subject_expr() -> Expression<'input, 'a>
            = first:star_named_expression() c:comma() rest:star_named_expressions()? {
                Expression::Tuple(Box::new(
                    make_tuple_from_elements(first.with_comma(c), rest.unwrap_or_default()))
                )
            }
            / named_expression()

        rule case_block() -> MatchCase<'input, 'a>
            = kw:lit("case") pattern:patterns() guard:guard()? col:lit(":") body:block() {
                make_case(kw, pattern, guard, col, body)
            }

        rule guard() -> (TokenRef<'input, 'a>, Expression<'input, 'a>)
            = kw:lit("if") exp:named_expression() { (kw, exp) }

        rule patterns() -> MatchPattern<'input, 'a>
            = pats:open_sequence_pattern() {
                MatchPattern::Sequence(make_list_pattern(None, pats, None))
            }
            / pattern()

        rule pattern() -> MatchPattern<'input, 'a>
            = as_pattern()
            / or_pattern()

        rule as_pattern() -> MatchPattern<'input, 'a>
            = pat:or_pattern() kw:lit("as") target:pattern_capture_target() {
                make_as_pattern(Some(pat), Some(kw), Some(target))
            }

        rule or_pattern() -> MatchPattern<'input, 'a>
            = pats:separated(<closed_pattern()>, <lit("|")>) {
                make_or_pattern(pats.0, pats.1)
            }

        rule closed_pattern() -> MatchPattern<'input, 'a>
            = literal_pattern()
            / capture_pattern()
            / wildcard_pattern()
            / value_pattern()
            / group_pattern()
            / sequence_pattern()
            / mapping_pattern()
            / class_pattern()

        rule literal_pattern() -> MatchPattern<'input, 'a>
            = val:signed_number() !(lit("+") / lit("-")) { make_match_value(val) }
            / val:complex_number() { make_match_value(val) }
            / val:strings() { make_match_value(val.into()) }
            / n:lit("None") { make_match_singleton(make_name(n)) }
            / n:lit("True") { make_match_singleton(make_name(n)) }
            / n:lit("False") { make_match_singleton(make_name(n)) }

        rule literal_expr() -> Expression<'input, 'a>
            = val:signed_number() !(lit("+") / lit("-")) { val }
            / val:complex_number() { val }
            / val:strings() { val.into() }
            / n:lit("None") { Expression::Name(Box::new(make_name(n))) }
            / n:lit("True") { Expression::Name(Box::new(make_name(n))) }
            / n:lit("False") { Expression::Name(Box::new(make_name(n))) }

        rule complex_number() -> Expression<'input, 'a>
            = re:signed_real_number() op:(lit("+")/lit("-")) im:imaginary_number() {?
                make_binary_op(re, op, im).map_err(|_| "complex number")
            }

        rule signed_number() -> Expression<'input, 'a>
            = n:tok(Number, "number") { make_number(n) }
            / op:lit("-") n:tok(Number, "number") {?
                make_unary_op(op, make_number(n)).map_err(|_| "signed number")
            }

        rule signed_real_number() -> Expression<'input, 'a>
            = real_number()
            / op:lit("-") n:real_number() {?
                make_unary_op(op, n).map_err(|_| "signed real number")
            }

        rule real_number() -> Expression<'input, 'a>
            = n:tok(Number, "number") {? ensure_real_number(n) }

        rule imaginary_number() -> Expression<'input, 'a>
            = n:tok(Number, "number") {? ensure_imaginary_number(n) }

        rule capture_pattern() -> MatchPattern<'input, 'a>
            = t:pattern_capture_target() { make_as_pattern(None, None, Some(t)) }

        rule pattern_capture_target() -> Name<'input, 'a>
            = !lit("_") n:name() !(lit(".") / lit("(") / lit("=")) { n }

        rule wildcard_pattern() -> MatchPattern<'input, 'a>
            = lit("_") { make_as_pattern(None, None, None) }

        rule value_pattern() -> MatchPattern<'input, 'a>
            = v:attr() !(lit(".") / lit("(") / lit("=")) {
                make_match_value(v.into())
            }

        // In upstream attr and name_or_attr are mutually recursive, but rust-peg
        // doesn't support this yet.
        rule attr() -> NameOrAttribute<'input, 'a>
            = &(name() lit(".")) v:name_or_attr() { v }

        #[cache_left_rec]
        rule name_or_attr() -> NameOrAttribute<'input, 'a>
            = val:name_or_attr() d:lit(".") attr:name() {
                NameOrAttribute::A(Box::new(make_attribute(val.into(), d, attr)))
            }
            / n:name() { NameOrAttribute::N(Box::new(n)) }

        rule group_pattern() -> MatchPattern<'input, 'a>
            = l:lpar() pat:pattern() r:rpar() { pat.with_parens(l, r) }

        rule sequence_pattern() -> MatchPattern<'input, 'a>
            = l:lbrak() pats:maybe_sequence_pattern()? r:rbrak() {
                MatchPattern::Sequence(
                    make_list_pattern(Some(l), pats.unwrap_or_default(), Some(r))
                )
            }
            / l:lpar() pats:open_sequence_pattern()? r:rpar() {
                MatchPattern::Sequence(make_tuple_pattern(l, pats.unwrap_or_default(), r))
            }

        rule open_sequence_pattern() -> Vec<StarrableMatchSequenceElement<'input, 'a>>
            = pat:maybe_star_pattern() c:comma() pats:maybe_sequence_pattern()? {
                make_open_sequence_pattern(pat, c, pats.unwrap_or_default())
            }

        rule maybe_sequence_pattern() -> Vec<StarrableMatchSequenceElement<'input, 'a>>
            = pats:separated_trailer(<maybe_star_pattern()>, <comma()>) {
                comma_separate(pats.0, pats.1, pats.2)
            }

        rule maybe_star_pattern() -> StarrableMatchSequenceElement<'input, 'a>
            = s:star_pattern() { StarrableMatchSequenceElement::Starred(s) }
            / p:pattern() {
                StarrableMatchSequenceElement::Simple(
                    make_match_sequence_element(p)
                )
            }

        rule star_pattern() -> MatchStar<'input, 'a>
            = star:lit("*") t:pattern_capture_target() {make_match_star(star, Some(t))}
            / star:lit("*") t:wildcard_pattern() { make_match_star(star, None) }

        rule mapping_pattern() -> MatchPattern<'input, 'a>
            = l:lbrace() r:rbrace() {
                make_match_mapping(l, vec![], None, None, None, None, r)
            }
            / l:lbrace() rest:double_star_pattern() trail:comma()? r:rbrace() {
                make_match_mapping(l, vec![], None, Some(rest.0), Some(rest.1), trail, r)
            }
            / l:lbrace() items:items_pattern() c:comma() rest:double_star_pattern()
                trail:comma()? r:rbrace() {
                    make_match_mapping(l, items, Some(c), Some(rest.0), Some(rest.1), trail, r)
                }
            / l:lbrace() items:items_pattern() trail:comma()? r:rbrace() {
                make_match_mapping(l, items, trail, None, None, None, r)
            }

        rule items_pattern() -> Vec<MatchMappingElement<'input, 'a>>
            = pats:separated(<key_value_pattern()>, <comma()>) {
                comma_separate(pats.0, pats.1, None)
            }

        rule key_value_pattern() -> MatchMappingElement<'input, 'a>
            = key:(literal_expr() / a:attr() {a.into()}) colon:lit(":") pat:pattern() {
                make_match_mapping_element(key, colon, pat)
            }

        rule double_star_pattern() -> (TokenRef<'input, 'a>, Name<'input, 'a>)
            = star:lit("**") n:pattern_capture_target() { (star, n) }

        rule class_pattern() -> MatchPattern<'input, 'a>
            = cls:name_or_attr() l:lit("(") r:lit(")") {
                make_class_pattern(cls, l, vec![], None, vec![], None, r)
            }
            / cls:name_or_attr() l:lit("(") pats:positional_patterns() c:comma()? r:lit(")") {
                make_class_pattern(cls, l, pats, c, vec![], None, r)
            }
            / cls:name_or_attr() l:lit("(") kwds:keyword_patterns() c:comma()? r:lit(")") {
                make_class_pattern(cls, l, vec![], None, kwds, c, r)
            }
            / cls:name_or_attr() l:lit("(") pats:positional_patterns() c:comma()
                kwds:keyword_patterns() trail:comma()? r:lit(")") {
                    make_class_pattern(cls, l, pats, Some(c), kwds, trail, r)
            }

        rule positional_patterns() -> Vec<MatchSequenceElement<'input, 'a>>
            = pats:separated(<p:pattern() { make_match_sequence_element(p) }>, <comma()>) {
                comma_separate(pats.0, pats.1, None)
            }

        rule keyword_patterns() -> Vec<MatchKeywordElement<'input, 'a>>
            = pats:separated(<keyword_pattern()>, <comma()>) {
                comma_separate(pats.0, pats.1, None)
            }

        rule keyword_pattern() -> MatchKeywordElement<'input, 'a>
            = arg:name() eq:lit("=") value:pattern() {
                make_match_keyword_element(arg, eq, value)
            }

        // Type statement

        rule type_stmt() -> TypeAlias<'input, 'a>
            = t:lit("type") n:name() ps:type_params()? eq:lit("=") v:expression() {
                make_type_alias(t, n, ps, eq, v)
            }

        // Type parameter declaration

        rule type_params() -> TypeParameters<'input, 'a>
            = lb:lbrak() ps:separated_trailer(<type_param()>, <comma()>) rb:rbrak() {
                make_type_parameters(lb, comma_separate(ps.0, ps.1, ps.2), rb)
            }

        rule type_param() -> TypeParam<'input, 'a>
            = n:name() b:type_param_bound()? def:default()? { make_type_var(n, b, def) }
            / s:lit("*") n:name() def:default_or_starred()? { make_type_var_tuple(s, n, def)  }
            / s:lit("**") n:name() def:default()? { make_param_spec(s, n, def) }


        rule type_param_bound() -> TypeParamBound<'input, 'a>
            = c:lit(":") e:expression() { make_type_param_bound(c, e) }
        // Expressions

        #[cache]
        rule expression() -> Expression<'input, 'a>
            = _conditional_expression()
            / lambdef()

        rule _conditional_expression() -> Expression<'input, 'a>
            = body:disjunction() i:lit("if") test:disjunction() e:lit("else") oe:expression() {
                Expression::IfExp(Box::new(make_ifexp(body, i, test, e, oe)))
            }
            / disjunction()

        rule yield_expr() -> Expression<'input, 'a>
            = y:lit("yield") f:lit("from") a:expression() {
                Expression::Yield(Box::new(make_yield(y, Some(f), Some(a))))
            }
            / y:lit("yield") a:star_expressions()? {
                Expression::Yield(Box::new(make_yield(y, None, a)))
            }

        rule star_expressions() -> Expression<'input, 'a>
            = first:star_expression()
                rest:(comma:comma() e:star_expression() { (comma, expr_to_element(e)) })+
                comma:comma()? {
                    Expression::Tuple(Box::new(make_tuple(expr_to_element(first), rest, comma, None, None)))
            }
            / e:star_expression() comma:comma() {
                Expression::Tuple(Box::new(make_tuple(expr_to_element(e), vec![], Some(comma), None, None)))
            }
            / star_expression()

        #[cache]
        rule star_expression() -> Expression<'input, 'a>
            = star:lit("*") e:bitwise_or() {
                Expression::StarredElement(Box::new(make_starred_element(star, expr_to_element(e))))
            }
            / expression()

        rule star_named_expressions() -> Vec<Element<'input, 'a>>
            = exps:separated_trailer(<star_named_expression()>, <comma()>) {
                comma_separate(exps.0, exps.1, exps.2)
            }

        rule star_named_expression() -> Element<'input, 'a>
            = star:lit("*") e:bitwise_or() {
                Element::Starred(Box::new(make_starred_element(star, expr_to_element(e))))
            }
            / e:named_expression() { expr_to_element(e) }

        rule named_expression() -> Expression<'input, 'a>
            = a:name() op:lit(":=") b:expression() {
                Expression::NamedExpr(Box::new(make_named_expr(a, op, b)))
            }
            / e:expression() !lit(":=") { e }

        #[cache]
        rule disjunction() -> Expression<'input, 'a>
            = a:conjunction() b:(or:lit("or") inner:conjunction() { (or, inner) })+ {?
                make_boolean_op(a, b).map_err(|e| "expected disjunction")
            }
            / conjunction()

        #[cache]
        rule conjunction() -> Expression<'input, 'a>
            = a:inversion() b:(and:lit("and") inner:inversion() { (and, inner) })+ {?
                make_boolean_op(a, b).map_err(|e| "expected conjunction")
            }
            / inversion()

        #[cache]
        rule inversion() -> Expression<'input, 'a>
            = not:lit("not") a:inversion() {?
                make_unary_op(not, a).map_err(|e| "expected inversion")
            }
            / comparison()

        // Comparison operators

        #[cache]
        rule comparison() -> Expression<'input, 'a>
            = a:bitwise_or() b:compare_op_bitwise_or_pair()+ { make_comparison(a, b) }
            / bitwise_or()

        // This implementation diverges slightly from CPython (3.9) to avoid bloating
        // the parser cache and increase readability.
        #[cache]
        rule compare_op_bitwise_or_pair() -> (CompOp<'input, 'a>, Expression<'input, 'a>)
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

        rule _op_bitwise_or(o: &'static str) -> (CompOp<'input, 'a>, Expression<'input, 'a>)
            = op:lit(o) e:bitwise_or() {?
                make_comparison_operator(op)
                    .map(|op| (op, e))
                    .map_err(|_| "comparison")
            }

        rule _op_bitwise_or2(first: &'static str, second: &'static str) -> (CompOp<'input, 'a>, Expression<'input, 'a>)
            = f:lit(first) s:lit(second) e:bitwise_or() {?
                make_comparison_operator_2(f, s)
                    .map(|op| (op, e))
                    .map_err(|_| "comparison")
            }

        #[cache_left_rec]
        rule bitwise_or() -> Expression<'input, 'a>
            = a:bitwise_or() op:lit("|") b:bitwise_xor() {?
                make_binary_op(a, op, b).map_err(|e| "expected bitwise_or")
            }
            / bitwise_xor()

        #[cache_left_rec]
        rule bitwise_xor() -> Expression<'input, 'a>
            = a:bitwise_xor() op:lit("^") b:bitwise_and() {?
                make_binary_op(a, op, b).map_err(|e| "expected bitwise_xor")
            }
            / bitwise_and()

        #[cache_left_rec]
        rule bitwise_and() -> Expression<'input, 'a>
            = a:bitwise_and() op:lit("&") b:shift_expr() {?
                make_binary_op(a, op, b).map_err(|e| "expected bitwise_and")
            }
            / shift_expr()

        #[cache_left_rec]
        rule shift_expr() -> Expression<'input, 'a>
            = a:shift_expr() op:lit("<<") b:sum() {?
                make_binary_op(a, op, b).map_err(|e| "expected shift_expr")
            }
            / a:shift_expr() op:lit(">>") b:sum() {?
                make_binary_op(a, op, b).map_err(|e| "expected shift_expr")
            }
            / sum()

        #[cache_left_rec]
        rule sum() -> Expression<'input, 'a>
            = a:sum() op:lit("+") b:term() {?
                make_binary_op(a, op, b).map_err(|e| "expected sum")
            }
            / a:sum() op:lit("-") b:term() {?
                make_binary_op(a, op, b).map_err(|e| "expected sum")
            }
            / term()

        #[cache_left_rec]
        rule term() -> Expression<'input, 'a>
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
        rule factor() -> Expression<'input, 'a>
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

        rule power() -> Expression<'input, 'a>
            = a:await_primary() op:lit("**") b:factor() {?
                make_binary_op(a, op, b).map_err(|e| "expected power")
            }
            / await_primary()

        // Primary elements

        rule await_primary() -> Expression<'input, 'a>
            = aw:tok(AWAIT, "AWAIT") e:primary() {
                Expression::Await(Box::new(make_await(aw, e)))
            }
            / primary()

        #[cache_left_rec]
        rule primary() -> Expression<'input, 'a>
            = v:primary() dot:lit(".") attr:name() {
                Expression::Attribute(Box::new(make_attribute(v, dot, attr)))
            }
            / a:primary() b:genexp() {
                Expression::Call(Box::new(make_genexp_call(a, b)))
            }
            / f:primary() lpar:lit("(") arg:arguments()? rpar:lit(")") {
                Expression::Call(Box::new(make_call(f, lpar, arg.unwrap_or_default(), rpar)))
            }
            / v:primary() lbrak:lbrak() s:slices() rbrak:rbrak() {
                Expression::Subscript(Box::new(make_subscript(v, lbrak, s, rbrak)))
            }
            / atom()

        rule slices() -> Vec<SubscriptElement<'input, 'a>>
            = s:slice() !lit(",") { vec![SubscriptElement { slice: s, comma: None }] }
            / slices:separated_trailer(<slice()>, <comma()>) {
                make_slices(slices.0, slices.1, slices.2)
            }

        rule slice() -> BaseSlice<'input, 'a>
            = l:expression()? col:lit(":") u:expression()?
                rest:(c:lit(":") s:expression()? {(c, s)})? {
                    make_slice(l, col, u, rest)
            }
            / e:starred_expression() { make_index_from_arg(e) }
            / v:named_expression() { make_index(v) }

        rule atom() -> Expression<'input, 'a>
            = n:name() { Expression::Name(Box::new(n)) }
            / n:lit("True") { Expression::Name(Box::new(make_name(n))) }
            / n:lit("False") { Expression::Name(Box::new(make_name(n))) }
            / n:lit("None") { Expression::Name(Box::new(make_name(n))) }
            / &(tok(STRING, "") / tok(FStringStart, "")) s:strings() {s.into()}
            / n:tok(Number, "NUMBER") { make_number(n) }
            / &lit("(") e:(tuple() / group() / (g:genexp() {Expression::GeneratorExp(Box::new(g))})) {e}
            / &lit("[") e:(list() / listcomp()) {e}
            / &lit("{") e:(dict() / set() / dictcomp() / setcomp()) {e}
            / lit("...") { Expression::Ellipsis(Box::new(Ellipsis {lpar: vec![], rpar: vec![]}))}

        rule group() -> Expression<'input, 'a>
            = lpar:lpar() e:(yield_expr() / named_expression()) rpar:rpar() {
                e.with_parens(lpar, rpar)
            }

        // Lambda functions

        rule lambdef() -> Expression<'input, 'a>
            = kw:lit("lambda") p:lambda_params()? c:lit(":") b:expression() {
                Expression::Lambda(Box::new(make_lambda(kw, p.unwrap_or_default(), c, b)))
            }

        rule lambda_params() -> Parameters<'input, 'a>
            = lambda_parameters()

        // lambda_parameters etc. duplicates parameters but without annotations or type
        // comments, and if there's no comma after a parameter, we expect a colon, not a
        // close parenthesis.

        rule lambda_parameters() -> Parameters<'input, 'a>
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

        rule lambda_slash_no_default() -> (Vec<Param<'input, 'a>>, ParamSlash<'input, 'a>)
            = a:lambda_param_no_default()+ tok:lit("/") com:comma() {
                (a, ParamSlash { comma: Some(com), tok } )
            }
            / a:lambda_param_no_default()+ tok:lit("/") &lit(":") {
                (a, ParamSlash { comma: None, tok })
            }

        rule lambda_slash_with_default() -> (Vec<Param<'input, 'a>>, ParamSlash<'input, 'a>)
            = a:lambda_param_no_default()* b:lambda_param_with_default()+ tok:lit("/") c:comma(){
                (concat(a, b), ParamSlash { comma: Some(c), tok })
            }
            / a:lambda_param_no_default()* b:lambda_param_with_default()+ tok:lit("/") &lit(":") {
                (concat(a, b), ParamSlash { comma: None, tok })
            }

        rule lambda_star_etc() -> StarEtc<'input, 'a>
            = star:lit("*") a:lambda_param_no_default()
                b:lambda_param_maybe_default()* kw:lambda_kwds()? {
                    StarEtc(Some(StarArg::Param(
                        Box::new(add_param_star(a, star))
                    )), b, kw)
            }
            / lit("*") c:comma() b:lambda_param_maybe_default()+ kw:lambda_kwds()? {
                StarEtc(Some(StarArg::Star(Box::new(ParamStar {comma: c}))), b, kw)
            }
            / kw:lambda_kwds() { StarEtc(None, vec![], Some(kw)) }

        rule lambda_kwds() -> Param<'input, 'a>
            = star:lit("**") a:lambda_param_no_default() {
                add_param_star(a, star)
            }

        rule lambda_param_no_default() -> Param<'input, 'a>
            = a:lambda_param() c:lit(",") {
                add_param_default(a, None, Some(c))
            }
            / a:lambda_param() &lit(":") {a}

        rule lambda_param_with_default() -> Param<'input, 'a>
            = a:lambda_param() def:default() c:lit(",") {
                add_param_default(a, Some(def), Some(c))
            }
            / a:lambda_param() def:default() &lit(":") {
                add_param_default(a, Some(def), None)
            }

        rule lambda_param_maybe_default() -> Param<'input, 'a>
            = a:lambda_param() def:default()? c:lit(",") {
                add_param_default(a, def, Some(c))
            }
            / a:lambda_param() def:default()? &lit(":") {
                add_param_default(a, def, None)
            }

        rule lambda_param() -> Param<'input, 'a>
            = name:name() { Param { name, ..Default::default() } }

        // Literals

        rule strings() -> String<'input, 'a>
            = s:(str:tok(STRING, "STRING") t:&_ {(make_string(str), t)}
                / str:fstring() t:&_ {(String::Formatted(str), t)})+ {?
                make_strings(s)
            }

        rule list() -> Expression<'input, 'a>
            = lbrak:lbrak() e:star_named_expressions()? rbrak:rbrak() {
                Expression::List(Box::new(
                    make_list(lbrak, e.unwrap_or_default(), rbrak))
                )
            }

        rule tuple() -> Expression<'input, 'a>
            = lpar:lpar() first:star_named_expression() &lit(",")
                rest:(c:comma() e:star_named_expression() {(c, e)})*
                trailing_comma:comma()? rpar:rpar() {
                    Expression::Tuple(Box::new(
                        make_tuple(first, rest, trailing_comma, Some(lpar), Some(rpar))
                    ))
            }
            / lpar:lpar() rpar:lit(")") {
                Expression::Tuple(Box::new(Tuple::default().with_parens(
                    lpar, RightParen { rpar_tok: rpar }
                )))}

        rule set() -> Expression<'input, 'a>
            = lbrace:lbrace() e:star_named_expressions()? rbrace:rbrace() {
                Expression::Set(Box::new(make_set(lbrace, e.unwrap_or_default(), rbrace)))
            }

        // Dicts

        rule dict() -> Expression<'input, 'a>
            = lbrace:lbrace() els:double_starred_keypairs()? rbrace:rbrace() {
                Expression::Dict(Box::new(make_dict(lbrace, els.unwrap_or_default(), rbrace)))
            }


        rule double_starred_keypairs() -> Vec<DictElement<'input, 'a>>
            = pairs:separated_trailer(<double_starred_kvpair()>, <comma()>) {
                    make_double_starred_keypairs(pairs.0, pairs.1, pairs.2)
            }

        rule double_starred_kvpair() -> DictElement<'input, 'a>
            = s:lit("**") e:bitwise_or() {
                DictElement::Starred(make_double_starred_element(s, e))
            }
            / k:kvpair() { make_dict_element(k) }

        rule kvpair() -> (Expression<'input, 'a>, TokenRef<'input, 'a>, Expression<'input, 'a>)
            = k:expression() colon:lit(":") v:expression() { (k, colon, v) }

        // Comprehensions & generators

        rule for_if_clauses() -> CompFor<'input, 'a>
            = c:for_if_clause()+ {? merge_comp_fors(c) }

        rule for_if_clause() -> CompFor<'input, 'a>
            = asy:_async() f:lit("for") tgt:star_targets() i:lit("in")
                iter:disjunction() ifs:_comp_if()* {
                    make_for_if(Some(asy), f, tgt, i, iter, ifs)
            }
            / f:lit("for") tgt:star_targets() i:lit("in")
            iter:disjunction() ifs:_comp_if()* {
                make_for_if(None, f, tgt, i, iter, ifs)
            }

        rule _comp_if() -> CompIf<'input, 'a>
            = kw:lit("if") cond:disjunction() {
                make_comp_if(kw, cond)
            }

        rule listcomp() -> Expression<'input, 'a>
            = lbrak:lbrak() elt:named_expression() comp:for_if_clauses() rbrak:rbrak() {
                Expression::ListComp(Box::new(make_list_comp(lbrak, elt, comp, rbrak)))
            }

        rule setcomp() -> Expression<'input, 'a>
            = l:lbrace() elt:named_expression() comp:for_if_clauses() r:rbrace() {
                Expression::SetComp(Box::new(make_set_comp(l, elt, comp, r)))
            }

        rule genexp() -> GeneratorExp<'input, 'a>
            = lpar:lpar() g:_bare_genexp() rpar:rpar() {
                g.with_parens(lpar, rpar)
            }

        rule _bare_genexp() -> GeneratorExp<'input, 'a>
            = elt:named_expression() comp:for_if_clauses() {
                make_bare_genexp(elt, comp)
            }

        rule dictcomp() -> Expression<'input, 'a>
            = lbrace:lbrace() elt:kvpair() comp:for_if_clauses() rbrace:rbrace() {
                Expression::DictComp(Box::new(make_dict_comp(lbrace, elt, comp, rbrace)))
            }

        // Function call arguments

        rule arguments() -> Vec<Arg<'input, 'a>>
            = a:args() trail:comma()? &lit(")") {add_arguments_trailing_comma(a, trail)}

        rule args() -> Vec<Arg<'input, 'a>>
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

        rule _posarg() -> Arg<'input, 'a>
            = a:(starred_expression() / e:named_expression() { make_arg(e) })
                !lit("=") { a }

        rule kwargs() -> Vec<Arg<'input, 'a>>
            = sitems:separated(<kwarg_or_starred()>, <comma()>)
                scomma:comma()
                ditems:separated(<kwarg_or_double_starred()>, <comma()>) {
                    concat(
                        comma_separate(sitems.0, sitems.1, Some(scomma)),
                        comma_separate(ditems.0, ditems.1, None),
                    )
            }
            / items:separated(<kwarg_or_starred()>, <comma()>) {
                    comma_separate(items.0, items.1, None)
            }
            / items:separated(<kwarg_or_double_starred()>, <comma()>) {
                    comma_separate(items.0, items.1, None)
            }

        rule starred_expression() -> Arg<'input, 'a>
            = star:lit("*") e:expression() { make_star_arg(star, e) }

        rule kwarg_or_starred() -> Arg<'input, 'a>
            = _kwarg()
            / starred_expression()

        rule kwarg_or_double_starred() -> Arg<'input, 'a>
            = _kwarg()
            / star:lit("**") e:expression() { make_star_arg(star, e) }

        rule _kwarg() -> Arg<'input, 'a>
            = n:name() eq:lit("=") v:expression() {
                make_kwarg(n, eq, v)
            }

        // Assignment targets
        // Generic targets

        rule star_targets() -> AssignTargetExpression<'input, 'a>
            = a:star_target() !lit(",") {a}
            / targets:separated_trailer(<t:star_target() {assign_target_to_element(t)}>, <comma()>) {
                AssignTargetExpression::Tuple(Box::new(
                    make_tuple(targets.0, targets.1, targets.2, None, None)
                ))
            }

        rule star_targets_list_seq() -> Vec<Element<'input, 'a>>
            = targets:separated_trailer(<t:star_target() { assign_target_to_element(t) }>, <comma()>) {
                comma_separate(targets.0, targets.1, targets.2)
            }

        // This differs from star_targets below because it requires at least two items
        // in the tuple
        rule star_targets_tuple_seq() -> Tuple<'input, 'a>
            = first:(t:star_target() {assign_target_to_element(t)})
                rest:(c:comma() t:star_target() {(c, assign_target_to_element(t))})+
                trail:comma()? {
                    make_tuple(first, rest, trail, None, None)
            }
            / t:star_target() trail:comma()? {
                make_tuple(assign_target_to_element(t), vec![], trail, None, None)
            }

        #[cache]
        rule star_target() -> AssignTargetExpression<'input, 'a>
            = star:lit("*") !lit("*") t:star_target() {
                AssignTargetExpression::StarredElement(Box::new(
                    make_starred_element(star, assign_target_to_element(t))
                ))
            }
            / target_with_star_atom()

        #[cache]
        rule target_with_star_atom() -> AssignTargetExpression<'input, 'a>
            = a:t_primary() dot:lit(".") n:name() !t_lookahead() {
                AssignTargetExpression::Attribute(Box::new(make_attribute(a, dot, n)))
            }
            / a:t_primary() lbrak:lbrak() s:slices() rbrak:rbrak() !t_lookahead() {
                AssignTargetExpression::Subscript(Box::new(
                    make_subscript(a, lbrak, s, rbrak)
                ))
            }
            / a:star_atom() {a}

        rule star_atom() -> AssignTargetExpression<'input, 'a>
            = a:name() { AssignTargetExpression::Name(Box::new(a)) }
            / lpar:lpar() a:target_with_star_atom() rpar:rpar() { a.with_parens(lpar, rpar) }
            / lpar:lpar() a:star_targets_tuple_seq()? rpar:rpar() {
               AssignTargetExpression::Tuple(Box::new(
                   a.unwrap_or_default().with_parens(lpar, rpar)
               ))
            }
            / lbrak:lbrak() a:star_targets_list_seq()? rbrak:rbrak() {
                AssignTargetExpression::List(Box::new(
                    make_list(lbrak, a.unwrap_or_default(), rbrak)
                ))
            }

        rule single_target() -> AssignTargetExpression<'input, 'a>
            = single_subscript_attribute_target()
            / n:name() { AssignTargetExpression::Name(Box::new(n)) }
            / lpar:lpar() t:single_target() rpar:rpar() { t.with_parens(lpar, rpar) }

        rule single_subscript_attribute_target() -> AssignTargetExpression<'input, 'a>
            = a:t_primary() dot:lit(".") n:name() !t_lookahead() {
                AssignTargetExpression::Attribute(Box::new(make_attribute(a, dot, n)))
            }
            / a:t_primary() lbrak:lbrak() s:slices() rbrak:rbrak() !t_lookahead() {
                AssignTargetExpression::Subscript(Box::new(
                    make_subscript(a, lbrak, s, rbrak)
                ))
            }


        #[cache_left_rec]
        rule t_primary() -> Expression<'input, 'a>
            = value:t_primary() dot:lit(".") attr:name() &t_lookahead() {
                Expression::Attribute(Box::new(make_attribute(value, dot, attr)))
            }
            / v:t_primary() l:lbrak() s:slices() r:rbrak() &t_lookahead() {
                Expression::Subscript(Box::new(make_subscript(v, l, s, r)))
            }
            / f:t_primary() gen:genexp() &t_lookahead() {
                Expression::Call(Box::new(make_genexp_call(f, gen)))
            }
            / f:t_primary() lpar:lit("(") arg:arguments()? rpar:lit(")") &t_lookahead() {
                Expression::Call(Box::new(make_call(f, lpar, arg.unwrap_or_default(), rpar)))
            }
            / a:atom() &t_lookahead() {a}

        rule t_lookahead() -> ()
            = (lit("(") / lit("[") / lit(".")) {}

        // Targets for del statements

        rule del_targets() -> Vec<Element<'input, 'a>>
            = t:separated_trailer(<u:del_target() {u.into()}>, <comma()>) {
                comma_separate(t.0, t.1, t.2)
            }

        rule del_target() -> DelTargetExpression<'input, 'a>
            = a:t_primary() d:lit(".") n:name() !t_lookahead() {
                DelTargetExpression::Attribute(Box::new(make_attribute(a, d, n)))
            }
            / a:t_primary() lbrak:lbrak() s:slices() rbrak:rbrak() !t_lookahead() {
                DelTargetExpression::Subscript(Box::new(
                    make_subscript(a, lbrak, s, rbrak)
                ))
            }
            / del_t_atom()

        rule del_t_atom() -> DelTargetExpression<'input, 'a>
            = n:name() { DelTargetExpression::Name(Box::new(n)) }
            / l:lpar() d:del_target() r:rpar() { d.with_parens(l, r) }
            / l:lpar() d:del_targets()? r:rpar() {
                make_del_tuple(Some(l), d.unwrap_or_default(), Some(r))
            }
            / l:lbrak() d:del_targets()? r:rbrak() {
                DelTargetExpression::List(Box::new(
                    make_list(l, d.unwrap_or_default(), r)
                ))
            }

        // F-strings

        rule fstring() -> FormattedString<'input, 'a>
            = start:tok(FStringStart, "f\"")
                parts:(_f_string() / _f_replacement())*
                end:tok(FStringEnd, "\"") {
                    make_fstring(start.string, parts, end.string)
            }

        rule _f_string() -> FormattedStringContent<'input, 'a>
            = t:tok(FStringString, "f-string contents") {
                FormattedStringContent::Text(make_fstringtext(t.string))
            }

        rule _f_replacement() -> FormattedStringContent<'input, 'a>
            = lb:lit("{") e:_f_expr() eq:lit("=")?
                conv:(t:lit("!") c:_f_conversion() {(t,c)})?
                spec:(t:lit(":") s:_f_spec() {(t,s)})?
                rb:lit("}") {
                    FormattedStringContent::Expression(Box::new(
                        make_fstring_expression(lb, e, eq, conv, spec, rb)
                    ))
            }

        rule _f_expr() -> Expression<'input, 'a>
            = (g:_bare_genexp() {Expression::GeneratorExp(Box::new(g))})
            / star_expressions()
            / yield_expr()

        rule _f_conversion() -> &'a str
            = lit("r") {"r"} / lit("s") {"s"} / lit("a") {"a"}

        rule _f_spec() -> Vec<FormattedStringContent<'input, 'a>>
            = (_f_string() / _f_replacement())*

        // CST helpers

        rule comma() -> Comma<'input, 'a>
            = c:lit(",") { make_comma(c) }

        rule dots() -> Vec<Dot<'input, 'a>>
            = ds:((dot:lit(".") { make_dot(dot) })+
                / tok:lit("...") {
                    vec![make_dot(tok), make_dot(tok), make_dot(tok)]}
            )+ { ds.into_iter().flatten().collect() }

        rule lpar() -> LeftParen<'input, 'a>
            = a:lit("(") { make_lpar(a) }

        rule rpar() -> RightParen<'input, 'a>
            = a:lit(")") { make_rpar(a) }

        rule lbrak() -> LeftSquareBracket<'input, 'a>
            = tok:lit("[") { make_left_bracket(tok) }

        rule rbrak() -> RightSquareBracket<'input, 'a>
            = tok:lit("]") { make_right_bracket(tok) }

        rule lbrace() -> LeftCurlyBrace<'input, 'a>
            = tok:lit("{") { make_left_brace(tok) }

        rule rbrace() -> RightCurlyBrace<'input, 'a>
            = tok:lit("}") { make_right_brace(tok) }

        /// matches any token, not just whitespace
        rule _() -> TokenRef<'input, 'a>
            = [t] { t }

        rule lit(lit: &'static str) -> TokenRef<'input, 'a>
            = [t] {? if t.string == lit { Ok(t) } else { Err(lit) } }

        rule tok(tok: TokType, err: &'static str) -> TokenRef<'input, 'a>
            = [t] {? if t.r#type == tok { Ok(t) } else { Err(err) } }

        rule name() -> Name<'input, 'a>
            = !( lit("False") / lit("None") / lit("True") / lit("and") / lit("as") / lit("assert") / lit("async") / lit("await")
                / lit("break") / lit("class") / lit("continue") / lit("def") / lit("del") / lit("elif") / lit("else")
                / lit("except") / lit("finally") / lit("for") / lit("from") / lit("global") / lit("if") / lit("import")
                / lit("in") / lit("is") / lit("lambda") / lit("nonlocal") / lit("not") / lit("or") / lit("pass") / lit("raise")
                / lit("return") / lit("try") / lit("while") / lit("with") / lit("yield")
            )
            t:tok(NameTok, "NAME") {make_name(t)}

        rule _async() -> TokenRef<'input, 'a>
            = tok(Async, "ASYNC")

        rule separated_trailer<El, Sep>(el: rule<El>, sep: rule<Sep>) -> (El, Vec<(Sep, El)>, Option<Sep>)
            = e:el() rest:(s:sep() e:el() {(s, e)})* trailer:sep()? {(e, rest, trailer)}

        rule separated<El, Sep>(el: rule<El>, sep: rule<Sep>) -> (El, Vec<(Sep, El)>)
            = e:el() rest:(s:sep() e:el() {(s, e)})* {(e, rest)}

        rule traced<T>(e: rule<T>) -> T =
            &(_* {
                #[cfg(feature = "trace")]
                {
                    println!("[PEG_INPUT_START]");
                    println!("{}", input);
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
fn make_function_def<'input, 'a>(
    async_tok: Option<TokenRef<'input, 'a>>,
    def_tok: TokenRef<'input, 'a>,
    name: Name<'input, 'a>,
    type_parameters: Option<TypeParameters<'input, 'a>>,
    open_paren_tok: TokenRef<'input, 'a>,
    params: Option<Parameters<'input, 'a>>,
    close_paren_tok: TokenRef<'input, 'a>,
    returns: Option<Annotation<'input, 'a>>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> FunctionDef<'input, 'a> {
    let asynchronous = async_tok.as_ref().map(|_| make_async());
    FunctionDef {
        name,
        type_parameters,
        params: params.unwrap_or_default(),
        body,
        decorators: Default::default(),
        returns,
        asynchronous,
        async_tok,
        def_tok,
        open_paren_tok,
        close_paren_tok,
        colon_tok,
    }
}

fn make_decorator<'input, 'a>(
    at_tok: TokenRef<'input, 'a>,
    name: Expression<'input, 'a>,
    newline_tok: TokenRef<'input, 'a>,
) -> Decorator<'input, 'a> {
    Decorator {
        decorator: name,
        newline_tok,
        at_tok,
    }
}

fn make_comparison<'input, 'a>(
    head: Expression<'input, 'a>,
    tail: Vec<(CompOp<'input, 'a>, Expression<'input, 'a>)>,
) -> Expression<'input, 'a> {
    let mut comparisons = vec![];
    for (operator, e) in tail {
        comparisons.push(ComparisonTarget {
            operator,
            comparator: e,
        });
    }
    Expression::Comparison(Box::new(Comparison {
        left: Box::new(head),
        comparisons,
        lpar: vec![],
        rpar: vec![],
    }))
}

fn make_comparison_operator<'input, 'a>(
    tok: TokenRef<'input, 'a>,
) -> Result<'a, CompOp<'input, 'a>> {
    match tok.string {
        "<" => Ok(CompOp::LessThan { tok }),
        ">" => Ok(CompOp::GreaterThan { tok }),
        "<=" => Ok(CompOp::LessThanEqual { tok }),
        ">=" => Ok(CompOp::GreaterThanEqual { tok }),
        "==" => Ok(CompOp::Equal { tok }),
        "!=" => Ok(CompOp::NotEqual { tok }),
        "in" => Ok(CompOp::In { tok }),
        "is" => Ok(CompOp::Is { tok }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_comparison_operator_2<'input, 'a>(
    first: TokenRef<'input, 'a>,
    second: TokenRef<'input, 'a>,
) -> Result<'a, CompOp<'input, 'a>> {
    match (first.string, second.string) {
        ("is", "not") => Ok(CompOp::IsNot {
            is_tok: first,
            not_tok: second,
        }),
        ("not", "in") => Ok(CompOp::NotIn {
            not_tok: first,
            in_tok: second,
        }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_boolean_op<'input, 'a>(
    head: Expression<'input, 'a>,
    tail: Vec<(TokenRef<'input, 'a>, Expression<'input, 'a>)>,
) -> Result<'a, Expression<'input, 'a>> {
    if tail.is_empty() {
        return Ok(head);
    }

    let mut expr = head;
    for (tok, right) in tail {
        expr = Expression::BooleanOperation(Box::new(BooleanOperation {
            left: Box::new(expr),
            operator: make_boolean_operator(tok)?,
            right: Box::new(right),
            lpar: vec![],
            rpar: vec![],
        }))
    }
    Ok(expr)
}

fn make_boolean_operator<'input, 'a>(
    tok: TokenRef<'input, 'a>,
) -> Result<'a, BooleanOp<'input, 'a>> {
    match tok.string {
        "and" => Ok(BooleanOp::And { tok }),
        "or" => Ok(BooleanOp::Or { tok }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_binary_op<'input, 'a>(
    left: Expression<'input, 'a>,
    op: TokenRef<'input, 'a>,
    right: Expression<'input, 'a>,
) -> Result<'a, Expression<'input, 'a>> {
    let operator = make_binary_operator(op)?;
    Ok(Expression::BinaryOperation(Box::new(BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
        lpar: vec![],
        rpar: vec![],
    })))
}

fn make_binary_operator<'input, 'a>(tok: TokenRef<'input, 'a>) -> Result<'a, BinaryOp<'input, 'a>> {
    match tok.string {
        "+" => Ok(BinaryOp::Add { tok }),
        "-" => Ok(BinaryOp::Subtract { tok }),
        "*" => Ok(BinaryOp::Multiply { tok }),
        "/" => Ok(BinaryOp::Divide { tok }),
        "//" => Ok(BinaryOp::FloorDivide { tok }),
        "%" => Ok(BinaryOp::Modulo { tok }),
        "**" => Ok(BinaryOp::Power { tok }),
        "<<" => Ok(BinaryOp::LeftShift { tok }),
        ">>" => Ok(BinaryOp::RightShift { tok }),
        "|" => Ok(BinaryOp::BitOr { tok }),
        "&" => Ok(BinaryOp::BitAnd { tok }),
        "^" => Ok(BinaryOp::BitXor { tok }),
        "@" => Ok(BinaryOp::MatrixMultiply { tok }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_unary_op<'input, 'a>(
    op: TokenRef<'input, 'a>,
    tail: Expression<'input, 'a>,
) -> Result<'a, Expression<'input, 'a>> {
    let operator = make_unary_operator(op)?;
    Ok(Expression::UnaryOperation(Box::new(UnaryOperation {
        operator,
        expression: Box::new(tail),
        lpar: vec![],
        rpar: vec![],
    })))
}

fn make_unary_operator<'input, 'a>(tok: TokenRef<'input, 'a>) -> Result<'a, UnaryOp<'input, 'a>> {
    match tok.string {
        "+" => Ok(UnaryOp::Plus { tok }),
        "-" => Ok(UnaryOp::Minus { tok }),
        "~" => Ok(UnaryOp::BitInvert { tok }),
        "not" => Ok(UnaryOp::Not { tok }),
        _ => Err(ParserError::OperatorError),
    }
}

fn make_number<'input, 'a>(num: TokenRef<'input, 'a>) -> Expression<'input, 'a> {
    super::numbers::parse_number(num.string)
}

fn make_indented_block<'input, 'a>(
    nl: TokenRef<'input, 'a>,
    indent: TokenRef<'input, 'a>,
    statements: Vec<Statement<'input, 'a>>,
    dedent: TokenRef<'input, 'a>,
) -> Suite<'input, 'a> {
    Suite::IndentedBlock(IndentedBlock {
        body: statements,
        indent: Default::default(),
        newline_tok: nl,
        indent_tok: indent,
        dedent_tok: dedent,
    })
}

struct SimpleStatementParts<'input, 'a> {
    first_tok: TokenRef<'input, 'a>, // The first token of the first statement. Used for its whitespace
    first_statement: SmallStatement<'input, 'a>,
    rest: Vec<(TokenRef<'input, 'a>, SmallStatement<'input, 'a>)>, // semicolon, statement pairs
    last_semi: Option<TokenRef<'input, 'a>>,
    nl: TokenRef<'input, 'a>,
}

fn make_semicolon<'input, 'a>(tok: TokenRef<'input, 'a>) -> Semicolon<'input, 'a> {
    Semicolon { tok }
}

fn _make_simple_statement<'input, 'a>(
    parts: SimpleStatementParts<'input, 'a>,
) -> (
    TokenRef<'input, 'a>,
    Vec<SmallStatement<'input, 'a>>,
    TokenRef<'input, 'a>,
) {
    let mut body = vec![];

    let mut current = parts.first_statement;
    for (semi, next) in parts.rest {
        body.push(current.with_semicolon(Some(make_semicolon(semi))));
        current = next;
    }
    if let Some(semi) = parts.last_semi {
        current = current.with_semicolon(Some(make_semicolon(semi)));
    }
    body.push(current);

    (parts.first_tok, body, parts.nl)
}

fn make_simple_statement_suite<'input, 'a>(
    parts: SimpleStatementParts<'input, 'a>,
) -> Suite<'input, 'a> {
    let (first_tok, body, newline_tok) = _make_simple_statement(parts);

    Suite::SimpleStatementSuite(SimpleStatementSuite {
        body,
        first_tok,
        newline_tok,
    })
}

fn make_simple_statement_line<'input, 'a>(
    parts: SimpleStatementParts<'input, 'a>,
) -> SimpleStatementLine<'input, 'a> {
    let (first_tok, body, newline_tok) = _make_simple_statement(parts);
    SimpleStatementLine {
        body,
        first_tok,
        newline_tok,
    }
}

fn make_if<'input, 'a>(
    if_tok: TokenRef<'input, 'a>,
    cond: Expression<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    block: Suite<'input, 'a>,
    orelse: Option<OrElse<'input, 'a>>,
    is_elif: bool,
) -> If<'input, 'a> {
    If {
        test: cond,
        body: block,
        orelse: orelse.map(Box::new),
        is_elif,
        if_tok,
        colon_tok,
    }
}

fn make_else<'input, 'a>(
    else_tok: TokenRef<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    block: Suite<'input, 'a>,
) -> Else<'input, 'a> {
    Else {
        body: block,
        else_tok,
        colon_tok,
    }
}

struct StarEtc<'input, 'a>(
    Option<StarArg<'input, 'a>>,
    Vec<Param<'input, 'a>>,
    Option<Param<'input, 'a>>,
);

fn make_parameters<'input, 'a>(
    posonly: Option<(Vec<Param<'input, 'a>>, ParamSlash<'input, 'a>)>,
    params: Vec<Param<'input, 'a>>,
    star_etc: Option<StarEtc<'input, 'a>>,
) -> Parameters<'input, 'a> {
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

fn add_param_default<'input, 'a>(
    param: Param<'input, 'a>,
    def: Option<(AssignEqual<'input, 'a>, Expression<'input, 'a>)>,
    comma_tok: Option<TokenRef<'input, 'a>>,
) -> Param<'input, 'a> {
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

fn add_param_star<'input, 'a>(
    param: Param<'input, 'a>,
    star: TokenRef<'input, 'a>,
) -> Param<'input, 'a> {
    let str = star.string;
    Param {
        star: Some(str),
        star_tok: Some(star),
        ..param
    }
}

fn make_assign_equal<'input, 'a>(tok: TokenRef<'input, 'a>) -> AssignEqual<'input, 'a> {
    AssignEqual { tok }
}

fn make_comma<'input, 'a>(tok: TokenRef<'input, 'a>) -> Comma<'input, 'a> {
    Comma { tok }
}

fn concat<T>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.into_iter().chain(b.into_iter()).collect()
}

fn make_name_or_attr<'input, 'a>(
    first_tok: Name<'input, 'a>,
    mut tail: Vec<(TokenRef<'input, 'a>, Name<'input, 'a>)>,
) -> NameOrAttribute<'input, 'a> {
    if let Some((dot, name)) = tail.pop() {
        let dot = make_dot(dot);
        return NameOrAttribute::A(Box::new(Attribute {
            attr: name,
            dot,
            lpar: Default::default(),
            rpar: Default::default(),
            value: Box::new(make_name_or_attr(first_tok, tail).into()),
        }));
    } else {
        NameOrAttribute::N(Box::new(first_tok))
    }
}

fn make_name<'input, 'a>(tok: TokenRef<'input, 'a>) -> Name<'input, 'a> {
    Name {
        value: tok.string,
        ..Default::default()
    }
}

fn make_dot<'input, 'a>(tok: TokenRef<'input, 'a>) -> Dot<'input, 'a> {
    Dot { tok }
}

fn make_import_alias<'input, 'a>(
    name: NameOrAttribute<'input, 'a>,
    asname: Option<(TokenRef<'input, 'a>, Name<'input, 'a>)>,
) -> ImportAlias<'input, 'a> {
    ImportAlias {
        name,
        asname: asname.map(|(x, y)| make_as_name(x, AssignTargetExpression::Name(Box::new(y)))),
        comma: None,
    }
}

fn make_as_name<'input, 'a>(
    as_tok: TokenRef<'input, 'a>,
    name: AssignTargetExpression<'input, 'a>,
) -> AsName<'input, 'a> {
    AsName { name, as_tok }
}

type ParenthesizedImportNames<'input, 'a> = (
    Option<LeftParen<'input, 'a>>,
    ImportNames<'input, 'a>,
    Option<RightParen<'input, 'a>>,
);

fn make_import_from<'input, 'a>(
    from_tok: TokenRef<'input, 'a>,
    dots: Vec<Dot<'input, 'a>>,
    module: Option<NameOrAttribute<'input, 'a>>,
    import_tok: TokenRef<'input, 'a>,
    aliases: ParenthesizedImportNames<'input, 'a>,
) -> ImportFrom<'input, 'a> {
    let (lpar, names, rpar) = aliases;

    ImportFrom {
        module,
        names,
        relative: dots,
        lpar,
        rpar,
        semicolon: None,
        from_tok,
        import_tok,
    }
}

fn make_import<'input, 'a>(
    import_tok: TokenRef<'input, 'a>,
    names: Vec<ImportAlias<'input, 'a>>,
) -> Import<'input, 'a> {
    Import {
        names,
        semicolon: None,
        import_tok,
    }
}

fn make_import_from_as_names<'input, 'a>(
    first: ImportAlias<'input, 'a>,
    tail: Vec<(Comma<'input, 'a>, ImportAlias<'input, 'a>)>,
) -> Vec<ImportAlias<'input, 'a>> {
    let mut ret = vec![];
    let mut cur = first;
    for (comma, alias) in tail {
        ret.push(cur.with_comma(comma));
        cur = alias;
    }
    ret.push(cur);
    ret
}

fn make_lpar<'input, 'a>(tok: TokenRef<'input, 'a>) -> LeftParen<'input, 'a> {
    LeftParen { lpar_tok: tok }
}

fn make_rpar<'input, 'a>(tok: TokenRef<'input, 'a>) -> RightParen<'input, 'a> {
    RightParen { rpar_tok: tok }
}

fn make_module<'input, 'a>(
    body: Vec<Statement<'input, 'a>>,
    tok: TokenRef<'input, 'a>,
    encoding: &str,
) -> Module<'input, 'a> {
    Module {
        body,
        eof_tok: tok,
        default_indent: "    ",
        default_newline: "\n",
        has_trailing_newline: false,
        encoding: encoding.to_string(),
    }
}

fn make_attribute<'input, 'a>(
    value: Expression<'input, 'a>,
    dot: TokenRef<'input, 'a>,
    attr: Name<'input, 'a>,
) -> Attribute<'input, 'a> {
    let dot = make_dot(dot);
    Attribute {
        attr,
        dot,
        lpar: Default::default(),
        rpar: Default::default(),
        value: Box::new(value),
    }
}

fn make_starred_element<'input, 'a>(
    star_tok: TokenRef<'input, 'a>,
    rest: Element<'input, 'a>,
) -> StarredElement<'input, 'a> {
    let value = match rest {
        Element::Simple { value, .. } => value,
        _ => panic!("Internal error while making starred element"),
    };
    StarredElement {
        value: Box::new(value),
        lpar: Default::default(),
        rpar: Default::default(),
        comma: Default::default(),
        star_tok,
    }
}

fn assign_target_to_element<'input, 'a>(
    expr: AssignTargetExpression<'input, 'a>,
) -> Element<'input, 'a> {
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

fn make_assignment<'input, 'a>(
    lhs: Vec<(AssignTargetExpression<'input, 'a>, TokenRef<'input, 'a>)>,
    rhs: Expression<'input, 'a>,
) -> Assign<'input, 'a> {
    let mut targets = vec![];
    for (target, equal_tok) in lhs {
        targets.push(AssignTarget { target, equal_tok });
    }
    Assign {
        targets,
        value: rhs,
        semicolon: Default::default(),
    }
}

fn expr_to_element<'input, 'a>(expr: Expression<'input, 'a>) -> Element<'input, 'a> {
    match expr {
        Expression::StarredElement(inner_expr) => Element::Starred(inner_expr),
        _ => Element::Simple {
            value: expr,
            comma: Default::default(),
        },
    }
}

fn make_tuple<'input, 'a>(
    first: Element<'input, 'a>,
    rest: Vec<(Comma<'input, 'a>, Element<'input, 'a>)>,
    trailing_comma: Option<Comma<'input, 'a>>,
    lpar: Option<LeftParen<'input, 'a>>,
    rpar: Option<RightParen<'input, 'a>>,
) -> Tuple<'input, 'a> {
    let elements = comma_separate(first, rest, trailing_comma);

    let lpar = lpar.map(|l| vec![l]).unwrap_or_default();
    let rpar = rpar.map(|r| vec![r]).unwrap_or_default();

    Tuple {
        elements,
        lpar,
        rpar,
    }
}

fn make_tuple_from_elements<'input, 'a>(
    first: Element<'input, 'a>,
    mut rest: Vec<Element<'input, 'a>>,
) -> Tuple<'input, 'a> {
    rest.insert(0, first);
    Tuple {
        elements: rest,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_kwarg<'input, 'a>(
    name: Name<'input, 'a>,
    eq: TokenRef<'input, 'a>,
    value: Expression<'input, 'a>,
) -> Arg<'input, 'a> {
    let equal = Some(make_assign_equal(eq));
    let keyword = Some(name);
    Arg {
        value,
        keyword,
        equal,
        comma: None,
        star: "",
        star_tok: None,
    }
}

fn make_star_arg<'input, 'a>(
    star: TokenRef<'input, 'a>,
    expr: Expression<'input, 'a>,
) -> Arg<'input, 'a> {
    let str = star.string;
    Arg {
        value: expr,
        keyword: None,
        equal: None,
        comma: None,
        star: str,
        star_tok: Some(star),
    }
}

fn make_call<'input, 'a>(
    func: Expression<'input, 'a>,
    lpar_tok: TokenRef<'input, 'a>,
    args: Vec<Arg<'input, 'a>>,
    rpar_tok: TokenRef<'input, 'a>,
) -> Call<'input, 'a> {
    let lpar = vec![];
    let rpar = vec![];
    let func = Box::new(func);

    Call {
        func,
        args,
        lpar,
        rpar,
        lpar_tok,
        rpar_tok,
    }
}

fn make_genexp_call<'input, 'a>(
    func: Expression<'input, 'a>,
    mut genexp: GeneratorExp<'input, 'a>,
) -> Call<'input, 'a> {
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
            value: Expression::GeneratorExp(Box::new(genexp)),
            keyword: None,
            equal: None,
            comma: None,
            star: "",
            star_tok: None,
        }],
        lpar: vec![],
        rpar: vec![],
        lpar_tok,
        rpar_tok,
    }
}

fn make_arg<'input, 'a>(expr: Expression<'input, 'a>) -> Arg<'input, 'a> {
    Arg {
        value: expr,
        keyword: Default::default(),
        equal: Default::default(),
        comma: Default::default(),
        star: Default::default(),
        star_tok: None,
    }
}

fn make_comp_if<'input, 'a>(
    if_tok: TokenRef<'input, 'a>,
    test: Expression<'input, 'a>,
) -> CompIf<'input, 'a> {
    CompIf { test, if_tok }
}

fn make_for_if<'input, 'a>(
    async_tok: Option<TokenRef<'input, 'a>>,
    for_tok: TokenRef<'input, 'a>,
    target: AssignTargetExpression<'input, 'a>,
    in_tok: TokenRef<'input, 'a>,
    iter: Expression<'input, 'a>,
    ifs: Vec<CompIf<'input, 'a>>,
) -> CompFor<'input, 'a> {
    let inner_for_in = None;
    let asynchronous = async_tok.as_ref().map(|_| make_async());

    CompFor {
        target,
        iter,
        ifs,
        inner_for_in,
        asynchronous,
        async_tok,
        for_tok,
        in_tok,
    }
}

fn make_bare_genexp<'input, 'a>(
    elt: Expression<'input, 'a>,
    for_in: CompFor<'input, 'a>,
) -> GeneratorExp<'input, 'a> {
    GeneratorExp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn merge_comp_fors<'input, 'a>(
    comp_fors: Vec<CompFor<'input, 'a>>,
) -> GrammarResult<CompFor<'input, 'a>> {
    if comp_fors.len() > MAX_RECURSION_DEPTH {
        return Err("shallower comprehension");
    }
    let mut it = comp_fors.into_iter().rev();
    let first = it.next().expect("cant merge empty comp_fors");

    Ok(it.fold(first, |acc, curr| CompFor {
        inner_for_in: Some(Box::new(acc)),
        ..curr
    }))
}

fn make_left_bracket<'input, 'a>(tok: TokenRef<'input, 'a>) -> LeftSquareBracket<'input, 'a> {
    LeftSquareBracket { tok }
}

fn make_right_bracket<'input, 'a>(tok: TokenRef<'input, 'a>) -> RightSquareBracket<'input, 'a> {
    RightSquareBracket { tok }
}

fn make_left_brace<'input, 'a>(tok: TokenRef<'input, 'a>) -> LeftCurlyBrace<'input, 'a> {
    LeftCurlyBrace { tok }
}

fn make_right_brace<'input, 'a>(tok: TokenRef<'input, 'a>) -> RightCurlyBrace<'input, 'a> {
    RightCurlyBrace { tok }
}

fn make_list_comp<'input, 'a>(
    lbracket: LeftSquareBracket<'input, 'a>,
    elt: Expression<'input, 'a>,
    for_in: CompFor<'input, 'a>,
    rbracket: RightSquareBracket<'input, 'a>,
) -> ListComp<'input, 'a> {
    ListComp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_set_comp<'input, 'a>(
    lbrace: LeftCurlyBrace<'input, 'a>,
    elt: Expression<'input, 'a>,
    for_in: CompFor<'input, 'a>,
    rbrace: RightCurlyBrace<'input, 'a>,
) -> SetComp<'input, 'a> {
    SetComp {
        elt: Box::new(elt),
        for_in: Box::new(for_in),
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_dict_comp<'input, 'a>(
    lbrace: LeftCurlyBrace<'input, 'a>,
    kvpair: (
        Expression<'input, 'a>,
        TokenRef<'input, 'a>,
        Expression<'input, 'a>,
    ),
    for_in: CompFor<'input, 'a>,
    rbrace: RightCurlyBrace<'input, 'a>,
) -> DictComp<'input, 'a> {
    let (key, colon_tok, value) = kvpair;

    DictComp {
        key: Box::new(key),
        value: Box::new(value),
        for_in: Box::new(for_in),
        lbrace,
        rbrace,
        lpar: vec![],
        rpar: vec![],
        colon_tok,
    }
}

fn make_list<'input, 'a>(
    lbracket: LeftSquareBracket<'input, 'a>,
    elements: Vec<Element<'input, 'a>>,
    rbracket: RightSquareBracket<'input, 'a>,
) -> List<'input, 'a> {
    List {
        elements,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_set<'input, 'a>(
    lbrace: LeftCurlyBrace<'input, 'a>,
    elements: Vec<Element<'input, 'a>>,
    rbrace: RightCurlyBrace<'input, 'a>,
) -> Set<'input, 'a> {
    Set {
        elements,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn comma_separate<'input, 'a, T>(
    first: T,
    rest: Vec<(Comma<'input, 'a>, T)>,
    trailing_comma: Option<Comma<'input, 'a>>,
) -> Vec<T>
where
    T: WithComma<'input, 'a>,
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

fn make_dict<'input, 'a>(
    lbrace: LeftCurlyBrace<'input, 'a>,
    elements: Vec<DictElement<'input, 'a>>,
    rbrace: RightCurlyBrace<'input, 'a>,
) -> Dict<'input, 'a> {
    Dict {
        elements,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_double_starred_keypairs<'input, 'a>(
    first: DictElement<'input, 'a>,
    rest: Vec<(Comma<'input, 'a>, DictElement<'input, 'a>)>,
    trailing_comma: Option<Comma<'input, 'a>>,
) -> Vec<DictElement<'input, 'a>> {
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

fn make_dict_element<'input, 'a>(
    el: (
        Expression<'input, 'a>,
        TokenRef<'input, 'a>,
        Expression<'input, 'a>,
    ),
) -> DictElement<'input, 'a> {
    let (key, colon_tok, value) = el;
    DictElement::Simple {
        key,
        value,
        comma: Default::default(),
        colon_tok,
    }
}

fn make_double_starred_element<'input, 'a>(
    star_tok: TokenRef<'input, 'a>,
    value: Expression<'input, 'a>,
) -> StarredDictElement<'input, 'a> {
    StarredDictElement {
        value,
        comma: Default::default(),
        star_tok,
    }
}

fn make_index<'input, 'a>(value: Expression<'input, 'a>) -> BaseSlice<'input, 'a> {
    BaseSlice::Index(Box::new(Index {
        value,
        star: None,
        star_tok: None,
    }))
}

fn make_index_from_arg<'input, 'a>(arg: Arg<'input, 'a>) -> BaseSlice<'input, 'a> {
    BaseSlice::Index(Box::new(Index {
        value: arg.value,
        star: Some(arg.star),
        star_tok: arg.star_tok,
    }))
}

fn make_colon<'input, 'a>(tok: TokenRef<'input, 'a>) -> Colon<'input, 'a> {
    Colon { tok }
}

fn make_slice<'input, 'a>(
    lower: Option<Expression<'input, 'a>>,
    first_colon: TokenRef<'input, 'a>,
    upper: Option<Expression<'input, 'a>>,
    rest: Option<(TokenRef<'input, 'a>, Option<Expression<'input, 'a>>)>,
) -> BaseSlice<'input, 'a> {
    let first_colon = make_colon(first_colon);
    let (second_colon, step) = if let Some((tok, step)) = rest {
        (Some(make_colon(tok)), step)
    } else {
        (None, None)
    };
    BaseSlice::Slice(Box::new(Slice {
        lower,
        upper,
        step,
        first_colon,
        second_colon,
    }))
}

fn make_slices<'input, 'a>(
    first: BaseSlice<'input, 'a>,
    rest: Vec<(Comma<'input, 'a>, BaseSlice<'input, 'a>)>,
    trailing_comma: Option<Comma<'input, 'a>>,
) -> Vec<SubscriptElement<'input, 'a>> {
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

fn make_subscript<'input, 'a>(
    value: Expression<'input, 'a>,
    lbracket: LeftSquareBracket<'input, 'a>,
    slice: Vec<SubscriptElement<'input, 'a>>,
    rbracket: RightSquareBracket<'input, 'a>,
) -> Subscript<'input, 'a> {
    Subscript {
        value: Box::new(value),
        slice,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_ifexp<'input, 'a>(
    body: Expression<'input, 'a>,
    if_tok: TokenRef<'input, 'a>,
    test: Expression<'input, 'a>,
    else_tok: TokenRef<'input, 'a>,
    orelse: Expression<'input, 'a>,
) -> IfExp<'input, 'a> {
    IfExp {
        test: Box::new(test),
        body: Box::new(body),
        orelse: Box::new(orelse),
        lpar: Default::default(),
        rpar: Default::default(),
        if_tok,
        else_tok,
    }
}

fn add_arguments_trailing_comma<'input, 'a>(
    mut args: Vec<Arg<'input, 'a>>,
    trailing_comma: Option<Comma<'input, 'a>>,
) -> Vec<Arg<'input, 'a>> {
    if let Some(comma) = trailing_comma {
        let last = args.pop().unwrap();
        args.push(last.with_comma(comma));
    }
    args
}

fn make_lambda<'input, 'a>(
    lambda_tok: TokenRef<'input, 'a>,
    params: Parameters<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    expr: Expression<'input, 'a>,
) -> Lambda<'input, 'a> {
    let colon = make_colon(colon_tok);
    Lambda {
        params: Box::new(params),
        body: Box::new(expr),
        colon,
        lpar: Default::default(),
        rpar: Default::default(),
        lambda_tok,
    }
}

fn make_annotation<'input, 'a>(
    tok: TokenRef<'input, 'a>,
    ann: Expression<'input, 'a>,
) -> Annotation<'input, 'a> {
    Annotation {
        annotation: ann,
        tok,
    }
}

fn make_ann_assignment<'input, 'a>(
    target: AssignTargetExpression<'input, 'a>,
    col: TokenRef<'input, 'a>,
    ann: Expression<'input, 'a>,
    rhs: Option<(TokenRef<'input, 'a>, Expression<'input, 'a>)>,
) -> AnnAssign<'input, 'a> {
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

fn make_yield<'input, 'a>(
    yield_tok: TokenRef<'input, 'a>,
    f: Option<TokenRef<'input, 'a>>,
    e: Option<Expression<'input, 'a>>,
) -> Yield<'input, 'a> {
    let value = match (f, e) {
        (None, None) => None,
        (Some(f), Some(e)) => Some(YieldValue::From(Box::new(make_from(f, e)))),
        (None, Some(e)) => Some(YieldValue::Expression(Box::new(e))),
        _ => panic!("yield from without expression"),
    };
    Yield {
        value: value.map(Box::new),
        lpar: Default::default(),
        rpar: Default::default(),
        yield_tok,
    }
}

fn make_from<'input, 'a>(tok: TokenRef<'input, 'a>, e: Expression<'input, 'a>) -> From<'input, 'a> {
    From { item: e, tok }
}

fn make_return<'input, 'a>(
    return_tok: TokenRef<'input, 'a>,
    value: Option<Expression<'input, 'a>>,
) -> Return<'input, 'a> {
    Return {
        value,
        semicolon: Default::default(),
        return_tok,
    }
}

fn make_assert<'input, 'a>(
    assert_tok: TokenRef<'input, 'a>,
    test: Expression<'input, 'a>,
    rest: Option<(Comma<'input, 'a>, Expression<'input, 'a>)>,
) -> Assert<'input, 'a> {
    let (comma, msg) = if let Some((c, msg)) = rest {
        (Some(c), Some(msg))
    } else {
        (None, None)
    };

    Assert {
        test,
        msg,
        comma,
        semicolon: Default::default(),
        assert_tok,
    }
}

fn make_raise<'input, 'a>(
    raise_tok: TokenRef<'input, 'a>,
    exc: Option<Expression<'input, 'a>>,
    rest: Option<(TokenRef<'input, 'a>, Expression<'input, 'a>)>,
) -> Raise<'input, 'a> {
    let cause = rest.map(|(t, e)| make_from(t, e));

    Raise {
        exc,
        cause,
        semicolon: Default::default(),
        raise_tok,
    }
}

fn make_global<'input, 'a>(
    tok: TokenRef<'input, 'a>,
    init: Vec<(Name<'input, 'a>, Comma<'input, 'a>)>,
    last: Name<'input, 'a>,
) -> Global<'input, 'a> {
    let mut names: Vec<NameItem<'input, 'a>> = init
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
        semicolon: Default::default(),
        tok,
    }
}

fn make_nonlocal<'input, 'a>(
    tok: TokenRef<'input, 'a>,
    init: Vec<(Name<'input, 'a>, Comma<'input, 'a>)>,
    last: Name<'input, 'a>,
) -> Nonlocal<'input, 'a> {
    let mut names: Vec<NameItem<'input, 'a>> = init
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
        semicolon: Default::default(),
        tok,
    }
}

#[allow(clippy::too_many_arguments)]
fn make_for<'input, 'a>(
    async_tok: Option<TokenRef<'input, 'a>>,
    for_tok: TokenRef<'input, 'a>,
    target: AssignTargetExpression<'input, 'a>,
    in_tok: TokenRef<'input, 'a>,
    iter: Expression<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
    orelse: Option<Else<'input, 'a>>,
) -> For<'input, 'a> {
    let asynchronous = async_tok.as_ref().map(|_| make_async());

    For {
        target,
        iter,
        body,
        orelse,
        asynchronous,
        async_tok,
        for_tok,
        in_tok,
        colon_tok,
    }
}

fn make_while<'input, 'a>(
    while_tok: TokenRef<'input, 'a>,
    test: Expression<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
    orelse: Option<Else<'input, 'a>>,
) -> While<'input, 'a> {
    While {
        test,
        body,
        orelse,
        while_tok,
        colon_tok,
    }
}

fn make_await<'input, 'a>(
    await_tok: TokenRef<'input, 'a>,
    expression: Expression<'input, 'a>,
) -> Await<'input, 'a> {
    Await {
        expression: Box::new(expression),
        lpar: Default::default(),
        rpar: Default::default(),
        await_tok,
    }
}

fn make_class_def<'input, 'a>(
    class_tok: TokenRef<'input, 'a>,
    name: Name<'input, 'a>,
    type_parameters: Option<TypeParameters<'input, 'a>>,
    args: Option<(
        LeftParen<'input, 'a>,
        Option<Vec<Arg<'input, 'a>>>,
        RightParen<'input, 'a>,
    )>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> std::result::Result<ClassDef<'input, 'a>, &'static str> {
    let mut bases = vec![];
    let mut keywords = vec![];
    let mut lpar_tok = None;
    let mut rpar_tok = None;
    let mut lpar = None;
    let mut rpar = None;

    if let Some((lpar_, args, rpar_)) = args {
        lpar_tok = Some(lpar_.lpar_tok);
        rpar_tok = Some(rpar_.rpar_tok);
        lpar = Some(lpar_);
        rpar = Some(rpar_);
        if let Some(args) = args {
            let mut current_arg = &mut bases;
            let mut seen_keyword = false;
            for arg in args {
                if arg.star == "**" || arg.keyword.is_some() {
                    current_arg = &mut keywords;
                    seen_keyword = true;
                }
                if seen_keyword
                    && (arg.star == "*" || (arg.star.is_empty() && arg.keyword.is_none()))
                {
                    return Err("Positional argument follows keyword argument");
                }
                // TODO: libcst-python does validation here
                current_arg.push(arg);
            }
        }
    }
    Ok(ClassDef {
        name,
        type_parameters,
        body,
        bases,
        keywords,
        decorators: vec![],
        lpar,
        rpar,
        class_tok,
        lpar_tok,
        rpar_tok,
        colon_tok,
    })
}

fn make_string<'input, 'a>(tok: TokenRef<'input, 'a>) -> String<'input, 'a> {
    String::Simple(SimpleString {
        value: tok.string,
        ..Default::default()
    })
}

fn make_strings<'input, 'a>(
    s: Vec<(String<'input, 'a>, TokenRef<'input, 'a>)>,
) -> GrammarResult<String<'input, 'a>> {
    if s.len() > MAX_RECURSION_DEPTH {
        return Err("shorter concatenated string");
    }
    let mut strings = s.into_iter().rev();
    let (first, _) = strings.next().expect("no strings to make a string of");
    Ok(strings.fold(first, |acc, (str, tok)| {
        let ret: String<'input, 'a> = String::Concatenated(ConcatenatedString {
            left: Box::new(str),
            right: Box::new(acc),
            lpar: Default::default(),
            rpar: Default::default(),
            right_tok: tok,
        });
        ret
    }))
}

fn make_fstring_expression<'input, 'a>(
    lbrace_tok: TokenRef<'input, 'a>,
    expression: Expression<'input, 'a>,
    eq: Option<TokenRef<'input, 'a>>,
    conversion_pair: Option<(TokenRef<'input, 'a>, &'a str)>,
    format_pair: Option<(
        TokenRef<'input, 'a>,
        Vec<FormattedStringContent<'input, 'a>>,
    )>,
    rbrace_tok: TokenRef<'input, 'a>,
) -> FormattedStringExpression<'input, 'a> {
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
        equal,
        lbrace_tok,
        after_expr_tok,
    }
}

fn make_fstring<'input, 'a>(
    start: &'a str,
    parts: Vec<FormattedStringContent<'input, 'a>>,
    end: &'a str,
) -> FormattedString<'input, 'a> {
    FormattedString {
        start,
        parts,
        end,
        lpar: Default::default(),
        rpar: Default::default(),
    }
}

fn make_finally<'input, 'a>(
    finally_tok: TokenRef<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> Finally<'input, 'a> {
    Finally {
        body,
        finally_tok,
        colon_tok,
    }
}

fn make_except<'input, 'a>(
    except_tok: TokenRef<'input, 'a>,
    exp: Option<Expression<'input, 'a>>,
    as_: Option<(TokenRef<'input, 'a>, Name<'input, 'a>)>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> ExceptHandler<'input, 'a> {
    // TODO: AsName should come from outside
    let name = as_.map(|(x, y)| make_as_name(x, AssignTargetExpression::Name(Box::new(y))));
    ExceptHandler {
        body,
        r#type: exp,
        name,
        except_tok,
        colon_tok,
    }
}

fn make_except_star<'input, 'a>(
    except_tok: TokenRef<'input, 'a>,
    star_tok: TokenRef<'input, 'a>,
    exp: Expression<'input, 'a>,
    as_: Option<(TokenRef<'input, 'a>, Name<'input, 'a>)>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> ExceptStarHandler<'input, 'a> {
    // TODO: AsName should come from outside
    let name = as_.map(|(x, y)| make_as_name(x, AssignTargetExpression::Name(Box::new(y))));
    ExceptStarHandler {
        body,
        r#type: exp,
        name,
        except_tok,
        colon_tok,
        star_tok,
    }
}

fn make_try<'input, 'a>(
    try_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
    handlers: Vec<ExceptHandler<'input, 'a>>,
    orelse: Option<Else<'input, 'a>>,
    finalbody: Option<Finally<'input, 'a>>,
) -> Try<'input, 'a> {
    Try {
        body,
        handlers,
        orelse,
        finalbody,
        try_tok,
    }
}

fn make_try_star<'input, 'a>(
    try_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
    handlers: Vec<ExceptStarHandler<'input, 'a>>,
    orelse: Option<Else<'input, 'a>>,
    finalbody: Option<Finally<'input, 'a>>,
) -> TryStar<'input, 'a> {
    TryStar {
        body,
        handlers,
        orelse,
        finalbody,
        try_tok,
    }
}

fn make_aug_op<'input, 'a>(tok: TokenRef<'input, 'a>) -> Result<'a, AugOp<'input, 'a>> {
    Ok(match tok.string {
        "+=" => AugOp::AddAssign { tok },
        "-=" => AugOp::SubtractAssign { tok },
        "*=" => AugOp::MultiplyAssign { tok },
        "@=" => AugOp::MatrixMultiplyAssign { tok },
        "/=" => AugOp::DivideAssign { tok },
        "%=" => AugOp::ModuloAssign { tok },
        "&=" => AugOp::BitAndAssign { tok },
        "|=" => AugOp::BitOrAssign { tok },
        "^=" => AugOp::BitXorAssign { tok },
        "<<=" => AugOp::LeftShiftAssign { tok },
        ">>=" => AugOp::RightShiftAssign { tok },
        "**=" => AugOp::PowerAssign { tok },
        "//=" => AugOp::FloorDivideAssign { tok },
        _ => return Err(ParserError::OperatorError),
    })
}

fn make_aug_assign<'input, 'a>(
    target: AssignTargetExpression<'input, 'a>,
    operator: AugOp<'input, 'a>,
    value: Expression<'input, 'a>,
) -> AugAssign<'input, 'a> {
    AugAssign {
        target,
        operator,
        value,
        semicolon: Default::default(),
    }
}

fn make_with_item<'input, 'a>(
    item: Expression<'input, 'a>,
    as_: Option<TokenRef<'input, 'a>>,
    n: Option<AssignTargetExpression<'input, 'a>>,
) -> WithItem<'input, 'a> {
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

fn make_with<'input, 'a>(
    async_tok: Option<TokenRef<'input, 'a>>,
    with_tok: TokenRef<'input, 'a>,
    lpar: Option<LeftParen<'input, 'a>>,
    items: Vec<WithItem<'input, 'a>>,
    rpar: Option<RightParen<'input, 'a>>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> With<'input, 'a> {
    let asynchronous = async_tok.as_ref().map(|_| make_async());
    With {
        items,
        body,
        asynchronous,
        lpar,
        rpar,
        async_tok,
        with_tok,
        colon_tok,
    }
}

fn make_del<'input, 'a>(
    tok: TokenRef<'input, 'a>,
    target: DelTargetExpression<'input, 'a>,
) -> Del<'input, 'a> {
    Del {
        target,
        semicolon: Default::default(),
        tok,
    }
}

fn make_del_tuple<'input, 'a>(
    lpar: Option<LeftParen<'input, 'a>>,
    elements: Vec<Element<'input, 'a>>,
    rpar: Option<RightParen<'input, 'a>>,
) -> DelTargetExpression<'input, 'a> {
    DelTargetExpression::Tuple(Box::new(Tuple {
        elements,
        lpar: lpar.map(|x| vec![x]).unwrap_or_default(),
        rpar: rpar.map(|x| vec![x]).unwrap_or_default(),
    }))
}

fn make_named_expr<'input, 'a>(
    name: Name<'input, 'a>,
    tok: TokenRef<'input, 'a>,
    expr: Expression<'input, 'a>,
) -> NamedExpr<'input, 'a> {
    NamedExpr {
        target: Box::new(Expression::Name(Box::new(name))),
        value: Box::new(expr),
        lpar: Default::default(),
        rpar: Default::default(),
        walrus_tok: tok,
    }
}

fn make_match<'input, 'a>(
    match_tok: TokenRef<'input, 'a>,
    subject: Expression<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    indent_tok: TokenRef<'input, 'a>,
    cases: Vec<MatchCase<'input, 'a>>,
    dedent_tok: TokenRef<'input, 'a>,
) -> Match<'input, 'a> {
    Match {
        subject,
        cases,
        indent: Default::default(),
        match_tok,
        colon_tok,
        indent_tok,
        dedent_tok,
    }
}

fn make_case<'input, 'a>(
    case_tok: TokenRef<'input, 'a>,
    pattern: MatchPattern<'input, 'a>,
    guard: Option<(TokenRef<'input, 'a>, Expression<'input, 'a>)>,
    colon_tok: TokenRef<'input, 'a>,
    body: Suite<'input, 'a>,
) -> MatchCase<'input, 'a> {
    let (if_tok, guard) = match guard {
        Some((if_tok, guard)) => (Some(if_tok), Some(guard)),
        None => (None, None),
    };
    MatchCase {
        pattern,
        guard,
        body,
        case_tok,
        if_tok,
        colon_tok,
    }
}

fn make_match_value<'input, 'a>(value: Expression<'input, 'a>) -> MatchPattern<'input, 'a> {
    MatchPattern::Value(MatchValue { value })
}

fn make_match_singleton<'input, 'a>(value: Name<'input, 'a>) -> MatchPattern<'input, 'a> {
    MatchPattern::Singleton(MatchSingleton { value })
}

fn make_list_pattern<'input, 'a>(
    lbracket: Option<LeftSquareBracket<'input, 'a>>,
    patterns: Vec<StarrableMatchSequenceElement<'input, 'a>>,
    rbracket: Option<RightSquareBracket<'input, 'a>>,
) -> MatchSequence<'input, 'a> {
    MatchSequence::MatchList(MatchList {
        patterns,
        lbracket,
        rbracket,
        lpar: Default::default(),
        rpar: Default::default(),
    })
}

fn make_as_pattern<'input, 'a>(
    pattern: Option<MatchPattern<'input, 'a>>,
    as_tok: Option<TokenRef<'input, 'a>>,
    name: Option<Name<'input, 'a>>,
) -> MatchPattern<'input, 'a> {
    MatchPattern::As(Box::new(MatchAs {
        pattern,
        name,
        lpar: Default::default(),
        rpar: Default::default(),
        as_tok,
    }))
}

fn make_bit_or<'input, 'a>(tok: TokenRef<'input, 'a>) -> BitOr<'input, 'a> {
    BitOr { tok }
}

fn make_or_pattern<'input, 'a>(
    first: MatchPattern<'input, 'a>,
    rest: Vec<(TokenRef<'input, 'a>, MatchPattern<'input, 'a>)>,
) -> MatchPattern<'input, 'a> {
    if rest.is_empty() {
        return first;
    }

    let mut patterns = vec![];
    let mut current = first;
    for (sep, next) in rest {
        let op = make_bit_or(sep);
        patterns.push(MatchOrElement {
            pattern: current,
            separator: Some(op),
        });
        current = next;
    }
    patterns.push(MatchOrElement {
        pattern: current,
        separator: None,
    });
    MatchPattern::Or(Box::new(MatchOr {
        patterns,
        lpar: Default::default(),
        rpar: Default::default(),
    }))
}

fn ensure_real_number<'input, 'a>(
    tok: TokenRef<'input, 'a>,
) -> GrammarResult<Expression<'input, 'a>> {
    match make_number(tok) {
        e @ (Expression::Integer(_) | Expression::Float(_)) => Ok(e),
        _ => Err("real number"),
    }
}

fn ensure_imaginary_number<'input, 'a>(
    tok: TokenRef<'input, 'a>,
) -> GrammarResult<Expression<'input, 'a>> {
    match make_number(tok) {
        e @ Expression::Imaginary(_) => Ok(e),
        _ => Err("imaginary number"),
    }
}

fn make_tuple_pattern<'input, 'a>(
    lpar: LeftParen<'input, 'a>,
    patterns: Vec<StarrableMatchSequenceElement<'input, 'a>>,
    rpar: RightParen<'input, 'a>,
) -> MatchSequence<'input, 'a> {
    MatchSequence::MatchTuple(MatchTuple {
        patterns,
        lpar: vec![lpar],
        rpar: vec![rpar],
    })
}

fn make_open_sequence_pattern<'input, 'a>(
    first: StarrableMatchSequenceElement<'input, 'a>,
    comma: Comma<'input, 'a>,
    mut rest: Vec<StarrableMatchSequenceElement<'input, 'a>>,
) -> Vec<StarrableMatchSequenceElement<'input, 'a>> {
    rest.insert(0, first.with_comma(comma));
    rest
}

fn make_match_sequence_element<'input, 'a>(
    value: MatchPattern<'input, 'a>,
) -> MatchSequenceElement<'input, 'a> {
    MatchSequenceElement {
        value,
        comma: Default::default(),
    }
}

fn make_match_star<'input, 'a>(
    star_tok: TokenRef<'input, 'a>,
    name: Option<Name<'input, 'a>>,
) -> MatchStar<'input, 'a> {
    MatchStar {
        name,
        comma: Default::default(),
        star_tok,
    }
}

fn make_match_mapping<'input, 'a>(
    lbrace: LeftCurlyBrace<'input, 'a>,
    mut elements: Vec<MatchMappingElement<'input, 'a>>,
    el_comma: Option<Comma<'input, 'a>>,
    star_tok: Option<TokenRef<'input, 'a>>,
    rest: Option<Name<'input, 'a>>,
    trailing_comma: Option<Comma<'input, 'a>>,
    rbrace: RightCurlyBrace<'input, 'a>,
) -> MatchPattern<'input, 'a> {
    if let Some(c) = el_comma {
        if let Some(el) = elements.pop() {
            elements.push(el.with_comma(c));
        }
        // TODO: else raise error
    }
    MatchPattern::Mapping(MatchMapping {
        elements,
        rest,
        trailing_comma,
        lbrace,
        rbrace,
        lpar: Default::default(),
        rpar: Default::default(),
        star_tok,
    })
}

fn make_match_mapping_element<'input, 'a>(
    key: Expression<'input, 'a>,
    colon_tok: TokenRef<'input, 'a>,
    pattern: MatchPattern<'input, 'a>,
) -> MatchMappingElement<'input, 'a> {
    MatchMappingElement {
        key,
        pattern,
        comma: Default::default(),
        colon_tok,
    }
}

fn make_class_pattern<'input, 'a>(
    cls: NameOrAttribute<'input, 'a>,
    lpar_tok: TokenRef<'input, 'a>,
    mut patterns: Vec<MatchSequenceElement<'input, 'a>>,
    pat_comma: Option<Comma<'input, 'a>>,
    mut kwds: Vec<MatchKeywordElement<'input, 'a>>,
    kwd_comma: Option<Comma<'input, 'a>>,
    rpar_tok: TokenRef<'input, 'a>,
) -> MatchPattern<'input, 'a> {
    if let Some(c) = pat_comma {
        if let Some(el) = patterns.pop() {
            patterns.push(el.with_comma(c));
        }
        // TODO: else raise error
    }
    if let Some(c) = kwd_comma {
        if let Some(el) = kwds.pop() {
            kwds.push(el.with_comma(c));
        }
        // TODO: else raise error
    }
    MatchPattern::Class(MatchClass {
        cls,
        patterns,
        kwds,
        lpar: Default::default(),
        rpar: Default::default(),
        lpar_tok,
        rpar_tok,
    })
}

fn make_match_keyword_element<'input, 'a>(
    key: Name<'input, 'a>,
    equal_tok: TokenRef<'input, 'a>,
    pattern: MatchPattern<'input, 'a>,
) -> MatchKeywordElement<'input, 'a> {
    MatchKeywordElement {
        key,
        pattern,
        comma: Default::default(),
        equal_tok,
    }
}

struct TypeParamBound<'input, 'a>(TokenRef<'input, 'a>, Expression<'input, 'a>);

fn make_type_param_bound<'input, 'a>(
    colon_tok: TokenRef<'input, 'a>,
    e: Expression<'input, 'a>,
) -> TypeParamBound<'input, 'a> {
    TypeParamBound(colon_tok, e)
}

fn make_param_spec<'input, 'a>(
    star_tok: TokenRef<'input, 'a>,
    name: Name<'input, 'a>,
    def: Option<(AssignEqual<'input, 'a>, Expression<'input, 'a>)>,
) -> TypeParam<'input, 'a> {
    let (equal, default) = match def {
        Some((a, b)) => (Some(a), Some(b)),
        None => (None, None),
    };
    TypeParam {
        param: TypeVarLike::ParamSpec(ParamSpec { name, star_tok }),
        comma: Default::default(),
        equal: equal,
        star: "",
        default: default,
        star_tok: None,
    }
}

fn make_type_var_tuple<'input, 'a>(
    star_tok: TokenRef<'input, 'a>,
    name: Name<'input, 'a>,
    def: Option<(
        AssignEqual<'input, 'a>,
        Option<TokenRef<'input, 'a>>,
        Expression<'input, 'a>,
    )>,
) -> TypeParam<'input, 'a> {
    let (equal, default_star, default) = match def {
        Some((a, b, c)) => (Some(a), b, Some(c)),
        None => (None, None, None),
    };
    let star = match default_star {
        Some(a) => a.string,
        None => "",
    };

    TypeParam {
        param: TypeVarLike::TypeVarTuple(TypeVarTuple { name, star_tok }),
        comma: Default::default(),
        equal: equal,
        star: star,
        default: default,
        star_tok: default_star,
    }
}

fn make_type_var<'input, 'a>(
    name: Name<'input, 'a>,
    bound: Option<TypeParamBound<'input, 'a>>,
    def: Option<(AssignEqual<'input, 'a>, Expression<'input, 'a>)>,
) -> TypeParam<'input, 'a> {
    let (bound, colon) = match bound {
        Some(TypeParamBound(c, e)) => (Some(Box::new(e)), Some(make_colon(c))),
        _ => (None, None),
    };
    let (equal, default) = match def {
        Some((a, b)) => (Some(a), Some(b)),
        None => (None, None),
    };
    TypeParam {
        param: TypeVarLike::TypeVar(TypeVar { name, bound, colon }),
        comma: Default::default(),
        equal: equal,
        star: "",
        default: default,
        star_tok: None,
    }
}

fn make_type_parameters<'input, 'a>(
    lbracket: LeftSquareBracket<'input, 'a>,
    params: Vec<TypeParam<'input, 'a>>,
    rbracket: RightSquareBracket<'input, 'a>,
) -> TypeParameters<'input, 'a> {
    TypeParameters {
        lbracket,
        params,
        rbracket,
    }
}

fn make_type_alias<'input, 'a>(
    type_tok: TokenRef<'input, 'a>,
    name: Name<'input, 'a>,
    type_parameters: Option<TypeParameters<'input, 'a>>,
    equals_tok: TokenRef<'input, 'a>,
    value: Expression<'input, 'a>,
) -> TypeAlias<'input, 'a> {
    let lbracket_tok = if let Some(tp) = &type_parameters {
        Some(tp.lbracket.tok)
    } else {
        None
    };
    TypeAlias {
        type_tok,
        name,
        type_parameters,
        equals_tok,
        value: Box::new(value),
        semicolon: Default::default(),
        lbracket_tok,
    }
}
