use super::*;

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
            = small_stmt() **<1,> ";"
        rule compound_stmt() -> Statement<'a>
            = &("def" / "@" / [Async]) f:function_def() { Statement::FunctionDef(f) }
        rule small_stmt() -> Statement<'a>
            = "pass" { Statement::Pass }

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
            = [n@tok!(Name)] { Param {name: Name {value: n.string}, whitespace_after_param: SimpleWhitespace(""), whitespace_after_star: SimpleWhitespace("")}}
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
        name: Name { value: name.string },
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
        decorator: Name { value: name.string },
        leading_lines: parse_empty_lines(config, &mut at.whitespace_before, None)?,
        whitespace_after_at: parse_simple_whitespace(config, &mut at.whitespace_after)?,
        trailing_whitespace: Default::default(), //parse_trailing_whitespace(config, &mut newline.whitespace_before)?,
    })
}
