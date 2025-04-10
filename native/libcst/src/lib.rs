// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::cmp::{max, min};

pub mod tokenizer;

pub use tokenizer::whitespace_parser::Config;
use tokenizer::{whitespace_parser, TokConfig, Token, TokenIterator};

mod nodes;
use nodes::deflated::Module as DeflatedModule;
pub use nodes::*;

mod parser;
use parser::{ParserError, Result, TokVec};

#[cfg(feature = "py")]
pub mod py;

pub fn tokenize(text: &str) -> Result<Vec<Token>> {
    let iter = TokenIterator::new(
        text,
        &TokConfig {
            async_hacks: false,
            split_fstring: true,
        },
    );

    iter.collect::<std::result::Result<Vec<_>, _>>()
        .map_err(|err| ParserError::TokenizerError(err, text))
}

pub fn parse_module<'a>(
    mut module_text: &'a str,
    encoding: Option<&str>,
) -> Result<'a, Module<'a>> {
    // Strip UTF-8 BOM
    if let Some(stripped) = module_text.strip_prefix('\u{feff}') {
        module_text = stripped;
    }
    let tokens = tokenize(module_text)?;
    let conf = whitespace_parser::Config::new(module_text, &tokens);
    let tokvec = tokens.into();
    let m = parse_tokens_without_whitespace(&tokvec, module_text, encoding)?;
    Ok(m.inflate(&conf)?)
}

pub fn parse_tokens_without_whitespace<'r, 'a>(
    tokens: &'r TokVec<'a>,
    module_text: &'a str,
    encoding: Option<&str>,
) -> Result<'a, DeflatedModule<'r, 'a>> {
    let m = parser::python::file(tokens, module_text, encoding)
        .map_err(|err| ParserError::ParserError(err, module_text))?;
    Ok(m)
}

pub fn parse_statement(text: &str) -> Result<Statement> {
    let tokens = tokenize(text)?;
    let conf = whitespace_parser::Config::new(text, &tokens);
    let tokvec = tokens.into();
    let stm = parser::python::statement_input(&tokvec, text)
        .map_err(|err| ParserError::ParserError(err, text))?;
    Ok(stm.inflate(&conf)?)
}

pub fn parse_expression(text: &str) -> Result<Expression> {
    let tokens = tokenize(text)?;
    let conf = whitespace_parser::Config::new(text, &tokens);
    let tokvec = tokens.into();
    let expr = parser::python::expression_input(&tokvec, text)
        .map_err(|err| ParserError::ParserError(err, text))?;
    Ok(expr.inflate(&conf)?)
}

// n starts from 1
fn bol_offset(source: &str, n: i32) -> usize {
    if n <= 1 {
        return 0;
    }
    source
        .match_indices('\n')
        .nth((n - 2) as usize)
        .map(|(index, _)| index + 1)
        .unwrap_or_else(|| source.len())
}

pub fn prettify_error(err: ParserError, label: &str) -> std::string::String {
    match err {
        ParserError::ParserError(e, module_text) => {
            use annotate_snippets::{Level, Renderer, Snippet};

            let loc = e.location;
            let context = 1;
            let line_start = max(
                1,
                loc.start_pos
                    .line
                    .checked_sub(context as usize)
                    .unwrap_or(1),
            );
            let start_offset = bol_offset(module_text, loc.start_pos.line as i32 - context);
            let end_offset = bol_offset(module_text, loc.end_pos.line as i32 + context + 1);
            let source = &module_text[start_offset..end_offset];
            let start = loc.start_pos.offset - start_offset;
            let end = loc.end_pos.offset - start_offset;
            let end = if start == end {
                min(end + 1, end_offset - start_offset + 1)
            } else {
                end
            };
            Renderer::styled()
                .render(
                    Level::Error.title(label).snippet(
                        Snippet::source(source)
                            .line_start(line_start)
                            .fold(false)
                            .annotations(vec![Level::Error.span(start..end).label(&format!(
                                "expected {} {} -> {}",
                                e.expected, loc.start_pos, loc.end_pos
                            ))]),
                    ),
                )
                .to_string()
        }
        e => format!("Parse error for {}: {}", label, e),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::TokError;

    #[test]
    fn test_simple() {
        let n = parse_module("1_", None);
        assert_eq!(
            n.err().unwrap(),
            ParserError::TokenizerError(TokError::BadDecimal, "1_")
        );
    }

    #[test]
    fn test_bare_minimum_funcdef() {
        parse_module("def f(): ...", None).expect("parse error");
    }

    #[test]
    fn test_funcdef_params() {
        parse_module("def g(a, b): ...", None).expect("parse error");
    }

    #[test]
    fn test_single_statement_with_no_newline() {
        for src in &[
            "(\n \\\n)",
            "(\n  \\\n)",
            "(\n    '''\n''')",
            "del _",
            "if _:\n    '''\n)'''",
            "if _:\n    ('''\n''')",
            "if _:\n     '''\n  '''",
            "if _:\n        '''\n    ''' ",
        ] {
            parse_module(src, None).unwrap_or_else(|e| panic!("'{}' doesn't parse: {}", src, e));
        }
    }

    #[test]
    fn bol_offset_first_line() {
        assert_eq!(0, bol_offset("hello", 1));
        assert_eq!(0, bol_offset("hello", 0));
        assert_eq!(0, bol_offset("hello\nhello", 1));
        assert_eq!(0, bol_offset("hello\nhello", 0));
    }

    #[test]
    fn bol_offset_second_line() {
        assert_eq!(5, bol_offset("hello", 2));
        assert_eq!(6, bol_offset("hello\nhello", 2));
        assert_eq!(6, bol_offset("hello\nhello\nhello", 2));
    }

    #[test]
    fn bol_offset_last_line() {
        assert_eq!(5, bol_offset("hello", 3));
        assert_eq!(11, bol_offset("hello\nhello", 3));
        assert_eq!(12, bol_offset("hello\nhello\nhello", 3));
    }
}
