// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::cmp::max;

use crate::tokenize::core::{TokConfig, TokError, TokType, Token, TokenIterator};
use peg::{Parse, ParseElem, ParseLiteral, RuleResult};
use thiserror::Error;

mod whitespace;
pub use whitespace::{
    parse_empty_lines, parse_parenthesizable_whitespace, parse_simple_whitespace,
    parse_trailing_whitespace, Comment, Config, EmptyLine, Newline, ParenthesizableWhitespace,
    SimpleWhitespace, State as WhitespaceState, TrailingWhitespace, WhitespaceError,
};
mod statement;
pub use statement::{
    CompoundStatement, Decorator, Else, FunctionDef, If, IndentedBlock, OrElse,
    SimpleStatementLine, SimpleStatementSuite, SmallStatement, Statement, Suite,
};

mod expression;
pub use expression::{Expression, Name, Param, ParamSlash, ParamStar, Parameters, StarArg};

mod op;
pub use op::{AssignEqual, Comma, Semicolon};

mod grammar;
use grammar::python;
mod codegen;
pub use codegen::{Codegen, CodegenState};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParserError<'a> {
    #[error("tokenizer error")]
    TokenizerError(TokError<'a>),
    #[error(transparent)]
    ParserError(#[from] peg::error::ParseError<<grammar::TokVec<'a> as Parse>::PositionRepr>),
    #[error(transparent)]
    WhitespaceError(#[from] WhitespaceError),
}

pub type Result<'a, T> = std::result::Result<T, ParserError<'a>>;

#[derive(Debug, Eq, PartialEq)]
pub struct Module<'a> {
    body: Vec<Statement<'a>>,
}

impl<'a> Codegen for Module<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        for s in &self.body {
            s.codegen(state);
        }
    }
}

pub fn parse_module<'a>(module_text: &'a str) -> Result<'a, Module> {
    let iter = TokenIterator::new(
        module_text,
        &TokConfig {
            async_hacks: false,
            split_fstring: true,
        },
    );

    let result = iter
        .collect::<std::result::Result<Vec<_>, _>>()
        .map_err(|e| ParserError::TokenizerError(e))?
        .into();

    // eprintln!("{:#?}", result);
    let conf = Config {
        default_newline: "\n",
        input: module_text,
        lines: module_text.split_inclusive('\n').collect(),
    };
    python::file(&result, &conf).or_else(|e| Err(ParserError::ParserError(e)))
}

// n starts from 1
fn bol_offset(source: &str, n: i32) -> usize {
    if n <= 1 {
        return 0;
    }
    source
        .match_indices("\n")
        .nth((n - 2) as usize)
        .map(|(index, _)| index)
        .unwrap_or_else(|| source.len())
}

pub fn prettify_error<'a>(module_text: &'a str, err: ParserError<'a>, label: &str) -> String {
    match err {
        ParserError::ParserError(e) => {
            let loc = e.location;
            let context = 1;
            let start_offset = bol_offset(module_text, loc.start_pos.line as i32 - 1 - context);
            let end_offset = bol_offset(module_text, loc.end_pos.line as i32 + context + 1);
            let source = &module_text[start_offset..end_offset];
            chic::Error::new(label)
                .error(
                    max(
                        1,
                        loc.start_pos
                            .line
                            .checked_sub(context as usize)
                            .unwrap_or(1),
                    ),
                    loc.start_pos.offset - start_offset,
                    loc.end_pos.offset - start_offset,
                    source,
                    format!(
                        "expected {} {} -> {}",
                        e.expected, loc.start_pos, loc.end_pos
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
    use crate::parser::{bol_offset, whitespace::Fakeness};

    #[test]
    fn test_simple() {
        let n = parse_module("1_");
        assert_eq!(
            n.err().unwrap(),
            ParserError::TokenizerError(TokError::BadDecimal)
        );
    }

    #[test]
    fn test_bare_minimum_funcdef() {
        let m = parse_module("def f(): ...");
        assert_eq!(
            m,
            Ok(Module {
                body: vec![Statement::Compound(CompoundStatement::FunctionDef(
                    FunctionDef {
                        name: Name {
                            value: "f",
                            ..Default::default()
                        },
                        body: Suite::SimpleStatementSuite(SimpleStatementSuite {
                            body: vec![SmallStatement::Expr {
                                value: Expression::Ellipsis {
                                    lpar: vec![],
                                    rpar: vec![]
                                },
                                semicolon: None,
                            }],
                            trailing_whitespace: TrailingWhitespace {
                                newline: Newline(None, Fakeness::Fake),
                                ..Default::default()
                            },
                            ..Default::default()
                        }),
                        params: Default::default(),
                        decorators: vec![],
                        leading_lines: vec![],
                        lines_after_decorators: vec![],
                        whitespace_after_def: SimpleWhitespace(" "),
                        whitespace_after_name: Default::default(),
                        whitespace_before_colon: Default::default(),
                        whitespace_before_params: ParenthesizableWhitespace::SimpleWhitespace(
                            Default::default()
                        ),
                    }
                ))]
            })
        );
    }

    #[test]
    fn test_decorated_funcdef() {
        let text = "@hello\ndef f(): ...";
        let m = parse_module(text);
        assert_eq!(
            m,
            Ok(Module {
                body: vec![Statement::Compound(CompoundStatement::FunctionDef(
                    FunctionDef {
                        name: Name {
                            value: "f",
                            ..Default::default()
                        },
                        body: Suite::SimpleStatementSuite(SimpleStatementSuite {
                            body: vec![SmallStatement::Expr {
                                value: Expression::Ellipsis {
                                    lpar: vec![],
                                    rpar: vec![]
                                },
                                semicolon: None,
                            }],
                            trailing_whitespace: TrailingWhitespace {
                                newline: Newline(None, Fakeness::Fake),
                                ..Default::default()
                            },
                            ..Default::default()
                        }),
                        params: Default::default(),
                        decorators: vec![Decorator {
                            decorator: Name {
                                value: "hello",
                                ..Default::default()
                            },
                            ..Default::default()
                        }],
                        leading_lines: vec![],
                        lines_after_decorators: vec![],
                        whitespace_after_def: SimpleWhitespace(" "),
                        whitespace_after_name: Default::default(),
                        whitespace_before_colon: Default::default(),
                        whitespace_before_params: ParenthesizableWhitespace::SimpleWhitespace(
                            Default::default()
                        ),
                    }
                ))]
            })
        );
        let mut state = CodegenState {
            default_newline: "\n".to_string(),
            ..Default::default()
        };
        match &m.unwrap().body[0] {
            Statement::Compound(f) => f.codegen(&mut state),
            _ => {}
        }
        assert_eq!(state.to_string().trim_end(), text);
    }

    #[test]
    fn test_funcdef_params() {
        let m = parse_module("def g(a, b): ...");
        assert_eq!(
            m,
            Ok(Module {
                body: vec![Statement::Compound(CompoundStatement::FunctionDef(
                    FunctionDef {
                        name: Name {
                            value: "g",
                            ..Default::default()
                        },
                        body: Suite::SimpleStatementSuite(SimpleStatementSuite {
                            body: vec![SmallStatement::Expr {
                                value: Expression::Ellipsis {
                                    lpar: vec![],
                                    rpar: vec![]
                                },
                                semicolon: None,
                            }],
                            trailing_whitespace: TrailingWhitespace {
                                newline: Newline(None, Fakeness::Fake),
                                ..Default::default()
                            },
                            ..Default::default()
                        }),
                        params: Parameters {
                            params: vec![
                                Param {
                                    name: Name {
                                        value: "a",
                                        ..Default::default()
                                    },
                                    comma: Some(Comma {
                                        whitespace_after:
                                            ParenthesizableWhitespace::SimpleWhitespace(
                                                SimpleWhitespace(" ")
                                            ),
                                        whitespace_before:
                                            ParenthesizableWhitespace::SimpleWhitespace(
                                                SimpleWhitespace("")
                                            ),
                                    }),
                                    ..Default::default()
                                },
                                Param {
                                    name: Name {
                                        value: "b",
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                }
                            ],
                            ..Default::default()
                        },
                        decorators: Default::default(),
                        leading_lines: vec![],
                        lines_after_decorators: vec![],
                        whitespace_after_def: SimpleWhitespace(" "),
                        whitespace_after_name: Default::default(),
                        whitespace_before_colon: Default::default(),
                        whitespace_before_params: ParenthesizableWhitespace::SimpleWhitespace(
                            Default::default()
                        ),
                    }
                ))]
            })
        );
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
        assert_eq!(5, bol_offset("hello\nhello", 2));
        assert_eq!(5, bol_offset("hello\nhello\nhello", 2));
    }

    #[test]
    fn bol_offset_last_line() {
        assert_eq!(5, bol_offset("hello", 3));
        assert_eq!(11, bol_offset("hello\nhello", 3));
        assert_eq!(11, bol_offset("hello\nhello\nhello", 3));
    }
}
