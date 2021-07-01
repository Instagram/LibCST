// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use crate::tokenize::core::{TokConfig, TokError, TokState, TokType, Token, TokenIterator};
use peg::{Parse, ParseElem, ParseLiteral, RuleResult};
use thiserror::Error;

mod whitespace;
pub use whitespace::{
    parse_empty_lines, parse_simple_whitespace, parse_trailing_whitespace, Comment, Config,
    EmptyLine, Newline, SimpleWhitespace, State as WhitespaceState, TrailingWhitespace,
    WhitespaceError,
};
mod statement;
pub use statement::{
    CompoundStatement, Decorator, FunctionDef, IndentedBlock, SimpleStatementLine,
    SimpleStatementSuite, SmallStatement, Statement, Suite,
};

mod expression;
pub use expression::{Expression, Name, Param, Parameters};

mod op;
pub use op::{Comma, Semicolon};

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
        lines: module_text.lines().collect(),
    };
    python::file(&result, &conf).or_else(|e| Err(ParserError::ParserError(e)))
}

pub fn prettify_error<'a>(module_text: &'a str, err: ParserError<'a>, label: &str) -> String {
    match err {
        ParserError::ParserError(e) => chic::Error::new(label)
            .error(
                e.location.line,
                e.location.column,
                e.location.column + 1,
                module_text,
                format!("expected {}", e.expected),
            )
            .to_string(),
        e => format!("Parse error for {}: {}", label, e),
    }
}

#[cfg(test)]
mod test {
    use crate::parser::whitespace::Fakeness;

    use super::*;
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
                        whitespace_after_def: SimpleWhitespace(" "),
                        whitespace_after_name: Default::default(),
                        whitespace_before_colon: Default::default(),
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
                        whitespace_after_def: SimpleWhitespace(" "),
                        whitespace_after_name: Default::default(),
                        whitespace_before_colon: Default::default(),
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
                                    ..Default::default()
                                },
                                Param {
                                    name: Name {
                                        value: "b",
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                }
                            ]
                        },
                        decorators: Default::default(),
                        whitespace_after_def: SimpleWhitespace(" "),
                        whitespace_after_name: Default::default(),
                        whitespace_before_colon: Default::default(),
                    }
                ))]
            })
        );
    }
}
