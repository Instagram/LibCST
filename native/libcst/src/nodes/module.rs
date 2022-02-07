// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::mem::swap;
use std::rc::Rc;

use crate::tokenizer::whitespace_parser::parse_empty_lines;
use crate::tokenizer::Token;
use crate::{
    nodes::{
        codegen::{Codegen, CodegenState},
        statement::Statement,
        whitespace::EmptyLine,
    },
    tokenizer::whitespace_parser::Config,
};
#[cfg(feature = "py")]
use libcst_derive::TryIntoPy;

use super::traits::{Inflate, Result, WithLeadingLines};

type TokenRef<'a> = Rc<Token<'a>>;

#[derive(Debug, Eq, PartialEq)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Module<'a> {
    pub body: Vec<Statement<'a>>,
    pub header: Vec<EmptyLine<'a>>,
    pub footer: Vec<EmptyLine<'a>>,

    pub default_indent: &'a str,
    pub default_newline: &'a str,
    pub has_trailing_newline: bool,
    pub encoding: String,

    pub(crate) eof_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Module<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for h in &self.header {
            h.codegen(state);
        }
        for s in &self.body {
            s.codegen(state);
        }
        for nl in &self.footer {
            nl.codegen(state);
        }
    }
}

impl<'a> Inflate<'a> for Module<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.default_indent = config.default_indent;
        self.default_newline = config.default_newline;
        self.has_trailing_newline = config.has_trailing_newline();
        self.body = self.body.inflate(config)?;
        let mut footer = parse_empty_lines(
            config,
            &mut (*self.eof_tok).whitespace_before.borrow_mut(),
            Some(""),
        )?;
        let mut header = vec![];
        if let Some(stmt) = self.body.first_mut() {
            swap(stmt.leading_lines(), &mut header);
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
        } else {
            swap(&mut header, &mut footer);
        }
        self.footer = footer;
        self.header = header;
        Ok(self)
    }
}
