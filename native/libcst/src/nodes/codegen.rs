// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::fmt;
#[derive(Debug)]
pub struct CodegenState<'a> {
    pub tokens: String,
    pub indent_tokens: Vec<&'a str>,
    pub default_newline: &'a str,
    pub default_indent: &'a str,
}

impl<'a> CodegenState<'a> {
    pub fn indent(&mut self, v: &'a str) {
        self.indent_tokens.push(v);
    }
    pub fn dedent(&mut self) {
        self.indent_tokens.pop();
    }
    pub fn add_indent(&mut self) {
        self.tokens.extend(self.indent_tokens.iter().cloned());
    }
    pub fn add_token(&mut self, tok: &'a str) {
        self.tokens.push_str(tok);
    }
}

impl<'a> fmt::Display for CodegenState<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.tokens)
    }
}

pub trait Codegen<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>);
}

impl<'a, T> Codegen<'a> for Option<T>
where
    T: Codegen<'a>,
{
    fn codegen(&self, state: &mut CodegenState<'a>) {
        if let Some(s) = &self {
            s.codegen(state);
        }
    }
}

#[cfg(windows)]
const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &str = "\n";

impl<'a> Default for CodegenState<'a> {
    fn default() -> Self {
        Self {
            default_newline: LINE_ENDING,
            default_indent: "    ",
            indent_tokens: Default::default(),
            tokens: Default::default(),
        }
    }
}
