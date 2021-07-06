// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

#[derive(Debug)]
pub struct CodegenState<'a> {
    pub tokens: String,
    pub indent_tokens: Vec<&'a str>,
    pub default_newline: &'a str,
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

    pub fn to_string(self) -> String {
        self.tokens
    }
}

pub trait Codegen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> ();
}

#[cfg(windows)]
const LINE_ENDING: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static str = "\n";

impl<'a> Default for CodegenState<'a> {
    fn default() -> Self {
        Self {
            default_newline: LINE_ENDING,
            indent_tokens: Default::default(),
            tokens: Default::default(),
        }
    }
}
