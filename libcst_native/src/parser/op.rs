// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{whitespace::ParenthesizableWhitespace, Codegen, SimpleWhitespace};

#[derive(Debug, Eq, PartialEq)]
pub struct Semicolon<'a> {
    /// Any space that appears directly before this semicolon.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this semicolon.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Comma<'a> {
    /// Any space that appears directly before this comma.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this comma.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen for Comma<'a> {
    fn codegen(&self, state: &mut super::CodegenState) -> () {
        self.whitespace_before.codegen(state);
        state.add_token(",".to_string());
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignEqual<'a> {
    /// Any space that appears directly before this equal sign.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this equal sign.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen for AssignEqual<'a> {
    fn codegen(&self, state: &mut super::CodegenState) -> () {
        self.whitespace_before.codegen(state);
        state.add_token("=".to_string());
        self.whitespace_after.codegen(state);
    }
}
