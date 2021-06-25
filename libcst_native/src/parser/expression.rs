// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{Codegen, CodegenState, SimpleWhitespace};

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Parameters<'a> {
    pub params: Vec<Param<'a>>,
}
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Name<'a> {
    pub value: &'a str,
}

impl<'a> Codegen for Name<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        // TODO: parentheses
        state.add_token(self.value.to_string());
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Param<'a> {
    pub name: Name<'a>,

    pub whitespace_after_star: SimpleWhitespace<'a>,
    pub whitespace_after_param: SimpleWhitespace<'a>,
}
