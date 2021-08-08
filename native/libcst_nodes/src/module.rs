// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use crate::codegen::{Codegen, CodegenState};
use crate::statement::Statement;
use crate::whitespace::EmptyLine;

#[derive(Debug, Eq, PartialEq)]
pub struct Module<'a> {
    pub body: Vec<Statement<'a>>,
    pub header: Vec<EmptyLine<'a>>,
    pub footer: Vec<EmptyLine<'a>>,
}

impl<'a> Codegen<'a> for Module<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
