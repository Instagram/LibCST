// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{
    Codegen, CodegenState, Comma, EmptyLine, Expression, Name, Parameters, Semicolon,
    SimpleWhitespace, TrailingWhitespace,
};

#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'a> {
    FunctionDef(FunctionDef<'a>),
    SmallStatement(SmallStatement<'a>),
}

impl<'a> Codegen for Statement<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::SmallStatement(s) => s.codegen(state),
            &Self::FunctionDef(f) => f.codegen(state),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq)]
pub enum SmallStatement<'a> {
    Pass {
        semicolon: Option<Semicolon<'a>>,
    },
    Break {
        semicolon: Option<Semicolon<'a>>,
    },
    Continue {
        semicolon: Option<Semicolon<'a>>,
    },
    Return {
        value: Option<&'a str>, // TODO
        whitespace_after_return: SimpleWhitespace<'a>,
        semicolon: Option<Semicolon<'a>>,
    },
    Expr {
        value: Expression<'a>,
        semicolon: Option<Semicolon<'a>>,
    },
    Assert {
        test: &'a str,        // TODO
        msg: Option<&'a str>, // TODO
        comma: Option<Comma<'a>>,
        whitespace_after_assert: SimpleWhitespace<'a>,
        semicolon: Option<Semicolon<'a>>,
    }, // TODO Import, ImportFrom
       // TODO Assign, AnnAssign
       // TODO Raise
       // TODO Global, Nonlocal
}

impl<'a> Codegen for SmallStatement<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::Pass { .. } => state.add_token("pass".to_string()),
            &Self::Break { .. } => state.add_token("break".to_string()),
            &Self::Continue { .. } => state.add_token("continue".to_string()),
            &Self::Expr { value: e, .. } => e.codegen(state),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: Name<'a>,
    pub decorators: Vec<Decorator<'a>>,
    pub params: Parameters<'a>,

    pub whitespace_after_def: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> FunctionDef<'a> {
    pub fn with_decorators(self, decs: Vec<Decorator<'a>>) -> Self {
        Self {
            decorators: self
                .decorators
                .into_iter()
                .chain(decs.into_iter())
                .collect(),
            ..self
        }
    }
}

impl<'a> Codegen for FunctionDef<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        // TODO: leading lines
        for dec in self.decorators.iter() {
            dec.codegen(state);
        }
        // TODO: lines_after_decorators
        state.add_indent();

        // TODO: async
        state.add_token("def".to_string());
        self.whitespace_after_def.codegen(state);
        self.name.codegen(state);
        self.whitespace_after_name.codegen(state);
        state.add_token("(".to_string());
        // TODO: params
        state.add_token(")".to_string());
        // TODO: returns
        self.whitespace_before_colon.codegen(state);
        state.add_token(":".to_string());
        // TODO: body
        state.add_token(" ...".to_string());
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Decorator<'a> {
    pub decorator: Name<'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_at: SimpleWhitespace<'a>,
    pub trailing_whitespace: TrailingWhitespace<'a>,
}

impl<'a> Codegen for Decorator<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        for ll in self.leading_lines.iter() {
            ll.codegen(state);
        }
        state.add_indent();
        state.add_token("@".to_string());
        self.whitespace_after_at.codegen(state);
        self.decorator.codegen(state);
        self.trailing_whitespace.codegen(state);
    }
}
