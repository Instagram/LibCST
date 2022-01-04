// Copyright (c) Meta Platforms, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use crate::{
    tokenizer::whitespace_parser::{Config, WhitespaceError},
    Codegen, CodegenState, Comma, EmptyLine, LeftParen, RightParen,
};
use std::ops::Deref;

pub trait WithComma<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self;
}

pub trait ParenthesizedNode<'r, 'a: 'r> {
    fn lpar(&self) -> &Vec<LeftParen<'r, 'a>>;
    fn rpar(&self) -> &Vec<RightParen<'r, 'a>>;

    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        for lpar in self.lpar() {
            lpar.codegen(state);
        }
        f(state);
        for rpar in self.rpar() {
            rpar.codegen(state);
        }
    }

    fn with_parens(self, left: LeftParen<'r, 'a>, right: RightParen<'r, 'a>) -> Self;
}

impl<'r, 'a: 'r, T: ParenthesizedNode<'r, 'a>> ParenthesizedNode<'r, 'a> for Box<T> {
    fn lpar(&self) -> &Vec<LeftParen<'r, 'a>> {
        self.deref().lpar()
    }
    fn rpar(&self) -> &Vec<RightParen<'r, 'a>> {
        self.deref().rpar()
    }
    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        self.deref().parenthesize(state, f)
    }
    fn with_parens(self, left: LeftParen<'r, 'a>, right: RightParen<'r, 'a>) -> Self {
        Self::new((*self).with_parens(left, right))
    }
}

pub trait WithLeadingLines<'a> {
    fn leading_lines(&mut self) -> &mut Vec<EmptyLine<'a>>;
}

pub type Result<T> = std::result::Result<T, WhitespaceError>;

pub trait Inflate<'a>
where
    Self: Sized,
{
    type Inflated;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated>;
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Option<T> {
    type Inflated = Option<T::Inflated>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        self.map(|x| x.inflate(config)).transpose()
    }
}

impl<'a, T: Inflate<'a> + ?Sized> Inflate<'a> for Box<T> {
    type Inflated = Box<T::Inflated>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        match (*self).inflate(config) {
            Ok(a) => Ok(Box::new(a)),
            Err(e) => Err(e),
        }
    }
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Vec<T> {
    type Inflated = Vec<T::Inflated>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        self.into_iter().map(|item| item.inflate(config)).collect()
    }
}
