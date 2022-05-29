// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use libcst_derive::{cst_node, Codegen};

pub enum Error {}

type TokenRef<'r, 'a> = &'r &'a str;
pub type Result<T> = std::result::Result<T, Error>;

pub struct Config<'a> {
    #[allow(dead_code)]
    foo: &'a str,
}
pub trait Inflate<'a>
where
    Self: Sized,
{
    type Inflated;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated>;
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

pub struct CodegenState<'a> {
    #[allow(dead_code)]
    foo: &'a str,
}
pub trait Codegen<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>);
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WS<'a> {
    pub last_line: &'a str,
}

#[cst_node]
pub struct Parameters<'a> {
    pub params: Vec<Param<'a>>,
    pub foo: Param<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedParameters<'r, 'a> {
    type Inflated = Parameters<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let params = vec![];
        #[allow(clippy::blacklisted_name)]
        let foo = self.foo.inflate(config)?;
        Ok(Self::Inflated { params, foo })
    }
}

#[cst_node]
pub struct Param<'a> {
    pub star: Option<&'a str>,
    pub(crate) star_tok: Option<TokenRef<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedParam<'r, 'a> {
    type Inflated = Param<'a>;
    fn inflate(self, _config: &Config<'a>) -> Result<Self::Inflated> {
        Ok(Self::Inflated { star: self.star })
    }
}

impl<'a> Codegen<'a> for Param<'a> {
    fn codegen(&self, _state: &mut CodegenState<'a>) {}
}

#[cst_node]
pub struct BitOr<'a> {
    pub whitespace_before: WS<'a>,
    pub whitespace_after: WS<'a>,

    pub(crate) tok: TokenRef<'a>,
}

#[cst_node]
pub enum CompOp<'a> {
    LessThan {
        whitespace_before: WS<'a>,
        tok: TokenRef<'a>,
    },
    GreaterThan {
        whitespace_after: WS<'a>,
        tok: TokenRef<'a>,
    },
}

impl<'r, 'a> Inflate<'a> for DeflatedCompOp<'r, 'a> {
    type Inflated = CompOp<'a>;
    fn inflate(self, _config: &Config<'a>) -> Result<Self::Inflated> {
        Ok(match self {
            Self::LessThan { tok: _, .. } => Self::Inflated::LessThan {
                whitespace_before: WS { last_line: "yo" },
            },
            Self::GreaterThan { tok: _, .. } => Self::Inflated::GreaterThan {
                whitespace_after: WS { last_line: "" },
            },
        })
    }
}

impl<'a> Codegen<'a> for CompOp<'a> {
    fn codegen(&self, _state: &mut CodegenState<'a>) {}
}

#[cst_node(Codegen)]
enum Expr<'a> {
    #[allow(dead_code)]
    One(Box<Param<'a>>),
    #[allow(dead_code)]
    Two(CompOp<'a>),
}

fn main() {}
