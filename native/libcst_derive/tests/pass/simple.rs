// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use libcst_derive::cst_node;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WS<'a>(&'a str);

type TokenRef<'r, 'a> = &'r &'a str;

#[cst_node]
pub enum Foo<'a> {
    One(One<'a>),
    Two(Box<Two<'a>>),
}

#[cst_node]
pub struct One<'a> {
    pub two: Box<Two<'a>>,
    pub header: WS<'a>,

    pub(crate) newline_tok: TokenRef<'a>,
}

#[cst_node]
pub struct Two<'a> {
    pub whitespace_before: WS<'a>,
    pub(crate) tok: TokenRef<'a>,
}

#[cst_node]
struct Thin<'a> {
    pub whitespace: WS<'a>,
}

#[cst_node]
struct Value<'a> {
    pub value: &'a str,
}

#[cst_node]
struct Empty {}

#[cst_node]
enum Smol<'a> {
    #[allow(dead_code)]
    Thin(Thin<'a>),
    #[allow(dead_code)]
    Empty(Empty),
}

fn main() {}
