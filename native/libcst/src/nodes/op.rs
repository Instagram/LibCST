// Copyright (c) Meta Platforms, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{whitespace::ParenthesizableWhitespace, Codegen, CodegenState};
use crate::{
    nodes::common::TokenRef,
    nodes::traits::{Inflate, Result},
    tokenizer::whitespace_parser::{
        parse_parenthesizable_whitespace, parse_simple_whitespace, Config,
    },
};
use libcst_derive::IntoPy;

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct Semicolon<'a> {
    /// Any space that appears directly before this semicolon.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this semicolon.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct SemicolonTokens<'r, 'a> {
    pub inner: Semicolon<'a>,
    pub tok: TokenRef<'r, 'a>,
}

impl<'a> Codegen<'a> for Semicolon<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(";");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for SemicolonTokens<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.inner.whitespace_before = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_before.borrow_mut())?,
        );
        self.inner.whitespace_after = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?,
        );
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Comma<'r, 'a> {
    /// Any space that appears directly before this comma.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this comma.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[skip_py]
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Comma<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(",");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for Comma<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'r, 'a> Comma<'r, 'a> {
    pub fn inflate_before(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct AssignEqual<'r, 'a> {
    /// Any space that appears directly before this equal sign.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this equal sign.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[skip_py]
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for AssignEqual<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("=");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for AssignEqual<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct Dot<'r, 'a> {
    /// Any space that appears directly before this dot.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this dot.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[skip_py]
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Dot<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(".");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for Dot<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.inflate_before(config)?;
        self.inflate_after(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Dot<'r, 'a> {
    fn inflate_before(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(())
    }

    fn inflate_after(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ImportStar {}

impl<'r, 'a> Codegen<'a> for ImportStar {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
    }
}

impl<'r, 'a> Inflate<'a> for ImportStar {
    type Inflated = Self;
    fn inflate(self, _config: &Config<'a>) -> Result<Self> {
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum UnaryOp<'r, 'a> {
    Plus {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Minus {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitInvert {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Not {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
}

impl<'r, 'a> Codegen<'a> for UnaryOp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        let (tok, whitespace_after) = match self {
            Self::Plus {
                whitespace_after, ..
            } => ("+", whitespace_after),
            Self::Minus {
                whitespace_after, ..
            } => ("-", whitespace_after),
            Self::BitInvert {
                whitespace_after, ..
            } => ("~", whitespace_after),
            Self::Not {
                whitespace_after, ..
            } => ("not", whitespace_after),
        };
        state.add_token(tok);
        whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for UnaryOp<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        Ok(match self {
            Self::Plus { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Plus {
                    whitespace_after,
                    tok,
                }
            }
            Self::Minus { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Minus {
                    whitespace_after,
                    tok,
                }
            }
            Self::BitInvert { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitInvert {
                    whitespace_after,
                    tok,
                }
            }
            Self::Not { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Not {
                    whitespace_after,
                    tok,
                }
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum BooleanOp<'r, 'a> {
    And {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Or {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
}

impl<'r, 'a> Codegen<'a> for BooleanOp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        let (tok, ws_bef, ws_aft) = match self {
            Self::And {
                whitespace_after,
                whitespace_before,
                ..
            } => ("and", whitespace_before, whitespace_after),
            Self::Or {
                whitespace_after,
                whitespace_before,
                ..
            } => ("or", whitespace_before, whitespace_after),
        };
        ws_bef.codegen(state);
        state.add_token(tok);
        ws_aft.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for BooleanOp<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        Ok(match self {
            Self::And { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::And {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Or { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Or {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum BinaryOp<'r, 'a> {
    Add {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Subtract {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Multiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Divide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    FloorDivide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Modulo {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Power {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    LeftShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    RightShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitOr {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitAnd {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitXor {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    MatrixMultiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
}

impl<'r, 'a> Codegen<'a> for BinaryOp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        let (whitespace_before, whitespace_after, tok) = match self {
            Self::Add {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Subtract {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Multiply {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Divide {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::FloorDivide {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Modulo {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Power {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::LeftShift {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::RightShift {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::BitOr {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::BitAnd {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::BitXor {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::MatrixMultiply {
                whitespace_before,
                whitespace_after,
                tok,
            } => (whitespace_before, whitespace_after, tok),
        };
        whitespace_before.codegen(state);
        state.add_token(tok.string);
        whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for BinaryOp<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        Ok(match self {
            Self::Add { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Add {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Subtract { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Subtract {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Multiply { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Multiply {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Divide { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Divide {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::FloorDivide { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::FloorDivide {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Modulo { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Modulo {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Power { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Power {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::LeftShift { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::LeftShift {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::RightShift { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::RightShift {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::BitOr { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitOr {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::BitAnd { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitAnd {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::BitXor { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitXor {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::MatrixMultiply { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::MatrixMultiply {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum CompOp<'r, 'a> {
    LessThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    GreaterThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    LessThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    GreaterThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    Equal {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    NotEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    In {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    NotIn {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        not_tok: TokenRef<'r, 'a>,
        #[skip_py]
        in_tok: TokenRef<'r, 'a>,
    },
    Is {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    IsNot {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        is_tok: TokenRef<'r, 'a>,
        #[skip_py]
        not_tok: TokenRef<'r, 'a>,
    },
}

impl<'r, 'a> Codegen<'a> for CompOp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        let (bef, aft, first_tok, between) = match self {
            Self::LessThan {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::GreaterThan {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::LessThanEqual {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::GreaterThanEqual {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Equal {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::NotEqual {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::In {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Is {
                whitespace_before,
                whitespace_after,
                tok,
            } => (whitespace_before, whitespace_after, tok, None),
            Self::IsNot {
                whitespace_before,
                whitespace_between,
                whitespace_after,
                is_tok,
                not_tok,
            } => (
                whitespace_before,
                whitespace_after,
                is_tok,
                Some((whitespace_between, not_tok)),
            ),
            Self::NotIn {
                whitespace_before,
                whitespace_between,
                whitespace_after,
                not_tok,
                in_tok,
            } => (
                whitespace_before,
                whitespace_after,
                not_tok,
                Some((whitespace_between, in_tok)),
            ),
        };
        bef.codegen(state);
        state.add_token(first_tok.string);
        if let Some((btw, second_tok)) = between {
            btw.codegen(state);
            state.add_token(second_tok.string);
        }
        aft.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for CompOp<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        Ok(match self {
            Self::LessThan { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::LessThan {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::GreaterThan { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::GreaterThan {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::LessThanEqual { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::LessThanEqual {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::GreaterThanEqual { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::GreaterThanEqual {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Equal { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Equal {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::NotEqual { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::NotEqual {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::In { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::In {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::Is { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Is {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::IsNot {
                is_tok, not_tok, ..
            } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*is_tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_between = parse_parenthesizable_whitespace(
                    config,
                    &mut (*is_tok).whitespace_after.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*not_tok).whitespace_after.borrow_mut(),
                )?;
                Self::IsNot {
                    whitespace_before,
                    whitespace_between,
                    whitespace_after,
                    is_tok,
                    not_tok,
                }
            }
            Self::NotIn {
                not_tok, in_tok, ..
            } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*not_tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_between = parse_parenthesizable_whitespace(
                    config,
                    &mut (*not_tok).whitespace_after.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*in_tok).whitespace_after.borrow_mut(),
                )?;
                Self::NotIn {
                    whitespace_before,
                    whitespace_between,
                    whitespace_after,
                    not_tok,
                    in_tok,
                }
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Colon<'r, 'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[skip_py]
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Colon<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Colon<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(":");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum AugOp<'r, 'a> {
    AddAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    SubtractAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    MultiplyAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    MatrixMultiplyAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    DivideAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    ModuloAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitAndAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitOrAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    BitXorAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    LeftShiftAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    RightShiftAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    PowerAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
    FloorDivideAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[skip_py]
        tok: TokenRef<'r, 'a>,
    },
}

impl<'r, 'a> Inflate<'a> for AugOp<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        Ok(match self {
            Self::AddAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::AddAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::SubtractAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::SubtractAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::MultiplyAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::MultiplyAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::MatrixMultiplyAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::MatrixMultiplyAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::DivideAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::DivideAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::ModuloAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::ModuloAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::BitAndAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitAndAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::BitOrAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitOrAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::BitXorAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::BitXorAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::LeftShiftAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::LeftShiftAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::RightShiftAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::RightShiftAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::PowerAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::PowerAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
            Self::FloorDivideAssign { tok, .. } => {
                let whitespace_before = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_before.borrow_mut(),
                )?;
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::FloorDivideAssign {
                    whitespace_before,
                    whitespace_after,
                    tok,
                }
            }
        })
    }
}

impl<'r, 'a> Codegen<'a> for AugOp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        let (tok, bef, aft) = match self {
            Self::AddAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("+=", whitespace_before, whitespace_after),
            Self::SubtractAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("-=", whitespace_before, whitespace_after),
            Self::MultiplyAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("*=", whitespace_before, whitespace_after),
            Self::MatrixMultiplyAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("@=", whitespace_before, whitespace_after),
            Self::DivideAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("/=", whitespace_before, whitespace_after),
            Self::ModuloAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("%=", whitespace_before, whitespace_after),
            Self::BitAndAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("&=", whitespace_before, whitespace_after),
            Self::BitOrAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("|=", whitespace_before, whitespace_after),
            Self::BitXorAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("^=", whitespace_before, whitespace_after),
            Self::LeftShiftAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("<<=", whitespace_before, whitespace_after),
            Self::RightShiftAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => (">>=", whitespace_before, whitespace_after),
            Self::PowerAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("**=", whitespace_before, whitespace_after),
            Self::FloorDivideAssign {
                whitespace_before,
                whitespace_after,
                ..
            } => ("//=", whitespace_before, whitespace_after),
        };
        bef.codegen(state);
        state.add_token(tok);
        aft.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct BitOr<'r, 'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for BitOr<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for BitOr<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("|");
        self.whitespace_after.codegen(state);
    }
}
