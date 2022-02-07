// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::rc::Rc;

use super::{whitespace::ParenthesizableWhitespace, Codegen, CodegenState};
use crate::{
    nodes::traits::{Inflate, Result},
    tokenizer::{
        whitespace_parser::{parse_parenthesizable_whitespace, parse_simple_whitespace, Config},
        Token,
    },
};
#[cfg(feature = "py")]
use libcst_derive::TryIntoPy;

type TokenRef<'a> = Rc<Token<'a>>;

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Semicolon<'a> {
    /// Any space that appears directly before this semicolon.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this semicolon.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[cfg_attr(feature = "py", skip_py)]
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Semicolon<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(";");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for Semicolon<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_before.borrow_mut())?,
        );
        self.whitespace_after = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?,
        );
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Comma<'a> {
    /// Any space that appears directly before this comma.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this comma.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[cfg_attr(feature = "py", skip_py)]
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Comma<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(",");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for Comma<'a> {
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

impl<'a> Comma<'a> {
    pub fn inflate_before(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct AssignEqual<'a> {
    /// Any space that appears directly before this equal sign.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this equal sign.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[cfg_attr(feature = "py", skip_py)]
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for AssignEqual<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("=");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for AssignEqual<'a> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Dot<'a> {
    /// Any space that appears directly before this dot.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this dot.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[cfg_attr(feature = "py", skip_py)]
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Dot<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(".");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for Dot<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.inflate_before(config)?;
        self.inflate_after(config)?;
        Ok(self)
    }
}

impl<'a> Dot<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ImportStar {}

impl<'a> Codegen<'a> for ImportStar {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
    }
}

impl<'a> Inflate<'a> for ImportStar {
    fn inflate(self, _config: &Config<'a>) -> Result<Self> {
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum UnaryOp<'a> {
    Plus {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Minus {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitInvert {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Not {
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
}

impl<'a> Codegen<'a> for UnaryOp<'a> {
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

impl<'a> Inflate<'a> for UnaryOp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum BooleanOp<'a> {
    And {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Or {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
}

impl<'a> Codegen<'a> for BooleanOp<'a> {
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

impl<'a> Inflate<'a> for BooleanOp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum BinaryOp<'a> {
    Add {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Subtract {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Multiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Divide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    FloorDivide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Modulo {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Power {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    LeftShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    RightShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitOr {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitAnd {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitXor {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    MatrixMultiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
}

impl<'a> Codegen<'a> for BinaryOp<'a> {
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

impl<'a> Inflate<'a> for BinaryOp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum CompOp<'a> {
    LessThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    GreaterThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    LessThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    GreaterThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    Equal {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    NotEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    In {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    NotIn {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        not_tok: TokenRef<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        in_tok: TokenRef<'a>,
    },
    Is {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    IsNot {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        is_tok: TokenRef<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        not_tok: TokenRef<'a>,
    },
}

impl<'a> Codegen<'a> for CompOp<'a> {
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

impl<'a> Inflate<'a> for CompOp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Colon<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[cfg_attr(feature = "py", skip_py)]
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Colon<'a> {
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

impl<'a> Codegen<'a> for Colon<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(":");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum AugOp<'a> {
    AddAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    SubtractAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    MultiplyAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    MatrixMultiplyAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    DivideAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    ModuloAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitAndAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitOrAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    BitXorAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    LeftShiftAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    RightShiftAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    PowerAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
    FloorDivideAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        #[cfg_attr(feature = "py", skip_py)]
        tok: TokenRef<'a>,
    },
}

impl<'a> Inflate<'a> for AugOp<'a> {
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

impl<'a> Codegen<'a> for AugOp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct BitOr<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for BitOr<'a> {
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

impl<'a> Codegen<'a> for BitOr<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("|");
        self.whitespace_after.codegen(state);
    }
}
