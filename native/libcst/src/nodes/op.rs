// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{whitespace::ParenthesizableWhitespace, Codegen, CodegenState};
use crate::{
    nodes::traits::{Inflate, Result},
    tokenizer::{
        whitespace_parser::{parse_parenthesizable_whitespace, parse_simple_whitespace, Config},
        Token,
    },
};
use libcst_derive::cst_node;
#[cfg(feature = "py")]
use libcst_derive::TryIntoPy;

type TokenRef<'r, 'a> = &'r Token<'a>;

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedSemicolon<'r, 'a> {
    type Inflated = Semicolon<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_before.borrow_mut())?,
        );
        let whitespace_after = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?,
        );
        Ok(Self::Inflated {
            whitespace_before,
            whitespace_after,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedComma<'r, 'a> {
    type Inflated = Comma<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            whitespace_before,
            whitespace_after,
        })
    }
}

impl<'r, 'a> DeflatedComma<'r, 'a> {
    pub fn inflate_before(self, config: &Config<'a>) -> Result<Comma<'a>> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after = Default::default();
        Ok(Comma {
            whitespace_before,
            whitespace_after,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAssignEqual<'r, 'a> {
    type Inflated = AssignEqual<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            whitespace_before,
            whitespace_after,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedDot<'r, 'a> {
    type Inflated = Dot<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = self.inflate_before(config)?;
        let whitespace_after = self.inflate_after(config)?;
        Ok(Self::Inflated {
            whitespace_before,
            whitespace_after,
        })
    }
}

impl<'r, 'a> DeflatedDot<'r, 'a> {
    fn inflate_before(&mut self, config: &Config<'a>) -> Result<ParenthesizableWhitespace<'a>> {
        parse_parenthesizable_whitespace(config, &mut (*self.tok).whitespace_before.borrow_mut())
    }

    fn inflate_after(&mut self, config: &Config<'a>) -> Result<ParenthesizableWhitespace<'a>> {
        parse_parenthesizable_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())
    }
}

#[cst_node]
pub struct ImportStar {}

pub(crate) fn make_importstar<'r, 'a>() -> DeflatedImportStar<'r, 'a> {
    DeflatedImportStar {
        _phantom: Default::default(),
    }
}

impl<'a> Codegen<'a> for ImportStar {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedImportStar<'r, 'a> {
    type Inflated = ImportStar;
    fn inflate(self, _config: &Config<'a>) -> Result<Self::Inflated> {
        Ok(ImportStar {})
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedUnaryOp<'r, 'a> {
    type Inflated = UnaryOp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        Ok(match self {
            Self::Plus { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Inflated::Plus { whitespace_after }
            }
            Self::Minus { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Inflated::Minus { whitespace_after }
            }
            Self::BitInvert { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Inflated::BitInvert { whitespace_after }
            }
            Self::Not { tok, .. } => {
                let whitespace_after = parse_parenthesizable_whitespace(
                    config,
                    &mut (*tok).whitespace_after.borrow_mut(),
                )?;
                Self::Inflated::Not { whitespace_after }
            }
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedBooleanOp<'r, 'a> {
    type Inflated = BooleanOp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
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
                Self::Inflated::And {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Or {
                    whitespace_before,
                    whitespace_after,
                }
            }
        })
    }
}

#[cst_node]
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
        let (whitespace_before, whitespace_after) = match self {
            Self::Add {
                whitespace_before,
                whitespace_after,
            }
            | Self::Subtract {
                whitespace_before,
                whitespace_after,
            }
            | Self::Multiply {
                whitespace_before,
                whitespace_after,
            }
            | Self::Divide {
                whitespace_before,
                whitespace_after,
            }
            | Self::FloorDivide {
                whitespace_before,
                whitespace_after,
            }
            | Self::Modulo {
                whitespace_before,
                whitespace_after,
            }
            | Self::Power {
                whitespace_before,
                whitespace_after,
            }
            | Self::LeftShift {
                whitespace_before,
                whitespace_after,
            }
            | Self::RightShift {
                whitespace_before,
                whitespace_after,
            }
            | Self::BitOr {
                whitespace_before,
                whitespace_after,
            }
            | Self::BitAnd {
                whitespace_before,
                whitespace_after,
            }
            | Self::BitXor {
                whitespace_before,
                whitespace_after,
            }
            | Self::MatrixMultiply {
                whitespace_before,
                whitespace_after,
            } => (whitespace_before, whitespace_after),
        };
        let tok = match self {
            BinaryOp::Add { .. } => "+",
            BinaryOp::Subtract { .. } => "-",
            BinaryOp::Multiply { .. } => "*",
            BinaryOp::Divide { .. } => "/",
            BinaryOp::FloorDivide { .. } => "//",
            BinaryOp::Modulo { .. } => "%",
            BinaryOp::Power { .. } => "**",
            BinaryOp::LeftShift { .. } => "<<",
            BinaryOp::RightShift { .. } => ">>",
            BinaryOp::BitOr { .. } => "|",
            BinaryOp::BitAnd { .. } => "&",
            BinaryOp::BitXor { .. } => "^",
            BinaryOp::MatrixMultiply { .. } => "@",
        };
        whitespace_before.codegen(state);
        state.add_token(tok);
        whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedBinaryOp<'r, 'a> {
    type Inflated = BinaryOp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
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
                Self::Inflated::Add {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Subtract {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Multiply {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Divide {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::FloorDivide {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Modulo {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Power {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::LeftShift {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::RightShift {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::BitOr {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::BitAnd {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::BitXor {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::MatrixMultiply {
                    whitespace_before,
                    whitespace_after,
                }
            }
        })
    }
}

#[cst_node]
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
        let (bef, aft, between) = match self {
            Self::LessThan {
                whitespace_before,
                whitespace_after,
            }
            | Self::GreaterThan {
                whitespace_before,
                whitespace_after,
            }
            | Self::LessThanEqual {
                whitespace_before,
                whitespace_after,
            }
            | Self::GreaterThanEqual {
                whitespace_before,
                whitespace_after,
            }
            | Self::Equal {
                whitespace_before,
                whitespace_after,
            }
            | Self::NotEqual {
                whitespace_before,
                whitespace_after,
            }
            | Self::In {
                whitespace_before,
                whitespace_after,
            }
            | Self::Is {
                whitespace_before,
                whitespace_after,
            } => (whitespace_before, whitespace_after, None),
            Self::IsNot {
                whitespace_before,
                whitespace_between,
                whitespace_after,
            } => (
                whitespace_before,
                whitespace_after,
                Some(whitespace_between),
            ),
            Self::NotIn {
                whitespace_before,
                whitespace_between,
                whitespace_after,
            } => (
                whitespace_before,
                whitespace_after,
                Some(whitespace_between),
            ),
        };
        let (first_tok, second_tok) = match self {
            CompOp::LessThan { .. } => ("<", None),
            CompOp::GreaterThan { .. } => (">", None),
            CompOp::LessThanEqual { .. } => ("<=", None),
            CompOp::GreaterThanEqual { .. } => (">=", None),
            CompOp::Equal { .. } => ("==", None),
            CompOp::NotEqual { .. } => ("!=", None),
            CompOp::In { .. } => ("in", None),
            CompOp::NotIn { .. } => ("not", Some("in")),
            CompOp::Is { .. } => ("is", None),
            CompOp::IsNot { .. } => ("is", Some("not")),
        };
        bef.codegen(state);
        state.add_token(first_tok);
        if let (Some(btw), Some(second_tok)) = (between, second_tok) {
            btw.codegen(state);
            state.add_token(second_tok);
        }
        aft.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedCompOp<'r, 'a> {
    type Inflated = CompOp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
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
                Self::Inflated::LessThan {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::GreaterThan {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::LessThanEqual {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::GreaterThanEqual {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Equal {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::NotEqual {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::In {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::Is {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::IsNot {
                    whitespace_before,
                    whitespace_between,
                    whitespace_after,
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
                Self::Inflated::NotIn {
                    whitespace_before,
                    whitespace_between,
                    whitespace_after,
                }
            }
        })
    }
}

#[cst_node]
pub struct Colon<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    #[cfg_attr(feature = "py", skip_py)]
    pub(crate) tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedColon<'r, 'a> {
    type Inflated = Colon<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            whitespace_before,
            whitespace_after,
        })
    }
}

impl<'a> Codegen<'a> for Colon<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(":");
        self.whitespace_after.codegen(state);
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAugOp<'r, 'a> {
    type Inflated = AugOp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
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
                Self::Inflated::AddAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::SubtractAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::MultiplyAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::MatrixMultiplyAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::DivideAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::ModuloAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::BitAndAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::BitOrAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::BitXorAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::LeftShiftAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::RightShiftAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::PowerAssign {
                    whitespace_before,
                    whitespace_after,
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
                Self::Inflated::FloorDivideAssign {
                    whitespace_before,
                    whitespace_after,
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

#[cst_node]
pub struct BitOr<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedBitOr<'r, 'a> {
    type Inflated = BitOr<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            whitespace_before,
            whitespace_after,
        })
    }
}

impl<'a> Codegen<'a> for BitOr<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("|");
        self.whitespace_after.codegen(state);
    }
}
