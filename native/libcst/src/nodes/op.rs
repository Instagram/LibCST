// Copyright (c) Facebook, Inc. and its affiliates.
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Semicolon<'a> {
    /// Any space that appears directly before this semicolon.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this semicolon.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: Token<'a>,
}

impl<'a> Codegen<'a> for Semicolon<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(";");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for Semicolon<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut self.tok.whitespace_before)?,
        );
        self.whitespace_after = ParenthesizableWhitespace::SimpleWhitespace(
            parse_simple_whitespace(config, &mut self.tok.whitespace_after)?,
        );
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comma<'a> {
    /// Any space that appears directly before this comma.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this comma.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: Token<'a>,
}

impl<'a> Codegen<'a> for Comma<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(",");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for Comma<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_before)?;
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AssignEqual<'a> {
    /// Any space that appears directly before this equal sign.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this equal sign.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: Token<'a>,
}

impl<'a> Codegen<'a> for AssignEqual<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("=");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for AssignEqual<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_before)?;
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Dot<'a> {
    /// Any space that appears directly before this dot.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this dot.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: Token<'a>,
}

impl<'a> Codegen<'a> for Dot<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(".");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for Dot<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_before)?;
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportStar {}

impl<'a> Codegen<'a> for ImportStar {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("*");
    }
}

impl<'a> Inflate<'a> for ImportStar {
    fn inflate(&mut self, _config: &Config<'a>) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp<'a> {
    Plus(ParenthesizableWhitespace<'a>, Token<'a>),
    Minus(ParenthesizableWhitespace<'a>, Token<'a>),
    BitInvert(ParenthesizableWhitespace<'a>, Token<'a>),
    Not(ParenthesizableWhitespace<'a>, Token<'a>),
}

impl<'a> Codegen<'a> for UnaryOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        let (tok, whitespace_after) = match self {
            Self::Plus(ws, ..) => ("+", ws),
            Self::Minus(ws, ..) => ("-", ws),
            Self::BitInvert(ws, ..) => ("~", ws),
            Self::Not(ws, ..) => ("not", ws),
        };
        state.add_token(tok);
        whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for UnaryOp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        let (whitespace_after, tok) = match self {
            Self::Plus(a, b) | Self::Minus(a, b) | Self::BitInvert(a, b) | Self::Not(a, b) => {
                (a, b)
            }
        };
        *whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BooleanOp<'a> {
    And {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Or {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
}

impl<'a> Codegen<'a> for BooleanOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        let (whitespace_before, whitespace_after, tok) = match self {
            Self::And {
                whitespace_before,
                whitespace_after,
                tok,
            }
            | Self::Or {
                whitespace_before,
                whitespace_after,
                tok,
            } => (whitespace_before, whitespace_after, tok),
        };
        *whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
        *whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp<'a> {
    Add {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Subtract {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Multiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Divide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    FloorDivide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Modulo {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Power {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    LeftShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    RightShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    BitOr {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    BitAnd {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    BitXor {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    MatrixMultiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
}

impl<'a> Codegen<'a> for BinaryOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
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
        *whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
        *whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompOp<'a> {
    LessThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    GreaterThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    LessThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    GreaterThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    Equal {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    NotEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    In {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    NotIn {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        not_tok: Token<'a>,
        in_tok: Token<'a>,
    },
    Is {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    IsNot {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        is_tok: Token<'a>,
        not_tok: Token<'a>,
    },
}

impl<'a> Codegen<'a> for CompOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        let (whitespace_before, whitespace_after, first_tok, between) = match self {
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
        *whitespace_before =
            parse_parenthesizable_whitespace(config, &mut first_tok.whitespace_before)?;
        if let Some((whitespace_between, second_tok)) = between {
            *whitespace_between =
                parse_parenthesizable_whitespace(config, &mut first_tok.whitespace_after)?;
            *whitespace_after =
                parse_parenthesizable_whitespace(config, &mut second_tok.whitespace_after)?;
        } else {
            *whitespace_after =
                parse_parenthesizable_whitespace(config, &mut first_tok.whitespace_after)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Colon<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: Token<'a>,
}

impl<'a> Inflate<'a> for Colon<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_before)?;
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for Colon<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(":");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AugOp<'a> {
    AddAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    SubtractAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    MultiplyAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    MatrixMultiplyAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    DivideAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    ModuloAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    BitAndAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    BitOrAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    BitXorAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    LeftShiftAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    RightShiftAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    PowerAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
    FloorDivideAssign {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
        tok: Token<'a>,
    },
}

impl<'a> Inflate<'a> for AugOp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        let (tok, whitespace_before, whitespace_after) = match self {
            Self::AddAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::SubtractAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::MultiplyAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::MatrixMultiplyAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::DivideAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::ModuloAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::BitAndAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::BitOrAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::BitXorAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::LeftShiftAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::RightShiftAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::PowerAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
            Self::FloorDivideAssign {
                whitespace_before,
                whitespace_after,
                tok,
            } => (tok, whitespace_before, whitespace_after),
        };
        *whitespace_before = parse_parenthesizable_whitespace(config, &mut tok.whitespace_before)?;
        *whitespace_after = parse_parenthesizable_whitespace(config, &mut tok.whitespace_after)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for AugOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
