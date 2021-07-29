// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{whitespace::ParenthesizableWhitespace, Codegen, CodegenState};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Semicolon<'a> {
    /// Any space that appears directly before this semicolon.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this semicolon.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Semicolon<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(";");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comma<'a> {
    /// Any space that appears directly before this comma.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this comma.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Comma<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(",");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AssignEqual<'a> {
    /// Any space that appears directly before this equal sign.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this equal sign.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for AssignEqual<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("=");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Dot<'a> {
    /// Any space that appears directly before this dot.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    /// Any space that appears directly after this dot.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Dot<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(".");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportStar {}

impl<'a> Codegen<'a> for ImportStar {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("*");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp<'a> {
    Plus(ParenthesizableWhitespace<'a>),
    Minus(ParenthesizableWhitespace<'a>),
    BitInvert(ParenthesizableWhitespace<'a>),
    Not(ParenthesizableWhitespace<'a>),
}

impl<'a> Codegen<'a> for UnaryOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        let (tok, whitespace_after) = match self {
            Self::Plus(ws) => ("+", ws),
            Self::Minus(ws) => ("-", ws),
            Self::BitInvert(ws) => ("~", ws),
            Self::Not(ws) => ("not", ws),
        };
        state.add_token(tok);
        whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BooleanOp<'a> {
    And {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Or {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
}

impl<'a> Codegen<'a> for BooleanOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        let (tok, ws_bef, ws_aft) = match self {
            Self::And {
                whitespace_after,
                whitespace_before,
            } => ("and", whitespace_before, whitespace_after),
            Self::Or {
                whitespace_after,
                whitespace_before,
            } => ("or", whitespace_before, whitespace_after),
        };
        ws_bef.codegen(state);
        state.add_token(tok);
        ws_aft.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp<'a> {
    Add {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Subtract {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Multiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Divide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    FloorDivide {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Modulo {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Power {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    LeftShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    RightShift {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    BitOr {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    BitAnd {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    BitXor {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    MatrixMultiply {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
}

impl<'a> Codegen<'a> for BinaryOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        let (tok, bef, aft) = match self {
            Self::Add {
                whitespace_before,
                whitespace_after,
            } => ("+", whitespace_before, whitespace_after),
            Self::Subtract {
                whitespace_before,
                whitespace_after,
            } => ("-", whitespace_before, whitespace_after),
            Self::Multiply {
                whitespace_before,
                whitespace_after,
            } => ("*", whitespace_before, whitespace_after),
            Self::Divide {
                whitespace_before,
                whitespace_after,
            } => ("/", whitespace_before, whitespace_after),
            Self::FloorDivide {
                whitespace_before,
                whitespace_after,
            } => ("//", whitespace_before, whitespace_after),
            Self::Modulo {
                whitespace_before,
                whitespace_after,
            } => ("%", whitespace_before, whitespace_after),
            Self::Power {
                whitespace_before,
                whitespace_after,
            } => ("**", whitespace_before, whitespace_after),
            Self::LeftShift {
                whitespace_before,
                whitespace_after,
            } => ("<<", whitespace_before, whitespace_after),
            Self::RightShift {
                whitespace_before,
                whitespace_after,
            } => (">>", whitespace_before, whitespace_after),
            Self::BitOr {
                whitespace_before,
                whitespace_after,
            } => ("|", whitespace_before, whitespace_after),
            Self::BitAnd {
                whitespace_before,
                whitespace_after,
            } => ("&", whitespace_before, whitespace_after),
            Self::BitXor {
                whitespace_before,
                whitespace_after,
            } => ("^", whitespace_before, whitespace_after),
            Self::MatrixMultiply {
                whitespace_before,
                whitespace_after,
            } => ("@", whitespace_before, whitespace_after),
        };
        bef.codegen(state);
        state.add_token(tok);
        aft.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompOp<'a> {
    LessThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    GreaterThan {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    LessThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    GreaterThanEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Equal {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    NotEqual {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    In {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    NotIn {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    Is {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
    IsNot {
        whitespace_before: ParenthesizableWhitespace<'a>,
        whitespace_between: ParenthesizableWhitespace<'a>,
        whitespace_after: ParenthesizableWhitespace<'a>,
    },
}

impl<'a> Codegen<'a> for CompOp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        let (tok, bef, between, aft) = match self {
            Self::LessThan {
                whitespace_before,
                whitespace_after,
            } => ("<", whitespace_before, None, whitespace_after),
            Self::GreaterThan {
                whitespace_before,
                whitespace_after,
            } => (">", whitespace_before, None, whitespace_after),
            Self::LessThanEqual {
                whitespace_before,
                whitespace_after,
            } => ("<=", whitespace_before, None, whitespace_after),
            Self::GreaterThanEqual {
                whitespace_before,
                whitespace_after,
            } => (">=", whitespace_before, None, whitespace_after),
            Self::Equal {
                whitespace_before,
                whitespace_after,
            } => ("==", whitespace_before, None, whitespace_after),
            Self::NotEqual {
                whitespace_before,
                whitespace_after,
            } => ("!=", whitespace_before, None, whitespace_after),
            Self::In {
                whitespace_before,
                whitespace_after,
            } => ("in", whitespace_before, None, whitespace_after),
            Self::Is {
                whitespace_before,
                whitespace_after,
            } => ("is", whitespace_before, None, whitespace_after),
            Self::IsNot {
                whitespace_before,
                whitespace_between,
                whitespace_after,
            } => (
                "is",
                whitespace_before,
                Some(("not", whitespace_between)),
                whitespace_after,
            ),
            Self::NotIn {
                whitespace_before,
                whitespace_between,
                whitespace_after,
            } => (
                "not",
                whitespace_before,
                Some(("in", whitespace_between)),
                whitespace_after,
            ),
        };
        bef.codegen(state);
        state.add_token(tok);
        if let Some((second_tok, btw)) = between {
            btw.codegen(state);
            state.add_token(second_tok);
        }
        aft.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Colon<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Colon<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(":");
        self.whitespace_after.codegen(state);
    }
}
