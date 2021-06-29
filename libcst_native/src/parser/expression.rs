// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{whitespace::ParenthesizableWhitespace, Codegen, CodegenState, SimpleWhitespace};

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Parameters<'a> {
    pub params: Vec<Param<'a>>,
}
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Name<'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen for Name<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        self.parenthesize(state, |state| {
            state.add_token(self.value.to_string());
        });
    }
}

impl<'a> ParenthesizedNode<'a> for Name<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Param<'a> {
    pub name: Name<'a>,

    pub whitespace_after_star: SimpleWhitespace<'a>,
    pub whitespace_after_param: SimpleWhitespace<'a>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LeftParen<'a> {
    /// Any space that appears directly after this left parenthesis.
    whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen for LeftParen<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        state.add_token("(".to_string());
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RightParen<'a> {
    /// Any space that appears directly before this right parenthesis.
    whitespace_before: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen for RightParen<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        self.whitespace_before.codegen(state);
        state.add_token(")".to_string());
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression<'a> {
    Name(Name<'a>),
    Ellipsis {
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    Integer {
        /// A string representation of the integer, such as ``"100000"`` or
        /// ``"100_000"``.
        value: &'a str,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    Float {
        /// A string representation of the floating point number, such as ```"0.05"``,
        /// ``".050"``, or ``"5e-2"``.
        value: &'a str,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    Imaginary {
        /// A string representation of the complex number, such as ``"2j"``
        value: &'a str,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    SimpleString {
        /// The texual representation of the string, including quotes, prefix
        /// characters, and any escape characters present in the original source code,
        /// such as ``r"my string\n"``.
        value: &'a str,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    Comparison {
        left: Box<Expression<'a>>,
        comparisons: Vec<ComparisonTarget<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    UnaryOperation {
        operator: &'a str, // TODO
        expression: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    BinaryOperation {
        left: Box<Expression<'a>>,
        operator: &'a str, // TODO
        right: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    BooleanOperation {
        left: Box<Expression<'a>>,
        operator: &'a str, // TODO
        right: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    Attribute {
        value: Box<Expression<'a>>,
        attr: Name<'a>,
        dot: &'a str, // TODO
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    NamedExpr {
        target: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
        whitespace_before_walrus: ParenthesizableWhitespace<'a>,
        whitespace_after_walrus: ParenthesizableWhitespace<'a>,
    }, // TODO: FormattedString, ConcatenatedString, Subscript, Lambda, Call, Await, IfExp, Yield, Tuple, List, Set, Dict, comprehensions
}

impl<'a> Codegen for Expression<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::Ellipsis { .. } => state.add_token("...".to_string()),
            &Self::BinaryOperation {
                left,
                operator,
                right,
                ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                state.add_token(operator.to_string()); // TODO
                right.codegen(state);
            }),
            &Self::Integer { value, .. } => {
                self.parenthesize(state, |state| state.add_token(value.to_string()))
            }
            _ => panic!("codegen not implemented for {:#?}", self),
        }
    }
}

impl<'a> ParenthesizedNode<'a> for Expression<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        match &self {
            &Self::BinaryOperation { lpar, .. } => lpar,
            &Self::Integer { lpar, .. } => lpar,
            _ => todo!(),
        }
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        match &self {
            &Self::BinaryOperation { rpar, .. } => rpar,
            &Self::Integer { rpar, .. } => rpar,
            _ => todo!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ComparisonTarget<'a> {
    operator: &'a str, // TODO
    comparator: Expression<'a>,
}

trait ParenthesizedNode<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>>;
    fn rpar(&self) -> &Vec<RightParen<'a>>;

    fn parenthesize<F>(&self, state: &mut CodegenState, f: F) -> ()
    where
        F: FnOnce(&mut CodegenState),
    {
        for lpar in self.lpar() {
            lpar.codegen(state);
        }
        f(state);
        for rpar in self.rpar() {
            rpar.codegen(state);
        }
    }
}
