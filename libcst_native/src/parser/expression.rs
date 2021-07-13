// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{
    whitespace::ParenthesizableWhitespace, AssignEqual, BinaryOp, BooleanOp, Codegen, CodegenState,
    Comma, CompOp, Dot, UnaryOp,
};
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Parameters<'a> {
    pub params: Vec<Param<'a>>,
    pub star_arg: Option<StarArg<'a>>,
    pub kwonly_params: Vec<Param<'a>>,
    pub star_kwarg: Option<Param<'a>>,
    pub posonly_params: Vec<Param<'a>>,
    pub posonly_ind: Option<ParamSlash<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StarArg<'a> {
    Star(ParamStar<'a>),
    Param(Param<'a>),
}

impl<'a> Codegen<'a> for Parameters<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        let params_after_kwonly = self.star_kwarg.is_some();
        let params_after_regular = !self.kwonly_params.is_empty() || params_after_kwonly;
        let params_after_posonly = !self.params.is_empty() || params_after_regular;
        let star_included = self.star_arg.is_some() || !self.kwonly_params.is_empty();

        for p in &self.posonly_params {
            p.codegen(state, None, true);
        }

        match &self.posonly_ind {
            Some(ind) => ind.codegen(state, params_after_posonly),
            _ => {
                if !self.posonly_params.is_empty() {
                    if params_after_posonly {
                        state.add_token("/, ");
                    } else {
                        state.add_token("/");
                    }
                }
            }
        }

        let param_size = self.params.len();
        for (i, p) in self.params.iter().enumerate() {
            p.codegen(state, None, params_after_regular || i < param_size - 1);
        }

        let kwonly_size = self.kwonly_params.len();
        match &self.star_arg {
            None => {
                if star_included {
                    state.add_token("*, ")
                }
            }
            Some(StarArg::Param(p)) => p.codegen(
                state,
                Some("*"),
                kwonly_size > 0 || self.star_kwarg.is_some(),
            ),
            Some(StarArg::Star(s)) => s.codegen(state),
        }

        for (i, p) in self.kwonly_params.iter().enumerate() {
            p.codegen(state, None, params_after_kwonly || i < kwonly_size - 1);
        }

        match &self.star_kwarg {
            Some(star) => star.codegen(state, Some("**"), false),
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParamSlash<'a> {
    pub comma: Option<Comma<'a>>,
}

impl<'a> ParamSlash<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>, default_comma: bool) -> () {
        state.add_token("/");
        match (&self.comma, default_comma) {
            (Some(comma), _) => comma.codegen(state),
            (None, true) => state.add_token(", "),
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParamStar<'a> {
    pub comma: Comma<'a>,
}

impl<'a> Codegen<'a> for ParamStar<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        state.add_token("*");
        self.comma.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Name<'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Name<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
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

#[derive(Debug, Eq, PartialEq)]
pub struct Param<'a> {
    pub name: Name<'a>,
    // TODO: annotation
    pub equal: Option<AssignEqual<'a>>,
    pub default: Option<Expression<'a>>,

    pub comma: Option<Comma<'a>>,

    pub star: Option<&'a str>,

    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_param: ParenthesizableWhitespace<'a>,
}

impl<'a> Default for Param<'a> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            equal: None,
            default: None,
            comma: None,
            star: None,
            whitespace_after_param: ParenthesizableWhitespace::SimpleWhitespace(Default::default()),
            whitespace_after_star: ParenthesizableWhitespace::SimpleWhitespace(Default::default()),
        }
    }
}

impl<'a> Param<'a> {
    fn codegen(
        &'a self,
        state: &mut CodegenState<'a>,
        default_star: Option<&'a str>,
        default_comma: bool,
    ) -> () {
        match (self.star, default_star) {
            (Some(star), _) => state.add_token(star),
            (None, Some(star)) => state.add_token(star),
            _ => {}
        }
        self.whitespace_after_star.codegen(state);
        self.name.codegen(state);

        // TODO: annotation here

        match (&self.equal, &self.default) {
            (Some(equal), Some(def)) => {
                equal.codegen(state);
                def.codegen(state);
            }
            (None, Some(def)) => {
                state.add_token(" = ");
                def.codegen(state);
            }
            _ => {}
        }

        match &self.comma {
            Some(comma) => comma.codegen(state),
            None if default_comma => state.add_token(", "),
            _ => {}
        }

        self.whitespace_after_param.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LeftParen<'a> {
    /// Any space that appears directly after this left parenthesis.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for LeftParen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        state.add_token("(");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RightParen<'a> {
    /// Any space that appears directly before this right parenthesis.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for RightParen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        self.whitespace_before.codegen(state);
        state.add_token(")");
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
        operator: UnaryOp<'a>,
        expression: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    BinaryOperation {
        left: Box<Expression<'a>>,
        operator: BinaryOp<'a>,
        right: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    BooleanOperation {
        left: Box<Expression<'a>>,
        operator: BooleanOp<'a>,
        right: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
    },
    Attribute(Attribute<'a>),
    NamedExpr {
        target: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
        lpar: Vec<LeftParen<'a>>,
        rpar: Vec<RightParen<'a>>,
        whitespace_before_walrus: ParenthesizableWhitespace<'a>,
        whitespace_after_walrus: ParenthesizableWhitespace<'a>,
    }, // TODO: FormattedString, ConcatenatedString, Subscript, Lambda, Call, Await, IfExp, Yield, Tuple, List, Set, Dict, comprehensions
}

impl<'a> Codegen<'a> for Expression<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        match &self {
            &Self::Ellipsis { .. } => state.add_token("..."),
            &Self::BinaryOperation {
                left,
                operator,
                right,
                ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                operator.codegen(state);
                right.codegen(state);
            }),
            &Self::Integer { value, .. } => {
                self.parenthesize(state, |state| state.add_token(value))
            }
            &Self::Attribute(a) => a.codegen(state),
            &Self::UnaryOperation {
                expression,
                operator,
                ..
            } => self.parenthesize(state, |state| {
                operator.codegen(state);
                expression.codegen(state);
            }),
            &Self::Name(n) => n.codegen(state),
            &Self::Comparison {
                left, comparisons, ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                for comp in comparisons {
                    comp.codegen(state);
                }
            }),
            &Self::BooleanOperation {
                left,
                operator,
                right,
                ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                operator.codegen(state);
                right.codegen(state);
            }),
            _ => panic!("codegen not implemented for {:#?}", self),
        }
    }
}

impl<'a> ParenthesizedNode<'a> for Expression<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        match &self {
            &Self::BinaryOperation { lpar, .. } => lpar,
            &Self::Integer { lpar, .. } => lpar,
            &Self::UnaryOperation { lpar, .. } => lpar,
            &Self::Comparison { lpar, .. } => lpar,
            &Self::BooleanOperation { lpar, .. } => lpar,
            _ => panic!("lpar not implemented for {:#?}", self),
        }
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        match &self {
            &Self::BinaryOperation { rpar, .. } => rpar,
            &Self::Integer { rpar, .. } => rpar,
            &Self::UnaryOperation { rpar, .. } => rpar,
            &Self::Comparison { rpar, .. } => rpar,
            &Self::BooleanOperation { rpar, .. } => rpar,
            _ => panic!("rpar not implemented for {:#?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Attribute<'a> {
    pub value: Box<Expression<'a>>,
    pub attr: Name<'a>,
    pub dot: Dot<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Attribute<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        self.parenthesize(state, |state| {
            self.value.codegen(state);
            self.dot.codegen(state);
            self.attr.codegen(state);
        })
    }
}

impl<'a> ParenthesizedNode<'a> for Attribute<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NameOrAttribute<'a> {
    N(Name<'a>),
    A(Attribute<'a>),
}

impl<'a> Codegen<'a> for NameOrAttribute<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        match &self {
            &Self::N(n) => n.codegen(state),
            &Self::A(a) => a.codegen(state),
        }
    }
}

impl<'a> Into<Expression<'a>> for NameOrAttribute<'a> {
    fn into(self) -> Expression<'a> {
        match self {
            Self::N(n) => Expression::Name(n),
            Self::A(a) => Expression::Attribute(a),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ComparisonTarget<'a> {
    pub operator: CompOp<'a>,
    pub comparator: Expression<'a>,
}

impl<'a> Codegen<'a> for ComparisonTarget<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) -> () {
        self.operator.codegen(state);
        self.comparator.codegen(state);
    }
}

trait ParenthesizedNode<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>>;
    fn rpar(&self) -> &Vec<RightParen<'a>>;

    fn parenthesize<F>(&'a self, state: &mut CodegenState<'a>, f: F) -> ()
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
}
