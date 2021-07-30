// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use crate::{
    traits::{ParenthesizedNode, WithComma},
    whitespace::ParenthesizableWhitespace,
    AssignEqual, AssignTargetExpression, BinaryOp, BooleanOp, Codegen, CodegenState, Colon, Comma,
    CompOp, Dot, UnaryOp,
};
#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct Parameters<'a> {
    pub params: Vec<Param<'a>>,
    pub star_arg: Option<StarArg<'a>>,
    pub kwonly_params: Vec<Param<'a>>,
    pub star_kwarg: Option<Param<'a>>,
    pub posonly_params: Vec<Param<'a>>,
    pub posonly_ind: Option<ParamSlash<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StarArg<'a> {
    Star(ParamStar<'a>),
    Param(Box<Param<'a>>),
}

impl<'a> Codegen<'a> for Parameters<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

        if let Some(star) = &self.star_kwarg {
            star.codegen(state, Some("**"), false)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParamSlash<'a> {
    pub comma: Option<Comma<'a>>,
}

impl<'a> ParamSlash<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>, default_comma: bool) {
        state.add_token("/");
        match (&self.comma, default_comma) {
            (Some(comma), _) => comma.codegen(state),
            (None, true) => state.add_token(", "),
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParamStar<'a> {
    pub comma: Comma<'a>,
}

impl<'a> Codegen<'a> for ParamStar<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("*");
        self.comma.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct Name<'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Name<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
    ) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arg<'a> {
    pub value: Expression<'a>,
    pub keyword: Option<Name<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub comma: Option<Comma<'a>>,
    pub star: &'a str,
    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_arg: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Arg<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token(self.star);
        self.whitespace_after_star.codegen(state);
        if let Some(kw) = &self.keyword {
            kw.codegen(state);
        }
        if let Some(eq) = &self.equal {
            eq.codegen(state);
        } else if self.keyword.is_some() {
            state.add_token(" = ");
        }
        self.value.codegen(state);

        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }

        self.whitespace_after_arg.codegen(state);
    }
}

impl<'a> WithComma<'a> for Arg<'a> {
    fn with_comma(self, c: Comma<'a>) -> Self {
        Self {
            comma: Some(c),
            ..self
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LeftParen<'a> {
    /// Any space that appears directly after this left parenthesis.
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for LeftParen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("(");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RightParen<'a> {
    /// Any space that appears directly before this right parenthesis.
    pub whitespace_before: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for RightParen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(")");
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
    },
    Tuple(Tuple<'a>),
    Call(Call<'a>),
    GeneratorExp(GeneratorExp<'a>),
    ListComp(ListComp<'a>),
    SetComp(SetComp<'a>),
    DictComp(DictComp<'a>),
    List(List<'a>),
    Set(Set<'a>),
    Dict(Dict<'a>),
    Subscript(Subscript<'a>),
    // TODO: FormattedString, ConcatenatedString, Lambda, Await, IfExp, Yield
}

impl<'a> Codegen<'a> for Expression<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::Ellipsis { .. } => state.add_token("..."),
            Self::BinaryOperation {
                left,
                operator,
                right,
                ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                operator.codegen(state);
                right.codegen(state);
            }),
            Self::Integer { value, .. } => self.parenthesize(state, |state| state.add_token(value)),
            Self::Attribute(a) => a.codegen(state),
            Self::UnaryOperation {
                expression,
                operator,
                ..
            } => self.parenthesize(state, |state| {
                operator.codegen(state);
                expression.codegen(state);
            }),
            Self::Name(n) => n.codegen(state),
            Self::Comparison {
                left, comparisons, ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                for comp in comparisons {
                    comp.codegen(state);
                }
            }),
            Self::BooleanOperation {
                left,
                operator,
                right,
                ..
            } => self.parenthesize(state, |state| {
                left.codegen(state);
                operator.codegen(state);
                right.codegen(state);
            }),
            Self::SimpleString { value, .. } => self.parenthesize(state, |state| {
                state.add_token(value);
            }),
            Self::Tuple(t) => t.codegen(state),
            Self::Call(c) => c.codegen(state),
            Self::GeneratorExp(g) => g.codegen(state),
            Self::ListComp(l) => l.codegen(state),
            Self::SetComp(s) => s.codegen(state),
            Self::DictComp(d) => d.codegen(state),
            Self::List(l) => l.codegen(state),
            Self::Set(s) => s.codegen(state),
            Self::Dict(d) => d.codegen(state),
            Self::Subscript(s) => s.codegen(state),
            _ => panic!("codegen not implemented for {:#?}", self),
        }
    }
}

impl<'a> ParenthesizedNode<'a> for Expression<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        match self {
            Self::BinaryOperation { lpar, .. } => lpar,
            Self::Integer { lpar, .. } => lpar,
            Self::UnaryOperation { lpar, .. } => lpar,
            Self::Comparison { lpar, .. } => lpar,
            Self::BooleanOperation { lpar, .. } => lpar,
            Self::SimpleString { lpar, .. } => lpar,
            _ => panic!("lpar not implemented for {:#?}", self),
        }
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        match self {
            Self::BinaryOperation { rpar, .. } => rpar,
            Self::Integer { rpar, .. } => rpar,
            Self::UnaryOperation { rpar, .. } => rpar,
            Self::Comparison { rpar, .. } => rpar,
            Self::BooleanOperation { rpar, .. } => rpar,
            Self::SimpleString { rpar, .. } => rpar,
            _ => panic!("rpar not implemented for {:#?}", self),
        }
    }

    fn with_parens(self, leftpar: LeftParen<'a>, rightpar: RightParen<'a>) -> Self {
        match self {
            Self::BinaryOperation {
                lpar,
                rpar,
                left,
                right,
                operator,
            } => {
                let mut lpar = lpar;
                let mut rpar = rpar;
                lpar.push(leftpar);
                rpar.push(rightpar);
                Self::BinaryOperation {
                    lpar,
                    rpar,
                    left,
                    right,
                    operator,
                }
            }
            Self::Integer { rpar, lpar, value } => {
                let mut lpar = lpar;
                let mut rpar = rpar;
                lpar.push(leftpar);
                rpar.push(rightpar);
                Self::Integer { rpar, lpar, value }
            }
            Self::UnaryOperation {
                rpar,
                lpar,
                expression,
                operator,
            } => {
                let mut lpar = lpar;
                let mut rpar = rpar;
                lpar.push(leftpar);
                rpar.push(rightpar);
                Self::UnaryOperation {
                    rpar,
                    lpar,
                    expression,
                    operator,
                }
            }
            Self::Comparison {
                lpar,
                rpar,
                left,
                comparisons,
            } => {
                let mut lpar = lpar;
                let mut rpar = rpar;
                lpar.push(leftpar);
                rpar.push(rightpar);
                Self::Comparison {
                    lpar,
                    rpar,
                    left,
                    comparisons,
                }
            }
            Self::BooleanOperation {
                lpar,
                rpar,
                left,
                operator,
                right,
            } => {
                let mut lpar = lpar;
                let mut rpar = rpar;
                lpar.push(leftpar);
                rpar.push(rightpar);
                Self::BooleanOperation {
                    lpar,
                    rpar,
                    left,
                    operator,
                    right,
                }
            }
            Self::SimpleString { lpar, rpar, value } => {
                let mut lpar = lpar;
                let mut rpar = rpar;
                lpar.push(leftpar);
                rpar.push(rightpar);
                Self::SimpleString { lpar, rpar, value }
            }
            Self::Name(n) => Self::Name(n.with_parens(leftpar, rightpar)),
            Self::Attribute(a) => Self::Attribute(a.with_parens(leftpar, rightpar)),
            Self::Subscript(s) => Self::Subscript(s.with_parens(leftpar, rightpar)),
            _ => panic!("with_parens not implemented for {:#?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call<'a> {
    pub func: Box<Expression<'a>>,
    pub args: Vec<Arg<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_func: ParenthesizableWhitespace<'a>,
    pub whitespace_before_args: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Call<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.func.codegen(state);
            self.whitespace_after_func.codegen(state);
            state.add_token("(");
            self.whitespace_before_args.codegen(state);
            let arg_len = self.args.len();
            for (i, arg) in self.args.iter().enumerate() {
                arg.codegen(state);
                if arg.comma.is_none() && i + 1 < arg_len {
                    state.add_token(", ");
                }
            }
            state.add_token(")");
        })
    }
}

impl<'a> ParenthesizedNode<'a> for Call<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attribute<'a> {
    pub value: Box<Expression<'a>>,
    pub attr: Name<'a>,
    pub dot: Dot<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Attribute<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NameOrAttribute<'a> {
    N(Name<'a>),
    A(Attribute<'a>),
}

impl<'a> Codegen<'a> for NameOrAttribute<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::N(n) => n.codegen(state),
            Self::A(a) => a.codegen(state),
        }
    }
}

impl<'a> From<NameOrAttribute<'a>> for Expression<'a> {
    fn from(x: NameOrAttribute<'a>) -> Self {
        match x {
            NameOrAttribute::N(n) => Self::Name(n),
            NameOrAttribute::A(a) => Self::Attribute(a),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ComparisonTarget<'a> {
    pub operator: CompOp<'a>,
    pub comparator: Expression<'a>,
}

impl<'a> Codegen<'a> for ComparisonTarget<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.operator.codegen(state);
        self.comparator.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StarredElement<'a> {
    pub value: Expression<'a>,
    pub comma: Option<Comma<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for StarredElement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("*");
            self.whitespace_before_value.codegen(state);
            self.value.codegen(state);
        });
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

impl<'a> ParenthesizedNode<'a> for StarredElement<'a> {
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element<'a> {
    Simple {
        value: Expression<'a>,
        comma: Option<Comma<'a>>,
    },
    Starred(StarredElement<'a>),
}

impl<'a> Element<'a> {
    fn codegen(
        &'a self,
        state: &mut CodegenState<'a>,
        default_comma: bool,
        default_comma_whitespace: bool,
    ) {
        match self {
            Self::Simple { value, comma } => {
                value.codegen(state);
                if let Some(comma) = comma {
                    comma.codegen(state)
                }
            }
            Self::Starred(s) => s.codegen(state),
        }
        let maybe_comma = match self {
            Self::Simple { comma, .. } => comma,
            Self::Starred(s) => &s.comma,
        };
        if maybe_comma.is_none() && default_comma {
            state.add_token(if default_comma_whitespace { ", " } else { "," });
        }
    }
}

impl<'a> WithComma<'a> for Element<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        let comma = Some(comma);
        match self {
            Self::Simple { value, .. } => Self::Simple { comma, value },
            Self::Starred(s) => Self::Starred(StarredElement { comma, ..s }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Tuple<'a> {
    pub elements: Vec<Element<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for Tuple<'a> {
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        // HACK: for empty tuples the left parenthesis owns the whitespace.
        if self.elements.is_empty() {
            rpar.push(RightParen {
                whitespace_before: Default::default(),
            });
        } else {
            rpar.push(right);
        }
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for Tuple<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            let len = self.elements.len();
            if len == 1 {
                self.elements.first().unwrap().codegen(state, true, false);
            } else {
                for (idx, el) in self.elements.iter().enumerate() {
                    el.codegen(state, idx < len - 1, true);
                }
            }
        });
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GeneratorExp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for GeneratorExp<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for GeneratorExp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.elt.codegen(state);
            self.for_in.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ListComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for ListComp<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for ListComp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbracket.codegen(state);
            self.elt.codegen(state);
            self.for_in.codegen(state);
            self.rbracket.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftSquareBracket<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for LeftSquareBracket<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("[");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RightSquareBracket<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for RightSquareBracket<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("]");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SetComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for SetComp<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for SetComp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            self.elt.codegen(state);
            self.for_in.codegen(state);
            self.rbrace.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DictComp<'a> {
    pub key: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_before_colon: ParenthesizableWhitespace<'a>,
    pub whitespace_after_colon: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for DictComp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            self.key.codegen(state);
            self.whitespace_before_colon.codegen(state);
            state.add_token(":");
            self.whitespace_after_colon.codegen(state);
            self.value.codegen(state);
            self.for_in.codegen(state);
            self.rbrace.codegen(state);
        })
    }
}

impl<'a> ParenthesizedNode<'a> for DictComp<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftCurlyBrace<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for LeftCurlyBrace<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("{");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RightCurlyBrace<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for RightCurlyBrace<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("}");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompFor<'a> {
    pub target: AssignTargetExpression<'a>,
    pub iter: Expression<'a>,
    pub ifs: Vec<CompIf<'a>>,
    pub inner_for_in: Option<Box<CompFor<'a>>>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after_for: ParenthesizableWhitespace<'a>,
    pub whitespace_before_in: ParenthesizableWhitespace<'a>,
    pub whitespace_after_in: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for CompFor<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        if let Some(asynchronous) = &self.asynchronous {
            asynchronous.codegen(state);
        }
        state.add_token("for");
        self.whitespace_after_for.codegen(state);
        self.target.codegen(state);
        self.whitespace_before_in.codegen(state);
        state.add_token("in");
        self.whitespace_after_in.codegen(state);
        self.iter.codegen(state);
        for if_ in &self.ifs {
            if_.codegen(state);
        }
        if let Some(inner) = &self.inner_for_in {
            inner.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Asynchronous<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Asynchronous<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("async");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompIf<'a> {
    pub test: Expression<'a>,
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_before_test: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for CompIf<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("if");
        self.whitespace_before_test.codegen(state);
        self.test.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct List<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for List<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for List<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbracket.codegen(state);
            let len = self.elements.len();
            for (idx, el) in self.elements.iter().enumerate() {
                el.codegen(state, idx < len - 1, true);
            }
            self.rbracket.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Set<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for Set<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for Set<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            let len = self.elements.len();
            for (idx, el) in self.elements.iter().enumerate() {
                el.codegen(state, idx < len - 1, true);
            }
            self.rbrace.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Dict<'a> {
    pub elements: Vec<DictElement<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> ParenthesizedNode<'a> for Dict<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for Dict<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            let len = self.elements.len();
            for (idx, el) in self.elements.iter().enumerate() {
                el.codegen(state, idx < len - 1, true);
            }
            self.rbrace.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DictElement<'a> {
    Simple {
        key: Expression<'a>,
        value: Expression<'a>,
        comma: Option<Comma<'a>>,
        whitespace_before_colon: ParenthesizableWhitespace<'a>,
        whitespace_after_colon: ParenthesizableWhitespace<'a>,
    },
    Starred(DoubleStarredElement<'a>),
}

impl<'a> DictElement<'a> {
    fn codegen(
        &'a self,
        state: &mut CodegenState<'a>,
        default_comma: bool,
        default_comma_whitespace: bool,
    ) {
        match self {
            Self::Simple {
                key,
                value,
                comma,
                whitespace_before_colon,
                whitespace_after_colon,
            } => {
                key.codegen(state);
                whitespace_before_colon.codegen(state);
                state.add_token(":");
                whitespace_after_colon.codegen(state);
                value.codegen(state);
                if let Some(comma) = comma {
                    comma.codegen(state)
                }
            }
            Self::Starred(s) => s.codegen(state),
        }
        let maybe_comma = match self {
            Self::Simple { comma, .. } => comma,
            Self::Starred(s) => &s.comma,
        };
        if maybe_comma.is_none() && default_comma {
            state.add_token(if default_comma_whitespace { ", " } else { "," });
        }
    }
}

impl<'a> WithComma<'a> for DictElement<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        let comma = Some(comma);
        match self {
            Self::Starred(s) => Self::Starred(DoubleStarredElement { comma, ..s }),
            Self::Simple {
                key,
                value,
                whitespace_before_colon,
                whitespace_after_colon,
                ..
            } => Self::Simple {
                comma,
                key,
                value,
                whitespace_after_colon,
                whitespace_before_colon,
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DoubleStarredElement<'a> {
    pub value: Expression<'a>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for DoubleStarredElement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("**");
        self.whitespace_before_value.codegen(state);
        self.value.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BaseSlice<'a> {
    Index(Index<'a>),
    Slice(Slice<'a>),
}

impl<'a> Codegen<'a> for BaseSlice<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::Index(i) => i.codegen(state),
            Self::Slice(s) => s.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index<'a> {
    pub value: Expression<'a>,
}

impl<'a> Codegen<'a> for Index<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.value.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Slice<'a> {
    pub lower: Option<Expression<'a>>,
    pub upper: Option<Expression<'a>>,
    pub step: Option<Expression<'a>>,
    pub first_colon: Colon<'a>,
    pub second_colon: Option<Colon<'a>>,
}

impl<'a> Codegen<'a> for Slice<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        if let Some(lower) = &self.lower {
            lower.codegen(state);
        }
        self.first_colon.codegen(state);
        if let Some(upper) = &self.upper {
            upper.codegen(state);
        }
        if let Some(second_colon) = &self.second_colon {
            second_colon.codegen(state);
        } else if self.step.is_some() {
            state.add_token(";");
        }
        if let Some(step) = &self.step {
            step.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SubscriptElement<'a> {
    pub slice: BaseSlice<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> Codegen<'a> for SubscriptElement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.slice.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Subscript<'a> {
    pub value: Box<Expression<'a>>,
    pub slice: Vec<SubscriptElement<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_value: ParenthesizableWhitespace<'a>,
}

impl<'a> ParenthesizedNode<'a> for Subscript<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        &self.lpar
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        &self.rpar
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        let mut lpar = self.lpar;
        lpar.push(left);
        let mut rpar = self.rpar;
        rpar.push(right);
        Self { lpar, rpar, ..self }
    }
}

impl<'a> Codegen<'a> for Subscript<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.value.codegen(state);
            self.whitespace_after_value.codegen(state);
            self.lbracket.codegen(state);
            let len = self.slice.len();
            for (i, slice) in self.slice.iter().enumerate() {
                slice.codegen(state);
                if slice.comma.is_none() && i + 1 < len {
                    state.add_token(", ")
                }
            }
            self.rbracket.codegen(state);
        })
    }
}
