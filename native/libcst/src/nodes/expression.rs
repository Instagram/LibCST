// Copyright (c) Meta Platforms, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::mem::swap;

use crate::{
    inflate_helpers::adjust_parameters_trailing_whitespace,
    nodes::{
        common::TokenRef,
        traits::{Inflate, ParenthesizedNode, Result, WithComma},
        whitespace::ParenthesizableWhitespace,
        Annotation, AssignEqual, AssignTargetExpression, BinaryOp, BooleanOp, Codegen,
        CodegenState, Colon, Comma, CompOp, Dot, UnaryOp,
    },
    tokenizer::whitespace_parser::{parse_parenthesizable_whitespace, Config},
};
use libcst_derive::{cst_node, Codegen, Inflate, IntoPy, ParenthesizedNode};
use pyo3::{types::PyModule, IntoPy};

#[derive(Debug, Eq, PartialEq, Default, Clone, IntoPy)]
pub struct Parameters<'r, 'a> {
    pub params: Vec<Param<'r, 'a>>,
    pub star_arg: Option<StarArg<'r, 'a>>,
    pub kwonly_params: Vec<Param<'r, 'a>>,
    pub star_kwarg: Option<Param<'r, 'a>>,
    pub posonly_params: Vec<Param<'r, 'a>>,
    pub posonly_ind: Option<ParamSlash<'r, 'a>>,
}

impl<'r, 'a> Parameters<'r, 'a> {
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
            && self.star_arg.is_none()
            && self.kwonly_params.is_empty()
            && self.star_kwarg.is_none()
            && self.posonly_params.is_empty()
            && self.posonly_ind.is_none()
    }
}

impl<'r, 'a> Inflate<'a> for Parameters<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.posonly_params = self.posonly_params.inflate(config)?;
        self.posonly_ind = self.posonly_ind.inflate(config)?;
        self.params = self.params.inflate(config)?;
        self.star_arg = self.star_arg.inflate(config)?;
        self.kwonly_params = self.kwonly_params.inflate(config)?;
        self.star_kwarg = self.star_kwarg.inflate(config)?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Inflate, IntoPy)]
pub enum StarArg<'r, 'a> {
    Star(ParamStar<'r, 'a>),
    Param(Box<Param<'r, 'a>>),
}

impl<'r, 'a> Codegen<'a> for Parameters<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ParamSlash<'r, 'a> {
    pub comma: Option<Comma<'r, 'a>>,
}

impl<'r, 'a> ParamSlash<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        state.add_token("/");
        match (&self.comma, default_comma) {
            (Some(comma), _) => comma.codegen(state),
            (None, true) => state.add_token(", "),
            _ => {}
        }
    }
}

impl<'r, 'a> Inflate<'a> for ParamSlash<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ParamStar<'r, 'a> {
    pub comma: Comma<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for ParamStar<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
        self.comma.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for ParamStar<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone, ParenthesizedNode, IntoPy)]
pub struct Name<'r, 'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Name<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Name<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        });
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Param<'r, 'a> {
    pub name: Name<'r, 'a>,
    pub annotation: Option<Annotation<'r, 'a>>,
    pub equal: Option<AssignEqual<'r, 'a>>,
    pub default: Option<Expression<'r, 'a>>,

    pub comma: Option<Comma<'r, 'a>>,

    pub star: Option<&'a str>,

    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_param: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Option<TokenRef<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Param<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        // TODO: whitespace_after_param missing?
        self.name = self.name.inflate(config)?;
        self.annotation = self.annotation.inflate(config)?;
        self.equal = self.equal.inflate(config)?;
        self.default = self.default.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        if let Some(star_tok) = self.star_tok.as_mut() {
            self.whitespace_after_star = parse_parenthesizable_whitespace(
                config,
                &mut star_tok.whitespace_after.borrow_mut(),
            )?;
        }
        Ok(self)
    }
}

impl<'r, 'a> Default for Param<'r, 'a> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            annotation: None,
            equal: None,
            default: None,
            comma: None,
            star: Some(""), // Note: this preserves a quirk of the pure python parser
            whitespace_after_param: Default::default(),
            whitespace_after_star: Default::default(),
            star_tok: None,
        }
    }
}

impl<'r, 'a> Param<'r, 'a> {
    fn codegen(
        &self,
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

        if let Some(ann) = &self.annotation {
            ann.codegen(state, ":");
        }

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

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Arg<'r, 'a> {
    pub value: Expression<'r, 'a>,
    pub keyword: Option<Name<'r, 'a>>,
    pub equal: Option<AssignEqual<'r, 'a>>,
    pub comma: Option<Comma<'r, 'a>>,
    pub star: &'a str,
    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_arg: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Option<TokenRef<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Arg<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        if let Some(star_tok) = self.star_tok.as_mut() {
            self.whitespace_after_star = parse_parenthesizable_whitespace(
                config,
                &mut star_tok.whitespace_after.borrow_mut(),
            )?;
        }
        self.keyword = self.keyword.inflate(config)?;
        self.equal = self.equal.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        // whitespace_after_arg is handled in Call
        Ok(self)
    }
}

impl<'r, 'a> Arg<'r, 'a> {
    pub fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
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
        } else if default_comma {
            state.add_token(", ");
        }

        self.whitespace_after_arg.codegen(state);
    }
}

impl<'r, 'a> WithComma<'r, 'a> for Arg<'r, 'a> {
    fn with_comma(self, c: Comma<'r, 'a>) -> Self {
        Self {
            comma: Some(c),
            ..self
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct LeftParen<'r, 'a> {
    /// Any space that appears directly after this left parenthesis.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for LeftParen<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("(");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for LeftParen<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct RightParen<'r, 'a> {
    /// Any space that appears directly before this right parenthesis.
    pub whitespace_before: ParenthesizableWhitespace<'a>,

    pub(crate) rpar_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for RightParen<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(")");
    }
}

impl<'r, 'a> Inflate<'a> for RightParen<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.rpar_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq, Clone, ParenthesizedNode, Codegen, Inflate, IntoPy)]
pub enum Expression<'r, 'a> {
    Name(Name<'r, 'a>),
    Ellipsis(Ellipsis<'r, 'a>),
    Integer(Integer<'r, 'a>),
    Float(Float<'r, 'a>),
    Imaginary(Imaginary<'r, 'a>),
    Comparison(Comparison<'r, 'a>),
    UnaryOperation(UnaryOperation<'r, 'a>),
    BinaryOperation(BinaryOperation<'r, 'a>),
    BooleanOperation(BooleanOperation<'r, 'a>),
    Attribute(Attribute<'r, 'a>),
    Tuple(Tuple<'r, 'a>),
    Call(Call<'r, 'a>),
    GeneratorExp(GeneratorExp<'r, 'a>),
    ListComp(ListComp<'r, 'a>),
    SetComp(SetComp<'r, 'a>),
    DictComp(DictComp<'r, 'a>),
    List(List<'r, 'a>),
    Set(Set<'r, 'a>),
    Dict(Dict<'r, 'a>),
    Subscript(Subscript<'r, 'a>),
    StarredElement(StarredElement<'r, 'a>),
    IfExp(IfExp<'r, 'a>),
    Lambda(Lambda<'r, 'a>),
    Yield(Yield<'r, 'a>),
    Await(Await<'r, 'a>),
    SimpleString(SimpleString<'r, 'a>),
    ConcatenatedString(ConcatenatedString<'r, 'a>),
    FormattedString(FormattedString<'r, 'a>),
    NamedExpr(NamedExpr<'r, 'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Ellipsis<'r, 'a> {
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for Ellipsis<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("...");
        })
    }
}
impl<'r, 'a> Inflate<'a> for Ellipsis<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Integer<'r, 'a> {
    /// A string representation of the integer, such as ``"100000"`` or
    /// ``"100_000"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for Integer<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'r, 'a> Inflate<'a> for Integer<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Float<'r, 'a> {
    /// A string representation of the floating point number, such as ```"0.05"``,
    /// ``".050"``, or ``"5e-2"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for Float<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'r, 'a> Inflate<'a> for Float<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Imaginary<'r, 'a> {
    /// A string representation of the complex number, such as ``"2j"``
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for Imaginary<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'r, 'a> Inflate<'a> for Imaginary<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Comparison<'r, 'a> {
    pub left: Box<Expression<'r, 'a>>,
    pub comparisons: Vec<ComparisonTarget<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for Comparison<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            for comp in &self.comparisons {
                comp.codegen(state);
            }
        })
    }
}
impl<'r, 'a> Inflate<'a> for Comparison<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.comparisons = self.comparisons.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct UnaryOperation<'r, 'a> {
    pub operator: UnaryOp<'r, 'a>,
    pub expression: Box<Expression<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for UnaryOperation<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.operator.codegen(state);
            self.expression.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for UnaryOperation<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.expression = self.expression.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct BinaryOperation<'r, 'a> {
    pub left: Box<Expression<'r, 'a>>,
    pub operator: BinaryOp<'r, 'a>,
    pub right: Box<Expression<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for BinaryOperation<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.operator.codegen(state);
            self.right.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for BinaryOperation<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.right = self.right.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct BooleanOperation<'r, 'a> {
    pub left: Box<Expression<'r, 'a>>,
    pub operator: BooleanOp<'r, 'a>,
    pub right: Box<Expression<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for BooleanOperation<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.operator.codegen(state);
            self.right.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for BooleanOperation<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.right = self.right.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Call<'r, 'a> {
    pub func: Box<Expression<'r, 'a>>,
    pub args: Vec<Arg<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_after_func: ParenthesizableWhitespace<'a>,
    pub whitespace_before_args: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: TokenRef<'r, 'a>,
    pub(crate) rpar_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Call<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.func = self.func.inflate(config)?;
        self.whitespace_after_func = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_before_args = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_after.borrow_mut(),
        )?;
        self.args = self.args.inflate(config)?;

        if let Some(arg) = self.args.last_mut() {
            if arg.comma.is_none() {
                arg.whitespace_after_arg = parse_parenthesizable_whitespace(
                    config,
                    &mut (*self.rpar_tok).whitespace_before.borrow_mut(),
                )?;
            }
        }
        self.rpar = self.rpar.inflate(config)?;

        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Call<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.func.codegen(state);
            self.whitespace_after_func.codegen(state);
            state.add_token("(");
            self.whitespace_before_args.codegen(state);
            let arg_len = self.args.len();
            for (i, arg) in self.args.iter().enumerate() {
                arg.codegen(state, i + 1 < arg_len);
            }
            state.add_token(")");
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Attribute<'r, 'a> {
    pub value: Box<Expression<'r, 'a>>,
    pub attr: Name<'r, 'a>,
    pub dot: Dot<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Attribute<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.dot = self.dot.inflate(config)?;
        self.attr = self.attr.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Attribute<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.value.codegen(state);
            self.dot.codegen(state);
            self.attr.codegen(state);
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate, IntoPy)]
pub enum NameOrAttribute<'r, 'a> {
    N(Name<'r, 'a>),
    A(Attribute<'r, 'a>),
}

impl<'r, 'a> std::convert::From<NameOrAttribute<'r, 'a>> for Expression<'r, 'a> {
    fn from(x: NameOrAttribute<'r, 'a>) -> Self {
        match x {
            NameOrAttribute::N(n) => Self::Name(n),
            NameOrAttribute::A(a) => Self::Attribute(a),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct ComparisonTarget<'r, 'a> {
    pub operator: CompOp<'r, 'a>,
    pub comparator: Expression<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for ComparisonTarget<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.operator.codegen(state);
        self.comparator.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for ComparisonTarget<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.operator = self.operator.inflate(config)?;
        self.comparator = self.comparator.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct StarredElement<'r, 'a> {
    pub value: Box<Expression<'r, 'a>>,
    pub comma: Option<Comma<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> StarredElement<'r, 'a> {
    pub fn inflate_element(mut self, config: &Config<'a>, is_last: bool) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.whitespace_before_value = parse_parenthesizable_whitespace(
            config,
            &mut (*self.star_tok).whitespace_after.borrow_mut(),
        )?;
        self.value = self.value.inflate(config)?;
        self.comma = if is_last {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(self)
    }
}

impl<'r, 'a> Inflate<'a> for StarredElement<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        self.inflate_element(config, false)
    }
}

impl<'r, 'a> Codegen<'a> for StarredElement<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element<'r, 'a> {
    Simple {
        value: Expression<'r, 'a>,
        comma: Option<Comma<'r, 'a>>,
    },
    Starred(StarredElement<'r, 'a>),
}

// TODO: this could be a derive helper attribute to override the python class name
impl<'r, 'a> IntoPy<pyo3::PyObject> for Element<'r, 'a> {
    fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
        match self {
            Self::Starred(s) => s.into_py(py),
            Self::Simple { value, comma } => {
                let libcst = PyModule::import(py, "libcst").expect("libcst cannot be imported");
                let kwargs = [
                    Some(("value", value.into_py(py))),
                    comma.map(|x| ("comma", x.into_py(py))),
                ]
                .iter()
                .filter(|x| x.is_some())
                .map(|x| x.as_ref().unwrap())
                .collect::<Vec<_>>()
                .into_py_dict(py);
                libcst
                    .getattr("Element")
                    .expect("no Element found in libcst")
                    .call((), Some(kwargs))
                    .expect("conversion failed")
                    .into()
            }
        }
    }
}

impl<'r, 'a> Element<'r, 'a> {
    fn codegen(
        &self,
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

    pub fn inflate_element(self, config: &Config<'a>, is_last: bool) -> Result<Self> {
        Ok(match self {
            Self::Starred(s) => Self::Starred(s.inflate_element(config, is_last)?),
            Self::Simple { value, comma } => Self::Simple {
                value: value.inflate(config)?,
                comma: if is_last {
                    comma.map(|c| c.inflate_before(config)).transpose()?
                } else {
                    comma.inflate(config)?
                },
            },
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for Element<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        let comma = Some(comma);
        match self {
            Self::Simple { value, .. } => Self::Simple { comma, value },
            Self::Starred(s) => Self::Starred(StarredElement { comma, ..s }),
        }
    }
}
impl<'r, 'a> std::convert::From<Expression<'r, 'a>> for Element<'r, 'a> {
    fn from(e: Expression<'r, 'a>) -> Self {
        match e {
            Expression::StarredElement(e) => Element::Starred(e),
            value => Element::Simple { value, comma: None },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, ParenthesizedNode, IntoPy)]
pub struct Tuple<'r, 'a> {
    pub elements: Vec<Element<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Tuple<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Tuple<'r, 'a>> {
        self.lpar = self.lpar.inflate(config)?;
        let len = self.elements.len();
        self.elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        if !self.elements.is_empty() {
            // rpar only has whitespace if elements is non empty
            self.rpar = self.rpar.inflate(config)?;
        }
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Tuple<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct GeneratorExp<'r, 'a> {
    pub elt: Box<Expression<'r, 'a>>,
    pub for_in: Box<CompFor<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for GeneratorExp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.elt.codegen(state);
            self.for_in.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for GeneratorExp<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.elt = self.elt.inflate(config)?;
        self.for_in = self.for_in.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct ListComp<'r, 'a> {
    pub elt: Box<Expression<'r, 'a>>,
    pub for_in: Box<CompFor<'r, 'a>>,
    pub lbracket: LeftSquareBracket<'r, 'a>,
    pub rbracket: RightSquareBracket<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for ListComp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbracket.codegen(state);
            self.elt.codegen(state);
            self.for_in.codegen(state);
            self.rbracket.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for ListComp<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbracket = self.lbracket.inflate(config)?;
        self.elt = self.elt.inflate(config)?;
        self.for_in = self.for_in.inflate(config)?;
        self.rbracket = self.rbracket.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct LeftSquareBracket<'r, 'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for LeftSquareBracket<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("[");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for LeftSquareBracket<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct RightSquareBracket<'r, 'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for RightSquareBracket<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("]");
    }
}

impl<'r, 'a> Inflate<'a> for RightSquareBracket<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct SetComp<'r, 'a> {
    pub elt: Box<Expression<'r, 'a>>,
    pub for_in: Box<CompFor<'r, 'a>>,
    pub lbrace: LeftCurlyBrace<'r, 'a>,
    pub rbrace: RightCurlyBrace<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for SetComp<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbrace = self.lbrace.inflate(config)?;
        self.elt = self.elt.inflate(config)?;
        self.for_in = self.for_in.inflate(config)?;
        self.rbrace = self.rbrace.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for SetComp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            self.elt.codegen(state);
            self.for_in.codegen(state);
            self.rbrace.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct DictComp<'r, 'a> {
    pub key: Box<Expression<'r, 'a>>,
    pub value: Box<Expression<'r, 'a>>,
    pub for_in: Box<CompFor<'r, 'a>>,
    pub lbrace: LeftCurlyBrace<'r, 'a>,
    pub rbrace: RightCurlyBrace<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_before_colon: ParenthesizableWhitespace<'a>,
    pub whitespace_after_colon: ParenthesizableWhitespace<'a>,

    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for DictComp<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbrace = self.lbrace.inflate(config)?;
        self.key = self.key.inflate(config)?;
        self.whitespace_before_colon = parse_parenthesizable_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_colon = parse_parenthesizable_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_after.borrow_mut(),
        )?;
        self.value = self.value.inflate(config)?;
        self.for_in = self.for_in.inflate(config)?;
        self.rbrace = self.rbrace.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for DictComp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct LeftCurlyBrace<'r, 'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for LeftCurlyBrace<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for LeftCurlyBrace<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("{");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct RightCurlyBrace<'r, 'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for RightCurlyBrace<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for RightCurlyBrace<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("}");
    }
}

impl<'r, 'a> pyo3::conversion::IntoPy<pyo3::PyObject> for Box<CompFor<'r, 'a>> {
    fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
        (*self).into_py(py)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct CompFor<'r, 'a> {
    pub target: AssignTargetExpression<'r, 'a>,
    pub iter: Expression<'r, 'a>,
    pub ifs: Vec<CompIf<'r, 'a>>,
    pub inner_for_in: Option<Box<CompFor<'r, 'a>>>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_after_for: ParenthesizableWhitespace<'a>,
    pub whitespace_before_in: ParenthesizableWhitespace<'a>,
    pub whitespace_after_in: ParenthesizableWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'r, 'a>>,
    pub(crate) for_tok: TokenRef<'r, 'a>,
    pub(crate) in_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for CompFor<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

impl<'r, 'a> Inflate<'a> for CompFor<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.for_tok).whitespace_before.borrow_mut(),
        )?;
        if let (Some(asy_tok), Some(asy)) = (self.async_tok.as_mut(), self.asynchronous.as_mut()) {
            // If there is an async keyword, the start of the CompFor expression is
            // considered to be this keyword, so whitespace_before needs to adjust but
            // Asynchronous will own the whitespace before the for token.
            asy.whitespace_after = parse_parenthesizable_whitespace(
                config,
                &mut asy_tok.whitespace_before.borrow_mut(),
            )?;
            swap(&mut asy.whitespace_after, &mut self.whitespace_before);
        }
        self.whitespace_after_for = parse_parenthesizable_whitespace(
            config,
            &mut (*self.for_tok).whitespace_after.borrow_mut(),
        )?;
        self.target = self.target.inflate(config)?;
        self.whitespace_before_in = parse_parenthesizable_whitespace(
            config,
            &mut (*self.in_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_in = parse_parenthesizable_whitespace(
            config,
            &mut (*self.in_tok).whitespace_after.borrow_mut(),
        )?;
        self.iter = self.iter.inflate(config)?;
        self.ifs = self.ifs.inflate(config)?;
        self.inner_for_in = self.inner_for_in.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Asynchronous<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Asynchronous<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("async");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct CompIf<'r, 'a> {
    pub test: Expression<'r, 'a>,
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_before_test: ParenthesizableWhitespace<'a>,

    pub(crate) if_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for CompIf<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("if");
        self.whitespace_before_test.codegen(state);
        self.test.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for CompIf<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_before_test = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_after.borrow_mut(),
        )?;
        self.test = self.test.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct List<'r, 'a> {
    pub elements: Vec<Element<'r, 'a>>,
    pub lbracket: LeftSquareBracket<'r, 'a>,
    pub rbracket: RightSquareBracket<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for List<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbracket = self.lbracket.inflate(config)?;
        let len = self.elements.len();
        self.elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<_>>()?;
        if !self.elements.is_empty() {
            // lbracket owns all the whitespace if there are no elements
            self.rbracket = self.rbracket.inflate(config)?;
        }
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for List<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Set<'r, 'a> {
    pub elements: Vec<Element<'r, 'a>>,
    pub lbrace: LeftCurlyBrace<'r, 'a>,
    pub rbrace: RightCurlyBrace<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Set<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbrace = self.lbrace.inflate(config)?;
        let len = self.elements.len();
        self.elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<_>>()?;
        if !self.elements.is_empty() {
            self.rbrace = self.rbrace.inflate(config)?;
        }
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Set<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Dict<'r, 'a> {
    pub elements: Vec<DictElement<'r, 'a>>,
    pub lbrace: LeftCurlyBrace<'r, 'a>,
    pub rbrace: RightCurlyBrace<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Dict<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbrace = self.lbrace.inflate(config)?;
        let len = self.elements.len();
        self.elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<_>>()?;
        if !self.elements.is_empty() {
            self.rbrace = self.rbrace.inflate(config)?;
        }
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Dict<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DictElement<'r, 'a> {
    Simple {
        key: Expression<'r, 'a>,
        value: Expression<'r, 'a>,
        comma: Option<Comma<'r, 'a>>,
        whitespace_before_colon: ParenthesizableWhitespace<'a>,
        whitespace_after_colon: ParenthesizableWhitespace<'a>,
        colon_tok: TokenRef<'r, 'a>,
    },
    Starred(StarredDictElement<'r, 'a>),
}

// TODO: this could be a derive helper attribute to override the python class name
impl<'r, 'a> IntoPy<pyo3::PyObject> for DictElement<'r, 'a> {
    fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
        match self {
            Self::Starred(s) => s.into_py(py),
            Self::Simple {
                key,
                value,
                comma,
                whitespace_after_colon,
                whitespace_before_colon,
                ..
            } => {
                let libcst = PyModule::import(py, "libcst").expect("libcst cannot be imported");
                let kwargs = [
                    Some(("key", key.into_py(py))),
                    Some(("value", value.into_py(py))),
                    Some((
                        "whitespace_before_colon",
                        whitespace_before_colon.into_py(py),
                    )),
                    Some(("whitespace_after_colon", whitespace_after_colon.into_py(py))),
                    comma.map(|x| ("comma", x.into_py(py))),
                ]
                .iter()
                .filter(|x| x.is_some())
                .map(|x| x.as_ref().unwrap())
                .collect::<Vec<_>>()
                .into_py_dict(py);
                libcst
                    .getattr("DictElement")
                    .expect("no Element found in libcst")
                    .call((), Some(kwargs))
                    .expect("conversion failed")
                    .into()
            }
        }
    }
}

impl<'r, 'a> DictElement<'r, 'a> {
    pub fn inflate_element(self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        Ok(match self {
            Self::Starred(s) => Self::Starred(s.inflate_element(config, last_element)?),
            Self::Simple {
                key,
                value,
                comma,
                colon_tok,
                ..
            } => {
                let whitespace_before_colon = parse_parenthesizable_whitespace(
                    config,
                    &mut colon_tok.whitespace_before.borrow_mut(),
                )?;
                let whitespace_after_colon = parse_parenthesizable_whitespace(
                    config,
                    &mut colon_tok.whitespace_after.borrow_mut(),
                )?;
                Self::Simple {
                    key: key.inflate(config)?,
                    whitespace_before_colon,
                    whitespace_after_colon,
                    value: value.inflate(config)?,
                    comma: if last_element {
                        comma.map(|c| c.inflate_before(config)).transpose()
                    } else {
                        comma.inflate(config)
                    }?,
                    colon_tok,
                }
            }
        })
    }
}

impl<'r, 'a> DictElement<'r, 'a> {
    fn codegen(
        &self,
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
                ..
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

impl<'r, 'a> WithComma<'r, 'a> for DictElement<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        let comma = Some(comma);
        match self {
            Self::Starred(s) => Self::Starred(StarredDictElement { comma, ..s }),
            Self::Simple {
                key,
                value,
                whitespace_before_colon,
                whitespace_after_colon,
                colon_tok,
                ..
            } => Self::Simple {
                comma,
                key,
                value,
                whitespace_after_colon,
                whitespace_before_colon,
                colon_tok,
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct StarredDictElement<'r, 'a> {
    pub value: Expression<'r, 'a>,
    pub comma: Option<Comma<'r, 'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> StarredDictElement<'r, 'a> {
    fn inflate_element(mut self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        self.whitespace_before_value = parse_parenthesizable_whitespace(
            config,
            &mut (*self.star_tok).whitespace_after.borrow_mut(),
        )?;
        self.value = self.value.inflate(config)?;
        self.comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for StarredDictElement<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("**");
        self.whitespace_before_value.codegen(state);
        self.value.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate, IntoPy)]
pub enum BaseSlice<'r, 'a> {
    Index(Index<'r, 'a>),
    Slice(Slice<'r, 'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Index<'r, 'a> {
    pub value: Expression<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Index<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Index<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Slice<'r, 'a> {
    #[no_py_default]
    pub lower: Option<Expression<'r, 'a>>,
    #[no_py_default]
    pub upper: Option<Expression<'r, 'a>>,
    pub step: Option<Expression<'r, 'a>>,
    pub first_colon: Colon<'r, 'a>,
    pub second_colon: Option<Colon<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for Slice<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lower = self.lower.inflate(config)?;
        self.first_colon = self.first_colon.inflate(config)?;
        self.upper = self.upper.inflate(config)?;
        self.second_colon = self.second_colon.inflate(config)?;
        self.step = self.step.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Slice<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct SubscriptElement<'r, 'a> {
    pub slice: BaseSlice<'r, 'a>,
    pub comma: Option<Comma<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for SubscriptElement<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.slice = self.slice.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for SubscriptElement<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.slice.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Subscript<'r, 'a> {
    pub value: Box<Expression<'r, 'a>>,
    pub slice: Vec<SubscriptElement<'r, 'a>>,
    pub lbracket: LeftSquareBracket<'r, 'a>,
    pub rbracket: RightSquareBracket<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_after_value: ParenthesizableWhitespace<'a>,

    pub(crate) lbracket_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Subscript<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.whitespace_after_value = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lbracket_tok).whitespace_before.borrow_mut(),
        )?;
        self.lbracket = self.lbracket.inflate(config)?;
        self.slice = self.slice.inflate(config)?;
        self.rbracket = self.rbracket.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Subscript<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct IfExp<'r, 'a> {
    pub test: Box<Expression<'r, 'a>>,
    pub body: Box<Expression<'r, 'a>>,
    pub orelse: Box<Expression<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_before_if: ParenthesizableWhitespace<'a>,
    pub whitespace_after_if: ParenthesizableWhitespace<'a>,
    pub whitespace_before_else: ParenthesizableWhitespace<'a>,
    pub whitespace_after_else: ParenthesizableWhitespace<'a>,

    pub(crate) if_tok: TokenRef<'r, 'a>,
    pub(crate) else_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for IfExp<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.body = self.body.inflate(config)?;
        self.whitespace_before_if = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_if = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_after.borrow_mut(),
        )?;
        self.test = self.test.inflate(config)?;
        self.whitespace_before_else = parse_parenthesizable_whitespace(
            config,
            &mut (*self.else_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_else = parse_parenthesizable_whitespace(
            config,
            &mut (*self.else_tok).whitespace_after.borrow_mut(),
        )?;
        self.orelse = self.orelse.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for IfExp<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.body.codegen(state);
            self.whitespace_before_if.codegen(state);
            state.add_token("if");
            self.whitespace_after_if.codegen(state);
            self.test.codegen(state);
            self.whitespace_before_else.codegen(state);
            state.add_token("else");
            self.whitespace_after_else.codegen(state);
            self.orelse.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Lambda<'r, 'a> {
    pub params: Box<Parameters<'r, 'a>>,
    pub body: Box<Expression<'r, 'a>>,
    pub colon: Colon<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_after_lambda: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) lambda_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Lambda<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        if !self.params.is_empty() {
            self.whitespace_after_lambda = Some(parse_parenthesizable_whitespace(
                config,
                &mut (*self.lambda_tok).whitespace_after.borrow_mut(),
            )?);
        }
        self.params = self.params.inflate(config)?;
        adjust_parameters_trailing_whitespace(config, &mut self.params, &self.colon.tok)?;
        self.colon = self.colon.inflate(config)?;
        self.body = self.body.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Lambda<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("lambda");
            if let Some(ws) = &self.whitespace_after_lambda {
                ws.codegen(state);
            } else if !self.params.is_empty() {
                // there's one or more params, add a space
                state.add_token(" ")
            }
            self.params.codegen(state);
            self.colon.codegen(state);
            self.body.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct From<'r, 'a> {
    pub item: Expression<'r, 'a>,
    pub whitespace_before_from: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_from: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> From<'r, 'a> {
    pub fn codegen(&self, state: &mut CodegenState<'a>, default_space: &'a str) {
        if let Some(ws) = &self.whitespace_before_from {
            ws.codegen(state);
        } else {
            state.add_token(default_space);
        }
        state.add_token("from");
        self.whitespace_after_from.codegen(state);
        self.item.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for From<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before_from = Some(parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?);
        self.whitespace_after_from = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        self.item = self.item.inflate(config)?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum YieldValue<'r, 'a> {
    Expression(Expression<'r, 'a>),
    From(From<'r, 'a>),
}

impl<'r, 'a> Inflate<'a> for YieldValue<'r, 'a> {
    type Inflated = Self;
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
        Ok(match self {
            Self::Expression(e) => Self::Expression(e.inflate(config)?),
            Self::From(e) => {
                let mut e = e.inflate(config)?;
                e.whitespace_before_from = None;
                Self::From(e)
            }
        })
    }
}

impl<'r, 'a> YieldValue<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_space: &'a str) {
        match self {
            Self::Expression(e) => e.codegen(state),
            Self::From(f) => f.codegen(state, default_space),
        }
    }
}

impl<'r, 'a> pyo3::conversion::IntoPy<pyo3::PyObject> for Box<YieldValue<'r, 'a>> {
    fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
        (*self).into_py(py)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Yield<'r, 'a> {
    pub value: Option<Box<YieldValue<'r, 'a>>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_after_yield: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) yield_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Yield<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        if self.value.is_some() {
            self.whitespace_after_yield = Some(parse_parenthesizable_whitespace(
                config,
                &mut (*self.yield_tok).whitespace_after.borrow_mut(),
            )?);
        }
        self.value = self.value.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Yield<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("yield");
            if let Some(ws) = &self.whitespace_after_yield {
                ws.codegen(state);
            } else if self.value.is_some() {
                state.add_token(" ");
            }

            if let Some(val) = &self.value {
                val.codegen(state, "")
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct Await<'r, 'a> {
    pub expression: Box<Expression<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_after_await: ParenthesizableWhitespace<'a>,

    pub(crate) await_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Await<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.whitespace_after_await = parse_parenthesizable_whitespace(
            config,
            &mut (*self.await_tok).whitespace_after.borrow_mut(),
        )?;
        self.expression = self.expression.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Await<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("await");
            self.whitespace_after_await.codegen(state);
            self.expression.codegen(state);
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate, IntoPy)]
pub enum String<'r, 'a> {
    Simple(SimpleString<'r, 'a>),
    Concatenated(ConcatenatedString<'r, 'a>),
    Formatted(FormattedString<'r, 'a>),
}

impl<'r, 'a> std::convert::From<String<'r, 'a>> for Expression<'r, 'a> {
    fn from(s: String<'r, 'a>) -> Self {
        match s {
            String::Simple(s) => Self::SimpleString(s),
            String::Concatenated(s) => Self::ConcatenatedString(s),
            String::Formatted(s) => Self::FormattedString(s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct ConcatenatedString<'r, 'a> {
    pub left: Box<String<'r, 'a>>,
    pub right: Box<String<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
    pub whitespace_between: ParenthesizableWhitespace<'a>,

    // we capture the next token after each string piece so Inflate can extract the
    // whitespace between individual pieces
    pub(crate) right_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for ConcatenatedString<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.whitespace_between = parse_parenthesizable_whitespace(
            config,
            &mut (*self.right_tok).whitespace_before.borrow_mut(),
        )?;
        self.right = self.right.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for ConcatenatedString<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.whitespace_between.codegen(state);
            self.right.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, ParenthesizedNode, IntoPy)]
pub struct SimpleString<'r, 'a> {
    /// The texual representation of the string, including quotes, prefix
    /// characters, and any escape characters present in the original source code,
    /// such as ``r"my string\n"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for SimpleString<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for SimpleString<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| state.add_token(self.value))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct FormattedStringText<'a> {
    pub value: &'a str,
}

impl<'a> Inflate<'a> for FormattedStringText<'a> {
    type Inflated = Self;
    fn inflate(self, _config: &Config<'a>) -> Result<Self> {
        Ok(self)
    }
}

impl<'a> Codegen<'a> for FormattedStringText<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token(self.value);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct FormattedStringExpression<'r, 'a> {
    pub expression: Expression<'r, 'a>,
    pub conversion: Option<&'a str>,
    pub format_spec: Option<Vec<FormattedStringContent<'r, 'a>>>,
    pub whitespace_before_expression: ParenthesizableWhitespace<'a>,
    pub whitespace_after_expression: ParenthesizableWhitespace<'a>,
    pub equal: Option<AssignEqual<'r, 'a>>,

    pub(crate) lbrace_tok: TokenRef<'r, 'a>,
    // This is None if there's an equal sign, otherwise it's the first token of
    // (conversion, format spec, right brace) in that order
    pub(crate) after_expr_tok: Option<TokenRef<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for FormattedStringExpression<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before_expression = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lbrace_tok).whitespace_after.borrow_mut(),
        )?;
        self.expression = self.expression.inflate(config)?;
        self.equal = self.equal.inflate(config)?;
        if let Some(after_expr_tok) = self.after_expr_tok.as_mut() {
            self.whitespace_after_expression = parse_parenthesizable_whitespace(
                config,
                &mut after_expr_tok.whitespace_before.borrow_mut(),
            )?;
        }
        self.format_spec = self.format_spec.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for FormattedStringExpression<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("{");
        self.whitespace_before_expression.codegen(state);
        self.expression.codegen(state);
        if let Some(eq) = &self.equal {
            eq.codegen(state);
        }
        self.whitespace_after_expression.codegen(state);
        if let Some(conv) = &self.conversion {
            state.add_token("!");
            state.add_token(conv);
        }
        if let Some(specs) = &self.format_spec {
            state.add_token(":");
            for spec in specs {
                spec.codegen(state);
            }
        }
        state.add_token("}");
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate, IntoPy)]
pub enum FormattedStringContent<'a> {
    Text(FormattedStringText<'a>),
    Expression(FormattedStringExpression<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct FormattedString<'r, 'a> {
    pub parts: Vec<FormattedStringContent<'r, 'a>>,
    pub start: &'a str,
    pub end: &'a str,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for FormattedString<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.parts = self.parts.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for FormattedString<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.start);
            for part in &self.parts {
                part.codegen(state);
            }
            state.add_token(self.end);
        })
    }
}

#[cst_node]
#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode, IntoPy)]
pub struct NamedExpr<'a> {
    pub target: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,

    pub whitespace_before_walrus: ParenthesizableWhitespace<'a>,
    pub whitespace_after_walrus: ParenthesizableWhitespace<'a>,

    pub(crate) walrus_tok: TokenRef,
}

impl<'a> Codegen<'a> for NamedExpr<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.target.codegen(state);
            self.whitespace_before_walrus.codegen(state);
            state.add_token(":=");
            self.whitespace_after_walrus.codegen(state);
            self.value.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedNamedExpr<'r, 'a> {
    type Inflated = NamedExpr<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        self.lpar = self.lpar.inflate(config)?;
        self.target = self.target.inflate(config)?;
        self.whitespace_before_walrus = parse_parenthesizable_whitespace(
            config,
            &mut self.walrus_tok.whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_walrus = parse_parenthesizable_whitespace(
            config,
            &mut self.walrus_tok.whitespace_after.borrow_mut(),
        )?;
        self.value = self.value.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}
