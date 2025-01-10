// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::mem::swap;

use crate::{
    inflate_helpers::adjust_parameters_trailing_whitespace,
    nodes::{
        op::*,
        statement::*,
        traits::{Inflate, ParenthesizedDeflatedNode, ParenthesizedNode, Result, WithComma},
        whitespace::ParenthesizableWhitespace,
        Annotation, AssignEqual, AssignTargetExpression, BinaryOp, BooleanOp, Codegen,
        CodegenState, Colon, Comma, CompOp, Dot, UnaryOp,
    },
    tokenizer::{
        whitespace_parser::{parse_parenthesizable_whitespace, Config},
        Token,
    },
};
#[cfg(feature = "py")]
use libcst_derive::TryIntoPy;
use libcst_derive::{cst_node, Codegen, Inflate, ParenthesizedDeflatedNode, ParenthesizedNode};

type TokenRef<'r, 'a> = &'r Token<'a>;

#[cst_node(Default)]
pub struct Parameters<'a> {
    pub params: Vec<Param<'a>>,
    pub star_arg: Option<StarArg<'a>>,
    pub kwonly_params: Vec<Param<'a>>,
    pub star_kwarg: Option<Param<'a>>,
    pub posonly_params: Vec<Param<'a>>,
    pub posonly_ind: Option<ParamSlash<'a>>,
}

impl<'a> Parameters<'a> {
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
            && self.star_arg.is_none()
            && self.kwonly_params.is_empty()
            && self.star_kwarg.is_none()
            && self.posonly_params.is_empty()
            && self.posonly_ind.is_none()
    }
}

impl<'r, 'a> DeflatedParameters<'r, 'a> {
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
            && self.star_arg.is_none()
            && self.kwonly_params.is_empty()
            && self.star_kwarg.is_none()
            && self.posonly_params.is_empty()
            && self.posonly_ind.is_none()
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedParameters<'r, 'a> {
    type Inflated = Parameters<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let posonly_params = self.posonly_params.inflate(config)?;
        let posonly_ind = self.posonly_ind.inflate(config)?;
        let params = self.params.inflate(config)?;
        let star_arg = self.star_arg.inflate(config)?;
        let kwonly_params = self.kwonly_params.inflate(config)?;
        let star_kwarg = self.star_kwarg.inflate(config)?;
        Ok(Self::Inflated {
            params,
            star_arg,
            kwonly_params,
            star_kwarg,
            posonly_params,
            posonly_ind,
        })
    }
}

#[cst_node(Inflate)]
pub enum StarArg<'a> {
    Star(Box<ParamStar<'a>>),
    Param(Box<Param<'a>>),
}

impl<'a> Codegen<'a> for Parameters<'a> {
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

#[cst_node]
pub struct ParamSlash<'a> {
    pub comma: Option<Comma<'a>>,
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> ParamSlash<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        state.add_token("/");
        self.whitespace_after.codegen(state);
        match (&self.comma, default_comma) {
            (Some(comma), _) => comma.codegen(state),
            (None, true) => state.add_token(", "),
            _ => {}
        }
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedParamSlash<'r, 'a> {
    type Inflated = ParamSlash<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after.borrow_mut())?;
        let comma = self.comma.inflate(config)?;
        Ok(Self::Inflated {
            comma,
            whitespace_after,
        })
    }
}

#[cst_node]
pub struct ParamStar<'a> {
    pub comma: Comma<'a>,
}

impl<'a> Codegen<'a> for ParamStar<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
        self.comma.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedParamStar<'r, 'a> {
    type Inflated = ParamStar<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let comma = self.comma.inflate(config)?;
        Ok(Self::Inflated { comma })
    }
}

#[cst_node(ParenthesizedNode, Default)]
pub struct Name<'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedName<'r, 'a> {
    type Inflated = Name<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value: self.value,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for Name<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        });
    }
}

#[cst_node]
pub struct Param<'a> {
    pub name: Name<'a>,
    pub annotation: Option<Annotation<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub default: Option<Expression<'a>>,

    pub comma: Option<Comma<'a>>,

    pub star: Option<&'a str>,

    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_param: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Option<TokenRef<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedParam<'r, 'a> {
    type Inflated = Param<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let name = self.name.inflate(config)?;
        let annotation = self.annotation.inflate(config)?;
        let equal = self.equal.inflate(config)?;
        let default = self.default.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        let whitespace_after_star = if let Some(star_tok) = self.star_tok.as_mut() {
            parse_parenthesizable_whitespace(config, &mut star_tok.whitespace_after.borrow_mut())?
        } else {
            Default::default()
        };
        let whitespace_after_param = Default::default(); // TODO
        Ok(Self::Inflated {
            name,
            annotation,
            equal,
            default,
            comma,
            star: self.star,
            whitespace_after_star,
            whitespace_after_param,
        })
    }
}

impl<'r, 'a> Default for DeflatedParam<'r, 'a> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            annotation: None,
            equal: None,
            default: None,
            comma: None,
            star: Some(""), // Note: this preserves a quirk of the pure python parser
            star_tok: None,
        }
    }
}

impl<'a> Param<'a> {
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

#[cst_node]
pub struct Arg<'a> {
    pub value: Expression<'a>,
    pub keyword: Option<Name<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub comma: Option<Comma<'a>>,
    pub star: &'a str,
    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_arg: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Option<TokenRef<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedArg<'r, 'a> {
    type Inflated = Arg<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_star = if let Some(star_tok) = self.star_tok.as_mut() {
            parse_parenthesizable_whitespace(config, &mut star_tok.whitespace_after.borrow_mut())?
        } else {
            Default::default()
        };
        let keyword = self.keyword.inflate(config)?;
        let equal = self.equal.inflate(config)?;
        let value = self.value.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        // whitespace_after_arg is handled in Call
        let whitespace_after_arg = Default::default();
        Ok(Self::Inflated {
            value,
            keyword,
            equal,
            comma,
            star: self.star,
            whitespace_after_star,
            whitespace_after_arg,
        })
    }
}

impl<'a> Arg<'a> {
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

impl<'r, 'a> WithComma<'r, 'a> for DeflatedArg<'r, 'a> {
    fn with_comma(self, c: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(c),
            ..self
        }
    }
}

#[cst_node]
#[derive(Default)]
pub struct LeftParen<'a> {
    /// Any space that appears directly after this left parenthesis.
    pub whitespace_after: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for LeftParen<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("(");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedLeftParen<'r, 'a> {
    type Inflated = LeftParen<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated { whitespace_after })
    }
}

#[cst_node]
#[derive(Default)]
pub struct RightParen<'a> {
    /// Any space that appears directly before this right parenthesis.
    pub whitespace_before: ParenthesizableWhitespace<'a>,

    pub(crate) rpar_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for RightParen<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(")");
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedRightParen<'r, 'a> {
    type Inflated = RightParen<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.rpar_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(Self::Inflated { whitespace_before })
    }
}

#[cst_node(ParenthesizedNode, Codegen, Inflate)]
pub enum Expression<'a> {
    Name(Box<Name<'a>>),
    Ellipsis(Box<Ellipsis<'a>>),
    Integer(Box<Integer<'a>>),
    Float(Box<Float<'a>>),
    Imaginary(Box<Imaginary<'a>>),
    Comparison(Box<Comparison<'a>>),
    UnaryOperation(Box<UnaryOperation<'a>>),
    BinaryOperation(Box<BinaryOperation<'a>>),
    BooleanOperation(Box<BooleanOperation<'a>>),
    Attribute(Box<Attribute<'a>>),
    Tuple(Box<Tuple<'a>>),
    Call(Box<Call<'a>>),
    GeneratorExp(Box<GeneratorExp<'a>>),
    ListComp(Box<ListComp<'a>>),
    SetComp(Box<SetComp<'a>>),
    DictComp(Box<DictComp<'a>>),
    List(Box<List<'a>>),
    Set(Box<Set<'a>>),
    Dict(Box<Dict<'a>>),
    Subscript(Box<Subscript<'a>>),
    StarredElement(Box<StarredElement<'a>>),
    IfExp(Box<IfExp<'a>>),
    Lambda(Box<Lambda<'a>>),
    Yield(Box<Yield<'a>>),
    Await(Box<Await<'a>>),
    SimpleString(Box<SimpleString<'a>>),
    ConcatenatedString(Box<ConcatenatedString<'a>>),
    FormattedString(Box<FormattedString<'a>>),
    NamedExpr(Box<NamedExpr<'a>>),
}

#[cst_node(ParenthesizedNode)]
pub struct Ellipsis<'a> {
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Ellipsis<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("...");
        })
    }
}
impl<'r, 'a> Inflate<'a> for DeflatedEllipsis<'r, 'a> {
    type Inflated = Ellipsis<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated { lpar, rpar })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Integer<'a> {
    /// A string representation of the integer, such as ``"100000"`` or
    /// ``"100_000"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Integer<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedInteger<'r, 'a> {
    type Inflated = Integer<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value: self.value,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Float<'a> {
    /// A string representation of the floating point number, such as ```"0.05"``,
    /// ``".050"``, or ``"5e-2"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Float<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedFloat<'r, 'a> {
    type Inflated = Float<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value: self.value,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Imaginary<'a> {
    /// A string representation of the complex number, such as ``"2j"``
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Imaginary<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedImaginary<'r, 'a> {
    type Inflated = Imaginary<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value: self.value,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Comparison<'a> {
    pub left: Box<Expression<'a>>,
    pub comparisons: Vec<ComparisonTarget<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Comparison<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            for comp in &self.comparisons {
                comp.codegen(state);
            }
        })
    }
}
impl<'r, 'a> Inflate<'a> for DeflatedComparison<'r, 'a> {
    type Inflated = Comparison<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let left = self.left.inflate(config)?;
        let comparisons = self.comparisons.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            left,
            comparisons,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct UnaryOperation<'a> {
    pub operator: UnaryOp<'a>,
    pub expression: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for UnaryOperation<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.operator.codegen(state);
            self.expression.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedUnaryOperation<'r, 'a> {
    type Inflated = UnaryOperation<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let operator = self.operator.inflate(config)?;
        let expression = self.expression.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            operator,
            expression,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct BinaryOperation<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: BinaryOp<'a>,
    pub right: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for BinaryOperation<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.operator.codegen(state);
            self.right.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedBinaryOperation<'r, 'a> {
    type Inflated = BinaryOperation<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let left = self.left.inflate(config)?;
        let operator = self.operator.inflate(config)?;
        let right = self.right.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            left,
            operator,
            right,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct BooleanOperation<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: BooleanOp<'a>,
    pub right: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for BooleanOperation<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.operator.codegen(state);
            self.right.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedBooleanOperation<'r, 'a> {
    type Inflated = BooleanOperation<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let left = self.left.inflate(config)?;
        let operator = self.operator.inflate(config)?;
        let right = self.right.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            left,
            operator,
            right,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Call<'a> {
    pub func: Box<Expression<'a>>,
    pub args: Vec<Arg<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_func: ParenthesizableWhitespace<'a>,
    pub whitespace_before_args: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: TokenRef<'a>,
    pub(crate) rpar_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedCall<'r, 'a> {
    type Inflated = Call<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let func = self.func.inflate(config)?;
        let whitespace_after_func = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_before_args = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_after.borrow_mut(),
        )?;
        let mut args = self.args.inflate(config)?;

        if let Some(arg) = args.last_mut() {
            if arg.comma.is_none() {
                arg.whitespace_after_arg = parse_parenthesizable_whitespace(
                    config,
                    &mut (*self.rpar_tok).whitespace_before.borrow_mut(),
                )?;
            }
        }
        let rpar = self.rpar.inflate(config)?;

        Ok(Self::Inflated {
            func,
            args,
            lpar,
            rpar,
            whitespace_after_func,
            whitespace_before_args,
        })
    }
}

impl<'a> Codegen<'a> for Call<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct Attribute<'a> {
    pub value: Box<Expression<'a>>,
    pub attr: Name<'a>,
    pub dot: Dot<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedAttribute<'r, 'a> {
    type Inflated = Attribute<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let value = self.value.inflate(config)?;
        let dot = self.dot.inflate(config)?;
        let attr = self.attr.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value,
            attr,
            dot,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for Attribute<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.value.codegen(state);
            self.dot.codegen(state);
            self.attr.codegen(state);
        })
    }
}

#[cst_node(Codegen, Inflate)]
pub enum NameOrAttribute<'a> {
    N(Box<Name<'a>>),
    A(Box<Attribute<'a>>),
}

impl<'r, 'a> std::convert::From<DeflatedNameOrAttribute<'r, 'a>> for DeflatedExpression<'r, 'a> {
    fn from(x: DeflatedNameOrAttribute<'r, 'a>) -> Self {
        match x {
            DeflatedNameOrAttribute::N(n) => Self::Name(n),
            DeflatedNameOrAttribute::A(a) => Self::Attribute(a),
        }
    }
}

#[cst_node]
pub struct ComparisonTarget<'a> {
    pub operator: CompOp<'a>,
    pub comparator: Expression<'a>,
}

impl<'a> Codegen<'a> for ComparisonTarget<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.operator.codegen(state);
        self.comparator.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedComparisonTarget<'r, 'a> {
    type Inflated = ComparisonTarget<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let operator = self.operator.inflate(config)?;
        let comparator = self.comparator.inflate(config)?;
        Ok(Self::Inflated {
            operator,
            comparator,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct StarredElement<'a> {
    pub value: Box<Expression<'a>>,
    pub comma: Option<Comma<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'r, 'a> DeflatedStarredElement<'r, 'a> {
    pub fn inflate_element(self, config: &Config<'a>, is_last: bool) -> Result<StarredElement<'a>> {
        let lpar = self.lpar.inflate(config)?;
        let whitespace_before_value = parse_parenthesizable_whitespace(
            config,
            &mut (*self.star_tok).whitespace_after.borrow_mut(),
        )?;
        let value = self.value.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        let comma = if is_last {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(StarredElement {
            value,
            comma,
            lpar,
            rpar,
            whitespace_before_value,
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedStarredElement<'r, 'a> {
    type Inflated = StarredElement<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        self.inflate_element(config, false)
    }
}

impl<'a> Codegen<'a> for StarredElement<'a> {
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
#[cst_node(NoIntoPy)]
pub enum Element<'a> {
    Simple {
        value: Expression<'a>,
        comma: Option<Comma<'a>>,
    },
    Starred(Box<StarredElement<'a>>),
}

impl<'a> Element<'a> {
    pub fn codegen(
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
}
impl<'r, 'a> DeflatedElement<'r, 'a> {
    pub fn inflate_element(self, config: &Config<'a>, is_last: bool) -> Result<Element<'a>> {
        Ok(match self {
            Self::Starred(s) => Element::Starred(Box::new(s.inflate_element(config, is_last)?)),
            Self::Simple { value, comma } => Element::Simple {
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

impl<'r, 'a> WithComma<'r, 'a> for DeflatedElement<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        let comma = Some(comma);
        match self {
            Self::Simple { value, .. } => Self::Simple { comma, value },
            Self::Starred(mut s) => {
                s.comma = comma;
                Self::Starred(s)
            }
        }
    }
}
impl<'r, 'a> std::convert::From<DeflatedExpression<'r, 'a>> for DeflatedElement<'r, 'a> {
    fn from(e: DeflatedExpression<'r, 'a>) -> Self {
        match e {
            DeflatedExpression::StarredElement(e) => Self::Starred(e),
            value => Self::Simple { value, comma: None },
        }
    }
}

#[cst_node(ParenthesizedNode, Default)]
pub struct Tuple<'a> {
    pub elements: Vec<Element<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedTuple<'r, 'a> {
    type Inflated = Tuple<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let len = self.elements.len();
        let elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elements,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for Tuple<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct GeneratorExp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for GeneratorExp<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.elt.codegen(state);
            self.for_in.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedGeneratorExp<'r, 'a> {
    type Inflated = GeneratorExp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let elt = self.elt.inflate(config)?;
        let for_in = self.for_in.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elt,
            for_in,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct ListComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for ListComp<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbracket.codegen(state);
            self.elt.codegen(state);
            self.for_in.codegen(state);
            self.rbracket.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedListComp<'r, 'a> {
    type Inflated = ListComp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbracket = self.lbracket.inflate(config)?;
        let elt = self.elt.inflate(config)?;
        let for_in = self.for_in.inflate(config)?;
        let rbracket = self.rbracket.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elt,
            for_in,
            lbracket,
            rbracket,
            lpar,
            rpar,
        })
    }
}

#[cst_node]
#[derive(Default)]
pub struct LeftSquareBracket<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for LeftSquareBracket<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("[");
        self.whitespace_after.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedLeftSquareBracket<'r, 'a> {
    type Inflated = LeftSquareBracket<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated { whitespace_after })
    }
}

#[cst_node]
#[derive(Default)]
pub struct RightSquareBracket<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for RightSquareBracket<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("]");
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedRightSquareBracket<'r, 'a> {
    type Inflated = RightSquareBracket<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(Self::Inflated { whitespace_before })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct SetComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedSetComp<'r, 'a> {
    type Inflated = SetComp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbrace = self.lbrace.inflate(config)?;
        let elt = self.elt.inflate(config)?;
        let for_in = self.for_in.inflate(config)?;
        let rbrace = self.rbrace.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elt,
            for_in,
            lbrace,
            rbrace,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for SetComp<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            self.elt.codegen(state);
            self.for_in.codegen(state);
            self.rbrace.codegen(state);
        })
    }
}

#[cst_node(ParenthesizedNode)]
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

    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedDictComp<'r, 'a> {
    type Inflated = DictComp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbrace = self.lbrace.inflate(config)?;
        let key = self.key.inflate(config)?;
        let whitespace_before_colon = parse_parenthesizable_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_colon = parse_parenthesizable_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_after.borrow_mut(),
        )?;
        let value = self.value.inflate(config)?;
        let for_in = self.for_in.inflate(config)?;
        let rbrace = self.rbrace.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            key,
            value,
            for_in,
            lbrace,
            rbrace,
            lpar,
            rpar,
            whitespace_before_colon,
            whitespace_after_colon,
        })
    }
}

impl<'a> Codegen<'a> for DictComp<'a> {
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

#[cst_node]
pub struct LeftCurlyBrace<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Default for LeftCurlyBrace<'a> {
    fn default() -> Self {
        Self {
            whitespace_after: Default::default(),
        }
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedLeftCurlyBrace<'r, 'a> {
    type Inflated = LeftCurlyBrace<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(Self::Inflated { whitespace_after })
    }
}

impl<'a> Codegen<'a> for LeftCurlyBrace<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("{");
        self.whitespace_after.codegen(state);
    }
}

#[cst_node]
pub struct RightCurlyBrace<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Default for RightCurlyBrace<'a> {
    fn default() -> Self {
        Self {
            whitespace_before: Default::default(),
        }
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedRightCurlyBrace<'r, 'a> {
    type Inflated = RightCurlyBrace<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(Self::Inflated { whitespace_before })
    }
}

impl<'a> Codegen<'a> for RightCurlyBrace<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("}");
    }
}

#[cst_node]
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

    pub(crate) async_tok: Option<TokenRef<'a>>,
    pub(crate) for_tok: TokenRef<'a>,
    pub(crate) in_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for CompFor<'a> {
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

impl<'r, 'a> Inflate<'a> for DeflatedCompFor<'r, 'a> {
    type Inflated = CompFor<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let mut whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.for_tok).whitespace_before.borrow_mut(),
        )?;
        let asynchronous = if let Some(asy_tok) = self.async_tok.as_mut() {
            // If there is an async keyword, the start of the CompFor expression is
            // considered to be this keyword, so whitespace_before needs to adjust but
            // Asynchronous will own the whitespace before the for token.
            let mut asy_whitespace_after = parse_parenthesizable_whitespace(
                config,
                &mut asy_tok.whitespace_before.borrow_mut(),
            )?;
            swap(&mut asy_whitespace_after, &mut whitespace_before);
            Some(Asynchronous {
                whitespace_after: asy_whitespace_after,
            })
        } else {
            None
        };
        let whitespace_after_for = parse_parenthesizable_whitespace(
            config,
            &mut (*self.for_tok).whitespace_after.borrow_mut(),
        )?;
        let target = self.target.inflate(config)?;
        let whitespace_before_in = parse_parenthesizable_whitespace(
            config,
            &mut (*self.in_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_in = parse_parenthesizable_whitespace(
            config,
            &mut (*self.in_tok).whitespace_after.borrow_mut(),
        )?;
        let iter = self.iter.inflate(config)?;
        let ifs = self.ifs.inflate(config)?;
        let inner_for_in = self.inner_for_in.inflate(config)?;
        Ok(Self::Inflated {
            target,
            iter,
            ifs,
            inner_for_in,
            asynchronous,
            whitespace_before,
            whitespace_after_for,
            whitespace_before_in,
            whitespace_after_in,
        })
    }
}

#[cst_node]
pub struct Asynchronous<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Asynchronous<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("async");
        self.whitespace_after.codegen(state);
    }
}

pub(crate) fn make_async<'r, 'a>() -> DeflatedAsynchronous<'r, 'a> {
    DeflatedAsynchronous {
        _phantom: Default::default(),
    }
}

#[cst_node]
pub struct CompIf<'a> {
    pub test: Expression<'a>,
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub whitespace_before_test: ParenthesizableWhitespace<'a>,

    pub(crate) if_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for CompIf<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("if");
        self.whitespace_before_test.codegen(state);
        self.test.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedCompIf<'r, 'a> {
    type Inflated = CompIf<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_before_test = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_after.borrow_mut(),
        )?;
        let test = self.test.inflate(config)?;
        Ok(Self::Inflated {
            test,
            whitespace_before,
            whitespace_before_test,
        })
    }
}

#[cst_node(ParenthesizedNode)]
pub struct List<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedList<'r, 'a> {
    type Inflated = List<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbracket = self.lbracket.inflate(config)?;
        let len = self.elements.len();
        let elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        let rbracket = if !elements.is_empty() {
            // lbracket owns all the whitespace if there are no elements
            self.rbracket.inflate(config)?
        } else {
            Default::default()
        };
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elements,
            lbracket,
            rbracket,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for List<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct Set<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedSet<'r, 'a> {
    type Inflated = Set<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbrace = self.lbrace.inflate(config)?;
        let len = self.elements.len();
        let elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        let rbrace = if !elements.is_empty() {
            self.rbrace.inflate(config)?
        } else {
            Default::default()
        };
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elements,
            lbrace,
            rbrace,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for Set<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct Dict<'a> {
    pub elements: Vec<DictElement<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedDict<'r, 'a> {
    type Inflated = Dict<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbrace = self.lbrace.inflate(config)?;
        let len = self.elements.len();
        let elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        let rbrace = if !elements.is_empty() {
            self.rbrace.inflate(config)?
        } else {
            Default::default()
        };
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elements,
            lbrace,
            rbrace,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for Dict<'a> {
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

#[cst_node(NoIntoPy)]
pub enum DictElement<'a> {
    Simple {
        key: Expression<'a>,
        value: Expression<'a>,
        comma: Option<Comma<'a>>,
        whitespace_before_colon: ParenthesizableWhitespace<'a>,
        whitespace_after_colon: ParenthesizableWhitespace<'a>,
        colon_tok: TokenRef<'a>,
    },
    Starred(StarredDictElement<'a>),
}

impl<'r, 'a> DeflatedDictElement<'r, 'a> {
    pub fn inflate_element(
        self,
        config: &Config<'a>,
        last_element: bool,
    ) -> Result<DictElement<'a>> {
        Ok(match self {
            Self::Starred(s) => DictElement::Starred(s.inflate_element(config, last_element)?),
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
                DictElement::Simple {
                    key: key.inflate(config)?,
                    whitespace_before_colon,
                    whitespace_after_colon,
                    value: value.inflate(config)?,
                    comma: if last_element {
                        comma.map(|c| c.inflate_before(config)).transpose()
                    } else {
                        comma.inflate(config)
                    }?,
                }
            }
        })
    }
}

impl<'a> DictElement<'a> {
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

impl<'r, 'a> WithComma<'r, 'a> for DeflatedDictElement<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        let comma = Some(comma);
        match self {
            Self::Starred(s) => Self::Starred(DeflatedStarredDictElement { comma, ..s }),
            Self::Simple {
                key,
                value,
                colon_tok,
                ..
            } => Self::Simple {
                comma,
                key,
                value,
                colon_tok,
            },
        }
    }
}

#[cst_node]
pub struct StarredDictElement<'a> {
    pub value: Expression<'a>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'r, 'a> DeflatedStarredDictElement<'r, 'a> {
    fn inflate_element(
        self,
        config: &Config<'a>,
        last_element: bool,
    ) -> Result<StarredDictElement<'a>> {
        let whitespace_before_value = parse_parenthesizable_whitespace(
            config,
            &mut (*self.star_tok).whitespace_after.borrow_mut(),
        )?;
        let value = self.value.inflate(config)?;
        let comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(StarredDictElement {
            value,
            comma,
            whitespace_before_value,
        })
    }
}

impl<'a> Codegen<'a> for StarredDictElement<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("**");
        self.whitespace_before_value.codegen(state);
        self.value.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[cst_node(Codegen, Inflate)]
pub enum BaseSlice<'a> {
    Index(Box<Index<'a>>),
    Slice(Box<Slice<'a>>),
}

#[cst_node]
pub struct Index<'a> {
    pub value: Expression<'a>,
    pub star: Option<&'a str>,
    pub whitespace_after_star: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) star_tok: Option<TokenRef<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedIndex<'r, 'a> {
    type Inflated = Index<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let (star, whitespace_after_star) = if let Some(star_tok) = self.star_tok.as_mut() {
            (
                Some(star_tok.string),
                Some(parse_parenthesizable_whitespace(
                    config,
                    &mut star_tok.whitespace_after.borrow_mut(),
                )?),
            )
        } else {
            (None, None)
        };
        let value = self.value.inflate(config)?;
        Ok(Self::Inflated {
            value,
            star,
            whitespace_after_star,
        })
    }
}

impl<'a> Codegen<'a> for Index<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        if let Some(star) = self.star {
            state.add_token(star);
        }
        self.whitespace_after_star.codegen(state);
        self.value.codegen(state);
    }
}

#[cst_node]
pub struct Slice<'a> {
    #[cfg_attr(feature = "py", no_py_default)]
    pub lower: Option<Expression<'a>>,
    #[cfg_attr(feature = "py", no_py_default)]
    pub upper: Option<Expression<'a>>,
    pub step: Option<Expression<'a>>,
    pub first_colon: Colon<'a>,
    pub second_colon: Option<Colon<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedSlice<'r, 'a> {
    type Inflated = Slice<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lower = self.lower.inflate(config)?;
        let first_colon = self.first_colon.inflate(config)?;
        let upper = self.upper.inflate(config)?;
        let second_colon = self.second_colon.inflate(config)?;
        let step = self.step.inflate(config)?;
        Ok(Self::Inflated {
            lower,
            upper,
            step,
            first_colon,
            second_colon,
        })
    }
}

impl<'a> Codegen<'a> for Slice<'a> {
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

#[cst_node]
pub struct SubscriptElement<'a> {
    pub slice: BaseSlice<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedSubscriptElement<'r, 'a> {
    type Inflated = SubscriptElement<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let slice = self.slice.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        Ok(Self::Inflated { slice, comma })
    }
}

impl<'a> Codegen<'a> for SubscriptElement<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.slice.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Subscript<'a> {
    pub value: Box<Expression<'a>>,
    pub slice: Vec<SubscriptElement<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_value: ParenthesizableWhitespace<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedSubscript<'r, 'a> {
    type Inflated = Subscript<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let value = self.value.inflate(config)?;
        let whitespace_after_value = parse_parenthesizable_whitespace(
            config,
            &mut self.lbracket.tok.whitespace_before.borrow_mut(),
        )?;
        let lbracket = self.lbracket.inflate(config)?;
        let slice = self.slice.inflate(config)?;
        let rbracket = self.rbracket.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value,
            slice,
            lbracket,
            rbracket,
            lpar,
            rpar,
            whitespace_after_value,
        })
    }
}

impl<'a> Codegen<'a> for Subscript<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct IfExp<'a> {
    pub test: Box<Expression<'a>>,
    pub body: Box<Expression<'a>>,
    pub orelse: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_before_if: ParenthesizableWhitespace<'a>,
    pub whitespace_after_if: ParenthesizableWhitespace<'a>,
    pub whitespace_before_else: ParenthesizableWhitespace<'a>,
    pub whitespace_after_else: ParenthesizableWhitespace<'a>,

    pub(crate) if_tok: TokenRef<'a>,
    pub(crate) else_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedIfExp<'r, 'a> {
    type Inflated = IfExp<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let body = self.body.inflate(config)?;
        let whitespace_before_if = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_if = parse_parenthesizable_whitespace(
            config,
            &mut (*self.if_tok).whitespace_after.borrow_mut(),
        )?;
        let test = self.test.inflate(config)?;
        let whitespace_before_else = parse_parenthesizable_whitespace(
            config,
            &mut (*self.else_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_else = parse_parenthesizable_whitespace(
            config,
            &mut (*self.else_tok).whitespace_after.borrow_mut(),
        )?;
        let orelse = self.orelse.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            test,
            body,
            orelse,
            lpar,
            rpar,
            whitespace_before_if,
            whitespace_after_if,
            whitespace_before_else,
            whitespace_after_else,
        })
    }
}

impl<'a> Codegen<'a> for IfExp<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct Lambda<'a> {
    pub params: Box<Parameters<'a>>,
    pub body: Box<Expression<'a>>,
    pub colon: Colon<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_lambda: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) lambda_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedLambda<'r, 'a> {
    type Inflated = Lambda<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let whitespace_after_lambda = if !self.params.is_empty() {
            Some(parse_parenthesizable_whitespace(
                config,
                &mut (*self.lambda_tok).whitespace_after.borrow_mut(),
            )?)
        } else {
            Default::default()
        };
        let mut params = self.params.inflate(config)?;
        adjust_parameters_trailing_whitespace(config, &mut params, &self.colon.tok)?;
        let colon = self.colon.inflate(config)?;
        let body = self.body.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            params,
            body,
            colon,
            lpar,
            rpar,
            whitespace_after_lambda,
        })
    }
}

impl<'a> Codegen<'a> for Lambda<'a> {
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

#[cst_node]
pub struct From<'a> {
    pub item: Expression<'a>,
    pub whitespace_before_from: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_from: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> From<'a> {
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

impl<'r, 'a> Inflate<'a> for DeflatedFrom<'r, 'a> {
    type Inflated = From<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before_from = Some(parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?);
        let whitespace_after_from = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        let item = self.item.inflate(config)?;
        Ok(Self::Inflated {
            item,
            whitespace_before_from,
            whitespace_after_from,
        })
    }
}

#[cst_node]
pub enum YieldValue<'a> {
    Expression(Box<Expression<'a>>),
    From(Box<From<'a>>),
}

impl<'r, 'a> Inflate<'a> for DeflatedYieldValue<'r, 'a> {
    type Inflated = YieldValue<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        Ok(match self {
            Self::Expression(e) => Self::Inflated::Expression(e.inflate(config)?),
            Self::From(e) => {
                let mut e = e.inflate(config)?;
                e.whitespace_before_from = None;
                Self::Inflated::From(e)
            }
        })
    }
}

impl<'a> YieldValue<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_space: &'a str) {
        match self {
            Self::Expression(e) => e.codegen(state),
            Self::From(f) => f.codegen(state, default_space),
        }
    }
}

#[cst_node(ParenthesizedNode)]
pub struct Yield<'a> {
    pub value: Option<Box<YieldValue<'a>>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_yield: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) yield_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedYield<'r, 'a> {
    type Inflated = Yield<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let whitespace_after_yield = if self.value.is_some() {
            Some(parse_parenthesizable_whitespace(
                config,
                &mut (*self.yield_tok).whitespace_after.borrow_mut(),
            )?)
        } else {
            Default::default()
        };
        let value = self.value.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value,
            lpar,
            rpar,
            whitespace_after_yield,
        })
    }
}

impl<'a> Codegen<'a> for Yield<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct Await<'a> {
    pub expression: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_await: ParenthesizableWhitespace<'a>,

    pub(crate) await_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedAwait<'r, 'a> {
    type Inflated = Await<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let whitespace_after_await = parse_parenthesizable_whitespace(
            config,
            &mut (*self.await_tok).whitespace_after.borrow_mut(),
        )?;
        let expression = self.expression.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            expression,
            lpar,
            rpar,
            whitespace_after_await,
        })
    }
}

impl<'a> Codegen<'a> for Await<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("await");
            self.whitespace_after_await.codegen(state);
            self.expression.codegen(state);
        })
    }
}

#[cst_node(Codegen, Inflate)]
pub enum String<'a> {
    Simple(SimpleString<'a>),
    Concatenated(ConcatenatedString<'a>),
    Formatted(FormattedString<'a>),
}

impl<'r, 'a> std::convert::From<DeflatedString<'r, 'a>> for DeflatedExpression<'r, 'a> {
    fn from(s: DeflatedString<'r, 'a>) -> Self {
        match s {
            DeflatedString::Simple(s) => Self::SimpleString(Box::new(s)),
            DeflatedString::Concatenated(s) => Self::ConcatenatedString(Box::new(s)),
            DeflatedString::Formatted(s) => Self::FormattedString(Box::new(s)),
        }
    }
}

#[cst_node(ParenthesizedNode)]
pub struct ConcatenatedString<'a> {
    pub left: Box<String<'a>>,
    pub right: Box<String<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_between: ParenthesizableWhitespace<'a>,

    // we capture the next token after each string piece so Inflate can extract the
    // whitespace between individual pieces
    pub(crate) right_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedConcatenatedString<'r, 'a> {
    type Inflated = ConcatenatedString<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let left = self.left.inflate(config)?;
        let whitespace_between = parse_parenthesizable_whitespace(
            config,
            &mut (*self.right_tok).whitespace_before.borrow_mut(),
        )?;
        let right = self.right.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            left,
            right,
            lpar,
            rpar,
            whitespace_between,
        })
    }
}

impl<'a> Codegen<'a> for ConcatenatedString<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.whitespace_between.codegen(state);
            self.right.codegen(state);
        })
    }
}

#[cst_node(ParenthesizedNode, Default)]
pub struct SimpleString<'a> {
    /// The texual representation of the string, including quotes, prefix
    /// characters, and any escape characters present in the original source code,
    /// such as ``r"my string\n"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedSimpleString<'r, 'a> {
    type Inflated = SimpleString<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            value: self.value,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for SimpleString<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| state.add_token(self.value))
    }
}

#[cst_node]
pub struct FormattedStringText<'a> {
    pub value: &'a str,
}

impl<'r, 'a> Inflate<'a> for DeflatedFormattedStringText<'r, 'a> {
    type Inflated = FormattedStringText<'a>;
    fn inflate(self, _config: &Config<'a>) -> Result<Self::Inflated> {
        Ok(Self::Inflated { value: self.value })
    }
}

impl<'a> Codegen<'a> for FormattedStringText<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token(self.value);
    }
}

pub(crate) fn make_fstringtext<'r, 'a>(value: &'a str) -> DeflatedFormattedStringText<'r, 'a> {
    DeflatedFormattedStringText {
        value,
        _phantom: Default::default(),
    }
}

#[cst_node]
pub struct FormattedStringExpression<'a> {
    pub expression: Expression<'a>,
    pub conversion: Option<&'a str>,
    pub format_spec: Option<Vec<FormattedStringContent<'a>>>,
    pub whitespace_before_expression: ParenthesizableWhitespace<'a>,
    pub whitespace_after_expression: ParenthesizableWhitespace<'a>,
    pub equal: Option<AssignEqual<'a>>,

    pub(crate) lbrace_tok: TokenRef<'a>,
    // This is None if there's an equal sign, otherwise it's the first token of
    // (conversion, format spec, right brace) in that order
    pub(crate) after_expr_tok: Option<TokenRef<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedFormattedStringExpression<'r, 'a> {
    type Inflated = FormattedStringExpression<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before_expression = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lbrace_tok).whitespace_after.borrow_mut(),
        )?;
        let expression = self.expression.inflate(config)?;
        let equal = self.equal.inflate(config)?;
        let whitespace_after_expression = if let Some(after_expr_tok) = self.after_expr_tok.as_mut()
        {
            parse_parenthesizable_whitespace(
                config,
                &mut after_expr_tok.whitespace_before.borrow_mut(),
            )?
        } else {
            Default::default()
        };
        let format_spec = self.format_spec.inflate(config)?;
        Ok(Self::Inflated {
            expression,
            conversion: self.conversion,
            format_spec,
            whitespace_before_expression,
            whitespace_after_expression,
            equal,
        })
    }
}

impl<'a> Codegen<'a> for FormattedStringExpression<'a> {
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

#[cst_node(Codegen, Inflate)]
pub enum FormattedStringContent<'a> {
    Text(FormattedStringText<'a>),
    Expression(Box<FormattedStringExpression<'a>>),
}

#[cst_node(ParenthesizedNode)]
pub struct FormattedString<'a> {
    pub parts: Vec<FormattedStringContent<'a>>,
    pub start: &'a str,
    pub end: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedFormattedString<'r, 'a> {
    type Inflated = FormattedString<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let parts = self.parts.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            parts,
            start: self.start,
            end: self.end,
            lpar,
            rpar,
        })
    }
}

impl<'a> Codegen<'a> for FormattedString<'a> {
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

#[cst_node(ParenthesizedNode)]
pub struct NamedExpr<'a> {
    pub target: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,

    pub whitespace_before_walrus: ParenthesizableWhitespace<'a>,
    pub whitespace_after_walrus: ParenthesizableWhitespace<'a>,

    pub(crate) walrus_tok: TokenRef<'a>,
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
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let target = self.target.inflate(config)?;
        let whitespace_before_walrus = parse_parenthesizable_whitespace(
            config,
            &mut self.walrus_tok.whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_walrus = parse_parenthesizable_whitespace(
            config,
            &mut self.walrus_tok.whitespace_after.borrow_mut(),
        )?;
        let value = self.value.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            target,
            value,
            lpar,
            rpar,
            whitespace_before_walrus,
            whitespace_after_walrus,
        })
    }
}

#[cfg(feature = "py")]
mod py {

    use pyo3::types::PyAnyMethods;
    use pyo3::types::PyModule;

    use super::*;
    use crate::nodes::traits::py::TryIntoPy;

    // TODO: this could be a derive helper attribute to override the python class name
    impl<'a> TryIntoPy<pyo3::PyObject> for Element<'a> {
        fn try_into_py(self, py: pyo3::Python) -> pyo3::PyResult<pyo3::PyObject> {
            match self {
                Self::Starred(s) => s.try_into_py(py),
                Self::Simple { value, comma } => {
                    let libcst = PyModule::import_bound(py, "libcst")?;
                    let kwargs = [
                        Some(("value", value.try_into_py(py)?)),
                        comma
                            .map(|x| x.try_into_py(py))
                            .transpose()?
                            .map(|x| ("comma", x)),
                    ]
                    .iter()
                    .filter(|x| x.is_some())
                    .map(|x| x.as_ref().unwrap())
                    .collect::<Vec<_>>()
                    .into_py_dict_bound(py);
                    Ok(libcst
                        .getattr("Element")
                        .expect("no Element found in libcst")
                        .call((), Some(&kwargs))?
                        .into())
                }
            }
        }
    }

    // TODO: this could be a derive helper attribute to override the python class name
    impl<'a> TryIntoPy<pyo3::PyObject> for DictElement<'a> {
        fn try_into_py(self, py: pyo3::Python) -> pyo3::PyResult<pyo3::PyObject> {
            match self {
                Self::Starred(s) => s.try_into_py(py),
                Self::Simple {
                    key,
                    value,
                    comma,
                    whitespace_after_colon,
                    whitespace_before_colon,
                    ..
                } => {
                    let libcst = PyModule::import_bound(py, "libcst")?;
                    let kwargs = [
                        Some(("key", key.try_into_py(py)?)),
                        Some(("value", value.try_into_py(py)?)),
                        Some((
                            "whitespace_before_colon",
                            whitespace_before_colon.try_into_py(py)?,
                        )),
                        Some((
                            "whitespace_after_colon",
                            whitespace_after_colon.try_into_py(py)?,
                        )),
                        comma
                            .map(|x| x.try_into_py(py))
                            .transpose()?
                            .map(|x| ("comma", x)),
                    ]
                    .iter()
                    .filter(|x| x.is_some())
                    .map(|x| x.as_ref().unwrap())
                    .collect::<Vec<_>>()
                    .into_py_dict_bound(py);
                    Ok(libcst
                        .getattr("DictElement")
                        .expect("no Element found in libcst")
                        .call((), Some(&kwargs))?
                        .into())
                }
            }
        }
    }
}
