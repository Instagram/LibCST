// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::mem::swap;

use crate::{
    inflate_helpers::adjust_parameters_trailing_whitespace,
    nodes::{
        traits::{Inflate, ParenthesizedNode, Result, WithComma},
        whitespace::ParenthesizableWhitespace,
        Annotation, AssignEqual, AssignTargetExpression, BinaryOp, BooleanOp, Codegen,
        CodegenState, Colon, Comma, CompOp, Dot, UnaryOp,
    },
    tokenizer::{
        whitespace_parser::{parse_parenthesizable_whitespace, Config},
        Token,
    },
};
use libcst_derive::{Codegen, Inflate, ParenthesizedNode};

#[derive(Debug, Eq, PartialEq, Default, Clone)]
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

impl<'a> Inflate<'a> for Parameters<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        for p in &mut self.posonly_params {
            p.inflate(config)?;
        }
        self.posonly_ind.inflate(config)?;
        for p in &mut self.params {
            p.inflate(config)?;
        }
        self.star_arg.inflate(config)?;
        for p in &mut self.kwonly_params {
            p.inflate(config)?;
        }
        self.star_kwarg.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Inflate)]
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

impl<'a> Inflate<'a> for ParamSlash<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.comma.inflate(config)
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

impl<'a> Inflate<'a> for ParamStar<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.comma.inflate(config)
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone, ParenthesizedNode)]
pub struct Name<'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Name<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for Name<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        });
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param<'a> {
    pub name: Name<'a>,
    pub annotation: Option<Annotation<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub default: Option<Expression<'a>>,

    pub comma: Option<Comma<'a>>,

    pub star: Option<&'a str>,

    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_param: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Option<Token<'a>>,
}

impl<'a> Inflate<'a> for Param<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        // TODO: whitespace_after_param missing?
        self.name.inflate(config)?;
        self.annotation.inflate(config)?;
        self.equal.inflate(config)?;
        self.default.inflate(config)?;
        self.comma.inflate(config)?;
        if let Some(star_tok) = self.star_tok.as_mut() {
            self.whitespace_after_star =
                parse_parenthesizable_whitespace(config, &mut star_tok.whitespace_after)?;
        }
        Ok(())
    }
}

impl<'a> Default for Param<'a> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            annotation: None,
            equal: None,
            default: None,
            comma: None,
            star: None,
            whitespace_after_param: Default::default(),
            whitespace_after_star: Default::default(),
            star_tok: None,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arg<'a> {
    pub value: Expression<'a>,
    pub keyword: Option<Name<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub comma: Option<Comma<'a>>,
    pub star: &'a str,
    pub whitespace_after_star: ParenthesizableWhitespace<'a>,
    pub whitespace_after_arg: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Option<Token<'a>>,
}

impl<'a> Inflate<'a> for Arg<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        if let Some(star_tok) = self.star_tok.as_mut() {
            self.whitespace_after_star =
                parse_parenthesizable_whitespace(config, &mut star_tok.whitespace_after)?;
        }
        self.keyword.inflate(config)?;
        self.equal.inflate(config)?;
        self.value.inflate(config)?;
        self.comma.inflate(config)?;
        // whitespace_after_arg is handled in Call
        Ok(())
    }
}

impl<'a> Arg<'a> {
    pub fn codegen(&'a self, state: &mut CodegenState<'a>, default_comma: bool) {
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

    pub(crate) lpar_tok: Token<'a>,
}

impl<'a> Codegen<'a> for LeftParen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("(");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for LeftParen<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.lpar_tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RightParen<'a> {
    /// Any space that appears directly before this right parenthesis.
    pub whitespace_before: ParenthesizableWhitespace<'a>,

    pub(crate) rpar_tok: Token<'a>,
}

impl<'a> Codegen<'a> for RightParen<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token(")");
    }
}

impl<'a> Inflate<'a> for RightParen<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.rpar_tok.whitespace_before)?;
        Ok(())
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq, Clone, ParenthesizedNode, Codegen, Inflate)]
pub enum Expression<'a> {
    Name(Name<'a>),
    Ellipsis(Ellipsis<'a>),
    Integer(Integer<'a>),
    Float(Float<'a>),
    Imaginary(Imaginary<'a>),
    Comparison(Comparison<'a>),
    UnaryOperation(UnaryOperation<'a>),
    BinaryOperation(BinaryOperation<'a>),
    BooleanOperation(BooleanOperation<'a>),
    Attribute(Attribute<'a>),
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
    StarredElement(StarredElement<'a>),
    IfExp(IfExp<'a>),
    Lambda(Lambda<'a>),
    Yield(Yield<'a>),
    Await(Await<'a>),
    SimpleString(SimpleString<'a>),
    ConcatenatedString(ConcatenatedString<'a>),
    FormattedString(FormattedString<'a>),
    // TODO: NamedExpr
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Ellipsis<'a> {
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Ellipsis<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("...");
        })
    }
}
impl<'a> Inflate<'a> for Ellipsis<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Integer<'a> {
    /// A string representation of the integer, such as ``"100000"`` or
    /// ``"100_000"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Integer<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'a> Inflate<'a> for Integer<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Float<'a> {
    /// A string representation of the floating point number, such as ```"0.05"``,
    /// ``".050"``, or ``"5e-2"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Float<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'a> Inflate<'a> for Float<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Imaginary<'a> {
    /// A string representation of the complex number, such as ``"2j"``
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Imaginary<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        })
    }
}

impl<'a> Inflate<'a> for Imaginary<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Comparison<'a> {
    pub left: Box<Expression<'a>>,
    pub comparisons: Vec<ComparisonTarget<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for Comparison<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            for comp in &self.comparisons {
                comp.codegen(state);
            }
        })
    }
}
impl<'a> Inflate<'a> for Comparison<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.left.inflate(config)?;
        self.comparisons.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct UnaryOperation<'a> {
    pub operator: UnaryOp<'a>,
    pub expression: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for UnaryOperation<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.operator.codegen(state);
            self.expression.codegen(state);
        })
    }
}

impl<'a> Inflate<'a> for UnaryOperation<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.operator.inflate(config)?;
        self.expression.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct BinaryOperation<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: BinaryOp<'a>,
    pub right: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for BinaryOperation<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.operator.codegen(state);
            self.right.codegen(state);
        })
    }
}

impl<'a> Inflate<'a> for BinaryOperation<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.left.inflate(config)?;
        self.operator.inflate(config)?;
        self.right.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct BooleanOperation<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: BooleanOp<'a>,
    pub right: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for BooleanOperation<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.operator.codegen(state);
            self.right.codegen(state);
        })
    }
}

impl<'a> Inflate<'a> for BooleanOperation<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.left.inflate(config)?;
        self.operator.inflate(config)?;
        self.right.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Call<'a> {
    pub func: Box<Expression<'a>>,
    pub args: Vec<Arg<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_func: ParenthesizableWhitespace<'a>,
    pub whitespace_before_args: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: Token<'a>,
    pub(crate) rpar_tok: Token<'a>,
}

impl<'a> Inflate<'a> for Call<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.func.inflate(config)?;
        self.whitespace_after_func =
            parse_parenthesizable_whitespace(config, &mut self.lpar_tok.whitespace_before)?;
        self.whitespace_before_args =
            parse_parenthesizable_whitespace(config, &mut self.lpar_tok.whitespace_after)?;
        self.args.inflate(config)?;

        if let Some(arg) = self.args.last_mut() {
            if arg.comma.is_none() {
                arg.whitespace_after_arg =
                    parse_parenthesizable_whitespace(config, &mut self.rpar_tok.whitespace_before)?;
            }
        }
        self.rpar.inflate(config)?;

        Ok(())
    }
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
                arg.codegen(state, i + 1 < arg_len);
            }
            state.add_token(")");
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Attribute<'a> {
    pub value: Box<Expression<'a>>,
    pub attr: Name<'a>,
    pub dot: Dot<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Attribute<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.value.inflate(config)?;
        self.dot.inflate(config)?;
        self.attr.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
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

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen)]
pub enum NameOrAttribute<'a> {
    N(Name<'a>),
    A(Attribute<'a>),
}

impl<'a> Inflate<'a> for NameOrAttribute<'a> {
    fn inflate(&mut self, _config: &Config<'a>) -> Result<()> {
        // TODO
        Ok(())
    }
}

impl<'a> std::convert::From<NameOrAttribute<'a>> for Expression<'a> {
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

impl<'a> Inflate<'a> for ComparisonTarget<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.operator.inflate(config)?;
        self.comparator.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct StarredElement<'a> {
    pub value: Box<Expression<'a>>,
    pub comma: Option<Comma<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Token<'a>,
}

impl<'a> Inflate<'a> for StarredElement<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.whitespace_before_value =
            parse_parenthesizable_whitespace(config, &mut self.star_tok.whitespace_after)?;
        self.value.inflate(config)?;
        self.comma.inflate(config)?;
        Ok(())
    }
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element<'a> {
    Simple {
        value: Expression<'a>,
        comma: Option<Comma<'a>>,
    },
    Starred(StarredElement<'a>),
}

impl<'a> Inflate<'a> for Element<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::Starred(s) => s.inflate(config)?,
            Self::Simple { value, comma } => {
                value.inflate(config)?;
                comma.inflate(config)?;
            }
        }
        Ok(())
    }
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

    fn comma(&self) -> Option<&Comma> {
        match self {
            Self::Starred(s) => s.comma.as_ref(),
            Self::Simple { comma, .. } => comma.as_ref(),
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
impl<'a> std::convert::From<Expression<'a>> for Element<'a> {
    fn from(e: Expression<'a>) -> Self {
        match e {
            Expression::StarredElement(e) => Element::Starred(e),
            value => Element::Simple { value, comma: None },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, ParenthesizedNode)]
pub struct Tuple<'a> {
    pub elements: Vec<Element<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Tuple<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.elements.inflate(config)?;
        if !self.elements.is_empty() {
            if let Some(last) = self.elements.last() {
                if last.comma().is_none() {
                    // rpar only has whitespace if elements is non empty without a
                    // trailing comma
                    self.rpar.inflate(config)?;
                }
            }
        }
        Ok(())
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct GeneratorExp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for GeneratorExp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.elt.codegen(state);
            self.for_in.codegen(state);
        })
    }
}

impl<'a> Inflate<'a> for GeneratorExp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.elt.inflate(config)?;
        self.for_in.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct ListComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
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

impl<'a> Inflate<'a> for ListComp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.lbracket.inflate(config)?;
        self.elt.inflate(config)?;
        self.for_in.inflate(config)?;
        self.rbracket.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftSquareBracket<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: Token<'a>,
}

impl<'a> Codegen<'a> for LeftSquareBracket<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token("[");
        self.whitespace_after.codegen(state);
    }
}

impl<'a> Inflate<'a> for LeftSquareBracket<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RightSquareBracket<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub(crate) tok: Token<'a>,
}

impl<'a> Codegen<'a> for RightSquareBracket<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("]");
    }
}

impl<'a> Inflate<'a> for RightSquareBracket<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_before)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct SetComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for SetComp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.lbrace.inflate(config)?;
        self.elt.inflate(config)?;
        self.for_in.inflate(config)?;
        self.rbrace.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
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

    pub(crate) colon_tok: Token<'a>,
}

impl<'a> Inflate<'a> for DictComp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.lbrace.inflate(config)?;
        self.key.inflate(config)?;
        self.whitespace_before_colon =
            parse_parenthesizable_whitespace(config, &mut self.colon_tok.whitespace_before)?;
        self.whitespace_after_colon =
            parse_parenthesizable_whitespace(config, &mut self.colon_tok.whitespace_after)?;
        self.value.inflate(config)?;
        self.for_in.inflate(config)?;
        self.rbrace.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftCurlyBrace<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: Token<'a>,
}

impl<'a> Inflate<'a> for LeftCurlyBrace<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_after =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        Ok(())
    }
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
    pub(crate) tok: Token<'a>,
}

impl<'a> Inflate<'a> for RightCurlyBrace<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_before)?;
        Ok(())
    }
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

    pub(crate) async_tok: Option<Token<'a>>,
    pub(crate) for_tok: Token<'a>,
    pub(crate) in_tok: Token<'a>,
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

impl<'a> Inflate<'a> for CompFor<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.for_tok.whitespace_before)?;
        if let (Some(asy_tok), Some(asy)) = (self.async_tok.as_mut(), self.asynchronous.as_mut()) {
            // If there is an async keyword, the start of the CompFor expression is
            // considered to be this keyword, so whitespace_before needs to adjust but
            // Asynchronous will own the whitespace before the for token.
            asy.whitespace_after =
                parse_parenthesizable_whitespace(config, &mut asy_tok.whitespace_before)?;
            swap(&mut asy.whitespace_after, &mut self.whitespace_before);
        }
        self.whitespace_after_for =
            parse_parenthesizable_whitespace(config, &mut self.for_tok.whitespace_after)?;
        self.target.inflate(config)?;
        self.whitespace_before_in =
            parse_parenthesizable_whitespace(config, &mut self.in_tok.whitespace_before)?;
        self.whitespace_after_in =
            parse_parenthesizable_whitespace(config, &mut self.in_tok.whitespace_after)?;
        self.iter.inflate(config)?;
        self.ifs.inflate(config)?;
        self.inner_for_in.inflate(config)?;
        Ok(())
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

    pub(crate) if_tok: Token<'a>,
}

impl<'a> Codegen<'a> for CompIf<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("if");
        self.whitespace_before_test.codegen(state);
        self.test.codegen(state);
    }
}

impl<'a> Inflate<'a> for CompIf<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before =
            parse_parenthesizable_whitespace(config, &mut self.if_tok.whitespace_before)?;
        self.whitespace_before_test =
            parse_parenthesizable_whitespace(config, &mut self.if_tok.whitespace_after)?;
        self.test.inflate(config)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct List<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for List<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.lbracket.inflate(config)?;
        self.elements.inflate(config)?;
        if !self.elements.is_empty() {
            if let Some(last) = self.elements.last() {
                if last.comma().is_none() {
                    // lbracket owns all the whitespace if there are no elements
                    self.rbracket.inflate(config)?;
                }
            }
        }
        self.rpar.inflate(config)?;
        Ok(())
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Set<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Set<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.lbrace.inflate(config)?;
        self.elements.inflate(config)?;
        if !self.elements.is_empty() {
            if let Some(last) = self.elements.last() {
                if last.comma().is_none() {
                    self.rbrace.inflate(config)?;
                }
            }
        }
        self.rpar.inflate(config)?;
        Ok(())
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Dict<'a> {
    pub elements: Vec<DictElement<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Dict<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.lbrace.inflate(config)?;
        self.elements.inflate(config)?;
        if !self.elements.is_empty() {
            if let Some(last) = self.elements.last() {
                if last.comma().is_none() {
                    self.rbrace.inflate(config)?;
                }
            }
        }
        self.rpar.inflate(config)?;
        Ok(())
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
        colon_tok: Token<'a>,
    },
    Starred(DoubleStarredElement<'a>),
}

impl<'a> Inflate<'a> for DictElement<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::Starred(s) => s.inflate(config)?,
            Self::Simple {
                key,
                value,
                comma,
                whitespace_before_colon,
                whitespace_after_colon,
                colon_tok,
            } => {
                key.inflate(config)?;
                *whitespace_before_colon =
                    parse_parenthesizable_whitespace(config, &mut colon_tok.whitespace_before)?;
                *whitespace_after_colon =
                    parse_parenthesizable_whitespace(config, &mut colon_tok.whitespace_after)?;
                value.inflate(config)?;
                comma.inflate(config)?;
            }
        }
        Ok(())
    }
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

    fn comma(&self) -> Option<&Comma> {
        match self {
            Self::Simple { comma, .. } => comma.as_ref(),
            Self::Starred(s) => s.comma.as_ref(),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DoubleStarredElement<'a> {
    pub value: Expression<'a>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: Token<'a>,
}

impl<'a> Inflate<'a> for DoubleStarredElement<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before_value =
            parse_parenthesizable_whitespace(config, &mut self.star_tok.whitespace_after)?;
        self.value.inflate(config)?;
        self.comma.inflate(config)?;
        Ok(())
    }
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
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
pub enum BaseSlice<'a> {
    Index(Index<'a>),
    Slice(Slice<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index<'a> {
    pub value: Expression<'a>,
}

impl<'a> Inflate<'a> for Index<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.value.inflate(config)
    }
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

impl<'a> Inflate<'a> for Slice<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lower.inflate(config)?;
        self.first_colon.inflate(config)?;
        self.upper.inflate(config)?;
        self.second_colon.inflate(config)?;
        self.step.inflate(config)?;
        Ok(())
    }
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

impl<'a> Inflate<'a> for SubscriptElement<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.slice.inflate(config)?;
        self.comma.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for SubscriptElement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.slice.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Subscript<'a> {
    pub value: Box<Expression<'a>>,
    pub slice: Vec<SubscriptElement<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_value: ParenthesizableWhitespace<'a>,

    pub(crate) lbracket_tok: Token<'a>,
}

// HACK: in slices if there is a colon directly before or after the comma, the colon
// owns the whitespace. Not sure if this is by design or accident.
fn ltrim_comma(before: &BaseSlice, comma: &mut Comma) {
    if let BaseSlice::Slice(s) = &before {
        let trailing_second_colon = s.step.is_none() && s.second_colon.is_some();
        let trailing_first_colon = s.upper.is_none() && s.step.is_none();
        if trailing_first_colon || trailing_second_colon {
            comma.whitespace_before = Default::default();
        }
    }
}

fn rtrim_comma(comma: &mut Comma, after: &BaseSlice) {
    if let BaseSlice::Slice(s) = &after {
        let leading_first_colon = s.lower.is_none();
        if leading_first_colon {
            comma.whitespace_after = Default::default();
        }
    }
}

impl<'a> Inflate<'a> for Subscript<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.value.inflate(config)?;
        self.whitespace_after_value =
            parse_parenthesizable_whitespace(config, &mut self.lbracket_tok.whitespace_before)?;
        self.lbracket.inflate(config)?;
        // TODO: trailing comma fucks with the generic vec implementation
        self.slice.inflate(config)?;
        let mut prev_comma = None;
        for el in self.slice.iter_mut() {
            if let Some(comma) = el.comma.as_mut() {
                ltrim_comma(&el.slice, comma);
            }
            if let Some(comma) = prev_comma {
                rtrim_comma(comma, &el.slice);
            }
            prev_comma = el.comma.as_mut();
        }
        // TODO: This should go away once Tokens are references
        // if there is a trailing comma, it owns the whitespace before right bracket
        if let Some(SubscriptElement { comma: Some(_), .. }) = self.slice.last() {
        } else {
            self.rbracket.inflate(config)?;
        }
        self.rpar.inflate(config)?;
        Ok(())
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
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

    pub(crate) if_tok: Token<'a>,
    pub(crate) else_tok: Token<'a>,
}

impl<'a> Inflate<'a> for IfExp<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.body.inflate(config)?;
        self.whitespace_before_if =
            parse_parenthesizable_whitespace(config, &mut self.if_tok.whitespace_before)?;
        self.whitespace_after_if =
            parse_parenthesizable_whitespace(config, &mut self.if_tok.whitespace_after)?;
        self.test.inflate(config)?;
        self.whitespace_before_else =
            parse_parenthesizable_whitespace(config, &mut self.else_tok.whitespace_before)?;
        self.whitespace_after_else =
            parse_parenthesizable_whitespace(config, &mut self.else_tok.whitespace_after)?;
        self.orelse.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for IfExp<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Lambda<'a> {
    pub params: Box<Parameters<'a>>,
    pub body: Box<Expression<'a>>,
    pub colon: Colon<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_lambda: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) lambda_tok: Token<'a>,
}

impl<'a> Inflate<'a> for Lambda<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        if !self.params.is_empty() {
            self.whitespace_after_lambda = Some(parse_parenthesizable_whitespace(
                config,
                &mut self.lambda_tok.whitespace_after,
            )?);
        }
        self.params.inflate(config)?;
        adjust_parameters_trailing_whitespace(config, &mut self.params, &mut self.colon.tok)?;
        self.colon.inflate(config)?;
        self.body.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for Lambda<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct From<'a> {
    pub item: Expression<'a>,
    pub whitespace_before_from: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_from: ParenthesizableWhitespace<'a>,

    pub(crate) tok: Token<'a>,
}

impl<'a> From<'a> {
    pub fn codegen(&'a self, state: &mut CodegenState<'a>, default_space: &'a str) {
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

impl<'a> Inflate<'a> for From<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before_from = Some(parse_parenthesizable_whitespace(
            config,
            &mut self.tok.whitespace_before,
        )?);
        self.whitespace_after_from =
            parse_parenthesizable_whitespace(config, &mut self.tok.whitespace_after)?;
        self.item.inflate(config)?;
        Ok(())
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum YieldValue<'a> {
    Expression(Expression<'a>),
    From(From<'a>),
}

impl<'a> Inflate<'a> for YieldValue<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::Expression(e) => e.inflate(config),
            Self::From(e) => {
                e.inflate(config)?;
                e.whitespace_before_from = None;
                Ok(())
            }
        }
    }
}

impl<'a> YieldValue<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>, default_space: &'a str) {
        match self {
            Self::Expression(e) => e.codegen(state),
            Self::From(f) => f.codegen(state, default_space),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Yield<'a> {
    pub value: Option<Box<YieldValue<'a>>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_yield: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) yield_tok: Token<'a>,
}

impl<'a> Inflate<'a> for Yield<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        if self.value.is_some() {
            self.whitespace_after_yield = Some(parse_parenthesizable_whitespace(
                config,
                &mut self.yield_tok.whitespace_after,
            )?);
        }
        self.value.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for Yield<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct Await<'a> {
    pub expression: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_await: ParenthesizableWhitespace<'a>,

    pub(crate) await_tok: Token<'a>,
}

impl<'a> Inflate<'a> for Await<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.whitespace_after_await =
            parse_parenthesizable_whitespace(config, &mut self.await_tok.whitespace_after)?;
        self.expression.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for Await<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("await");
            self.whitespace_after_await.codegen(state);
            self.expression.codegen(state);
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
pub enum String<'a> {
    Simple(SimpleString<'a>),
    Concatenated(ConcatenatedString<'a>),
    Formatted(FormattedString<'a>),
}

impl<'a> std::convert::From<String<'a>> for Expression<'a> {
    fn from(s: String<'a>) -> Self {
        match s {
            String::Simple(s) => Self::SimpleString(s),
            String::Concatenated(s) => Self::ConcatenatedString(s),
            String::Formatted(s) => Self::FormattedString(s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct ConcatenatedString<'a> {
    pub left: Box<String<'a>>,
    pub right: Box<String<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_between: ParenthesizableWhitespace<'a>,

    // we capture the next token after each string piece so Inflate can extract the
    // whitespace between individual pieces
    pub(crate) right_tok: Token<'a>,
}

impl<'a> Inflate<'a> for ConcatenatedString<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.left.inflate(config)?;
        self.whitespace_between =
            parse_parenthesizable_whitespace(config, &mut self.right_tok.whitespace_before)?;
        self.right.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for ConcatenatedString<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.whitespace_between.codegen(state);
            self.right.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, ParenthesizedNode)]
pub struct SimpleString<'a> {
    /// The texual representation of the string, including quotes, prefix
    /// characters, and any escape characters present in the original source code,
    /// such as ``r"my string\n"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for SimpleString<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for SimpleString<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| state.add_token(self.value))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FormattedStringText<'a> {
    pub value: &'a str,
}

impl<'a> Inflate<'a> for FormattedStringText<'a> {
    fn inflate(&mut self, _config: &Config<'a>) -> Result<()> {
        Ok(())
    }
}

impl<'a> Codegen<'a> for FormattedStringText<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        state.add_token(self.value);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FormattedStringExpression<'a> {
    pub expression: Expression<'a>,
    pub conversion: Option<&'a str>,
    pub format_spec: Option<Vec<FormattedStringContent<'a>>>,
    pub whitespace_before_expression: ParenthesizableWhitespace<'a>,
    pub whitespace_after_expression: ParenthesizableWhitespace<'a>,
    pub equal: Option<AssignEqual<'a>>,

    pub(crate) lbrace_tok: Token<'a>,
    // This is None if there's an equal sign, otherwise it's the first token of
    // (conversion, format spec, right brace) in that order
    pub(crate) after_expr_tok: Option<Token<'a>>,
}

impl<'a> Inflate<'a> for FormattedStringExpression<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.whitespace_before_expression =
            parse_parenthesizable_whitespace(config, &mut self.lbrace_tok.whitespace_after)?;
        self.expression.inflate(config)?;
        self.equal.inflate(config)?;
        if let Some(after_expr_tok) = self.after_expr_tok.as_mut() {
            self.whitespace_after_expression =
                parse_parenthesizable_whitespace(config, &mut after_expr_tok.whitespace_before)?;
        }
        self.format_spec.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for FormattedStringExpression<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
pub enum FormattedStringContent<'a> {
    Text(FormattedStringText<'a>),
    Expression(FormattedStringExpression<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
pub struct FormattedString<'a> {
    pub parts: Vec<FormattedStringContent<'a>>,
    pub start: &'a str,
    pub end: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for FormattedString<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.lpar.inflate(config)?;
        self.parts.inflate(config)?;
        self.rpar.inflate(config)?;
        Ok(())
    }
}

impl<'a> Codegen<'a> for FormattedString<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.start);
            for part in &self.parts {
                part.codegen(state);
            }
            state.add_token(self.end);
        })
    }
}
