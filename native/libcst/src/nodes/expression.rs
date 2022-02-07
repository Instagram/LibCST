// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::{mem::swap, rc::Rc};

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
#[cfg(feature = "py")]
use libcst_derive::TryIntoPy;
use libcst_derive::{Codegen, Inflate, ParenthesizedNode};

type TokenRef<'a> = Rc<Token<'a>>;

#[derive(Debug, Eq, PartialEq, Default, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

#[derive(Debug, PartialEq, Eq, Clone, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ParamSlash<'a> {
    pub comma: Option<Comma<'a>>,
}

impl<'a> ParamSlash<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        state.add_token("/");
        match (&self.comma, default_comma) {
            (Some(comma), _) => comma.codegen(state),
            (None, true) => state.add_token(", "),
            _ => {}
        }
    }
}

impl<'a> Inflate<'a> for ParamSlash<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ParamStar<'a> {
    pub comma: Comma<'a>,
}

impl<'a> Codegen<'a> for ParamStar<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
        self.comma.codegen(state);
    }
}

impl<'a> Inflate<'a> for ParamStar<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Name<'a> {
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Name<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for Name<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token(self.value);
        });
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for Param<'a> {
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

impl<'a> Default for Param<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for Arg<'a> {
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

impl<'a> WithComma<'a> for Arg<'a> {
    fn with_comma(self, c: Comma<'a>) -> Self {
        Self {
            comma: Some(c),
            ..self
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for LeftParen<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.lpar_tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for RightParen<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.rpar_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, ParenthesizedNode, Codegen, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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
impl<'a> Inflate<'a> for Ellipsis<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for Integer<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for Float<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for Imaginary<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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
impl<'a> Inflate<'a> for Comparison<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.comparisons = self.comparisons.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for UnaryOperation<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.expression = self.expression.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for BinaryOperation<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.right = self.right.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for BooleanOperation<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.left = self.left.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.right = self.right.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for Call<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Attribute<'a> {
    pub value: Box<Expression<'a>>,
    pub attr: Name<'a>,
    pub dot: Dot<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Attribute<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.dot = self.dot.inflate(config)?;
        self.attr = self.attr.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
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

#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum NameOrAttribute<'a> {
    N(Box<Name<'a>>),
    A(Box<Attribute<'a>>),
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
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for ComparisonTarget<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.operator = self.operator.inflate(config)?;
        self.comparator = self.comparator.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct StarredElement<'a> {
    pub value: Box<Expression<'a>>,
    pub comma: Option<Comma<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'a> StarredElement<'a> {
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

impl<'a> Inflate<'a> for StarredElement<'a> {
    fn inflate(self, config: &Config<'a>) -> Result<Self> {
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element<'a> {
    Simple {
        value: Expression<'a>,
        comma: Option<Comma<'a>>,
    },
    Starred(Box<StarredElement<'a>>),
}

impl<'a> Element<'a> {
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
            Self::Starred(s) => Self::Starred(Box::new(s.inflate_element(config, is_last)?)),
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

impl<'a> WithComma<'a> for Element<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
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
impl<'a> std::convert::From<Expression<'a>> for Element<'a> {
    fn from(e: Expression<'a>) -> Self {
        match e {
            Expression::StarredElement(e) => Element::Starred(e),
            value => Element::Simple { value, comma: None },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Tuple<'a> {
    pub elements: Vec<Element<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Tuple<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Tuple<'a>> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for GeneratorExp<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.elt = self.elt.inflate(config)?;
        self.for_in = self.for_in.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for ListComp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for LeftSquareBracket<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for RightSquareBracket<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct SetComp<'a> {
    pub elt: Box<Expression<'a>>,
    pub for_in: Box<CompFor<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for SetComp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for DictComp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct LeftCurlyBrace<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for LeftCurlyBrace<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for LeftCurlyBrace<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("{");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct RightCurlyBrace<'a> {
    pub whitespace_before: ParenthesizableWhitespace<'a>,
    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for RightCurlyBrace<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for RightCurlyBrace<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before.codegen(state);
        state.add_token("}");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for CompFor<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Asynchronous<'a> {
    pub whitespace_after: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for Asynchronous<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("async");
        self.whitespace_after.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for CompIf<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct List<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for List<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Set<'a> {
    pub elements: Vec<Element<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Set<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Dict<'a> {
    pub elements: Vec<DictElement<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for Dict<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl<'a> DictElement<'a> {
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

impl<'a> WithComma<'a> for DictElement<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct StarredDictElement<'a> {
    pub value: Expression<'a>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_before_value: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'a> StarredDictElement<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum BaseSlice<'a> {
    Index(Box<Index<'a>>),
    Slice(Box<Slice<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Index<'a> {
    pub value: Expression<'a>,
}

impl<'a> Inflate<'a> for Index<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for Index<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Slice<'a> {
    #[cfg_attr(feature = "py", no_py_default)]
    pub lower: Option<Expression<'a>>,
    #[cfg_attr(feature = "py", no_py_default)]
    pub upper: Option<Expression<'a>>,
    pub step: Option<Expression<'a>>,
    pub first_colon: Colon<'a>,
    pub second_colon: Option<Colon<'a>>,
}

impl<'a> Inflate<'a> for Slice<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lower = self.lower.inflate(config)?;
        self.first_colon = self.first_colon.inflate(config)?;
        self.upper = self.upper.inflate(config)?;
        self.second_colon = self.second_colon.inflate(config)?;
        self.step = self.step.inflate(config)?;
        Ok(self)
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct SubscriptElement<'a> {
    pub slice: BaseSlice<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> Inflate<'a> for SubscriptElement<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.slice = self.slice.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Subscript<'a> {
    pub value: Box<Expression<'a>>,
    pub slice: Vec<SubscriptElement<'a>>,
    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_value: ParenthesizableWhitespace<'a>,

    pub(crate) lbracket_tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Subscript<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for IfExp<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Lambda<'a> {
    pub params: Box<Parameters<'a>>,
    pub body: Box<Expression<'a>>,
    pub colon: Colon<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_lambda: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) lambda_tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Lambda<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for From<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum YieldValue<'a> {
    Expression(Box<Expression<'a>>),
    From(Box<From<'a>>),
}

impl<'a> Inflate<'a> for YieldValue<'a> {
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

impl<'a> YieldValue<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_space: &'a str) {
        match self {
            Self::Expression(e) => e.codegen(state),
            Self::From(f) => f.codegen(state, default_space),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Yield<'a> {
    pub value: Option<Box<YieldValue<'a>>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_yield: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) yield_tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Yield<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Await<'a> {
    pub expression: Box<Expression<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
    pub whitespace_after_await: ParenthesizableWhitespace<'a>,

    pub(crate) await_tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Await<'a> {
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

impl<'a> Codegen<'a> for Await<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            state.add_token("await");
            self.whitespace_after_await.codegen(state);
            self.expression.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum String<'a> {
    Simple(SimpleString<'a>),
    Concatenated(ConcatenatedString<'a>),
    Formatted(FormattedString<'a>),
}

impl<'a> std::convert::From<String<'a>> for Expression<'a> {
    fn from(s: String<'a>) -> Self {
        match s {
            String::Simple(s) => Self::SimpleString(Box::new(s)),
            String::Concatenated(s) => Self::ConcatenatedString(Box::new(s)),
            String::Formatted(s) => Self::FormattedString(Box::new(s)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for ConcatenatedString<'a> {
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

impl<'a> Codegen<'a> for ConcatenatedString<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.left.codegen(state);
            self.whitespace_between.codegen(state);
            self.right.codegen(state);
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct SimpleString<'a> {
    /// The texual representation of the string, including quotes, prefix
    /// characters, and any escape characters present in the original source code,
    /// such as ``r"my string\n"``.
    pub value: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for SimpleString<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for SimpleString<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| state.add_token(self.value))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct FormattedStringText<'a> {
    pub value: &'a str,
}

impl<'a> Inflate<'a> for FormattedStringText<'a> {
    fn inflate(self, _config: &Config<'a>) -> Result<Self> {
        Ok(self)
    }
}

impl<'a> Codegen<'a> for FormattedStringText<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token(self.value);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for FormattedStringExpression<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum FormattedStringContent<'a> {
    Text(FormattedStringText<'a>),
    Expression(Box<FormattedStringExpression<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct FormattedString<'a> {
    pub parts: Vec<FormattedStringContent<'a>>,
    pub start: &'a str,
    pub end: &'a str,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Inflate<'a> for FormattedString<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.parts = self.parts.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
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

impl<'a> Inflate<'a> for NamedExpr<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
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

#[cfg(feature = "py")]
mod py {

    use pyo3::types::PyModule;

    use super::*;
    use crate::nodes::traits::py::TryIntoPy;

    // TODO: this could be a derive helper attribute to override the python class name
    impl<'a> TryIntoPy<pyo3::PyObject> for Element<'a> {
        fn try_into_py(self, py: pyo3::Python) -> pyo3::PyResult<pyo3::PyObject> {
            match self {
                Self::Starred(s) => s.try_into_py(py),
                Self::Simple { value, comma } => {
                    let libcst = PyModule::import(py, "libcst")?;
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
                    .into_py_dict(py);
                    Ok(libcst
                        .getattr("Element")
                        .expect("no Element found in libcst")
                        .call((), Some(kwargs))?
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
                    let libcst = PyModule::import(py, "libcst")?;
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
                    .into_py_dict(py);
                    Ok(libcst
                        .getattr("DictElement")
                        .expect("no Element found in libcst")
                        .call((), Some(kwargs))?
                        .into())
                }
            }
        }
    }
}
