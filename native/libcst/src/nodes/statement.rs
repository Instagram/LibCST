// Copyright (c) Meta Platforms, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::mem::swap;

use super::{
    inflate_helpers::adjust_parameters_trailing_whitespace, op::SemicolonTokens, Attribute,
    Codegen, CodegenState, Comma, Dot, EmptyLine, Expression, From, ImportStar, LeftParen, List,
    Name, NameOrAttribute, Parameters, ParenthesizableWhitespace, RightParen, Semicolon,
    SimpleWhitespace, StarredElement, Subscript, TrailingWhitespace, Tuple,
};
use crate::{
    nodes::{
        common::TokenRef,
        traits::{Inflate, Result, WithComma, WithLeadingLines},
        Arg, AssignEqual, Asynchronous, AugOp, BitOr, Element, ParenthesizedNode,
    },
    tokenizer::whitespace_parser::{
        parse_empty_lines, parse_parenthesizable_whitespace, parse_simple_whitespace,
        parse_trailing_whitespace, Config,
    },
    LeftCurlyBrace, LeftSquareBracket, RightCurlyBrace, RightSquareBracket,
};
use libcst_derive::{Codegen, Inflate, IntoPy, ParenthesizedNode};

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq, Clone, Inflate, Codegen, IntoPy)]
pub enum Statement<'r, 'a> {
    Simple(SimpleStatementLine<'r, 'a>),
    Compound(CompoundStatement<'r, 'a>),
}

impl<'r, 'a> WithLeadingLines<'a> for Statement<'r, 'a> {
    fn leading_lines(&mut self) -> &mut Vec<EmptyLine<'a>> {
        match self {
            Self::Simple(s) => &mut s.leading_lines,
            Self::Compound(c) => c.leading_lines(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Inflate, Codegen, IntoPy)]
#[allow(clippy::large_enum_variant)]
pub enum CompoundStatement<'r, 'a> {
    FunctionDef(FunctionDef<'r, 'a>),
    If(If<'r, 'a>),
    For(For<'r, 'a>),
    While(While<'r, 'a>),
    ClassDef(ClassDef<'r, 'a>),
    Try(Try<'r, 'a>),
    TryStar(TryStar<'r, 'a>),
    With(With<'r, 'a>),
    Match(Match<'r, 'a>),
}

impl<'r, 'a> WithLeadingLines<'a> for CompoundStatement<'r, 'a> {
    fn leading_lines(&mut self) -> &mut Vec<EmptyLine<'a>> {
        match self {
            Self::FunctionDef(f) => &mut f.leading_lines,
            Self::If(f) => &mut f.leading_lines,
            Self::For(f) => &mut f.leading_lines,
            Self::While(f) => &mut f.leading_lines,
            Self::ClassDef(c) => &mut c.leading_lines,
            Self::Try(t) => &mut t.leading_lines,
            Self::TryStar(t) => &mut t.leading_lines,
            Self::With(w) => &mut w.leading_lines,
            Self::Match(m) => &mut m.leading_lines,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Inflate, Codegen, IntoPy)]
pub enum Suite<'r, 'a> {
    IndentedBlock(IndentedBlock<'r, 'a>),
    SimpleStatementSuite(SimpleStatementSuite<'r, 'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct IndentedBlock<'r, 'a> {
    /// Sequence of statements belonging to this indented block.
    pub body: Vec<Statement<'r, 'a>>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub header: TrailingWhitespace<'a>,
    /// A string represents a specific indentation. A ``None`` value uses the modules's
    /// default indentation. This is included because indentation is allowed to be
    /// inconsistent across a file, just not ambiguously.
    pub indent: Option<&'a str>,
    /// Any trailing comments or lines after the dedent that are owned by this indented
    /// block. Statements own preceeding and same-line trailing comments, but not
    /// trailing lines, so it falls on :class:`IndentedBlock` to own it. In the case
    /// that a statement follows an :class:`IndentedBlock`, that statement will own the
    /// comments and lines that are at the same indent as the statement, and this
    /// :class:`IndentedBlock` will own the comments and lines that are indented
    /// further.
    pub footer: Vec<EmptyLine<'a>>,

    pub(crate) newline_tok: TokenRef<'r, 'a>,
    pub(crate) indent_tok: TokenRef<'r, 'a>,
    pub(crate) dedent_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for IndentedBlock<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.header.codegen(state);

        let indent = match self.indent {
            Some(i) => i,
            None => state.default_indent,
        };
        state.indent(indent);

        if self.body.is_empty() {
            // Empty indented blocks are not syntactically valid in Python unless they
            // contain a 'pass' statement, so add one here.
            state.add_indent();
            state.add_token("pass");
            state.add_token(state.default_newline);
        } else {
            for stmt in &self.body {
                // IndentedBlock is responsible for adjusting the current indentation
                // level, but its children are responsible for actually adding that
                // indentation to the token list.
                stmt.codegen(state);
            }
        }

        for f in &self.footer {
            f.codegen(state);
        }

        state.dedent();
    }
}

impl<'r, 'a> Inflate<'a> for IndentedBlock<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.body = self.body.inflate(config)?;
        // We want to be able to only keep comments in the footer that are actually for
        // this IndentedBlock. We do so by assuming that lines which are indented to the
        // same level as the block itself are comments that go at the footer of the
        // block. Comments that are indented to less than this indent are assumed to
        // belong to the next line of code. We override the indent here because the
        // dedent node's absolute indent is the resulting indentation after the dedent
        // is performed. Its this way because the whitespace state for both the dedent's
        // whitespace_after and the next BaseCompoundStatement's whitespace_before is
        // shared. This allows us to partially parse here and parse the rest of the
        // whitespace and comments on the next line, effectively making sure that
        // comments are attached to the correct node.
        let footer = parse_empty_lines(
            config,
            &mut (*self.dedent_tok).whitespace_after.borrow_mut(),
            Some(self.indent_tok.whitespace_before.borrow().absolute_indent),
        )?;
        let header = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        self.footer = footer;
        self.header = header;
        self.indent = self.indent_tok.relative_indent;
        if self.indent == Some(config.default_indent) {
            self.indent = None;
        }
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct SimpleStatementSuite<'r, 'a> {
    /// Sequence of small statements. All but the last statement are required to have
    /// a semicolon.
    pub body: Vec<SmallStatement<'r, 'a>>,

    /// The whitespace between the colon in the parent statement and the body.
    pub leading_whitespace: SimpleWhitespace<'a>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub trailing_whitespace: TrailingWhitespace<'a>,

    pub(crate) first_tok: TokenRef<'r, 'a>,
    pub(crate) newline_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for SimpleStatementSuite<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_whitespace = parse_simple_whitespace(
            config,
            &mut (*self.first_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;
        self.trailing_whitespace = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

fn _simple_statement_codegen<'r, 'a>(
    body: &[SmallStatement<'r, 'a>],
    trailing_whitespace: &TrailingWhitespace<'a>,
    state: &mut CodegenState<'a>,
) {
    for stmt in body {
        stmt.codegen(state);
        // TODO: semicolon
    }
    if body.is_empty() {
        // Empty simple statement blocks are not syntactically valid in Python
        // unless they contain a 'pass' statement, so add one here.
        state.add_token("pass")
    }
    trailing_whitespace.codegen(state);
}

impl<'r, 'a> Codegen<'a> for SimpleStatementSuite<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.leading_whitespace.codegen(state);
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct SimpleStatementLine<'r, 'a> {
    /// Sequence of small statements. All but the last statement are required to have
    /// a semicolon.
    pub body: Vec<SmallStatement<'r, 'a>>,

    /// Sequence of empty lines appearing before this simple statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub trailing_whitespace: TrailingWhitespace<'a>,

    pub(crate) first_tok: TokenRef<'r, 'a>,
    pub(crate) newline_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for SimpleStatementLine<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for line in &self.leading_lines {
            line.codegen(state);
        }
        state.add_indent();
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

impl<'r, 'a> Inflate<'a> for SimpleStatementLine<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.first_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.body = self.body.inflate(config)?;
        self.trailing_whitespace = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

#[allow(dead_code, clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq, Clone, Codegen, Inflate, IntoPy)]
pub enum SmallStatement<'r, 'a> {
    Pass(Pass<'r, 'a>),
    Break(Break<'r, 'a>),
    Continue(Continue<'r, 'a>),
    Return(Return<'r, 'a>),
    Expr(Expr<'r, 'a>),
    Assert(Assert<'r, 'a>),
    Import(Import<'r, 'a>),
    ImportFrom(ImportFrom<'r, 'a>),
    Assign(Assign<'r, 'a>),
    AnnAssign(AnnAssign<'r, 'a>),
    Raise(Raise<'r, 'a>),
    Global(Global<'r, 'a>),
    Nonlocal(Nonlocal<'r, 'a>),
    AugAssign(AugAssign<'r, 'a>),
    Del(Del<'r, 'a>),
}

impl<'r, 'a> SmallStatement<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        match self {
            Self::Pass(p) => Self::Pass(p.with_semicolon(semicolon)),
            Self::Break(p) => Self::Break(p.with_semicolon(semicolon)),
            Self::Continue(p) => Self::Continue(p.with_semicolon(semicolon)),
            Self::Expr(p) => Self::Expr(p.with_semicolon(semicolon)),
            Self::Import(i) => Self::Import(i.with_semicolon(semicolon)),
            Self::ImportFrom(i) => Self::ImportFrom(i.with_semicolon(semicolon)),
            Self::Assign(a) => Self::Assign(a.with_semicolon(semicolon)),
            Self::AnnAssign(a) => Self::AnnAssign(a.with_semicolon(semicolon)),
            Self::Return(r) => Self::Return(r.with_semicolon(semicolon)),
            Self::Assert(a) => Self::Assert(a.with_semicolon(semicolon)),
            Self::Raise(r) => Self::Raise(r.with_semicolon(semicolon)),
            Self::Global(g) => Self::Global(g.with_semicolon(semicolon)),
            Self::Nonlocal(l) => Self::Nonlocal(l.with_semicolon(semicolon)),
            Self::AugAssign(a) => Self::AugAssign(a.with_semicolon(semicolon)),
            Self::Del(d) => Self::Del(d.with_semicolon(semicolon)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Pass<'r, 'a> {
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}
impl<'r, 'a> Pass<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'r, 'a> Codegen<'a> for Pass<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("pass");
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for Pass<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Break<'r, 'a> {
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}
impl<'r, 'a> Break<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'r, 'a> Codegen<'a> for Break<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("break");
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for Break<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Continue<'r, 'a> {
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}
impl<'r, 'a> Continue<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'r, 'a> Codegen<'a> for Continue<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("continue");
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for Continue<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Expr<'r, 'a> {
    pub value: Expression<'r, 'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}
impl<'r, 'a> Expr<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}
impl<'r, 'a> Codegen<'a> for Expr<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state);
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for Expr<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Assign<'r, 'a> {
    pub targets: Vec<AssignTarget<'r, 'a>>,
    pub value: Expression<'r, 'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for Assign<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for target in &self.targets {
            target.codegen(state);
        }
        self.value.codegen(state);
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for Assign<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.targets = self.targets.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Assign<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct AssignTarget<'r, 'a> {
    pub target: AssignTargetExpression<'r, 'a>,
    pub whitespace_before_equal: SimpleWhitespace<'a>,
    pub whitespace_after_equal: SimpleWhitespace<'a>,

    pub(crate) equal_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for AssignTarget<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.whitespace_before_equal.codegen(state);
        state.add_token("=");
        self.whitespace_after_equal.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for AssignTarget<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.target = self.target.inflate(config)?;
        self.whitespace_before_equal = parse_simple_whitespace(
            config,
            &mut (*self.equal_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_equal =
            parse_simple_whitespace(config, &mut (*self.equal_tok).whitespace_after.borrow_mut())?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, ParenthesizedNode, Inflate, IntoPy)]
pub enum AssignTargetExpression<'r, 'a> {
    Name(Name<'r, 'a>),
    Attribute(Attribute<'r, 'a>),
    StarredElement(StarredElement<'r, 'a>),
    Tuple(Tuple<'r, 'a>),
    List(List<'r, 'a>),
    Subscript(Subscript<'r, 'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Import<'r, 'a> {
    pub names: Vec<ImportAlias<'r, 'a>>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
    pub whitespace_after_import: SimpleWhitespace<'a>,

    pub(crate) import_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Import<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("import");
        self.whitespace_after_import.codegen(state);
        for (i, name) in self.names.iter().enumerate() {
            name.codegen(state);
            if name.comma.is_none() && i < self.names.len() - 1 {
                state.add_token(", ");
            }
        }
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for Import<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_import = parse_simple_whitespace(
            config,
            &mut (*self.import_tok).whitespace_after.borrow_mut(),
        )?;
        self.names = self.names.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Import<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ImportFrom<'r, 'a> {
    #[no_py_default]
    pub module: Option<NameOrAttribute<'r, 'a>>,
    pub names: ImportNames<'r, 'a>,
    pub relative: Vec<Dot<'r, 'a>>,
    pub lpar: Option<LeftParen<'r, 'a>>,
    pub rpar: Option<RightParen<'r, 'a>>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
    pub whitespace_after_from: SimpleWhitespace<'a>,
    pub whitespace_before_import: SimpleWhitespace<'a>,
    pub whitespace_after_import: SimpleWhitespace<'a>,

    pub(crate) from_tok: TokenRef<'r, 'a>,
    pub(crate) import_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for ImportFrom<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("from");
        self.whitespace_after_from.codegen(state);
        for dot in &self.relative {
            dot.codegen(state);
        }
        if let Some(module) = &self.module {
            module.codegen(state);
        }
        self.whitespace_before_import.codegen(state);
        state.add_token("import");
        self.whitespace_after_import.codegen(state);
        if let Some(lpar) = &self.lpar {
            lpar.codegen(state);
        }
        self.names.codegen(state);
        if let Some(rpar) = &self.rpar {
            rpar.codegen(state);
        }

        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for ImportFrom<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_from =
            parse_simple_whitespace(config, &mut (*self.from_tok).whitespace_after.borrow_mut())?;

        self.module = self.module.inflate(config)?;

        self.whitespace_after_import = parse_simple_whitespace(
            config,
            &mut (*self.import_tok).whitespace_after.borrow_mut(),
        )?;

        self.relative = inflate_dots(self.relative, config)?;

        if !self.relative.is_empty() && self.module.is_none() {
            // For relative-only imports relocate the space after the final dot to be owned
            // by the import token.
            if let Some(Dot {
                whitespace_after: ParenthesizableWhitespace::SimpleWhitespace(dot_ws),
                ..
            }) = self.relative.last_mut()
            {
                swap(dot_ws, &mut self.whitespace_before_import);
            }
        } else {
            self.whitespace_before_import = parse_simple_whitespace(
                config,
                &mut (*self.import_tok).whitespace_before.borrow_mut(),
            )?;
        }

        self.lpar = self.lpar.inflate(config)?;
        self.names = self.names.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;

        self.semicolon = self.semicolon.inflate(config)?;

        Ok(self)
    }
}

fn inflate_dots<'r, 'a>(dots: Vec<Dot<'r, 'a>>, config: &Config<'a>) -> Result<Vec<Dot<'r, 'a>>> {
    let mut ret: Vec<Dot<'r, 'a>> = vec![];
    let mut last_tok: Option<TokenRef<'r, 'a>> = None;
    for mut dot in dots {
        if let Some(last_tokref) = &last_tok {
            // Consecutive dots having the same Token can only happen if `...` was
            // parsed as a single ELLIPSIS token. In this case the token's
            // whitespace_before belongs to the first dot, but the whitespace_after is
            // moved to the 3rd dot (by swapping it twice)
            if last_tokref.start_pos == dot.tok.start_pos {
                swap(
                    &mut ret.last_mut().unwrap().whitespace_after,
                    &mut dot.whitespace_after,
                );
                ret.push(dot);
                continue;
            }
        }
        last_tok = Some(&dot.tok);
        ret.push(dot.inflate(config)?);
    }
    Ok(ret)
}

impl<'r, 'a> ImportFrom<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ImportAlias<'r, 'a> {
    pub name: NameOrAttribute<'r, 'a>,
    pub asname: Option<AsName<'r, 'a>>,
    pub comma: Option<Comma<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for ImportAlias<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.name = self.name.inflate(config)?;
        self.asname = self.asname.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> WithComma<'r, 'a> for ImportAlias<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> ImportAlias<'r, 'a> {
        let comma = Some(comma);
        Self { comma, ..self }
    }
}

impl<'r, 'a> Codegen<'a> for ImportAlias<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.name.codegen(state);
        if let Some(asname) = &self.asname {
            asname.codegen(state);
        }
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct AsName<'r, 'a> {
    pub name: AssignTargetExpression<'r, 'a>,
    pub whitespace_before_as: ParenthesizableWhitespace<'a>,
    pub whitespace_after_as: ParenthesizableWhitespace<'a>,

    pub(crate) as_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for AsName<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before_as.codegen(state);
        state.add_token("as");
        self.whitespace_after_as.codegen(state);
        self.name.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for AsName<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before_as = parse_parenthesizable_whitespace(
            config,
            &mut (*self.as_tok).whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_as = parse_parenthesizable_whitespace(
            config,
            &mut (*self.as_tok).whitespace_after.borrow_mut(),
        )?;
        self.name = self.name.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Inflate, IntoPy)]
pub enum ImportNames<'r, 'a> {
    Star(ImportStar),
    Aliases(Vec<ImportAlias<'r, 'a>>),
}

impl<'r, 'a> Codegen<'a> for ImportNames<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        match self {
            Self::Star(s) => s.codegen(state),
            Self::Aliases(aliases) => {
                for (i, alias) in aliases.iter().enumerate() {
                    alias.codegen(state);
                    if alias.comma.is_none() && i < aliases.len() - 1 {
                        state.add_token(", ");
                    }
                }
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct FunctionDef<'r, 'a> {
    pub name: Name<'r, 'a>,
    pub params: Parameters<'r, 'a>,
    pub body: Suite<'r, 'a>,
    pub decorators: Vec<Decorator<'r, 'a>>,
    pub returns: Option<Annotation<'r, 'a>>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lines_after_decorators: Vec<EmptyLine<'a>>,
    pub whitespace_after_def: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_params: ParenthesizableWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'r, 'a>>,
    pub(crate) def_tok: TokenRef<'r, 'a>,
    pub(crate) open_paren_tok: TokenRef<'r, 'a>,
    pub(crate) close_paren_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> FunctionDef<'r, 'a> {
    pub fn with_decorators(self, decorators: Vec<Decorator<'r, 'a>>) -> Self {
        Self { decorators, ..self }
    }
}

impl<'r, 'a> Codegen<'a> for FunctionDef<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        for dec in self.decorators.iter() {
            dec.codegen(state);
        }
        for l in &self.lines_after_decorators {
            l.codegen(state);
        }
        state.add_indent();

        if let Some(asy) = &self.asynchronous {
            asy.codegen(state);
        }
        state.add_token("def");
        self.whitespace_after_def.codegen(state);
        self.name.codegen(state);
        self.whitespace_after_name.codegen(state);
        state.add_token("(");
        self.whitespace_before_params.codegen(state);
        self.params.codegen(state);
        state.add_token(")");

        if let Some(ann) = &self.returns {
            ann.codegen(state, "->");
        }

        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for FunctionDef<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.decorators = self.decorators.inflate(config)?;
        let (asynchronous, leading_lines) = if let Some(asy) = self.async_tok.as_mut() {
            let whitespace_after =
                parse_parenthesizable_whitespace(config, &mut asy.whitespace_after.borrow_mut())?;
            (
                Some(Asynchronous { whitespace_after }),
                Some(parse_empty_lines(
                    config,
                    &mut asy.whitespace_before.borrow_mut(),
                    None,
                )?),
            )
        } else {
            (None, None)
        };

        self.asynchronous = asynchronous;
        let leading_lines = if let Some(ll) = leading_lines {
            ll
        } else {
            parse_empty_lines(
                config,
                &mut (*self.def_tok).whitespace_before.borrow_mut(),
                None,
            )?
        };

        self.leading_lines = leading_lines;
        if let Some(dec) = self.decorators.first_mut() {
            swap(&mut self.lines_after_decorators, &mut self.leading_lines);
            swap(&mut dec.leading_lines, &mut self.leading_lines);
        }

        self.whitespace_after_def =
            parse_simple_whitespace(config, &mut (*self.def_tok).whitespace_after.borrow_mut())?;

        self.name = self.name.inflate(config)?;
        self.whitespace_after_name = parse_simple_whitespace(
            config,
            &mut (*self.open_paren_tok).whitespace_before.borrow_mut(),
        )?;

        self.whitespace_before_params = parse_parenthesizable_whitespace(
            config,
            &mut (*self.open_paren_tok).whitespace_after.borrow_mut(),
        )?;
        self.params = self.params.inflate(config)?;
        adjust_parameters_trailing_whitespace(config, &mut self.params, &self.close_paren_tok)?;

        self.returns = self.returns.inflate(config)?;
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;

        self.body = self.body.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, IntoPy)]
pub struct Decorator<'r, 'a> {
    pub decorator: Expression<'r, 'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_at: SimpleWhitespace<'a>,
    pub trailing_whitespace: TrailingWhitespace<'a>,

    pub(crate) at_tok: TokenRef<'r, 'a>,
    pub(crate) newline_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Decorator<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in self.leading_lines.iter() {
            ll.codegen(state);
        }
        state.add_indent();
        state.add_token("@");
        self.whitespace_after_at.codegen(state);
        self.decorator.codegen(state);
        self.trailing_whitespace.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for Decorator<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.at_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_after_at =
            parse_simple_whitespace(config, &mut (*self.at_tok).whitespace_after.borrow_mut())?;
        self.decorator = self.decorator.inflate(config)?;
        self.trailing_whitespace = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(self)
    }
}

impl<'r, 'a> pyo3::conversion::IntoPy<pyo3::PyObject> for Box<OrElse<'r, 'a>> {
    fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
        (*self).into_py(py)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct If<'r, 'a> {
    /// The expression that, when evaluated, should give us a truthy value
    pub test: Expression<'r, 'a>,
    // The body of this compound statement.
    pub body: Suite<'r, 'a>,

    /// An optional ``elif`` or ``else`` clause. ``If`` signifies an ``elif`` block.
    pub orelse: Option<Box<OrElse<'r, 'a>>>,

    /// Sequence of empty lines appearing before this compound statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,

    /// The whitespace appearing after the ``if`` keyword but before the test
    /// expression.
    pub whitespace_before_test: SimpleWhitespace<'a>,

    /// The whitespace appearing after the test expression but before the colon.
    pub whitespace_after_test: SimpleWhitespace<'a>,

    /// Signifies if this instance represents an ``elif`` or an ``if`` block.
    #[skip_py]
    pub is_elif: bool,

    pub(crate) if_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for If<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        state.add_indent();

        state.add_token(if self.is_elif { "elif" } else { "if" });
        self.whitespace_before_test.codegen(state);
        self.test.codegen(state);
        self.whitespace_after_test.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
        if let Some(orelse) = &self.orelse {
            orelse.codegen(state)
        }
    }
}

impl<'r, 'a> Inflate<'a> for If<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.if_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_before_test =
            parse_simple_whitespace(config, &mut (*self.if_tok).whitespace_after.borrow_mut())?;
        self.test = self.test.inflate(config)?;
        self.whitespace_after_test = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;
        self.orelse = self.orelse.inflate(config)?;

        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Inflate, Codegen, IntoPy)]
pub enum OrElse<'r, 'a> {
    Elif(If<'r, 'a>),
    Else(Else<'r, 'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Else<'r, 'a> {
    pub body: Suite<'r, 'a>,
    /// Sequence of empty lines appearing before this compound statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,
    /// The whitespace appearing after the ``else`` keyword but before the colon.
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) else_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Else<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        state.add_indent();

        state.add_token("else");
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for Else<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.else_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;

        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Annotation<'r, 'a> {
    pub annotation: Expression<'r, 'a>,
    pub whitespace_before_indicator: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_indicator: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Annotation<'r, 'a> {
    pub fn codegen(&self, state: &mut CodegenState<'a>, default_indicator: &'a str) {
        if let Some(ws) = &self.whitespace_before_indicator {
            ws.codegen(state);
        } else if default_indicator == "->" {
            state.add_token(" ");
        } else {
            panic!("Variable annotation but whitespace is None");
        }

        state.add_token(default_indicator);
        self.whitespace_after_indicator.codegen(state);
        self.annotation.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for Annotation<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_before_indicator = Some(parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?);
        self.whitespace_after_indicator = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        self.annotation = self.annotation.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct AnnAssign<'r, 'a> {
    pub target: AssignTargetExpression<'r, 'a>,
    pub annotation: Annotation<'r, 'a>,
    pub value: Option<Expression<'r, 'a>>,
    pub equal: Option<AssignEqual<'r, 'a>>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for AnnAssign<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.annotation.codegen(state, ":");
        if let Some(eq) = &self.equal {
            eq.codegen(state);
        } else if self.value.is_some() {
            state.add_token(" = ");
        }
        if let Some(value) = &self.value {
            value.codegen(state);
        }

        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for AnnAssign<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.target = self.target.inflate(config)?;
        self.annotation = self.annotation.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.equal = self.equal.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> AnnAssign<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Return<'r, 'a> {
    pub value: Option<Expression<'r, 'a>>,
    pub whitespace_after_return: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,

    pub(crate) return_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Return<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("return");
        if let Some(ws) = &self.whitespace_after_return {
            ws.codegen(state);
        } else if self.value.is_some() {
            state.add_token(" ");
        }

        if let Some(val) = &self.value {
            val.codegen(state);
        }
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for Return<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        if self.value.is_some() {
            self.whitespace_after_return = Some(parse_simple_whitespace(
                config,
                &mut (*self.return_tok).whitespace_after.borrow_mut(),
            )?);
        } else {
            // otherwise space is owned by semicolon or small statement
            // whitespace is not None to preserve a quirk of the pure python parser
            self.whitespace_after_return = Some(Default::default())
        }
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Return<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Assert<'r, 'a> {
    pub test: Expression<'r, 'a>,
    pub msg: Option<Expression<'r, 'a>>,
    pub comma: Option<Comma<'r, 'a>>,
    pub whitespace_after_assert: SimpleWhitespace<'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,

    pub(crate) assert_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Assert<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("assert");
        self.whitespace_after_assert.codegen(state);
        self.test.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        } else if self.msg.is_some() {
            state.add_token(", ");
        }
        if let Some(msg) = &self.msg {
            msg.codegen(state);
        }
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}
impl<'r, 'a> Inflate<'a> for Assert<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_assert = parse_simple_whitespace(
            config,
            &mut (*self.assert_tok).whitespace_after.borrow_mut(),
        )?;

        self.test = self.test.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        self.msg = self.msg.inflate(config)?;

        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Assert<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Raise<'r, 'a> {
    pub exc: Option<Expression<'r, 'a>>,
    pub cause: Option<From<'r, 'a>>,
    pub whitespace_after_raise: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,

    pub(crate) raise_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Raise<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        if self.exc.is_some() {
            self.whitespace_after_raise = Some(parse_simple_whitespace(
                config,
                &mut (*self.raise_tok).whitespace_after.borrow_mut(),
            )?);
        }

        self.exc = self.exc.inflate(config)?;
        self.cause = self.cause.inflate(config)?;
        if self.exc.is_none() {
            if let Some(cause) = self.cause.as_mut() {
                // in `raise from`, `raise` owns the shared whitespace
                cause.whitespace_before_from = None;
            }
        }
        self.semicolon = self.semicolon.inflate(config)?;

        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Raise<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("raise");
        if let Some(ws) = &self.whitespace_after_raise {
            ws.codegen(state);
        } else if self.exc.is_some() {
            state.add_token(" ");
        }

        if let Some(exc) = &self.exc {
            exc.codegen(state);
        }

        if let Some(cause) = &self.cause {
            cause.codegen(state, " ");
        }

        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Raise<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct NameItem<'r, 'a> {
    pub name: Name<'r, 'a>,
    pub comma: Option<Comma<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for NameItem<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.name = self.name.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> NameItem<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        self.name.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        } else if default_comma {
            state.add_token(", ");
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Global<'r, 'a> {
    pub names: Vec<NameItem<'r, 'a>>,
    pub whitespace_after_global: SimpleWhitespace<'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,

    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Global<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_global =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        self.names = self.names.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Global<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("global");
        self.whitespace_after_global.codegen(state);
        let len = self.names.len();
        for (i, name) in self.names.iter().enumerate() {
            name.codegen(state, i + 1 != len);
        }

        if let Some(semicolon) = &self.semicolon {
            semicolon.codegen(state);
        }
    }
}

impl<'r, 'a> Global<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Nonlocal<'r, 'a> {
    pub names: Vec<NameItem<'r, 'a>>,
    pub whitespace_after_nonlocal: SimpleWhitespace<'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,

    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Nonlocal<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_nonlocal =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        self.names = self.names.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Nonlocal<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("nonlocal");
        self.whitespace_after_nonlocal.codegen(state);
        let len = self.names.len();
        for (i, name) in self.names.iter().enumerate() {
            name.codegen(state, i + 1 != len);
        }

        if let Some(semicolon) = &self.semicolon {
            semicolon.codegen(state);
        }
    }
}

impl<'r, 'a> Nonlocal<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct For<'r, 'a> {
    pub target: AssignTargetExpression<'r, 'a>,
    pub iter: Expression<'r, 'a>,
    pub body: Suite<'r, 'a>,
    pub orelse: Option<Else<'r, 'a>>,
    pub asynchronous: Option<Asynchronous<'a>>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_for: SimpleWhitespace<'a>,
    pub whitespace_before_in: SimpleWhitespace<'a>,
    pub whitespace_after_in: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'r, 'a>>,
    pub(crate) for_tok: TokenRef<'r, 'a>,
    pub(crate) in_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for For<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();

        if let Some(asy) = &self.asynchronous {
            asy.codegen(state);
        }
        state.add_token("for");
        self.whitespace_after_for.codegen(state);
        self.target.codegen(state);
        self.whitespace_before_in.codegen(state);
        state.add_token("in");
        self.whitespace_after_in.codegen(state);
        self.iter.codegen(state);
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
        if let Some(e) = &self.orelse {
            e.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for For<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        let (asynchronous, leading_lines) = if let Some(asy) = self.async_tok.as_mut() {
            let whitespace_after =
                parse_parenthesizable_whitespace(config, &mut asy.whitespace_after.borrow_mut())?;
            (
                Some(Asynchronous { whitespace_after }),
                Some(parse_empty_lines(
                    config,
                    &mut asy.whitespace_before.borrow_mut(),
                    None,
                )?),
            )
        } else {
            (None, None)
        };
        self.leading_lines = if let Some(ll) = leading_lines {
            ll
        } else {
            parse_empty_lines(
                config,
                &mut (*self.for_tok).whitespace_before.borrow_mut(),
                None,
            )?
        };
        self.asynchronous = asynchronous;
        self.whitespace_after_for =
            parse_simple_whitespace(config, &mut (*self.for_tok).whitespace_after.borrow_mut())?;
        self.target = self.target.inflate(config)?;
        self.whitespace_before_in =
            parse_simple_whitespace(config, &mut (*self.in_tok).whitespace_before.borrow_mut())?;
        self.whitespace_after_in =
            parse_simple_whitespace(config, &mut (*self.in_tok).whitespace_after.borrow_mut())?;
        self.iter = self.iter.inflate(config)?;
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;

        self.body = self.body.inflate(config)?;
        self.orelse = self.orelse.inflate(config)?;

        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct While<'r, 'a> {
    pub test: Expression<'r, 'a>,
    pub body: Suite<'r, 'a>,
    pub orelse: Option<Else<'r, 'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_while: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) while_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for While<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();

        state.add_token("while");
        self.whitespace_after_while.codegen(state);
        self.test.codegen(state);
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
        if let Some(orelse) = &self.orelse {
            orelse.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for While<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.while_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_after_while =
            parse_simple_whitespace(config, &mut (*self.while_tok).whitespace_after.borrow_mut())?;
        self.test = self.test.inflate(config)?;
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;
        self.orelse = self.orelse.inflate(config)?;

        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ClassDef<'r, 'a> {
    pub name: Name<'r, 'a>,
    pub body: Suite<'r, 'a>,
    pub bases: Vec<Arg<'r, 'a>>,
    pub keywords: Vec<Arg<'r, 'a>>,
    pub decorators: Vec<Decorator<'r, 'a>>,
    pub lpar: Option<LeftParen<'r, 'a>>,
    pub rpar: Option<RightParen<'r, 'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lines_after_decorators: Vec<EmptyLine<'a>>,
    pub whitespace_after_class: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) class_tok: TokenRef<'r, 'a>,
    pub(crate) parens_tok: Option<(TokenRef<'r, 'a>, TokenRef<'r, 'a>)>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for ClassDef<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        for dec in &self.decorators {
            dec.codegen(state);
        }
        for lad in &self.lines_after_decorators {
            lad.codegen(state);
        }
        state.add_indent();

        state.add_token("class");
        self.whitespace_after_class.codegen(state);
        self.name.codegen(state);
        self.whitespace_after_name.codegen(state);

        let need_parens = !self.bases.is_empty() || !self.keywords.is_empty();

        if let Some(lpar) = &self.lpar {
            lpar.codegen(state);
        } else if need_parens {
            state.add_token("(");
        }
        let args = self.bases.iter().chain(self.keywords.iter());
        let len = self.bases.len() + self.keywords.len();
        for (i, arg) in args.enumerate() {
            arg.codegen(state, i + 1 < len);
        }

        if let Some(rpar) = &self.rpar {
            rpar.codegen(state);
        } else if need_parens {
            state.add_token(")");
        }

        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for ClassDef<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.class_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.decorators = self.decorators.inflate(config)?;
        if let Some(dec) = self.decorators.first_mut() {
            swap(&mut self.lines_after_decorators, &mut self.leading_lines);
            swap(&mut dec.leading_lines, &mut self.leading_lines);
        }

        self.whitespace_after_class =
            parse_simple_whitespace(config, &mut (*self.class_tok).whitespace_after.borrow_mut())?;
        self.name = self.name.inflate(config)?;

        if let Some((lpar_tok, _)) = self.parens_tok.as_mut() {
            self.whitespace_after_name =
                parse_simple_whitespace(config, &mut lpar_tok.whitespace_before.borrow_mut())?;
            self.lpar = self.lpar.map(|lpar| lpar.inflate(config)).transpose()?;
            self.bases = self.bases.inflate(config)?;
            self.keywords = self.keywords.inflate(config)?;
            self.rpar = self.rpar.map(|lpar| lpar.inflate(config)).transpose()?;
            // TODO: set whitespace_after_arg for last arg?
        }

        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;

        Ok(self)
    }
}

impl<'r, 'a> ClassDef<'r, 'a> {
    pub fn with_decorators(self, decorators: Vec<Decorator<'r, 'a>>) -> Self {
        Self { decorators, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Finally<'r, 'a> {
    pub body: Suite<'r, 'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) finally_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Finally<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();

        state.add_token("finally");
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for Finally<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.finally_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ExceptHandler<'r, 'a> {
    pub body: Suite<'r, 'a>,
    pub r#type: Option<Expression<'r, 'a>>,
    pub name: Option<AsName<'r, 'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_except: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) except_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for ExceptHandler<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();

        state.add_token("except");
        self.whitespace_after_except.codegen(state);
        if let Some(t) = &self.r#type {
            t.codegen(state);
        }
        if let Some(n) = &self.name {
            n.codegen(state);
        }
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for ExceptHandler<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.except_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_after_except = parse_simple_whitespace(
            config,
            &mut (*self.except_tok).whitespace_after.borrow_mut(),
        )?;

        self.r#type = self.r#type.inflate(config)?;
        self.name = self.name.inflate(config)?;
        if self.name.is_some() {
            self.whitespace_before_colon = parse_simple_whitespace(
                config,
                &mut (*self.colon_tok).whitespace_before.borrow_mut(),
            )?;
        }

        self.body = self.body.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct ExceptStarHandler<'r, 'a> {
    pub body: Suite<'r, 'a>,
    pub r#type: Expression<'r, 'a>,
    pub name: Option<AsName<'r, 'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_except: SimpleWhitespace<'a>,
    pub whitespace_after_star: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) except_tok: TokenRef<'r, 'a>,
    pub(crate) star_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for ExceptStarHandler<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();

        state.add_token("except");
        self.whitespace_after_except.codegen(state);
        state.add_token("*");
        self.whitespace_after_star.codegen(state);
        self.r#type.codegen(state);
        if let Some(n) = &self.name {
            n.codegen(state);
        }
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for ExceptStarHandler<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut self.except_tok.whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_after_except =
            parse_simple_whitespace(config, &mut self.except_tok.whitespace_after.borrow_mut())?;
        self.whitespace_after_star =
            parse_simple_whitespace(config, &mut self.star_tok.whitespace_after.borrow_mut())?;

        self.r#type = self.r#type.inflate(config)?;
        self.name = self.name.inflate(config)?;
        if self.name.is_some() {
            self.whitespace_before_colon = parse_simple_whitespace(
                config,
                &mut self.colon_tok.whitespace_before.borrow_mut(),
            )?;
        }

        self.body = self.body.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Try<'r, 'a> {
    pub body: Suite<'r, 'a>,
    pub handlers: Vec<ExceptHandler<'r, 'a>>,
    pub orelse: Option<Else<'r, 'a>>,
    pub finalbody: Option<Finally<'r, 'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) try_tok: TokenRef<'r, 'a>,
    // colon_tok unnecessary
}

impl<'r, 'a> Codegen<'a> for Try<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();
        state.add_token("try");
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
        for h in &self.handlers {
            h.codegen(state);
        }
        if let Some(e) = &self.orelse {
            e.codegen(state);
        }
        if let Some(f) = &self.finalbody {
            f.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for Try<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.try_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_before_colon =
            parse_simple_whitespace(config, &mut (*self.try_tok).whitespace_after.borrow_mut())?;
        self.body = self.body.inflate(config)?;
        self.handlers = self.handlers.inflate(config)?;
        self.orelse = self.orelse.inflate(config)?;
        self.finalbody = self.finalbody.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct TryStar<'r, 'a> {
    pub body: Suite<'r, 'a>,
    pub handlers: Vec<ExceptStarHandler<'r, 'a>>,
    pub orelse: Option<Else<'r, 'a>>,
    pub finalbody: Option<Finally<'r, 'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) try_tok: TokenRef<'r, 'a>,
    // colon_tok unnecessary
}

impl<'r, 'a> Codegen<'a> for TryStar<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();
        state.add_token("try");
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
        for h in &self.handlers {
            h.codegen(state);
        }
        if let Some(e) = &self.orelse {
            e.codegen(state);
        }
        if let Some(f) = &self.finalbody {
            f.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for TryStar<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut (*self.try_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_before_colon =
            parse_simple_whitespace(config, &mut (*self.try_tok).whitespace_after.borrow_mut())?;
        self.body = self.body.inflate(config)?;
        self.handlers = self.handlers.inflate(config)?;
        self.orelse = self.orelse.inflate(config)?;
        self.finalbody = self.finalbody.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct AugAssign<'r, 'a> {
    pub target: AssignTargetExpression<'r, 'a>,
    pub operator: AugOp<'r, 'a>,
    pub value: Expression<'r, 'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,
}

impl<'r, 'a> Inflate<'a> for AugAssign<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.target = self.target.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for AugAssign<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.operator.codegen(state);
        self.value.codegen(state);

        if let Some(s) = &self.semicolon {
            s.codegen(state);
        }
    }
}

impl<'r, 'a> AugAssign<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct WithItem<'r, 'a> {
    pub item: Expression<'r, 'a>,
    pub asname: Option<AsName<'r, 'a>>,
    pub comma: Option<Comma<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for WithItem<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.item.codegen(state);
        if let Some(n) = &self.asname {
            n.codegen(state);
        }
        if let Some(c) = &self.comma {
            c.codegen(state);
        }
    }
}

impl<'r, 'a> WithComma<'r, 'a> for WithItem<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

impl<'r, 'a> Inflate<'a> for WithItem<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.item = self.item.inflate(config)?;
        self.asname = self.asname.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct With<'r, 'a> {
    pub items: Vec<WithItem<'r, 'a>>,
    pub body: Suite<'r, 'a>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_with: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'r, 'a>>,
    pub(crate) with_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for With<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for ll in &self.leading_lines {
            ll.codegen(state);
        }
        state.add_indent();

        if let Some(asy) = &self.asynchronous {
            asy.codegen(state);
        }
        state.add_token("with");
        self.whitespace_after_with.codegen(state);
        let len = self.items.len();
        for (i, item) in self.items.iter().enumerate() {
            item.codegen(state);
            if item.comma.is_none() && i + 1 < len {
                state.add_token(", ");
            }
        }
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for With<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        let (asynchronous, leading_lines) = if let Some(asy) = self.async_tok.as_mut() {
            let whitespace_after =
                parse_parenthesizable_whitespace(config, &mut asy.whitespace_after.borrow_mut())?;
            (
                Some(Asynchronous { whitespace_after }),
                Some(parse_empty_lines(
                    config,
                    &mut asy.whitespace_before.borrow_mut(),
                    None,
                )?),
            )
        } else {
            (None, None)
        };

        self.asynchronous = asynchronous;

        self.leading_lines = if let Some(ll) = leading_lines {
            ll
        } else {
            parse_empty_lines(
                config,
                &mut (*self.with_tok).whitespace_before.borrow_mut(),
                None,
            )?
        };

        self.whitespace_after_with =
            parse_simple_whitespace(config, &mut (*self.with_tok).whitespace_after.borrow_mut())?;
        self.items = self.items.inflate(config)?;
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;

        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, ParenthesizedNode, Inflate, IntoPy)]
pub enum DelTargetExpression<'r, 'a> {
    Name(Name<'r, 'a>),
    Attribute(Attribute<'r, 'a>),
    Tuple(Tuple<'r, 'a>),
    List(List<'r, 'a>),
    Subscript(Subscript<'r, 'a>),
}

impl<'r, 'a> std::convert::From<DelTargetExpression<'r, 'a>> for Expression<'r, 'a> {
    fn from(d: DelTargetExpression<'r, 'a>) -> Self {
        match d {
            DelTargetExpression::Attribute(a) => Expression::Attribute(a),
            DelTargetExpression::List(l) => Expression::List(l),
            DelTargetExpression::Name(n) => Expression::Name(n),
            DelTargetExpression::Subscript(s) => Expression::Subscript(s),
            DelTargetExpression::Tuple(t) => Expression::Tuple(t),
        }
    }
}
impl<'r, 'a> std::convert::From<DelTargetExpression<'r, 'a>> for Element<'r, 'a> {
    fn from(d: DelTargetExpression<'r, 'a>) -> Self {
        Element::Simple {
            value: d.into(),
            comma: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Del<'r, 'a> {
    pub target: DelTargetExpression<'r, 'a>,
    pub whitespace_after_del: SimpleWhitespace<'a>,
    pub semicolon: Option<SemicolonTokens<'r, 'a>>,

    pub(crate) tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Inflate<'a> for Del<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_del =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        self.target = self.target.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'r, 'a> Codegen<'a> for Del<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("del");
        self.whitespace_after_del.codegen(state);
        self.target.codegen(state);
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'r, 'a> Del<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<SemicolonTokens<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct Match<'r, 'a> {
    pub subject: Expression<'r, 'a>,
    pub cases: Vec<MatchCase<'r, 'a>>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_match: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
    pub whitespace_after_colon: TrailingWhitespace<'a>,
    pub indent: Option<&'a str>,
    pub footer: Vec<EmptyLine<'a>>,

    pub(crate) match_tok: TokenRef<'r, 'a>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
    pub(crate) indent_tok: TokenRef<'r, 'a>,
    pub(crate) dedent_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for Match<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        state.add_indent();
        state.add_token("match");
        self.whitespace_after_match.codegen(state);
        self.subject.codegen(state);
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.whitespace_after_colon.codegen(state);

        let indent = self.indent.unwrap_or(state.default_indent);
        state.indent(indent);

        // Note: empty cases is a syntax error
        for c in &self.cases {
            c.codegen(state);
        }

        for f in &self.footer {
            f.codegen(state);
        }
        state.dedent();
    }
}

impl<'r, 'a> Inflate<'a> for Match<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut self.match_tok.whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_after_match =
            parse_simple_whitespace(config, &mut self.match_tok.whitespace_after.borrow_mut())?;
        self.subject = self.subject.inflate(config)?;
        self.whitespace_before_colon =
            parse_simple_whitespace(config, &mut self.colon_tok.whitespace_before.borrow_mut())?;
        self.whitespace_after_colon =
            parse_trailing_whitespace(config, &mut self.colon_tok.whitespace_after.borrow_mut())?;
        self.indent = self.indent_tok.relative_indent;
        if self.indent == Some(config.default_indent) {
            self.indent = None;
        }
        self.cases = self.cases.inflate(config)?;
        // See note about footers in `IndentedBlock`'s inflate fn
        self.footer = parse_empty_lines(
            config,
            &mut self.dedent_tok.whitespace_after.borrow_mut(),
            Some(self.indent_tok.whitespace_before.borrow().absolute_indent),
        )?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchCase<'r, 'a> {
    pub pattern: MatchPattern<'r, 'a>,
    pub guard: Option<Expression<'r, 'a>>,
    pub body: Suite<'r, 'a>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_case: SimpleWhitespace<'a>,
    pub whitespace_before_if: SimpleWhitespace<'a>,
    pub whitespace_after_if: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) case_tok: TokenRef<'r, 'a>,
    pub(crate) if_tok: Option<TokenRef<'r, 'a>>,
    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for MatchCase<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        state.add_indent();
        state.add_token("case");
        self.whitespace_after_case.codegen(state);
        self.pattern.codegen(state);
        if let Some(guard) = &self.guard {
            self.whitespace_before_if.codegen(state);
            state.add_token("if");
            self.whitespace_after_if.codegen(state);
            guard.codegen(state);
        }
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for MatchCase<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.leading_lines = parse_empty_lines(
            config,
            &mut self.case_tok.whitespace_before.borrow_mut(),
            None,
        )?;
        self.whitespace_after_case =
            parse_simple_whitespace(config, &mut self.case_tok.whitespace_after.borrow_mut())?;
        self.pattern = self.pattern.inflate(config)?;
        if let Some(if_tok) = self.if_tok.as_mut() {
            self.whitespace_before_if =
                parse_simple_whitespace(config, &mut if_tok.whitespace_before.borrow_mut())?;
            self.whitespace_after_if =
                parse_simple_whitespace(config, &mut if_tok.whitespace_after.borrow_mut())?;

            self.guard = self.guard.inflate(config)?;
        }
        self.whitespace_before_colon =
            parse_simple_whitespace(config, &mut self.colon_tok.whitespace_before.borrow_mut())?;
        self.body = self.body.inflate(config)?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, IntoPy, Codegen, Inflate, ParenthesizedNode)]
pub enum MatchPattern<'r, 'a> {
    Value(MatchValue<'r, 'a>),
    Singleton(MatchSingleton<'r, 'a>),
    Sequence(MatchSequence<'r, 'a>),
    Mapping(MatchMapping<'r, 'a>),
    Class(MatchClass<'r, 'a>),
    As(Box<MatchAs<'r, 'a>>),
    Or(Box<MatchOr<'r, 'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchValue<'r, 'a> {
    pub value: Expression<'r, 'a>,
}

impl<'r, 'a> ParenthesizedNode<'r, 'a> for MatchValue<'r, 'a> {
    fn lpar(&self) -> &Vec<LeftParen<'r, 'a>> {
        self.value.lpar()
    }
    fn rpar(&self) -> &Vec<RightParen<'r, 'a>> {
        self.value.rpar()
    }
    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        self.value.parenthesize(state, f)
    }
    fn with_parens(self, left: LeftParen<'r, 'a>, right: RightParen<'r, 'a>) -> Self {
        Self {
            value: self.value.with_parens(left, right),
        }
    }
}

impl<'r, 'a> Codegen<'a> for MatchValue<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state)
    }
}

impl<'r, 'a> Inflate<'a> for MatchValue<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchSingleton<'r, 'a> {
    pub value: Name<'r, 'a>,
}

impl<'r, 'a> ParenthesizedNode<'r, 'a> for MatchSingleton<'r, 'a> {
    fn lpar(&self) -> &Vec<LeftParen<'r, 'a>> {
        self.value.lpar()
    }
    fn rpar(&self) -> &Vec<RightParen<'r, 'a>> {
        self.value.rpar()
    }
    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        self.value.parenthesize(state, f)
    }
    fn with_parens(self, left: LeftParen<'r, 'a>, right: RightParen<'r, 'a>) -> Self {
        Self {
            value: self.value.with_parens(left, right),
        }
    }
}

impl<'r, 'a> Codegen<'a> for MatchSingleton<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state)
    }
}

impl<'r, 'a> Inflate<'a> for MatchSingleton<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, IntoPy, Codegen, Inflate, ParenthesizedNode)]
pub enum MatchSequence<'r, 'a> {
    MatchList(MatchList<'r, 'a>),
    MatchTuple(MatchTuple<'r, 'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy, ParenthesizedNode)]
pub struct MatchList<'r, 'a> {
    pub patterns: Vec<StarrableMatchSequenceElement<'r, 'a>>,
    pub lbracket: Option<LeftSquareBracket<'r, 'a>>,
    pub rbracket: Option<RightSquareBracket<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for MatchList<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbracket.codegen(state);
            let len = self.patterns.len();
            if len == 1 {
                self.patterns.first().unwrap().codegen(state, false, false);
            } else {
                for (idx, pat) in self.patterns.iter().enumerate() {
                    pat.codegen(state, idx < len - 1, true);
                }
            }
            self.rbracket.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for MatchList<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbracket = self.lbracket.inflate(config)?;

        let len = self.patterns.len();
        self.patterns = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;

        self.rbracket = self.rbracket.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy, ParenthesizedNode)]
pub struct MatchTuple<'r, 'a> {
    pub patterns: Vec<StarrableMatchSequenceElement<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for MatchTuple<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            let len = self.patterns.len();
            if len == 1 {
                self.patterns.first().unwrap().codegen(state, true, false);
            } else {
                for (idx, pat) in self.patterns.iter().enumerate() {
                    pat.codegen(state, idx < len - 1, true);
                }
            }
        })
    }
}

impl<'r, 'a> Inflate<'a> for MatchTuple<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        let len = self.patterns.len();
        self.patterns = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub enum StarrableMatchSequenceElement<'r, 'a> {
    Simple(MatchSequenceElement<'r, 'a>),
    Starred(MatchStar<'r, 'a>),
}

impl<'r, 'a> StarrableMatchSequenceElement<'r, 'a> {
    fn codegen(
        &self,
        state: &mut CodegenState<'a>,
        default_comma: bool,
        default_comma_whitespace: bool,
    ) {
        match &self {
            Self::Simple(s) => s.codegen(state, default_comma, default_comma_whitespace),
            Self::Starred(s) => s.codegen(state, default_comma, default_comma_whitespace),
        }
    }
    fn inflate_element(self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        Ok(match self {
            Self::Simple(s) => Self::Simple(s.inflate_element(config, last_element)?),
            Self::Starred(s) => Self::Starred(s.inflate_element(config, last_element)?),
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for StarrableMatchSequenceElement<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        match self {
            Self::Simple(s) => Self::Simple(s.with_comma(comma)),
            Self::Starred(s) => Self::Starred(s.with_comma(comma)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchSequenceElement<'r, 'a> {
    pub value: MatchPattern<'r, 'a>,
    pub comma: Option<Comma<'r, 'a>>,
}

impl<'r, 'a> MatchSequenceElement<'r, 'a> {
    fn codegen(
        &self,
        state: &mut CodegenState<'a>,
        default_comma: bool,
        default_comma_whitespace: bool,
    ) {
        self.value.codegen(state);
        self.comma.codegen(state);
        if self.comma.is_none() && default_comma {
            state.add_token(if default_comma_whitespace { ", " } else { "," });
        }
    }

    fn inflate_element(mut self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        self.comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(self)
    }
}

impl<'r, 'a> WithComma<'r, 'a> for MatchSequenceElement<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchStar<'r, 'a> {
    pub name: Option<Name<'r, 'a>>,
    pub comma: Option<Comma<'r, 'a>>,
    pub whitespace_before_name: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> MatchStar<'r, 'a> {
    fn codegen(
        &self,
        state: &mut CodegenState<'a>,
        default_comma: bool,
        default_comma_whitespace: bool,
    ) {
        state.add_token("*");
        self.whitespace_before_name.codegen(state);
        if let Some(name) = &self.name {
            name.codegen(state);
        } else {
            state.add_token("_");
        }
        self.comma.codegen(state);
        if self.comma.is_none() && default_comma {
            state.add_token(if default_comma_whitespace { ", " } else { "," });
        }
    }

    fn inflate_element(mut self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        self.whitespace_before_name = parse_parenthesizable_whitespace(
            config,
            &mut self.star_tok.whitespace_after.borrow_mut(),
        )?;
        self.name = self.name.inflate(config)?;
        self.comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(self)
    }
}

impl<'r, 'a> WithComma<'r, 'a> for MatchStar<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy, ParenthesizedNode)]
pub struct MatchMapping<'r, 'a> {
    pub elements: Vec<MatchMappingElement<'r, 'a>>,
    pub rest: Option<Name<'r, 'a>>,
    pub trailing_comma: Option<Comma<'r, 'a>>,
    pub lbrace: LeftCurlyBrace<'r, 'a>,
    pub rbrace: RightCurlyBrace<'r, 'a>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,

    pub whitespace_before_rest: SimpleWhitespace<'a>,

    pub(crate) star_tok: Option<TokenRef<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for MatchMapping<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.lbrace.codegen(state);
            let len = self.elements.len();
            for (idx, el) in self.elements.iter().enumerate() {
                el.codegen(state, self.rest.is_some() || idx < len - 1);
            }
            if let Some(rest) = &self.rest {
                state.add_token("**");
                self.whitespace_before_rest.codegen(state);
                rest.codegen(state);
                self.trailing_comma.codegen(state);
            }
            self.rbrace.codegen(state);
        })
    }
}

impl<'r, 'a> Inflate<'a> for MatchMapping<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.lbrace = self.lbrace.inflate(config)?;

        let len = self.elements.len();
        let no_star = self.star_tok.is_none();
        self.elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, no_star && idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;

        if let Some(star_tok) = self.star_tok.as_mut() {
            self.whitespace_before_rest =
                parse_simple_whitespace(config, &mut star_tok.whitespace_after.borrow_mut())?;
            self.rest = self.rest.inflate(config)?;
            self.trailing_comma = self
                .trailing_comma
                .map(|c| c.inflate_before(config))
                .transpose()?;
        }

        self.rbrace = self.rbrace.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchMappingElement<'r, 'a> {
    pub key: Expression<'r, 'a>,
    pub pattern: MatchPattern<'r, 'a>,
    pub comma: Option<Comma<'r, 'a>>,

    pub whitespace_before_colon: ParenthesizableWhitespace<'a>,
    pub whitespace_after_colon: ParenthesizableWhitespace<'a>,

    pub(crate) colon_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> MatchMappingElement<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        self.key.codegen(state);
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.whitespace_after_colon.codegen(state);
        self.pattern.codegen(state);
        self.comma.codegen(state);
        if self.comma.is_none() && default_comma {
            state.add_token(", ");
        }
    }

    fn inflate_element(mut self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        self.key = self.key.inflate(config)?;
        self.whitespace_before_colon = parse_parenthesizable_whitespace(
            config,
            &mut self.colon_tok.whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_colon = parse_parenthesizable_whitespace(
            config,
            &mut self.colon_tok.whitespace_after.borrow_mut(),
        )?;
        self.pattern = self.pattern.inflate(config)?;
        self.comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(self)
    }
}

impl<'r, 'a> WithComma<'r, 'a> for MatchMappingElement<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy, ParenthesizedNode)]
pub struct MatchClass<'r, 'a> {
    pub cls: NameOrAttribute<'r, 'a>,
    pub patterns: Vec<MatchSequenceElement<'r, 'a>>,
    pub kwds: Vec<MatchKeywordElement<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,

    pub whitespace_after_cls: ParenthesizableWhitespace<'a>,
    pub whitespace_before_patterns: ParenthesizableWhitespace<'a>,
    pub whitespace_after_kwds: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: TokenRef<'r, 'a>,
    pub(crate) rpar_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> Codegen<'a> for MatchClass<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            self.cls.codegen(state);
            self.whitespace_after_cls.codegen(state);
            state.add_token("(");
            self.whitespace_before_patterns.codegen(state);
            let patlen = self.patterns.len();
            let kwdlen = self.kwds.len();
            for (idx, pat) in self.patterns.iter().enumerate() {
                pat.codegen(state, idx < patlen - 1 + kwdlen, patlen == 1 && kwdlen == 0);
            }
            for (idx, kwd) in self.kwds.iter().enumerate() {
                kwd.codegen(state, idx < kwdlen - 1);
            }
            self.whitespace_after_kwds.codegen(state);
            state.add_token(")");
        })
    }
}

impl<'r, 'a> Inflate<'a> for MatchClass<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;

        self.cls = self.cls.inflate(config)?;
        self.whitespace_after_cls = parse_parenthesizable_whitespace(
            config,
            &mut self.lpar_tok.whitespace_before.borrow_mut(),
        )?;
        self.whitespace_before_patterns = parse_parenthesizable_whitespace(
            config,
            &mut self.lpar_tok.whitespace_after.borrow_mut(),
        )?;

        let patlen = self.patterns.len();
        let kwdlen = self.kwds.len();
        self.patterns = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(idx, pat)| pat.inflate_element(config, idx + 1 == patlen + kwdlen))
            .collect::<Result<_>>()?;
        self.kwds = self
            .kwds
            .into_iter()
            .enumerate()
            .map(|(idx, kwd)| kwd.inflate_element(config, idx + 1 == kwdlen))
            .collect::<Result<_>>()?;

        self.whitespace_after_kwds = parse_parenthesizable_whitespace(
            config,
            &mut self.rpar_tok.whitespace_before.borrow_mut(),
        )?;

        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchKeywordElement<'r, 'a> {
    pub key: Name<'r, 'a>,
    pub pattern: MatchPattern<'r, 'a>,
    pub comma: Option<Comma<'r, 'a>>,

    pub whitespace_before_equal: ParenthesizableWhitespace<'a>,
    pub whitespace_after_equal: ParenthesizableWhitespace<'a>,

    pub(crate) equal_tok: TokenRef<'r, 'a>,
}

impl<'r, 'a> MatchKeywordElement<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        self.key.codegen(state);
        self.whitespace_before_equal.codegen(state);
        state.add_token("=");
        self.whitespace_after_equal.codegen(state);
        self.pattern.codegen(state);
        self.comma.codegen(state);
        if self.comma.is_none() && default_comma {
            state.add_token(", ");
        }
    }
    fn inflate_element(mut self, config: &Config<'a>, last_element: bool) -> Result<Self> {
        self.key = self.key.inflate(config)?;
        self.whitespace_before_equal = parse_parenthesizable_whitespace(
            config,
            &mut self.equal_tok.whitespace_before.borrow_mut(),
        )?;
        self.whitespace_after_equal = parse_parenthesizable_whitespace(
            config,
            &mut self.equal_tok.whitespace_after.borrow_mut(),
        )?;
        self.pattern = self.pattern.inflate(config)?;
        self.comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(self)
    }
}

impl<'r, 'a> WithComma<'r, 'a> for MatchKeywordElement<'r, 'a> {
    fn with_comma(self, comma: Comma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy, ParenthesizedNode)]
pub struct MatchAs<'r, 'a> {
    pub pattern: Option<MatchPattern<'r, 'a>>,
    pub name: Option<Name<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,

    pub whitespace_before_as: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_as: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) as_tok: Option<TokenRef<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for MatchAs<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            if let Some(pat) = &self.pattern {
                pat.codegen(state);
                self.whitespace_before_as.codegen(state);
                state.add_token("as");
                self.whitespace_after_as.codegen(state);
            }
            if let Some(name) = &self.name {
                name.codegen(state);
            } else {
                state.add_token("_");
            }
        })
    }
}

impl<'r, 'a> Inflate<'a> for MatchAs<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.pattern = self.pattern.inflate(config)?;
        if let Some(as_tok) = self.as_tok.as_mut() {
            self.whitespace_before_as = Some(parse_parenthesizable_whitespace(
                config,
                &mut as_tok.whitespace_before.borrow_mut(),
            )?);
            self.whitespace_after_as = Some(parse_parenthesizable_whitespace(
                config,
                &mut as_tok.whitespace_after.borrow_mut(),
            )?);
        }
        self.name = self.name.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy)]
pub struct MatchOrElement<'r, 'a> {
    pub pattern: MatchPattern<'r, 'a>,
    pub separator: Option<BitOr<'r, 'a>>,
}

impl<'r, 'a> MatchOrElement<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_separator: bool) {
        self.pattern.codegen(state);
        self.separator.codegen(state);
        if self.separator.is_none() && default_separator {
            state.add_token(" | ");
        }
    }
}

impl<'r, 'a> Inflate<'a> for MatchOrElement<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.pattern = self.pattern.inflate(config)?;
        self.separator = self.separator.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, IntoPy, ParenthesizedNode)]
pub struct MatchOr<'r, 'a> {
    pub patterns: Vec<MatchOrElement<'r, 'a>>,
    pub lpar: Vec<LeftParen<'r, 'a>>,
    pub rpar: Vec<RightParen<'r, 'a>>,
}

impl<'r, 'a> Codegen<'a> for MatchOr<'r, 'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            let len = self.patterns.len();
            for (idx, pat) in self.patterns.iter().enumerate() {
                pat.codegen(state, idx + 1 < len)
            }
        })
    }
}

impl<'r, 'a> Inflate<'a> for MatchOr<'r, 'a> {
    type Inflated = Self;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.patterns = self.patterns.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}
