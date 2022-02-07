// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::{mem::swap, rc::Rc};

use super::{
    inflate_helpers::adjust_parameters_trailing_whitespace, Attribute, Codegen, CodegenState,
    Comma, Dot, EmptyLine, Expression, From, ImportStar, LeftParen, List, Name, NameOrAttribute,
    Parameters, ParenthesizableWhitespace, RightParen, Semicolon, SimpleWhitespace, StarredElement,
    Subscript, TrailingWhitespace, Tuple,
};
use crate::{
    nodes::{
        traits::{Inflate, Result, WithComma, WithLeadingLines},
        Arg, AssignEqual, Asynchronous, AugOp, BitOr, Element, ParenthesizedNode,
    },
    tokenizer::{
        whitespace_parser::{
            parse_empty_lines, parse_parenthesizable_whitespace, parse_simple_whitespace,
            parse_trailing_whitespace, Config,
        },
        Token,
    },
    LeftCurlyBrace, LeftSquareBracket, RightCurlyBrace, RightSquareBracket,
};
#[cfg(feature = "py")]
use libcst_derive::TryIntoPy;
use libcst_derive::{Codegen, Inflate, ParenthesizedNode};

type TokenRef<'a> = Rc<Token<'a>>;

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq, Clone, Inflate, Codegen)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum Statement<'a> {
    Simple(SimpleStatementLine<'a>),
    Compound(CompoundStatement<'a>),
}

impl<'a> WithLeadingLines<'a> for Statement<'a> {
    fn leading_lines(&mut self) -> &mut Vec<EmptyLine<'a>> {
        match self {
            Self::Simple(s) => &mut s.leading_lines,
            Self::Compound(c) => c.leading_lines(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Inflate, Codegen)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
#[allow(clippy::large_enum_variant)]
pub enum CompoundStatement<'a> {
    FunctionDef(FunctionDef<'a>),
    If(If<'a>),
    For(For<'a>),
    While(While<'a>),
    ClassDef(ClassDef<'a>),
    Try(Try<'a>),
    TryStar(TryStar<'a>),
    With(With<'a>),
    Match(Match<'a>),
}

impl<'a> WithLeadingLines<'a> for CompoundStatement<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, Inflate, Codegen)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum Suite<'a> {
    IndentedBlock(IndentedBlock<'a>),
    SimpleStatementSuite(SimpleStatementSuite<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct IndentedBlock<'a> {
    /// Sequence of statements belonging to this indented block.
    pub body: Vec<Statement<'a>>,
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

    pub(crate) newline_tok: TokenRef<'a>,
    pub(crate) indent_tok: TokenRef<'a>,
    pub(crate) dedent_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for IndentedBlock<'a> {
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

impl<'a> Inflate<'a> for IndentedBlock<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct SimpleStatementSuite<'a> {
    /// Sequence of small statements. All but the last statement are required to have
    /// a semicolon.
    pub body: Vec<SmallStatement<'a>>,

    /// The whitespace between the colon in the parent statement and the body.
    pub leading_whitespace: SimpleWhitespace<'a>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub trailing_whitespace: TrailingWhitespace<'a>,

    pub(crate) first_tok: TokenRef<'a>,
    pub(crate) newline_tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for SimpleStatementSuite<'a> {
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

fn _simple_statement_codegen<'a>(
    body: &[SmallStatement<'a>],
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

impl<'a> Codegen<'a> for SimpleStatementSuite<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.leading_whitespace.codegen(state);
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct SimpleStatementLine<'a> {
    /// Sequence of small statements. All but the last statement are required to have
    /// a semicolon.
    pub body: Vec<SmallStatement<'a>>,

    /// Sequence of empty lines appearing before this simple statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub trailing_whitespace: TrailingWhitespace<'a>,

    pub(crate) first_tok: TokenRef<'a>,
    pub(crate) newline_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for SimpleStatementLine<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        for line in &self.leading_lines {
            line.codegen(state);
        }
        state.add_indent();
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

impl<'a> Inflate<'a> for SimpleStatementLine<'a> {
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
#[derive(Debug, Eq, PartialEq, Clone, Codegen, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum SmallStatement<'a> {
    Pass(Pass<'a>),
    Break(Break<'a>),
    Continue(Continue<'a>),
    Return(Return<'a>),
    Expr(Expr<'a>),
    Assert(Assert<'a>),
    Import(Import<'a>),
    ImportFrom(ImportFrom<'a>),
    Assign(Assign<'a>),
    AnnAssign(AnnAssign<'a>),
    Raise(Raise<'a>),
    Global(Global<'a>),
    Nonlocal(Nonlocal<'a>),
    AugAssign(AugAssign<'a>),
    Del(Del<'a>),
}

impl<'a> SmallStatement<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Pass<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'a> Pass<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'a> Codegen<'a> for Pass<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("pass");
        self.semicolon.codegen(state);
    }
}
impl<'a> Inflate<'a> for Pass<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Break<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'a> Break<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'a> Codegen<'a> for Break<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("break");
        self.semicolon.codegen(state);
    }
}
impl<'a> Inflate<'a> for Break<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Continue<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'a> Continue<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'a> Codegen<'a> for Continue<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("continue");
        self.semicolon.codegen(state);
    }
}
impl<'a> Inflate<'a> for Continue<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Expr<'a> {
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'a> Expr<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}
impl<'a> Codegen<'a> for Expr<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state);
        self.semicolon.codegen(state);
    }
}
impl<'a> Inflate<'a> for Expr<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Assign<'a> {
    pub targets: Vec<AssignTarget<'a>>,
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Assign<'a> {
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

impl<'a> Inflate<'a> for Assign<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.targets = self.targets.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Assign<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct AssignTarget<'a> {
    pub target: AssignTargetExpression<'a>,
    pub whitespace_before_equal: SimpleWhitespace<'a>,
    pub whitespace_after_equal: SimpleWhitespace<'a>,

    pub(crate) equal_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for AssignTarget<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.whitespace_before_equal.codegen(state);
        state.add_token("=");
        self.whitespace_after_equal.codegen(state);
    }
}

impl<'a> Inflate<'a> for AssignTarget<'a> {
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
#[derive(Debug, PartialEq, Eq, Clone, Codegen, ParenthesizedNode, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum AssignTargetExpression<'a> {
    Name(Box<Name<'a>>),
    Attribute(Box<Attribute<'a>>),
    StarredElement(Box<StarredElement<'a>>),
    Tuple(Box<Tuple<'a>>),
    List(Box<List<'a>>),
    Subscript(Box<Subscript<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Import<'a> {
    pub names: Vec<ImportAlias<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
    pub whitespace_after_import: SimpleWhitespace<'a>,

    pub(crate) import_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Import<'a> {
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

impl<'a> Inflate<'a> for Import<'a> {
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

impl<'a> Import<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ImportFrom<'a> {
    #[cfg_attr(feature = "py", no_py_default)]
    pub module: Option<NameOrAttribute<'a>>,
    pub names: ImportNames<'a>,
    pub relative: Vec<Dot<'a>>,
    pub lpar: Option<LeftParen<'a>>,
    pub rpar: Option<RightParen<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
    pub whitespace_after_from: SimpleWhitespace<'a>,
    pub whitespace_before_import: SimpleWhitespace<'a>,
    pub whitespace_after_import: SimpleWhitespace<'a>,

    pub(crate) from_tok: TokenRef<'a>,
    pub(crate) import_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for ImportFrom<'a> {
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

impl<'a> Inflate<'a> for ImportFrom<'a> {
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

fn inflate_dots<'a>(dots: Vec<Dot<'a>>, config: &Config<'a>) -> Result<Vec<Dot<'a>>> {
    let mut ret: Vec<Dot<'a>> = vec![];
    let mut last_tok: Option<TokenRef<'a>> = None;
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
        last_tok = Some(dot.tok.clone());
        ret.push(dot.inflate(config)?);
    }
    Ok(ret)
}

impl<'a> ImportFrom<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ImportAlias<'a> {
    pub name: NameOrAttribute<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> Inflate<'a> for ImportAlias<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.name = self.name.inflate(config)?;
        self.asname = self.asname.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

impl<'a> WithComma<'a> for ImportAlias<'a> {
    fn with_comma(self, comma: Comma<'a>) -> ImportAlias<'a> {
        let comma = Some(comma);
        Self { comma, ..self }
    }
}

impl<'a> Codegen<'a> for ImportAlias<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct AsName<'a> {
    pub name: AssignTargetExpression<'a>,
    pub whitespace_before_as: ParenthesizableWhitespace<'a>,
    pub whitespace_after_as: ParenthesizableWhitespace<'a>,

    pub(crate) as_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for AsName<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.whitespace_before_as.codegen(state);
        state.add_token("as");
        self.whitespace_after_as.codegen(state);
        self.name.codegen(state);
    }
}

impl<'a> Inflate<'a> for AsName<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum ImportNames<'a> {
    Star(ImportStar),
    Aliases(Vec<ImportAlias<'a>>),
}

impl<'a> Codegen<'a> for ImportNames<'a> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct FunctionDef<'a> {
    pub name: Name<'a>,
    pub params: Parameters<'a>,
    pub body: Suite<'a>,
    pub decorators: Vec<Decorator<'a>>,
    pub returns: Option<Annotation<'a>>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lines_after_decorators: Vec<EmptyLine<'a>>,
    pub whitespace_after_def: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_params: ParenthesizableWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'a>>,
    pub(crate) def_tok: TokenRef<'a>,
    pub(crate) open_paren_tok: TokenRef<'a>,
    pub(crate) close_paren_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> FunctionDef<'a> {
    pub fn with_decorators(self, decorators: Vec<Decorator<'a>>) -> Self {
        Self { decorators, ..self }
    }
}

impl<'a> Codegen<'a> for FunctionDef<'a> {
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

impl<'a> Inflate<'a> for FunctionDef<'a> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Decorator<'a> {
    pub decorator: Expression<'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_at: SimpleWhitespace<'a>,
    pub trailing_whitespace: TrailingWhitespace<'a>,

    pub(crate) at_tok: TokenRef<'a>,
    pub(crate) newline_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Decorator<'a> {
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

impl<'a> Inflate<'a> for Decorator<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct If<'a> {
    /// The expression that, when evaluated, should give us a truthy value
    pub test: Expression<'a>,
    // The body of this compound statement.
    pub body: Suite<'a>,

    /// An optional ``elif`` or ``else`` clause. ``If`` signifies an ``elif`` block.
    pub orelse: Option<Box<OrElse<'a>>>,

    /// Sequence of empty lines appearing before this compound statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,

    /// The whitespace appearing after the ``if`` keyword but before the test
    /// expression.
    pub whitespace_before_test: SimpleWhitespace<'a>,

    /// The whitespace appearing after the test expression but before the colon.
    pub whitespace_after_test: SimpleWhitespace<'a>,

    /// Signifies if this instance represents an ``elif`` or an ``if`` block.
    #[cfg_attr(feature = "py", skip_py)]
    pub is_elif: bool,

    pub(crate) if_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for If<'a> {
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

impl<'a> Inflate<'a> for If<'a> {
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
#[derive(Debug, PartialEq, Eq, Clone, Inflate, Codegen)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum OrElse<'a> {
    Elif(If<'a>),
    Else(Else<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Else<'a> {
    pub body: Suite<'a>,
    /// Sequence of empty lines appearing before this compound statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,
    /// The whitespace appearing after the ``else`` keyword but before the colon.
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) else_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Else<'a> {
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

impl<'a> Inflate<'a> for Else<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Annotation<'a> {
    pub annotation: Expression<'a>,
    pub whitespace_before_indicator: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_indicator: ParenthesizableWhitespace<'a>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Annotation<'a> {
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

impl<'a> Inflate<'a> for Annotation<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct AnnAssign<'a> {
    pub target: AssignTargetExpression<'a>,
    pub annotation: Annotation<'a>,
    pub value: Option<Expression<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for AnnAssign<'a> {
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

impl<'a> Inflate<'a> for AnnAssign<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.target = self.target.inflate(config)?;
        self.annotation = self.annotation.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.equal = self.equal.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'a> AnnAssign<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Return<'a> {
    pub value: Option<Expression<'a>>,
    pub whitespace_after_return: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) return_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Return<'a> {
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

impl<'a> Inflate<'a> for Return<'a> {
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

impl<'a> Return<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Assert<'a> {
    pub test: Expression<'a>,
    pub msg: Option<Expression<'a>>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_after_assert: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) assert_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Assert<'a> {
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
impl<'a> Inflate<'a> for Assert<'a> {
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

impl<'a> Assert<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Raise<'a> {
    pub exc: Option<Expression<'a>>,
    pub cause: Option<From<'a>>,
    pub whitespace_after_raise: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) raise_tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Raise<'a> {
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

impl<'a> Codegen<'a> for Raise<'a> {
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

impl<'a> Raise<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct NameItem<'a> {
    pub name: Name<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> Inflate<'a> for NameItem<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.name = self.name.inflate(config)?;
        self.comma = self.comma.inflate(config)?;
        Ok(self)
    }
}

impl<'a> NameItem<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_comma: bool) {
        self.name.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        } else if default_comma {
            state.add_token(", ");
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Global<'a> {
    pub names: Vec<NameItem<'a>>,
    pub whitespace_after_global: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Global<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_global =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        self.names = self.names.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for Global<'a> {
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

impl<'a> Global<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Nonlocal<'a> {
    pub names: Vec<NameItem<'a>>,
    pub whitespace_after_nonlocal: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Nonlocal<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_nonlocal =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        self.names = self.names.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for Nonlocal<'a> {
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

impl<'a> Nonlocal<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct For<'a> {
    pub target: AssignTargetExpression<'a>,
    pub iter: Expression<'a>,
    pub body: Suite<'a>,
    pub orelse: Option<Else<'a>>,
    pub asynchronous: Option<Asynchronous<'a>>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_for: SimpleWhitespace<'a>,
    pub whitespace_before_in: SimpleWhitespace<'a>,
    pub whitespace_after_in: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'a>>,
    pub(crate) for_tok: TokenRef<'a>,
    pub(crate) in_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for For<'a> {
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

impl<'a> Inflate<'a> for For<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct While<'a> {
    pub test: Expression<'a>,
    pub body: Suite<'a>,
    pub orelse: Option<Else<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_while: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) while_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for While<'a> {
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

impl<'a> Inflate<'a> for While<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ClassDef<'a> {
    pub name: Name<'a>,
    pub body: Suite<'a>,
    pub bases: Vec<Arg<'a>>,
    pub keywords: Vec<Arg<'a>>,
    pub decorators: Vec<Decorator<'a>>,
    pub lpar: Option<LeftParen<'a>>,
    pub rpar: Option<RightParen<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lines_after_decorators: Vec<EmptyLine<'a>>,
    pub whitespace_after_class: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) class_tok: TokenRef<'a>,
    pub(crate) parens_tok: Option<(TokenRef<'a>, TokenRef<'a>)>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for ClassDef<'a> {
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

impl<'a> Inflate<'a> for ClassDef<'a> {
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
            self.rpar = self.rpar.map(|rpar| rpar.inflate(config)).transpose()?;
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

impl<'a> ClassDef<'a> {
    pub fn with_decorators(self, decorators: Vec<Decorator<'a>>) -> Self {
        Self { decorators, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Finally<'a> {
    pub body: Suite<'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) finally_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Finally<'a> {
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

impl<'a> Inflate<'a> for Finally<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ExceptHandler<'a> {
    pub body: Suite<'a>,
    pub r#type: Option<Expression<'a>>,
    pub name: Option<AsName<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_except: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) except_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for ExceptHandler<'a> {
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

impl<'a> Inflate<'a> for ExceptHandler<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct ExceptStarHandler<'a> {
    pub body: Suite<'a>,
    pub r#type: Expression<'a>,
    pub name: Option<AsName<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_except: SimpleWhitespace<'a>,
    pub whitespace_after_star: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) except_tok: TokenRef<'a>,
    pub(crate) star_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for ExceptStarHandler<'a> {
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

impl<'a> Inflate<'a> for ExceptStarHandler<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Try<'a> {
    pub body: Suite<'a>,
    pub handlers: Vec<ExceptHandler<'a>>,
    pub orelse: Option<Else<'a>>,
    pub finalbody: Option<Finally<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) try_tok: TokenRef<'a>,
    // colon_tok unnecessary
}

impl<'a> Codegen<'a> for Try<'a> {
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

impl<'a> Inflate<'a> for Try<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct TryStar<'a> {
    pub body: Suite<'a>,
    pub handlers: Vec<ExceptStarHandler<'a>>,
    pub orelse: Option<Else<'a>>,
    pub finalbody: Option<Finally<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) try_tok: TokenRef<'a>,
    // colon_tok unnecessary
}

impl<'a> Codegen<'a> for TryStar<'a> {
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

impl<'a> Inflate<'a> for TryStar<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct AugAssign<'a> {
    pub target: AssignTargetExpression<'a>,
    pub operator: AugOp<'a>,
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Inflate<'a> for AugAssign<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.target = self.target.inflate(config)?;
        self.operator = self.operator.inflate(config)?;
        self.value = self.value.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for AugAssign<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.operator.codegen(state);
        self.value.codegen(state);

        if let Some(s) = &self.semicolon {
            s.codegen(state);
        }
    }
}

impl<'a> AugAssign<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct WithItem<'a> {
    pub item: Expression<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> WithItem<'a> {
    fn inflate_withitem(mut self, config: &Config<'a>, is_last: bool) -> Result<Self> {
        self.item = self.item.inflate(config)?;
        self.asname = self.asname.inflate(config)?;
        self.comma = if is_last {
            self.comma.map(|c| c.inflate_before(config)).transpose()?
        } else {
            self.comma.map(|c| c.inflate(config)).transpose()?
        };
        Ok(self)
    }
}

impl<'a> Codegen<'a> for WithItem<'a> {
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

impl<'a> WithComma<'a> for WithItem<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct With<'a> {
    pub items: Vec<WithItem<'a>>,
    pub body: Suite<'a>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lpar: Option<LeftParen<'a>>,
    pub rpar: Option<RightParen<'a>>,
    pub whitespace_after_with: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'a>>,
    pub(crate) with_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for With<'a> {
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

        // TODO: Force parens whenever there are newlines in
        // the commas of self.items.
        //
        // For now, only the python API does this.
        let need_parens = false;
        if let Some(lpar) = &self.lpar {
            lpar.codegen(state);
        } else if need_parens {
            state.add_token("(");
        }

        let len = self.items.len();
        for (i, item) in self.items.iter().enumerate() {
            item.codegen(state);
            if item.comma.is_none() && i + 1 < len {
                state.add_token(", ");
            }
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

impl<'a> Inflate<'a> for With<'a> {
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
        self.lpar = self.lpar.map(|lpar| lpar.inflate(config)).transpose()?;
        let len = self.items.len();
        self.items = self
            .items
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_withitem(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        if !self.items.is_empty() {
            // rpar only has whitespace if items is non empty
            self.rpar = self.rpar.map(|rpar| rpar.inflate(config)).transpose()?;
        }
        self.whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        self.body = self.body.inflate(config)?;

        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Codegen, ParenthesizedNode, Inflate)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum DelTargetExpression<'a> {
    Name(Box<Name<'a>>),
    Attribute(Box<Attribute<'a>>),
    Tuple(Box<Tuple<'a>>),
    List(Box<List<'a>>),
    Subscript(Box<Subscript<'a>>),
}

impl<'a> std::convert::From<DelTargetExpression<'a>> for Expression<'a> {
    fn from(d: DelTargetExpression<'a>) -> Self {
        match d {
            DelTargetExpression::Attribute(a) => Expression::Attribute(a),
            DelTargetExpression::List(l) => Expression::List(l),
            DelTargetExpression::Name(n) => Expression::Name(n),
            DelTargetExpression::Subscript(s) => Expression::Subscript(s),
            DelTargetExpression::Tuple(t) => Expression::Tuple(t),
        }
    }
}
impl<'a> std::convert::From<DelTargetExpression<'a>> for Element<'a> {
    fn from(d: DelTargetExpression<'a>) -> Element {
        Element::Simple {
            value: d.into(),
            comma: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Del<'a> {
    pub target: DelTargetExpression<'a>,
    pub whitespace_after_del: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'a> Inflate<'a> for Del<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.whitespace_after_del =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        self.target = self.target.inflate(config)?;
        self.semicolon = self.semicolon.inflate(config)?;
        Ok(self)
    }
}

impl<'a> Codegen<'a> for Del<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("del");
        self.whitespace_after_del.codegen(state);
        self.target.codegen(state);
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

impl<'a> Del<'a> {
    pub fn with_semicolon(self, semicolon: Option<Semicolon<'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct Match<'a> {
    pub subject: Expression<'a>,
    pub cases: Vec<MatchCase<'a>>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_match: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
    pub whitespace_after_colon: TrailingWhitespace<'a>,
    pub indent: Option<&'a str>,
    pub footer: Vec<EmptyLine<'a>>,

    pub(crate) match_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
    pub(crate) indent_tok: TokenRef<'a>,
    pub(crate) dedent_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for Match<'a> {
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

impl<'a> Inflate<'a> for Match<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchCase<'a> {
    pub pattern: MatchPattern<'a>,
    pub guard: Option<Expression<'a>>,
    pub body: Suite<'a>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_case: SimpleWhitespace<'a>,
    pub whitespace_before_if: SimpleWhitespace<'a>,
    pub whitespace_after_if: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) case_tok: TokenRef<'a>,
    pub(crate) if_tok: Option<TokenRef<'a>>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for MatchCase<'a> {
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

impl<'a> Inflate<'a> for MatchCase<'a> {
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
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum MatchPattern<'a> {
    Value(MatchValue<'a>),
    Singleton(MatchSingleton<'a>),
    Sequence(MatchSequence<'a>),
    Mapping(MatchMapping<'a>),
    Class(MatchClass<'a>),
    As(Box<MatchAs<'a>>),
    Or(Box<MatchOr<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchValue<'a> {
    pub value: Expression<'a>,
}

impl<'a> ParenthesizedNode<'a> for MatchValue<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        self.value.lpar()
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        self.value.rpar()
    }
    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        self.value.parenthesize(state, f)
    }
    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        Self {
            value: self.value.with_parens(left, right),
        }
    }
}

impl<'a> Codegen<'a> for MatchValue<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state)
    }
}

impl<'a> Inflate<'a> for MatchValue<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchSingleton<'a> {
    pub value: Name<'a>,
}

impl<'a> ParenthesizedNode<'a> for MatchSingleton<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        self.value.lpar()
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        self.value.rpar()
    }
    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        self.value.parenthesize(state, f)
    }
    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        Self {
            value: self.value.with_parens(left, right),
        }
    }
}

impl<'a> Codegen<'a> for MatchSingleton<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state)
    }
}

impl<'a> Inflate<'a> for MatchSingleton<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.value = self.value.inflate(config)?;
        Ok(self)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone, Codegen, Inflate, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum MatchSequence<'a> {
    MatchList(MatchList<'a>),
    MatchTuple(MatchTuple<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchList<'a> {
    pub patterns: Vec<StarrableMatchSequenceElement<'a>>,
    pub lbracket: Option<LeftSquareBracket<'a>>,
    pub rbracket: Option<RightSquareBracket<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for MatchList<'a> {
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

impl<'a> Inflate<'a> for MatchList<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchTuple<'a> {
    pub patterns: Vec<StarrableMatchSequenceElement<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for MatchTuple<'a> {
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

impl<'a> Inflate<'a> for MatchTuple<'a> {
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
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub enum StarrableMatchSequenceElement<'a> {
    Simple(MatchSequenceElement<'a>),
    Starred(MatchStar<'a>),
}

impl<'a> StarrableMatchSequenceElement<'a> {
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

impl<'a> WithComma<'a> for StarrableMatchSequenceElement<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        match self {
            Self::Simple(s) => Self::Simple(s.with_comma(comma)),
            Self::Starred(s) => Self::Starred(s.with_comma(comma)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchSequenceElement<'a> {
    pub value: MatchPattern<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> MatchSequenceElement<'a> {
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

impl<'a> WithComma<'a> for MatchSequenceElement<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchStar<'a> {
    pub name: Option<Name<'a>>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_before_name: ParenthesizableWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'a> MatchStar<'a> {
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

impl<'a> WithComma<'a> for MatchStar<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchMapping<'a> {
    pub elements: Vec<MatchMappingElement<'a>>,
    pub rest: Option<Name<'a>>,
    pub trailing_comma: Option<Comma<'a>>,
    pub lbrace: LeftCurlyBrace<'a>,
    pub rbrace: RightCurlyBrace<'a>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,

    pub whitespace_before_rest: SimpleWhitespace<'a>,

    pub(crate) star_tok: Option<TokenRef<'a>>,
}

impl<'a> Codegen<'a> for MatchMapping<'a> {
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

impl<'a> Inflate<'a> for MatchMapping<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchMappingElement<'a> {
    pub key: Expression<'a>,
    pub pattern: MatchPattern<'a>,
    pub comma: Option<Comma<'a>>,

    pub whitespace_before_colon: ParenthesizableWhitespace<'a>,
    pub whitespace_after_colon: ParenthesizableWhitespace<'a>,

    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'a> MatchMappingElement<'a> {
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

impl<'a> WithComma<'a> for MatchMappingElement<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchClass<'a> {
    pub cls: NameOrAttribute<'a>,
    pub patterns: Vec<MatchSequenceElement<'a>>,
    pub kwds: Vec<MatchKeywordElement<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,

    pub whitespace_after_cls: ParenthesizableWhitespace<'a>,
    pub whitespace_before_patterns: ParenthesizableWhitespace<'a>,
    pub whitespace_after_kwds: ParenthesizableWhitespace<'a>,

    pub(crate) lpar_tok: TokenRef<'a>,
    pub(crate) rpar_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for MatchClass<'a> {
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

impl<'a> Inflate<'a> for MatchClass<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchKeywordElement<'a> {
    pub key: Name<'a>,
    pub pattern: MatchPattern<'a>,
    pub comma: Option<Comma<'a>>,

    pub whitespace_before_equal: ParenthesizableWhitespace<'a>,
    pub whitespace_after_equal: ParenthesizableWhitespace<'a>,

    pub(crate) equal_tok: TokenRef<'a>,
}

impl<'a> MatchKeywordElement<'a> {
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

impl<'a> WithComma<'a> for MatchKeywordElement<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchAs<'a> {
    pub pattern: Option<MatchPattern<'a>>,
    pub name: Option<Name<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,

    pub whitespace_before_as: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_as: Option<ParenthesizableWhitespace<'a>>,

    pub(crate) as_tok: Option<TokenRef<'a>>,
}

impl<'a> Codegen<'a> for MatchAs<'a> {
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

impl<'a> Inflate<'a> for MatchAs<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchOrElement<'a> {
    pub pattern: MatchPattern<'a>,
    pub separator: Option<BitOr<'a>>,
}

impl<'a> MatchOrElement<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>, default_separator: bool) {
        self.pattern.codegen(state);
        self.separator.codegen(state);
        if self.separator.is_none() && default_separator {
            state.add_token(" | ");
        }
    }
}

impl<'a> Inflate<'a> for MatchOrElement<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.pattern = self.pattern.inflate(config)?;
        self.separator = self.separator.inflate(config)?;
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, ParenthesizedNode)]
#[cfg_attr(feature = "py", derive(TryIntoPy))]
pub struct MatchOr<'a> {
    pub patterns: Vec<MatchOrElement<'a>>,
    pub lpar: Vec<LeftParen<'a>>,
    pub rpar: Vec<RightParen<'a>>,
}

impl<'a> Codegen<'a> for MatchOr<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.parenthesize(state, |state| {
            let len = self.patterns.len();
            for (idx, pat) in self.patterns.iter().enumerate() {
                pat.codegen(state, idx + 1 < len)
            }
        })
    }
}

impl<'a> Inflate<'a> for MatchOr<'a> {
    fn inflate(mut self, config: &Config<'a>) -> Result<Self> {
        self.lpar = self.lpar.inflate(config)?;
        self.patterns = self.patterns.inflate(config)?;
        self.rpar = self.rpar.inflate(config)?;
        Ok(self)
    }
}
