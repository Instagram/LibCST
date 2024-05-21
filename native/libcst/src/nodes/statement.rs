// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::mem::swap;

use super::{
    inflate_helpers::adjust_parameters_trailing_whitespace, Attribute, Codegen, CodegenState,
    Comma, Dot, EmptyLine, Expression, From, ImportStar, LeftParen, List, Name, NameOrAttribute,
    Parameters, ParenthesizableWhitespace, RightParen, Semicolon, SimpleWhitespace, StarredElement,
    Subscript, TrailingWhitespace, Tuple,
};
use crate::{
    nodes::{
        expression::*,
        op::*,
        traits::{
            Inflate, ParenthesizedDeflatedNode, ParenthesizedNode, Result, WithComma,
            WithLeadingLines,
        },
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
use libcst_derive::{cst_node, Codegen, Inflate, ParenthesizedDeflatedNode, ParenthesizedNode};

type TokenRef<'r, 'a> = &'r Token<'a>;

#[allow(clippy::large_enum_variant)]
#[cst_node(Inflate, Codegen)]
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

#[allow(clippy::large_enum_variant)]
#[cst_node(Inflate, Codegen)]
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

#[cst_node(Inflate, Codegen)]
pub enum Suite<'a> {
    IndentedBlock(IndentedBlock<'a>),
    SimpleStatementSuite(SimpleStatementSuite<'a>),
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedIndentedBlock<'r, 'a> {
    type Inflated = IndentedBlock<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let body = self.body.inflate(config)?;
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
        let mut indent = self.indent_tok.relative_indent;
        if indent == Some(config.default_indent) {
            indent = None;
        }
        Ok(Self::Inflated {
            body,
            header,
            indent,
            footer,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedSimpleStatementSuite<'r, 'a> {
    type Inflated = SimpleStatementSuite<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_whitespace = parse_simple_whitespace(
            config,
            &mut (*self.first_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;
        let trailing_whitespace = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            body,
            leading_whitespace,
            trailing_whitespace,
        })
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

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedSimpleStatementLine<'r, 'a> {
    type Inflated = SimpleStatementLine<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.first_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let body = self.body.inflate(config)?;
        let trailing_whitespace = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            body,
            leading_lines,
            trailing_whitespace,
        })
    }
}

#[allow(dead_code, clippy::large_enum_variant)]
#[cst_node(Codegen, Inflate)]
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
    TypeAlias(TypeAlias<'a>),
}

impl<'r, 'a> DeflatedSmallStatement<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
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
            Self::TypeAlias(t) => Self::TypeAlias(t.with_semicolon(semicolon)),
        }
    }
}

#[cst_node]
pub struct Pass<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'r, 'a> DeflatedPass<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'a> Codegen<'a> for Pass<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("pass");
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for DeflatedPass<'r, 'a> {
    type Inflated = Pass<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated { semicolon })
    }
}

#[cst_node]
pub struct Break<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'r, 'a> DeflatedBreak<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'a> Codegen<'a> for Break<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("break");
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for DeflatedBreak<'r, 'a> {
    type Inflated = Break<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated { semicolon })
    }
}

#[cst_node]
pub struct Continue<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'r, 'a> DeflatedContinue<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon }
    }
}
impl<'a> Codegen<'a> for Continue<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("continue");
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for DeflatedContinue<'r, 'a> {
    type Inflated = Continue<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated { semicolon })
    }
}

#[cst_node]
pub struct Expr<'a> {
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}
impl<'r, 'a> DeflatedExpr<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}
impl<'a> Codegen<'a> for Expr<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.value.codegen(state);
        self.semicolon.codegen(state);
    }
}
impl<'r, 'a> Inflate<'a> for DeflatedExpr<'r, 'a> {
    type Inflated = Expr<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let value = self.value.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated { value, semicolon })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAssign<'r, 'a> {
    type Inflated = Assign<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let targets = self.targets.inflate(config)?;
        let value = self.value.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            targets,
            value,
            semicolon,
        })
    }
}

impl<'r, 'a> DeflatedAssign<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAssignTarget<'r, 'a> {
    type Inflated = AssignTarget<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let target = self.target.inflate(config)?;
        let whitespace_before_equal = parse_simple_whitespace(
            config,
            &mut (*self.equal_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_equal =
            parse_simple_whitespace(config, &mut (*self.equal_tok).whitespace_after.borrow_mut())?;
        Ok(Self::Inflated {
            target,
            whitespace_before_equal,
            whitespace_after_equal,
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[cst_node(Codegen, ParenthesizedNode, Inflate)]
pub enum AssignTargetExpression<'a> {
    Name(Box<Name<'a>>),
    Attribute(Box<Attribute<'a>>),
    StarredElement(Box<StarredElement<'a>>),
    Tuple(Box<Tuple<'a>>),
    List(Box<List<'a>>),
    Subscript(Box<Subscript<'a>>),
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedImport<'r, 'a> {
    type Inflated = Import<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_import = parse_simple_whitespace(
            config,
            &mut (*self.import_tok).whitespace_after.borrow_mut(),
        )?;
        let names = self.names.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            names,
            semicolon,
            whitespace_after_import,
        })
    }
}

impl<'r, 'a> DeflatedImport<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedImportFrom<'r, 'a> {
    type Inflated = ImportFrom<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_from =
            parse_simple_whitespace(config, &mut (*self.from_tok).whitespace_after.borrow_mut())?;

        let module = self.module.inflate(config)?;

        let whitespace_after_import = parse_simple_whitespace(
            config,
            &mut (*self.import_tok).whitespace_after.borrow_mut(),
        )?;

        let mut relative = inflate_dots(self.relative, config)?;
        let mut whitespace_before_import = Default::default();

        if !relative.is_empty() && module.is_none() {
            // For relative-only imports relocate the space after the final dot to be owned
            // by the import token.
            if let Some(Dot {
                whitespace_after: ParenthesizableWhitespace::SimpleWhitespace(dot_ws),
                ..
            }) = relative.last_mut()
            {
                swap(dot_ws, &mut whitespace_before_import);
            }
        } else {
            whitespace_before_import = parse_simple_whitespace(
                config,
                &mut (*self.import_tok).whitespace_before.borrow_mut(),
            )?;
        }

        let lpar = self.lpar.inflate(config)?;
        let names = self.names.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;

        let semicolon = self.semicolon.inflate(config)?;

        Ok(Self::Inflated {
            module,
            names,
            relative,
            lpar,
            rpar,
            semicolon,
            whitespace_after_from,
            whitespace_before_import,
            whitespace_after_import,
        })
    }
}

fn inflate_dots<'r, 'a>(
    dots: Vec<DeflatedDot<'r, 'a>>,
    config: &Config<'a>,
) -> Result<Vec<Dot<'a>>> {
    let mut ret: Vec<Dot<'a>> = vec![];
    let mut last_tok: Option<TokenRef<'r, 'a>> = None;
    for dot in dots {
        if let Some(last_tokref) = &last_tok {
            // Consecutive dots having the same Token can only happen if `...` was
            // parsed as a single ELLIPSIS token. In this case the token's
            // whitespace_before belongs to the first dot, but the whitespace_after is
            // moved to the 3rd dot (by swapping it twice)
            if last_tokref.start_pos == dot.tok.start_pos {
                let mut subsequent_dot = Dot {
                    whitespace_before: Default::default(),
                    whitespace_after: Default::default(),
                };
                swap(
                    &mut ret.last_mut().unwrap().whitespace_after,
                    &mut subsequent_dot.whitespace_after,
                );
                ret.push(subsequent_dot);
                continue;
            }
        }
        last_tok = Some(dot.tok);
        ret.push(dot.inflate(config)?);
    }
    Ok(ret)
}

impl<'r, 'a> DeflatedImportFrom<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
pub struct ImportAlias<'a> {
    pub name: NameOrAttribute<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedImportAlias<'r, 'a> {
    type Inflated = ImportAlias<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let name = self.name.inflate(config)?;
        let asname = self.asname.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        Ok(Self::Inflated {
            name,
            asname,
            comma,
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedImportAlias<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
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

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAsName<'r, 'a> {
    type Inflated = AsName<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before_as = parse_parenthesizable_whitespace(
            config,
            &mut (*self.as_tok).whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_as = parse_parenthesizable_whitespace(
            config,
            &mut (*self.as_tok).whitespace_after.borrow_mut(),
        )?;
        let name = self.name.inflate(config)?;
        Ok(Self::Inflated {
            name,
            whitespace_before_as,
            whitespace_after_as,
        })
    }
}

#[cst_node(Inflate)]
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

#[cst_node]
pub struct FunctionDef<'a> {
    pub name: Name<'a>,
    pub type_parameters: Option<TypeParameters<'a>>,
    pub params: Parameters<'a>,
    pub body: Suite<'a>,
    pub decorators: Vec<Decorator<'a>>,
    pub returns: Option<Annotation<'a>>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lines_after_decorators: Vec<EmptyLine<'a>>,
    pub whitespace_after_def: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_after_type_parameters: SimpleWhitespace<'a>,
    pub whitespace_before_params: ParenthesizableWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) async_tok: Option<TokenRef<'a>>,
    pub(crate) def_tok: TokenRef<'a>,
    pub(crate) open_paren_tok: TokenRef<'a>,
    pub(crate) close_paren_tok: TokenRef<'a>,
    pub(crate) colon_tok: TokenRef<'a>,
}

impl<'r, 'a> DeflatedFunctionDef<'r, 'a> {
    pub fn with_decorators(self, decorators: Vec<DeflatedDecorator<'r, 'a>>) -> Self {
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

        if let Some(tp) = &self.type_parameters {
            tp.codegen(state);
            self.whitespace_after_type_parameters.codegen(state);
        }

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

impl<'r, 'a> Inflate<'a> for DeflatedFunctionDef<'r, 'a> {
    type Inflated = FunctionDef<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let mut decorators = self.decorators.inflate(config)?;
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

        let mut leading_lines = if let Some(ll) = leading_lines {
            ll
        } else {
            parse_empty_lines(
                config,
                &mut (*self.def_tok).whitespace_before.borrow_mut(),
                None,
            )?
        };

        let mut lines_after_decorators = Default::default();

        if let Some(dec) = decorators.first_mut() {
            swap(&mut lines_after_decorators, &mut leading_lines);
            swap(&mut dec.leading_lines, &mut leading_lines);
        }

        let whitespace_after_def =
            parse_simple_whitespace(config, &mut (*self.def_tok).whitespace_after.borrow_mut())?;

        let name = self.name.inflate(config)?;

        let whitespace_after_name;
        let mut type_parameters = Default::default();
        let mut whitespace_after_type_parameters = Default::default();

        if let Some(tp) = self.type_parameters {
            let rbracket_tok = tp.rbracket.tok.clone();
            whitespace_after_name = parse_simple_whitespace(
                config,
                &mut tp.lbracket.tok.whitespace_before.borrow_mut(),
            )?;
            type_parameters = Some(tp.inflate(config)?);
            whitespace_after_type_parameters =
                parse_simple_whitespace(config, &mut rbracket_tok.whitespace_after.borrow_mut())?;
        } else {
            whitespace_after_name = parse_simple_whitespace(
                config,
                &mut self.open_paren_tok.whitespace_before.borrow_mut(),
            )?;
        }

        let whitespace_before_params = parse_parenthesizable_whitespace(
            config,
            &mut (*self.open_paren_tok).whitespace_after.borrow_mut(),
        )?;
        let mut params = self.params.inflate(config)?;
        adjust_parameters_trailing_whitespace(config, &mut params, &self.close_paren_tok)?;

        let returns = self.returns.inflate(config)?;
        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;

        let body = self.body.inflate(config)?;
        Ok(Self::Inflated {
            name,
            type_parameters,
            params,
            body,
            decorators,
            returns,
            asynchronous,
            leading_lines,
            lines_after_decorators,
            whitespace_after_def,
            whitespace_after_name,
            whitespace_after_type_parameters,
            whitespace_before_params,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedDecorator<'r, 'a> {
    type Inflated = Decorator<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.at_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_after_at =
            parse_simple_whitespace(config, &mut (*self.at_tok).whitespace_after.borrow_mut())?;
        let decorator = self.decorator.inflate(config)?;
        let trailing_whitespace = parse_trailing_whitespace(
            config,
            &mut (*self.newline_tok).whitespace_before.borrow_mut(),
        )?;
        Ok(Self::Inflated {
            decorator,
            leading_lines,
            whitespace_after_at,
            trailing_whitespace,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedIf<'r, 'a> {
    type Inflated = If<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.if_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_before_test =
            parse_simple_whitespace(config, &mut (*self.if_tok).whitespace_after.borrow_mut())?;
        let test = self.test.inflate(config)?;
        let whitespace_after_test = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;
        let orelse = self.orelse.inflate(config)?;

        Ok(Self::Inflated {
            test,
            body,
            orelse,
            leading_lines,
            whitespace_before_test,
            whitespace_after_test,
            is_elif: self.is_elif,
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[cst_node(Inflate, Codegen)]
pub enum OrElse<'a> {
    Elif(If<'a>),
    Else(Else<'a>),
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedElse<'r, 'a> {
    type Inflated = Else<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.else_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;

        Ok(Self::Inflated {
            body,
            leading_lines,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAnnotation<'r, 'a> {
    type Inflated = Annotation<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_before_indicator = Some(parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_before.borrow_mut(),
        )?);
        let whitespace_after_indicator = parse_parenthesizable_whitespace(
            config,
            &mut (*self.tok).whitespace_after.borrow_mut(),
        )?;
        let annotation = self.annotation.inflate(config)?;
        Ok(Self::Inflated {
            annotation,
            whitespace_before_indicator,
            whitespace_after_indicator,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedAnnAssign<'r, 'a> {
    type Inflated = AnnAssign<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let target = self.target.inflate(config)?;
        let annotation = self.annotation.inflate(config)?;
        let value = self.value.inflate(config)?;
        let equal = self.equal.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            target,
            annotation,
            value,
            equal,
            semicolon,
        })
    }
}

impl<'r, 'a> DeflatedAnnAssign<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedReturn<'r, 'a> {
    type Inflated = Return<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_return = if self.value.is_some() {
            Some(parse_simple_whitespace(
                config,
                &mut (*self.return_tok).whitespace_after.borrow_mut(),
            )?)
        } else {
            // otherwise space is owned by semicolon or small statement
            // whitespace is not None to preserve a quirk of the pure python parser
            Some(Default::default())
        };
        let value = self.value.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            value,
            whitespace_after_return,
            semicolon,
        })
    }
}

impl<'r, 'a> DeflatedReturn<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
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
impl<'r, 'a> Inflate<'a> for DeflatedAssert<'r, 'a> {
    type Inflated = Assert<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_assert = parse_simple_whitespace(
            config,
            &mut (*self.assert_tok).whitespace_after.borrow_mut(),
        )?;

        let test = self.test.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        let msg = self.msg.inflate(config)?;

        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            test,
            msg,
            comma,
            whitespace_after_assert,
            semicolon,
        })
    }
}

impl<'r, 'a> DeflatedAssert<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
pub struct Raise<'a> {
    pub exc: Option<Expression<'a>>,
    pub cause: Option<From<'a>>,
    pub whitespace_after_raise: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) raise_tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedRaise<'r, 'a> {
    type Inflated = Raise<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_raise = if self.exc.is_some() {
            Some(parse_simple_whitespace(
                config,
                &mut (*self.raise_tok).whitespace_after.borrow_mut(),
            )?)
        } else {
            Default::default()
        };

        let exc = self.exc.inflate(config)?;
        let mut cause = self.cause.inflate(config)?;
        if exc.is_none() {
            if let Some(cause) = cause.as_mut() {
                // in `raise from`, `raise` owns the shared whitespace
                cause.whitespace_before_from = None;
            }
        }
        let semicolon = self.semicolon.inflate(config)?;

        Ok(Self::Inflated {
            exc,
            cause,
            whitespace_after_raise,
            semicolon,
        })
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

impl<'r, 'a> DeflatedRaise<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
pub struct NameItem<'a> {
    pub name: Name<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedNameItem<'r, 'a> {
    type Inflated = NameItem<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let name = self.name.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        Ok(Self::Inflated { name, comma })
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

#[cst_node]
pub struct Global<'a> {
    pub names: Vec<NameItem<'a>>,
    pub whitespace_after_global: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedGlobal<'r, 'a> {
    type Inflated = Global<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_global =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        let names = self.names.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            names,
            whitespace_after_global,
            semicolon,
        })
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

impl<'r, 'a> DeflatedGlobal<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
pub struct Nonlocal<'a> {
    pub names: Vec<NameItem<'a>>,
    pub whitespace_after_nonlocal: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedNonlocal<'r, 'a> {
    type Inflated = Nonlocal<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_nonlocal =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        let names = self.names.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            names,
            whitespace_after_nonlocal,
            semicolon,
        })
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

impl<'r, 'a> DeflatedNonlocal<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedFor<'r, 'a> {
    type Inflated = For<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
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
        let leading_lines = if let Some(ll) = leading_lines {
            ll
        } else {
            parse_empty_lines(
                config,
                &mut (*self.for_tok).whitespace_before.borrow_mut(),
                None,
            )?
        };
        let whitespace_after_for =
            parse_simple_whitespace(config, &mut (*self.for_tok).whitespace_after.borrow_mut())?;
        let target = self.target.inflate(config)?;
        let whitespace_before_in =
            parse_simple_whitespace(config, &mut (*self.in_tok).whitespace_before.borrow_mut())?;
        let whitespace_after_in =
            parse_simple_whitespace(config, &mut (*self.in_tok).whitespace_after.borrow_mut())?;
        let iter = self.iter.inflate(config)?;
        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;

        let body = self.body.inflate(config)?;
        let orelse = self.orelse.inflate(config)?;

        Ok(Self::Inflated {
            target,
            iter,
            body,
            orelse,
            asynchronous,
            leading_lines,
            whitespace_after_for,
            whitespace_before_in,
            whitespace_after_in,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedWhile<'r, 'a> {
    type Inflated = While<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.while_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_after_while =
            parse_simple_whitespace(config, &mut (*self.while_tok).whitespace_after.borrow_mut())?;
        let test = self.test.inflate(config)?;
        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;
        let orelse = self.orelse.inflate(config)?;

        Ok(Self::Inflated {
            test,
            body,
            orelse,
            leading_lines,
            whitespace_after_while,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
pub struct ClassDef<'a> {
    pub name: Name<'a>,
    pub type_parameters: Option<TypeParameters<'a>>,
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
    pub whitespace_after_type_parameters: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,

    pub(crate) class_tok: TokenRef<'a>,
    pub(crate) lpar_tok: Option<TokenRef<'a>>,
    pub(crate) rpar_tok: Option<TokenRef<'a>>,
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

        if let Some(tp) = &self.type_parameters {
            tp.codegen(state);
            self.whitespace_after_type_parameters.codegen(state);
        }

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

impl<'r, 'a> Inflate<'a> for DeflatedClassDef<'r, 'a> {
    type Inflated = ClassDef<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let mut leading_lines = parse_empty_lines(
            config,
            &mut (*self.class_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let mut decorators = self.decorators.inflate(config)?;
        let mut lines_after_decorators = Default::default();
        if let Some(dec) = decorators.first_mut() {
            swap(&mut lines_after_decorators, &mut leading_lines);
            swap(&mut dec.leading_lines, &mut leading_lines);
        }

        let whitespace_after_class =
            parse_simple_whitespace(config, &mut (*self.class_tok).whitespace_after.borrow_mut())?;
        let name = self.name.inflate(config)?;

        let (mut whitespace_after_name, mut type_parameters, mut whitespace_after_type_parameters) =
            Default::default();

        if let Some(tparams) = self.type_parameters {
            let rbracket_tok = tparams.rbracket.tok.clone();
            whitespace_after_name = parse_simple_whitespace(
                config,
                &mut tparams.lbracket.tok.whitespace_before.borrow_mut(),
            )?;
            type_parameters = Some(tparams.inflate(config)?);
            whitespace_after_type_parameters =
                parse_simple_whitespace(config, &mut rbracket_tok.whitespace_after.borrow_mut())?;
        } else if let Some(lpar_tok) = self.lpar_tok.as_mut() {
            whitespace_after_name =
                parse_simple_whitespace(config, &mut lpar_tok.whitespace_before.borrow_mut())?;
        }

        let lpar = self.lpar.inflate(config)?;
        let bases = self.bases.inflate(config)?;
        let keywords = self.keywords.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;

        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;

        Ok(Self::Inflated {
            name,
            type_parameters,
            body,
            bases,
            keywords,
            decorators,
            lpar,
            rpar,
            leading_lines,
            lines_after_decorators,
            whitespace_after_class,
            whitespace_after_type_parameters,
            whitespace_after_name,
            whitespace_before_colon,
        })
    }
}

impl<'r, 'a> DeflatedClassDef<'r, 'a> {
    pub fn with_decorators(self, decorators: Vec<DeflatedDecorator<'r, 'a>>) -> Self {
        Self { decorators, ..self }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedFinally<'r, 'a> {
    type Inflated = Finally<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.finally_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;
        Ok(Self::Inflated {
            body,
            leading_lines,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedExceptHandler<'r, 'a> {
    type Inflated = ExceptHandler<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.except_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_after_except = parse_simple_whitespace(
            config,
            &mut (*self.except_tok).whitespace_after.borrow_mut(),
        )?;

        let r#type = self.r#type.inflate(config)?;
        let name = self.name.inflate(config)?;
        let whitespace_before_colon = if name.is_some() {
            parse_simple_whitespace(
                config,
                &mut (*self.colon_tok).whitespace_before.borrow_mut(),
            )?
        } else {
            Default::default()
        };

        let body = self.body.inflate(config)?;
        Ok(Self::Inflated {
            body,
            r#type,
            name,
            leading_lines,
            whitespace_after_except,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedExceptStarHandler<'r, 'a> {
    type Inflated = ExceptStarHandler<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut self.except_tok.whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_after_except =
            parse_simple_whitespace(config, &mut self.except_tok.whitespace_after.borrow_mut())?;
        let whitespace_after_star =
            parse_simple_whitespace(config, &mut self.star_tok.whitespace_after.borrow_mut())?;

        let r#type = self.r#type.inflate(config)?;
        let name = self.name.inflate(config)?;
        let whitespace_before_colon = if name.is_some() {
            parse_simple_whitespace(config, &mut self.colon_tok.whitespace_before.borrow_mut())?
        } else {
            Default::default()
        };

        let body = self.body.inflate(config)?;
        Ok(Self::Inflated {
            body,
            r#type,
            name,
            leading_lines,
            whitespace_after_except,
            whitespace_after_star,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedTry<'r, 'a> {
    type Inflated = Try<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.try_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_before_colon =
            parse_simple_whitespace(config, &mut (*self.try_tok).whitespace_after.borrow_mut())?;
        let body = self.body.inflate(config)?;
        let handlers = self.handlers.inflate(config)?;
        let orelse = self.orelse.inflate(config)?;
        let finalbody = self.finalbody.inflate(config)?;
        Ok(Self::Inflated {
            body,
            handlers,
            orelse,
            finalbody,
            leading_lines,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedTryStar<'r, 'a> {
    type Inflated = TryStar<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut (*self.try_tok).whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_before_colon =
            parse_simple_whitespace(config, &mut (*self.try_tok).whitespace_after.borrow_mut())?;
        let body = self.body.inflate(config)?;
        let handlers = self.handlers.inflate(config)?;
        let orelse = self.orelse.inflate(config)?;
        let finalbody = self.finalbody.inflate(config)?;
        Ok(Self::Inflated {
            body,
            handlers,
            orelse,
            finalbody,
            leading_lines,
            whitespace_before_colon,
        })
    }
}

#[cst_node]
pub struct AugAssign<'a> {
    pub target: AssignTargetExpression<'a>,
    pub operator: AugOp<'a>,
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'r, 'a> Inflate<'a> for DeflatedAugAssign<'r, 'a> {
    type Inflated = AugAssign<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let target = self.target.inflate(config)?;
        let operator = self.operator.inflate(config)?;
        let value = self.value.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            target,
            operator,
            value,
            semicolon,
        })
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

impl<'r, 'a> DeflatedAugAssign<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
pub struct WithItem<'a> {
    pub item: Expression<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'r, 'a> DeflatedWithItem<'r, 'a> {
    fn inflate_withitem(self, config: &Config<'a>, is_last: bool) -> Result<WithItem<'a>> {
        let item = self.item.inflate(config)?;
        let asname = self.asname.inflate(config)?;
        let comma = if is_last {
            self.comma.map(|c| c.inflate_before(config)).transpose()?
        } else {
            self.comma.map(|c| c.inflate(config)).transpose()?
        };
        Ok(WithItem {
            item,
            asname,
            comma,
        })
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

impl<'r, 'a> WithComma<'r, 'a> for DeflatedWithItem<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedWith<'r, 'a> {
    type Inflated = With<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
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

        let leading_lines = if let Some(ll) = leading_lines {
            ll
        } else {
            parse_empty_lines(
                config,
                &mut (*self.with_tok).whitespace_before.borrow_mut(),
                None,
            )?
        };

        let whitespace_after_with =
            parse_simple_whitespace(config, &mut (*self.with_tok).whitespace_after.borrow_mut())?;
        let lpar = self.lpar.map(|lpar| lpar.inflate(config)).transpose()?;
        let len = self.items.len();
        let items = self
            .items
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_withitem(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        let rpar = if !items.is_empty() {
            // rpar only has whitespace if items is non empty
            self.rpar.map(|rpar| rpar.inflate(config)).transpose()?
        } else {
            Default::default()
        };
        let whitespace_before_colon = parse_simple_whitespace(
            config,
            &mut (*self.colon_tok).whitespace_before.borrow_mut(),
        )?;
        let body = self.body.inflate(config)?;

        Ok(Self::Inflated {
            items,
            body,
            asynchronous,
            leading_lines,
            lpar,
            rpar,
            whitespace_after_with,
            whitespace_before_colon,
        })
    }
}

#[cst_node(Codegen, ParenthesizedNode, Inflate)]
pub enum DelTargetExpression<'a> {
    Name(Box<Name<'a>>),
    Attribute(Box<Attribute<'a>>),
    Tuple(Box<Tuple<'a>>),
    List(Box<List<'a>>),
    Subscript(Box<Subscript<'a>>),
}

impl<'r, 'a> std::convert::From<DeflatedDelTargetExpression<'r, 'a>>
    for DeflatedExpression<'r, 'a>
{
    fn from(d: DeflatedDelTargetExpression<'r, 'a>) -> Self {
        match d {
            DeflatedDelTargetExpression::Attribute(a) => Self::Attribute(a),
            DeflatedDelTargetExpression::List(l) => Self::List(l),
            DeflatedDelTargetExpression::Name(n) => Self::Name(n),
            DeflatedDelTargetExpression::Subscript(s) => Self::Subscript(s),
            DeflatedDelTargetExpression::Tuple(t) => Self::Tuple(t),
        }
    }
}
impl<'r, 'a> std::convert::From<DeflatedDelTargetExpression<'r, 'a>> for DeflatedElement<'r, 'a> {
    fn from(d: DeflatedDelTargetExpression<'r, 'a>) -> Self {
        Self::Simple {
            value: d.into(),
            comma: None,
        }
    }
}

#[cst_node]
pub struct Del<'a> {
    pub target: DelTargetExpression<'a>,
    pub whitespace_after_del: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) tok: TokenRef<'a>,
}

impl<'r, 'a> Inflate<'a> for DeflatedDel<'r, 'a> {
    type Inflated = Del<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_del =
            parse_simple_whitespace(config, &mut (*self.tok).whitespace_after.borrow_mut())?;
        let target = self.target.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            target,
            whitespace_after_del,
            semicolon,
        })
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

impl<'r, 'a> DeflatedDel<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatch<'r, 'a> {
    type Inflated = Match<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut self.match_tok.whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_after_match =
            parse_simple_whitespace(config, &mut self.match_tok.whitespace_after.borrow_mut())?;
        let subject = self.subject.inflate(config)?;
        let whitespace_before_colon =
            parse_simple_whitespace(config, &mut self.colon_tok.whitespace_before.borrow_mut())?;
        let whitespace_after_colon =
            parse_trailing_whitespace(config, &mut self.colon_tok.whitespace_after.borrow_mut())?;
        let mut indent = self.indent_tok.relative_indent;
        if indent == Some(config.default_indent) {
            indent = None;
        }
        let cases = self.cases.inflate(config)?;
        // See note about footers in `IndentedBlock`'s inflate fn
        let footer = parse_empty_lines(
            config,
            &mut self.dedent_tok.whitespace_after.borrow_mut(),
            Some(self.indent_tok.whitespace_before.borrow().absolute_indent),
        )?;
        Ok(Self::Inflated {
            subject,
            cases,
            leading_lines,
            whitespace_after_match,
            whitespace_before_colon,
            whitespace_after_colon,
            indent,
            footer,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchCase<'r, 'a> {
    type Inflated = MatchCase<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let leading_lines = parse_empty_lines(
            config,
            &mut self.case_tok.whitespace_before.borrow_mut(),
            None,
        )?;
        let whitespace_after_case =
            parse_simple_whitespace(config, &mut self.case_tok.whitespace_after.borrow_mut())?;
        let pattern = self.pattern.inflate(config)?;
        let (whitespace_before_if, whitespace_after_if, guard) =
            if let Some(if_tok) = self.if_tok.as_mut() {
                (
                    parse_simple_whitespace(config, &mut if_tok.whitespace_before.borrow_mut())?,
                    parse_simple_whitespace(config, &mut if_tok.whitespace_after.borrow_mut())?,
                    self.guard.inflate(config)?,
                )
            } else {
                Default::default()
            };
        let whitespace_before_colon =
            parse_simple_whitespace(config, &mut self.colon_tok.whitespace_before.borrow_mut())?;
        let body = self.body.inflate(config)?;
        Ok(Self::Inflated {
            pattern,
            guard,
            body,
            leading_lines,
            whitespace_after_case,
            whitespace_before_if,
            whitespace_after_if,
            whitespace_before_colon,
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[cst_node(Codegen, Inflate, ParenthesizedNode)]
pub enum MatchPattern<'a> {
    Value(MatchValue<'a>),
    Singleton(MatchSingleton<'a>),
    Sequence(MatchSequence<'a>),
    Mapping(MatchMapping<'a>),
    Class(MatchClass<'a>),
    As(Box<MatchAs<'a>>),
    Or(Box<MatchOr<'a>>),
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchValue<'r, 'a> {
    type Inflated = MatchValue<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let value = self.value.inflate(config)?;
        Ok(Self::Inflated { value })
    }
}

impl<'r, 'a> ParenthesizedDeflatedNode<'r, 'a> for DeflatedMatchValue<'r, 'a> {
    fn lpar(&self) -> &Vec<DeflatedLeftParen<'r, 'a>> {
        self.value.lpar()
    }
    fn rpar(&self) -> &Vec<DeflatedRightParen<'r, 'a>> {
        self.value.rpar()
    }
    fn with_parens(
        self,
        left: DeflatedLeftParen<'r, 'a>,
        right: DeflatedRightParen<'r, 'a>,
    ) -> Self {
        Self {
            value: self.value.with_parens(left, right),
        }
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchSingleton<'r, 'a> {
    type Inflated = MatchSingleton<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let value = self.value.inflate(config)?;
        Ok(Self::Inflated { value })
    }
}

impl<'r, 'a> ParenthesizedDeflatedNode<'r, 'a> for DeflatedMatchSingleton<'r, 'a> {
    fn lpar(&self) -> &Vec<DeflatedLeftParen<'r, 'a>> {
        self.value.lpar()
    }
    fn rpar(&self) -> &Vec<DeflatedRightParen<'r, 'a>> {
        self.value.rpar()
    }
    fn with_parens(
        self,
        left: DeflatedLeftParen<'r, 'a>,
        right: DeflatedRightParen<'r, 'a>,
    ) -> Self {
        Self {
            value: self.value.with_parens(left, right),
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[cst_node(Codegen, Inflate, ParenthesizedNode)]
pub enum MatchSequence<'a> {
    MatchList(MatchList<'a>),
    MatchTuple(MatchTuple<'a>),
}

#[cst_node(ParenthesizedNode)]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchList<'r, 'a> {
    type Inflated = MatchList<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbracket = self.lbracket.inflate(config)?;

        let len = self.patterns.len();
        let patterns = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;

        let rbracket = self.rbracket.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            patterns,
            lbracket,
            rbracket,
            lpar,
            rpar,
        })
    }
}

#[cst_node(ParenthesizedNode)]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchTuple<'r, 'a> {
    type Inflated = MatchTuple<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let len = self.patterns.len();
        let patterns = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            patterns,
            lpar,
            rpar,
        })
    }
}

#[allow(clippy::large_enum_variant)]
#[cst_node]
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
}
impl<'r, 'a> DeflatedStarrableMatchSequenceElement<'r, 'a> {
    fn inflate_element(
        self,
        config: &Config<'a>,
        last_element: bool,
    ) -> Result<StarrableMatchSequenceElement<'a>> {
        Ok(match self {
            Self::Simple(s) => {
                StarrableMatchSequenceElement::Simple(s.inflate_element(config, last_element)?)
            }
            Self::Starred(s) => {
                StarrableMatchSequenceElement::Starred(s.inflate_element(config, last_element)?)
            }
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedStarrableMatchSequenceElement<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        match self {
            Self::Simple(s) => Self::Simple(s.with_comma(comma)),
            Self::Starred(s) => Self::Starred(s.with_comma(comma)),
        }
    }
}

#[cst_node]
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
}
impl<'r, 'a> DeflatedMatchSequenceElement<'r, 'a> {
    fn inflate_element(
        self,
        config: &Config<'a>,
        last_element: bool,
    ) -> Result<MatchSequenceElement<'a>> {
        let value = self.value.inflate(config)?;
        let comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(MatchSequenceElement { value, comma })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedMatchSequenceElement<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[cst_node]
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
}
impl<'r, 'a> DeflatedMatchStar<'r, 'a> {
    fn inflate_element(self, config: &Config<'a>, last_element: bool) -> Result<MatchStar<'a>> {
        let whitespace_before_name = parse_parenthesizable_whitespace(
            config,
            &mut self.star_tok.whitespace_after.borrow_mut(),
        )?;
        let name = self.name.inflate(config)?;
        let comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(MatchStar {
            name,
            comma,
            whitespace_before_name,
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedMatchStar<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[cst_node(ParenthesizedNode)]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchMapping<'r, 'a> {
    type Inflated = MatchMapping<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let lbrace = self.lbrace.inflate(config)?;

        let len = self.elements.len();
        let no_star = self.star_tok.is_none();
        let elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(idx, el)| el.inflate_element(config, no_star && idx + 1 == len))
            .collect::<Result<Vec<_>>>()?;

        let (whitespace_before_rest, rest, trailing_comma) =
            if let Some(star_tok) = self.star_tok.as_mut() {
                (
                    parse_simple_whitespace(config, &mut star_tok.whitespace_after.borrow_mut())?,
                    self.rest.inflate(config)?,
                    self.trailing_comma
                        .map(|c| c.inflate_before(config))
                        .transpose()?,
                )
            } else {
                Default::default()
            };

        let rbrace = self.rbrace.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            elements,
            rest,
            trailing_comma,
            lbrace,
            rbrace,
            lpar,
            rpar,
            whitespace_before_rest,
        })
    }
}

#[cst_node]
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
}
impl<'r, 'a> DeflatedMatchMappingElement<'r, 'a> {
    fn inflate_element(
        self,
        config: &Config<'a>,
        last_element: bool,
    ) -> Result<MatchMappingElement<'a>> {
        let key = self.key.inflate(config)?;
        let whitespace_before_colon = parse_parenthesizable_whitespace(
            config,
            &mut self.colon_tok.whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_colon = parse_parenthesizable_whitespace(
            config,
            &mut self.colon_tok.whitespace_after.borrow_mut(),
        )?;
        let pattern = self.pattern.inflate(config)?;
        let comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(MatchMappingElement {
            key,
            pattern,
            comma,
            whitespace_before_colon,
            whitespace_after_colon,
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedMatchMappingElement<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[cst_node(ParenthesizedNode)]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchClass<'r, 'a> {
    type Inflated = MatchClass<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;

        let cls = self.cls.inflate(config)?;
        let whitespace_after_cls = parse_parenthesizable_whitespace(
            config,
            &mut self.lpar_tok.whitespace_before.borrow_mut(),
        )?;
        let whitespace_before_patterns = parse_parenthesizable_whitespace(
            config,
            &mut self.lpar_tok.whitespace_after.borrow_mut(),
        )?;

        let patlen = self.patterns.len();
        let kwdlen = self.kwds.len();
        let patterns = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(idx, pat)| pat.inflate_element(config, idx + 1 == patlen + kwdlen))
            .collect::<Result<_>>()?;
        let kwds = self
            .kwds
            .into_iter()
            .enumerate()
            .map(|(idx, kwd)| kwd.inflate_element(config, idx + 1 == kwdlen))
            .collect::<Result<_>>()?;

        let whitespace_after_kwds = parse_parenthesizable_whitespace(
            config,
            &mut self.rpar_tok.whitespace_before.borrow_mut(),
        )?;

        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            cls,
            patterns,
            kwds,
            lpar,
            rpar,
            whitespace_after_cls,
            whitespace_before_patterns,
            whitespace_after_kwds,
        })
    }
}

#[cst_node]
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
}
impl<'r, 'a> DeflatedMatchKeywordElement<'r, 'a> {
    fn inflate_element(
        self,
        config: &Config<'a>,
        last_element: bool,
    ) -> Result<MatchKeywordElement<'a>> {
        let key = self.key.inflate(config)?;
        let whitespace_before_equal = parse_parenthesizable_whitespace(
            config,
            &mut self.equal_tok.whitespace_before.borrow_mut(),
        )?;
        let whitespace_after_equal = parse_parenthesizable_whitespace(
            config,
            &mut self.equal_tok.whitespace_after.borrow_mut(),
        )?;
        let pattern = self.pattern.inflate(config)?;
        let comma = if last_element {
            self.comma.map(|c| c.inflate_before(config)).transpose()
        } else {
            self.comma.inflate(config)
        }?;
        Ok(MatchKeywordElement {
            key,
            pattern,
            comma,
            whitespace_before_equal,
            whitespace_after_equal,
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedMatchKeywordElement<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[cst_node(ParenthesizedNode)]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchAs<'r, 'a> {
    type Inflated = MatchAs<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let pattern = self.pattern.inflate(config)?;
        let (whitespace_before_as, whitespace_after_as) = if let Some(as_tok) = self.as_tok.as_mut()
        {
            (
                Some(parse_parenthesizable_whitespace(
                    config,
                    &mut as_tok.whitespace_before.borrow_mut(),
                )?),
                Some(parse_parenthesizable_whitespace(
                    config,
                    &mut as_tok.whitespace_after.borrow_mut(),
                )?),
            )
        } else {
            Default::default()
        };
        let name = self.name.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            pattern,
            name,
            lpar,
            rpar,
            whitespace_before_as,
            whitespace_after_as,
        })
    }
}

#[cst_node]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchOrElement<'r, 'a> {
    type Inflated = MatchOrElement<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let pattern = self.pattern.inflate(config)?;
        let separator = self.separator.inflate(config)?;
        Ok(Self::Inflated { pattern, separator })
    }
}

#[cst_node(ParenthesizedNode)]
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

impl<'r, 'a> Inflate<'a> for DeflatedMatchOr<'r, 'a> {
    type Inflated = MatchOr<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lpar = self.lpar.inflate(config)?;
        let patterns = self.patterns.inflate(config)?;
        let rpar = self.rpar.inflate(config)?;
        Ok(Self::Inflated {
            patterns,
            lpar,
            rpar,
        })
    }
}

#[cst_node]
pub struct TypeVar<'a> {
    pub name: Name<'a>,
    pub bound: Option<Box<Expression<'a>>>,
    pub colon: Option<Colon<'a>>,
}

impl<'a> Codegen<'a> for TypeVar<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.name.codegen(state);
        self.colon.codegen(state);
        if let Some(bound) = &self.bound {
            bound.codegen(state);
        }
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedTypeVar<'r, 'a> {
    type Inflated = TypeVar<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let name = self.name.inflate(config)?;
        let colon = self.colon.inflate(config)?;
        let bound = self.bound.inflate(config)?;
        Ok(Self::Inflated { name, bound, colon })
    }
}

#[cst_node]
pub struct TypeVarTuple<'a> {
    pub name: Name<'a>,

    pub whitespace_after_star: SimpleWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for TypeVarTuple<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("*");
        self.whitespace_after_star.codegen(state);
        self.name.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedTypeVarTuple<'r, 'a> {
    type Inflated = TypeVarTuple<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_star =
            parse_simple_whitespace(config, &mut self.star_tok.whitespace_after.borrow_mut())?;
        let name = self.name.inflate(config)?;
        Ok(Self::Inflated {
            name,
            whitespace_after_star,
        })
    }
}

#[cst_node]
pub struct ParamSpec<'a> {
    pub name: Name<'a>,

    pub whitespace_after_star: SimpleWhitespace<'a>,

    pub(crate) star_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for ParamSpec<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("**");
        self.whitespace_after_star.codegen(state);
        self.name.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedParamSpec<'r, 'a> {
    type Inflated = ParamSpec<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_star =
            parse_simple_whitespace(config, &mut self.star_tok.whitespace_after.borrow_mut())?;
        let name = self.name.inflate(config)?;
        Ok(Self::Inflated {
            name,
            whitespace_after_star,
        })
    }
}

#[cst_node(Inflate, Codegen)]
pub enum TypeVarLike<'a> {
    TypeVar(TypeVar<'a>),
    TypeVarTuple(TypeVarTuple<'a>),
    ParamSpec(ParamSpec<'a>),
}

#[cst_node]
pub struct TypeParam<'a> {
    pub param: TypeVarLike<'a>,
    pub comma: Option<Comma<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub star: &'a str,
    pub whitespace_after_star: SimpleWhitespace<'a>,
    pub default: Option<Expression<'a>>,
    pub star_tok: Option<TokenRef<'a>>,
}

impl<'a> Codegen<'a> for TypeParam<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.param.codegen(state);
        self.equal.codegen(state);
        state.add_token(self.star);
        self.whitespace_after_star.codegen(state);
        self.default.codegen(state);
        self.comma.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedTypeParam<'r, 'a> {
    type Inflated = TypeParam<'a>;
    fn inflate(mut self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_star = if let Some(star_tok) = self.star_tok.as_mut() {
            parse_simple_whitespace(config, &mut star_tok.whitespace_after.borrow_mut())?
        } else {
            Default::default()
        };
        let param = self.param.inflate(config)?;
        let equal = self.equal.inflate(config)?;
        let default = self.default.inflate(config)?;
        let comma = self.comma.inflate(config)?;
        Ok(Self::Inflated {
            param,
            comma,
            equal,
            star: self.star,
            whitespace_after_star,
            default,
        })
    }
}

impl<'r, 'a> WithComma<'r, 'a> for DeflatedTypeParam<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self {
        Self {
            comma: Some(comma),
            ..self
        }
    }
}

#[cst_node]
pub struct TypeParameters<'a> {
    pub params: Vec<TypeParam<'a>>,

    pub lbracket: LeftSquareBracket<'a>,
    pub rbracket: RightSquareBracket<'a>,
}

impl<'a> Codegen<'a> for TypeParameters<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        self.lbracket.codegen(state);
        let params_len = self.params.len();
        for (idx, param) in self.params.iter().enumerate() {
            param.codegen(state);
            if idx + 1 < params_len && param.comma.is_none() {
                state.add_token(", ");
            }
        }
        self.rbracket.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedTypeParameters<'r, 'a> {
    type Inflated = TypeParameters<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let lbracket = self.lbracket.inflate(config)?;
        let params = self.params.inflate(config)?;
        let rbracket = self.rbracket.inflate(config)?;
        Ok(Self::Inflated {
            params,
            lbracket,
            rbracket,
        })
    }
}

#[cst_node]
pub struct TypeAlias<'a> {
    pub name: Name<'a>,
    pub value: Box<Expression<'a>>,
    pub type_parameters: Option<TypeParameters<'a>>,

    pub whitespace_after_type: SimpleWhitespace<'a>,
    pub whitespace_after_name: Option<SimpleWhitespace<'a>>,
    pub whitespace_after_type_parameters: Option<SimpleWhitespace<'a>>,
    pub whitespace_after_equals: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,

    pub(crate) type_tok: TokenRef<'a>,
    pub(crate) lbracket_tok: Option<TokenRef<'a>>,
    pub(crate) equals_tok: TokenRef<'a>,
}

impl<'a> Codegen<'a> for TypeAlias<'a> {
    fn codegen(&self, state: &mut CodegenState<'a>) {
        state.add_token("type");
        self.whitespace_after_type.codegen(state);
        self.name.codegen(state);
        if self.whitespace_after_name.is_none() && self.type_parameters.is_none() {
            state.add_token(" ");
        } else {
            self.whitespace_after_name.codegen(state);
        }
        if self.type_parameters.is_some() {
            self.type_parameters.codegen(state);
            self.whitespace_after_type_parameters.codegen(state);
        }
        state.add_token("=");
        self.whitespace_after_equals.codegen(state);
        self.value.codegen(state);
        self.semicolon.codegen(state);
    }
}

impl<'r, 'a> Inflate<'a> for DeflatedTypeAlias<'r, 'a> {
    type Inflated = TypeAlias<'a>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        let whitespace_after_type =
            parse_simple_whitespace(config, &mut self.type_tok.whitespace_after.borrow_mut())?;
        let name = self.name.inflate(config)?;
        let whitespace_after_name = Some(if let Some(tok) = self.lbracket_tok {
            parse_simple_whitespace(config, &mut tok.whitespace_before.borrow_mut())
        } else {
            parse_simple_whitespace(config, &mut self.equals_tok.whitespace_before.borrow_mut())
        }?);
        let type_parameters = self.type_parameters.inflate(config)?;
        let whitespace_after_type_parameters = if type_parameters.is_some() {
            Some(parse_simple_whitespace(
                config,
                &mut self.equals_tok.whitespace_before.borrow_mut(),
            )?)
        } else {
            None
        };
        let whitespace_after_equals =
            parse_simple_whitespace(config, &mut self.equals_tok.whitespace_after.borrow_mut())?;
        let value = self.value.inflate(config)?;
        let semicolon = self.semicolon.inflate(config)?;
        Ok(Self::Inflated {
            name,
            value,
            type_parameters,
            whitespace_after_type,
            whitespace_after_name,
            whitespace_after_type_parameters,
            whitespace_after_equals,
            semicolon,
        })
    }
}

impl<'r, 'a> DeflatedTypeAlias<'r, 'a> {
    pub fn with_semicolon(self, semicolon: Option<DeflatedSemicolon<'r, 'a>>) -> Self {
        Self { semicolon, ..self }
    }
}
