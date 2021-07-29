// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{
    Attribute, Codegen, CodegenState, Comma, Dot, EmptyLine, Expression, ImportStar, LeftParen,
    List, Name, NameOrAttribute, Parameters, ParenthesizableWhitespace, RightParen, Semicolon,
    SimpleWhitespace, StarredElement, Subscript, TrailingWhitespace, Tuple,
};

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'a> {
    Simple(SimpleStatementLine<'a>),
    Compound(CompoundStatement<'a>),
}

impl<'a> Codegen<'a> for Statement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::Simple(s) => s.codegen(state),
            Self::Compound(f) => f.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompoundStatement<'a> {
    FunctionDef(FunctionDef<'a>),
    If(If<'a>),
}

impl<'a> Codegen<'a> for CompoundStatement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::FunctionDef(f) => f.codegen(state),
            Self::If(f) => f.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Suite<'a> {
    IndentedBlock(IndentedBlock<'a>),
    SimpleStatementSuite(SimpleStatementSuite<'a>),
}

impl<'a> Codegen<'a> for Suite<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::IndentedBlock(b) => b.codegen(state),
            Self::SimpleStatementSuite(s) => s.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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
}

impl<'a> Codegen<'a> for IndentedBlock<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.header.codegen(state);

        let indent = match self.indent {
            Some(i) => i,
            None => todo!(),
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

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleStatementSuite<'a> {
    /// Sequence of small statements. All but the last statement are required to have
    /// a semicolon.
    pub body: Vec<SmallStatement<'a>>,

    /// The whitespace between the colon in the parent statement and the body.
    pub leading_whitespace: SimpleWhitespace<'a>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub trailing_whitespace: TrailingWhitespace<'a>,
}

impl<'a> Default for SimpleStatementSuite<'a> {
    fn default() -> Self {
        Self {
            body: Default::default(),
            leading_whitespace: SimpleWhitespace(" "),
            trailing_whitespace: Default::default(),
        }
    }
}

fn _simple_statement_codegen<'a>(
    body: &'a [SmallStatement<'a>],
    trailing_whitespace: &'a TrailingWhitespace<'a>,
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
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.leading_whitespace.codegen(state);
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleStatementLine<'a> {
    /// Sequence of small statements. All but the last statement are required to have
    /// a semicolon.
    pub body: Vec<SmallStatement<'a>>,

    /// Sequence of empty lines appearing before this simple statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,
    /// Any optional trailing comment and the final ``NEWLINE`` at the end of the line.
    pub trailing_whitespace: TrailingWhitespace<'a>,
}

impl<'a> Codegen<'a> for SimpleStatementLine<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        for line in &self.leading_lines {
            line.codegen(state);
        }
        state.add_indent();
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

#[allow(dead_code, clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq)]
pub enum SmallStatement<'a> {
    Pass {
        semicolon: Option<Semicolon<'a>>,
    },
    Break {
        semicolon: Option<Semicolon<'a>>,
    },
    Continue {
        semicolon: Option<Semicolon<'a>>,
    },
    Return {
        value: Option<&'a str>, // TODO
        whitespace_after_return: SimpleWhitespace<'a>,
        semicolon: Option<Semicolon<'a>>,
    },
    Expr {
        value: Expression<'a>,
        semicolon: Option<Semicolon<'a>>,
    },
    Assert {
        test: &'a str,        // TODO
        msg: Option<&'a str>, // TODO
        comma: Option<Comma<'a>>,
        whitespace_after_assert: SimpleWhitespace<'a>,
        semicolon: Option<Semicolon<'a>>,
    },
    Import(Import<'a>),
    ImportFrom(ImportFrom<'a>),
    Assign(Assign<'a>),
    // TODO Assign, AnnAssign
    // TODO Raise
    // TODO Global, Nonlocal
}

impl<'a> Codegen<'a> for SmallStatement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::Pass { .. } => state.add_token("pass"),
            Self::Break { .. } => state.add_token("break"),
            Self::Continue { .. } => state.add_token("continue"),
            Self::Expr { value: e, .. } => e.codegen(state),
            Self::Import(i) => i.codegen(state),
            Self::ImportFrom(i) => i.codegen(state),
            Self::Assign(a) => a.codegen(state),
            _ => panic!("No codegen implemented for {:#?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign<'a> {
    pub targets: Vec<AssignTarget<'a>>,
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Assign<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        for target in &self.targets {
            target.codegen(state);
        }
        self.value.codegen(state);
        if let Some(semi) = &self.semicolon {
            semi.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AssignTarget<'a> {
    pub target: AssignTargetExpression<'a>,
    pub whitespace_before_equal: SimpleWhitespace<'a>,
    pub whitespace_after_equal: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for AssignTarget<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.whitespace_before_equal.codegen(state);
        state.add_token("=");
        self.whitespace_after_equal.codegen(state);
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AssignTargetExpression<'a> {
    Name(Name<'a>),
    Attribute(Attribute<'a>),
    StarredElement(StarredElement<'a>),
    Tuple(Tuple<'a>),
    List(List<'a>),
    Subscript(Subscript<'a>),
}

impl<'a> Codegen<'a> for AssignTargetExpression<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::Name(n) => n.codegen(state),
            Self::Attribute(a) => a.codegen(state),
            Self::StarredElement(e) => e.codegen(state),
            Self::Tuple(t) => t.codegen(state),
            Self::List(l) => l.codegen(state),
            Self::Subscript(s) => s.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Import<'a> {
    pub names: Vec<ImportAlias<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
    pub whitespace_after_import: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for Import<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq)]
pub struct ImportFrom<'a> {
    pub module: Option<NameOrAttribute<'a>>,
    pub names: ImportNames<'a>,
    pub relative: Vec<Dot<'a>>,
    pub lpar: Option<LeftParen<'a>>,
    pub rpar: Option<RightParen<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
    pub whitespace_after_from: SimpleWhitespace<'a>,
    pub whitespace_before_import: SimpleWhitespace<'a>,
    pub whitespace_after_import: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for ImportFrom<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq)]
pub struct ImportAlias<'a> {
    pub name: NameOrAttribute<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> ImportAlias<'a> {
    pub fn with_comma(self, comma: Comma<'a>) -> ImportAlias<'a> {
        let comma = Some(comma);
        Self { comma, ..self }
    }
}

impl<'a> Codegen<'a> for ImportAlias<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.name.codegen(state);
        if let Some(asname) = &self.asname {
            asname.codegen(state);
        }
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AsName<'a> {
    pub name: NameOrAttribute<'a>,
    pub whitespace_before_as: ParenthesizableWhitespace<'a>,
    pub whitespace_after_as: ParenthesizableWhitespace<'a>,
}

impl<'a> Codegen<'a> for AsName<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.whitespace_before_as.codegen(state);
        state.add_token("as");
        self.whitespace_after_as.codegen(state);
        self.name.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImportNames<'a> {
    Star(ImportStar),
    Aliases(Vec<ImportAlias<'a>>),
}

impl<'a> Codegen<'a> for ImportNames<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: Name<'a>,
    pub params: Parameters<'a>,
    pub body: Suite<'a>,

    pub leading_lines: Vec<EmptyLine<'a>>,
    pub lines_after_decorators: Vec<EmptyLine<'a>>,
    pub decorators: Vec<Decorator<'a>>,
    pub whitespace_after_def: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_params: ParenthesizableWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> FunctionDef<'a> {
    pub fn with_decorators(self, mut decs: Vec<Decorator<'a>>) -> Self {
        let mut lines_before_decorators = vec![];
        let lines_after_decorators = self.leading_lines;

        if let Some(first_dec) = decs.first_mut() {
            std::mem::swap(&mut first_dec.leading_lines, &mut lines_before_decorators);
        }
        Self {
            decorators: decs,
            leading_lines: lines_before_decorators,
            lines_after_decorators,
            ..self
        }
    }
}

impl<'a> Codegen<'a> for FunctionDef<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

        // TODO: async
        state.add_token("def");
        self.whitespace_after_def.codegen(state);
        self.name.codegen(state);
        self.whitespace_after_name.codegen(state);
        state.add_token("(");
        self.whitespace_before_params.codegen(state);
        self.params.codegen(state);
        state.add_token(")");
        // TODO: returns
        self.whitespace_before_colon.codegen(state);
        state.add_token(":");
        self.body.codegen(state);
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Decorator<'a> {
    pub decorator: Name<'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_at: SimpleWhitespace<'a>,
    pub trailing_whitespace: TrailingWhitespace<'a>,
}

impl<'a> Codegen<'a> for Decorator<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq)]
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
    pub is_elif: bool,
}

impl<'a> Codegen<'a> for If<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq)]
pub enum OrElse<'a> {
    Elif(If<'a>),
    Else(Else<'a>),
}

impl<'a> Codegen<'a> for OrElse<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::Elif(f) => f.codegen(state),
            Self::Else(f) => f.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Else<'a> {
    pub body: Suite<'a>,
    /// Sequence of empty lines appearing before this compound statement line.
    pub leading_lines: Vec<EmptyLine<'a>>,
    /// The whitespace appearing after the ``else`` keyword but before the colon.
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for Else<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
