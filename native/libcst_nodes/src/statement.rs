// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{
    Attribute, Codegen, CodegenState, Comma, Dot, EmptyLine, Expression, From, ImportStar,
    LeftParen, List, Name, NameOrAttribute, Parameters, ParenthesizableWhitespace, RightParen,
    Semicolon, SimpleWhitespace, StarredElement, Subscript, TrailingWhitespace, Tuple,
};
use crate::{traits::WithComma, Arg, AssignEqual, Asynchronous, AugOp, ParenthesizedNode};

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompoundStatement<'a> {
    FunctionDef(FunctionDef<'a>),
    If(If<'a>),
    For(For<'a>),
    While(While<'a>),
    ClassDef(ClassDef<'a>),
    Try(Try<'a>),
    With(With<'a>),
}

impl<'a> Codegen<'a> for CompoundStatement<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        match self {
            Self::FunctionDef(f) => f.codegen(state),
            Self::If(f) => f.codegen(state),
            Self::For(f) => f.codegen(state),
            Self::While(f) => f.codegen(state),
            Self::ClassDef(c) => c.codegen(state),
            Self::Try(t) => t.codegen(state),
            Self::With(w) => w.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
#[derive(Debug, Eq, PartialEq, Clone)]
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
    Return(Return<'a>),
    Expr {
        value: Expression<'a>,
        semicolon: Option<Semicolon<'a>>,
    },
    Assert(Assert<'a>),
    Import(Import<'a>),
    ImportFrom(ImportFrom<'a>),
    Assign(Assign<'a>),
    AnnAssign(AnnAssign<'a>),
    Raise(Raise<'a>),
    Global(Global<'a>),
    Nonlocal(Nonlocal<'a>),
    AugAssign(AugAssign<'a>),
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
            Self::AnnAssign(a) => a.codegen(state),
            Self::Return(r) => r.codegen(state),
            Self::Assert(a) => a.codegen(state),
            Self::Raise(r) => r.codegen(state),
            Self::Global(g) => g.codegen(state),
            Self::Nonlocal(l) => l.codegen(state),
            Self::AugAssign(a) => a.codegen(state),
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

impl<'a> ParenthesizedNode<'a> for AssignTargetExpression<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        match self {
            Self::Name(n) => n.lpar(),
            Self::Attribute(n) => n.lpar(),
            Self::StarredElement(n) => n.lpar(),
            Self::Tuple(n) => n.lpar(),
            Self::List(n) => n.lpar(),
            Self::Subscript(n) => n.lpar(),
        }
    }

    fn rpar(&self) -> &Vec<RightParen<'a>> {
        match self {
            Self::Name(n) => n.rpar(),
            Self::Attribute(n) => n.rpar(),
            Self::StarredElement(n) => n.rpar(),
            Self::Tuple(n) => n.rpar(),
            Self::List(n) => n.rpar(),
            Self::Subscript(n) => n.rpar(),
        }
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        match self {
            Self::Name(n) => Self::Name(n.with_parens(left, right)),
            Self::Attribute(n) => Self::Attribute(n.with_parens(left, right)),
            Self::StarredElement(n) => Self::StarredElement(n.with_parens(left, right)),
            Self::Tuple(n) => Self::Tuple(n.with_parens(left, right)),
            Self::List(n) => Self::List(n.with_parens(left, right)),
            Self::Subscript(n) => Self::Subscript(n.with_parens(left, right)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportAlias<'a> {
    pub name: NameOrAttribute<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> WithComma<'a> for ImportAlias<'a> {
    fn with_comma(self, comma: Comma<'a>) -> ImportAlias<'a> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AsName<'a> {
    pub name: AssignTargetExpression<'a>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Decorator<'a> {
    pub decorator: Expression<'a>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Annotation<'a> {
    pub annotation: Expression<'a>,
    pub whitespace_before_indicator: Option<ParenthesizableWhitespace<'a>>,
    pub whitespace_after_indicator: ParenthesizableWhitespace<'a>,
}

impl<'a> Annotation<'a> {
    pub fn codegen(&'a self, state: &mut CodegenState<'a>, default_indicator: &'a str) {
        if let Some(ws) = &self.whitespace_before_indicator {
            ws.codegen(state);
        } else if default_indicator == "->" {
            state.add_token(" ");
        } else {
            panic!("this should never happen");
        }

        state.add_token(default_indicator);
        self.whitespace_after_indicator.codegen(state);
        self.annotation.codegen(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AnnAssign<'a> {
    pub target: AssignTargetExpression<'a>,
    pub annotation: Annotation<'a>,
    pub value: Option<Expression<'a>>,
    pub equal: Option<AssignEqual<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for AnnAssign<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Return<'a> {
    pub value: Option<Expression<'a>>,
    pub whitespace_after_return: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Return<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assert<'a> {
    pub test: Expression<'a>,
    pub msg: Option<Expression<'a>>,
    pub comma: Option<Comma<'a>>,
    pub whitespace_after_assert: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Assert<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Raise<'a> {
    pub exc: Option<Expression<'a>>,
    pub cause: Option<From<'a>>,
    pub whitespace_after_raise: Option<SimpleWhitespace<'a>>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Raise<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NameItem<'a> {
    pub name: Name<'a>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> NameItem<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>, default_comma: bool) {
        self.name.codegen(state);
        if let Some(comma) = &self.comma {
            comma.codegen(state);
        } else if default_comma {
            state.add_token(", ");
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Global<'a> {
    pub names: Vec<NameItem<'a>>,
    pub whitespace_after_global: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Global<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Nonlocal<'a> {
    pub names: Vec<NameItem<'a>>,
    pub whitespace_after_nonlocal: SimpleWhitespace<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for Nonlocal<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
}

impl<'a> Codegen<'a> for For<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct While<'a> {
    pub test: Expression<'a>,
    pub body: Suite<'a>,
    pub orelse: Option<Else<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_while: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for While<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
}

impl<'a> Codegen<'a> for ClassDef<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

impl<'a> ClassDef<'a> {
    pub fn with_decorators(self, decorators: Vec<Decorator<'a>>) -> Self {
        Self { decorators, ..self }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Finally<'a> {
    pub body: Suite<'a>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for Finally<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExceptHandler<'a> {
    pub body: Suite<'a>,
    pub r#type: Option<Expression<'a>>,
    pub name: Option<AsName<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_except: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for ExceptHandler<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Try<'a> {
    pub body: Suite<'a>,
    pub handlers: Vec<ExceptHandler<'a>>,
    pub orelse: Option<Else<'a>>,
    pub finalbody: Option<Finally<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for Try<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AugAssign<'a> {
    pub target: AssignTargetExpression<'a>,
    pub operator: AugOp<'a>,
    pub value: Expression<'a>,
    pub semicolon: Option<Semicolon<'a>>,
}

impl<'a> Codegen<'a> for AugAssign<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
        self.target.codegen(state);
        self.operator.codegen(state);
        self.value.codegen(state);

        if let Some(s) = &self.semicolon {
            s.codegen(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WithItem<'a> {
    pub item: Expression<'a>,
    pub asname: Option<AsName<'a>>,
    pub comma: Option<Comma<'a>>,
}

impl<'a> Codegen<'a> for WithItem<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
pub struct With<'a> {
    pub items: Vec<WithItem<'a>>,
    pub body: Suite<'a>,
    pub asynchronous: Option<Asynchronous<'a>>,
    pub leading_lines: Vec<EmptyLine<'a>>,
    pub whitespace_after_with: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> Codegen<'a> for With<'a> {
    fn codegen(&'a self, state: &mut CodegenState<'a>) {
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
