// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::{
    Codegen, CodegenState, Comma, EmptyLine, Expression, Name, Parameters, Semicolon,
    SimpleWhitespace, TrailingWhitespace,
};

#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'a> {
    Simple(SimpleStatementLine<'a>),
    Compound(CompoundStatement<'a>),
}

impl<'a> Codegen for Statement<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::Simple(s) => s.codegen(state),
            &Self::Compound(f) => f.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompoundStatement<'a> {
    FunctionDef(FunctionDef<'a>),
    If(If<'a>),
}

impl<'a> Codegen for CompoundStatement<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::FunctionDef(f) => f.codegen(state),
            &Self::If(f) => f.codegen(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Suite<'a> {
    IndentedBlock(IndentedBlock<'a>),
    SimpleStatementSuite(SimpleStatementSuite<'a>),
}

impl<'a> Codegen for Suite<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::IndentedBlock(b) => b.codegen(state),
            &Self::SimpleStatementSuite(s) => s.codegen(state),
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

impl<'a> Codegen for IndentedBlock<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        self.header.codegen(state);

        let indent = match self.indent {
            Some(i) => i,
            None => todo!(),
        };
        state.indent(indent.to_string());

        if self.body.is_empty() {
            // Empty indented blocks are not syntactically valid in Python unless they
            // contain a 'pass' statement, so add one here.
            state.add_indent();
            state.add_token("pass".to_string());
            state.add_token(state.default_newline.to_owned());
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
    body: &Vec<SmallStatement<'a>>,
    trailing_whitespace: &TrailingWhitespace<'a>,
    state: &mut CodegenState,
) {
    for stmt in body {
        stmt.codegen(state);
        // TODO: semicolon
    }
    if body.is_empty() {
        // Empty simple statement blocks are not syntactically valid in Python
        // unless they contain a 'pass' statement, so add one here.
        state.add_token("pass".to_owned())
    }
    trailing_whitespace.codegen(state);
}

impl<'a> Codegen for SimpleStatementSuite<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
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

impl<'a> Codegen for SimpleStatementLine<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        for line in &self.leading_lines {
            line.codegen(state);
        }
        state.add_indent();
        _simple_statement_codegen(&self.body, &self.trailing_whitespace, state);
    }
}

#[allow(dead_code)]
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
    }, // TODO Import, ImportFrom
       // TODO Assign, AnnAssign
       // TODO Raise
       // TODO Global, Nonlocal
}

impl<'a> Codegen for SmallStatement<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::Pass { .. } => state.add_token("pass".to_string()),
            &Self::Break { .. } => state.add_token("break".to_string()),
            &Self::Continue { .. } => state.add_token("continue".to_string()),
            &Self::Expr { value: e, .. } => e.codegen(state),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: Name<'a>,
    pub params: Parameters<'a>,
    pub body: Suite<'a>,

    pub decorators: Vec<Decorator<'a>>,
    pub whitespace_after_def: SimpleWhitespace<'a>,
    pub whitespace_after_name: SimpleWhitespace<'a>,
    pub whitespace_before_colon: SimpleWhitespace<'a>,
}

impl<'a> FunctionDef<'a> {
    pub fn with_decorators(self, decs: Vec<Decorator<'a>>) -> Self {
        Self {
            decorators: self
                .decorators
                .into_iter()
                .chain(decs.into_iter())
                .collect(),
            ..self
        }
    }
}

impl<'a> Codegen for FunctionDef<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        // TODO: leading lines
        for dec in self.decorators.iter() {
            dec.codegen(state);
        }
        // TODO: lines_after_decorators
        state.add_indent();

        // TODO: async
        state.add_token("def".to_string());
        self.whitespace_after_def.codegen(state);
        self.name.codegen(state);
        self.whitespace_after_name.codegen(state);
        state.add_token("(".to_string());
        // TODO: params
        state.add_token(")".to_string());
        // TODO: returns
        self.whitespace_before_colon.codegen(state);
        state.add_token(":".to_string());
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

impl<'a> Codegen for Decorator<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        for ll in self.leading_lines.iter() {
            ll.codegen(state);
        }
        state.add_indent();
        state.add_token("@".to_string());
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

impl<'a> Codegen for If<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        state.add_indent();

        state.add_token(if self.is_elif { "elif" } else { "if" }.to_string());
        self.whitespace_before_test.codegen(state);
        self.test.codegen(state);
        self.whitespace_after_test.codegen(state);
        state.add_token(":".to_string());
        self.body.codegen(state);
        match &self.orelse {
            Some(orelse) => orelse.codegen(state),
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OrElse<'a> {
    Elif(If<'a>),
    Else(Else<'a>),
}

impl<'a> Codegen for OrElse<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::Elif(f) => f.codegen(state),
            &Self::Else(f) => f.codegen(state),
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

impl<'a> Codegen for Else<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        for l in &self.leading_lines {
            l.codegen(state);
        }
        state.add_indent();

        state.add_token("else".to_string());
        self.whitespace_before_colon.codegen(state);
        state.add_token(":".to_string());
        self.body.codegen(state);
    }
}
