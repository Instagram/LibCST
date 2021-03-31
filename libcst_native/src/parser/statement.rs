use super::{
    Codegen, CodegenState, EmptyLine, Name, Parameters, Semicolon, SimpleWhitespace,
    TrailingWhitespace,
};

#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'a> {
    FunctionDef(FunctionDef<'a>),
    Pass,
}

impl<'a> Codegen for Statement<'a> {
    fn codegen(&self, state: &mut CodegenState) -> () {
        match &self {
            &Self::Pass => state.add_token("pass".to_string()),
            &Self::FunctionDef(f) => f.codegen(state),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SmallStatement<'a> {
    pub semicolon: Option<Semicolon<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: Name<'a>,
    pub decorators: Vec<Decorator<'a>>,
    pub params: Parameters<'a>,

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
        // TODO: body
        state.add_token(" ...".to_string());
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
