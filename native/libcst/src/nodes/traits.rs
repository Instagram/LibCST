use crate::{
    tokenizer::whitespace_parser::{Config, WhitespaceError},
    Codegen, CodegenState, Comma, EmptyLine, LeftParen, RightParen,
};

pub trait WithComma<'a> {
    fn with_comma(self, comma: Comma<'a>) -> Self;
}

pub trait ParenthesizedNode<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>>;
    fn rpar(&self) -> &Vec<RightParen<'a>>;

    fn parenthesize<F>(&'a self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        for lpar in self.lpar() {
            lpar.codegen(state);
        }
        f(state);
        for rpar in self.rpar() {
            rpar.codegen(state);
        }
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self;
}

pub trait WithLeadingLines<'a> {
    fn leading_lines(&self) -> &Vec<EmptyLine<'a>>;
}

pub type Result<T> = std::result::Result<T, WhitespaceError>;

pub trait Inflate<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()>;
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Option<T> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        if let Some(t) = self {
            t.inflate(config)
        } else {
            Ok(())
        }
    }
}

impl<'a, T: Inflate<'a> + ?Sized> Inflate<'a> for Box<T> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        (**self).inflate(config)
    }
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Vec<T> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        for item in self.iter_mut() {
            item.inflate(config)?;
        }
        Ok(())
    }
}
