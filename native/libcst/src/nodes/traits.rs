use crate::{Codegen, CodegenState, Comma, EmptyLine, LeftParen, RightParen};

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
