use super::SimpleWhitespace;

#[derive(Debug, Eq, PartialEq)]
pub struct Semicolon<'a> {
    pub whitespace_before: SimpleWhitespace<'a>,
    pub whitespace_after: SimpleWhitespace<'a>,
}
