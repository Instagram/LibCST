/// A lightweight immutable version of libcst_tokenize::TextPosition that's slightly
/// cheaper to construct/store. Used for storing the start position of tokens.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TextPositionSnapshot {
    pub inner_byte_idx: usize,
    pub inner_char_column_number: usize,
    pub inner_line_number: usize,
}

impl TextPositionSnapshot {
    pub fn byte_idx(&self) -> usize {
        self.inner_byte_idx
    }

    pub fn char_column_number(&self) -> usize {
        self.inner_char_column_number
    }

    pub fn line_number(&self) -> usize {
        self.inner_line_number
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WhitespacePosition<'a> {
    pub line: usize,   // one-indexed (to match parso's behavior)
    pub column: usize, // zero-indexed (to match parso's behavior)
    pub column_byte: usize,
    pub absolute_indent: &'a str,
    pub is_parenthesized: bool,
    pub byte_offset: usize,
}

impl<'a> Default for WhitespacePosition<'a> {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            column_byte: 0,
            absolute_indent: "",
            is_parenthesized: false,
            byte_offset: 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenPosition<'a> {
    pub start_pos: TextPositionSnapshot,
    pub end_pos: TextPositionSnapshot,
    pub whitespace_before: WhitespacePosition<'a>,
    pub whitespace_after: WhitespacePosition<'a>,
    pub relative_indent: Option<&'a str>,
}
