// This implementation is Copyright (c) Meta Platforms, Inc. and affiliates.
//
// CPython 3.10.0a5 and the original C code this is based on is
// Copyright (c) 2001-2021 Python Software Foundation; All Rights Reserved
//
// Portions of this module (f-string splitting) are based on parso's tokenize.py, which is also PSF
// licensed.

/// Helper types for string processing in the core tokenizer.
use std::convert::TryFrom;

use crate::tokenizer::text_position::TextPositionSnapshot;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum StringQuoteSize {
    Single,
    Triple,
}

impl From<StringQuoteSize> for usize {
    fn from(qs: StringQuoteSize) -> Self {
        match qs {
            StringQuoteSize::Single => 1,
            StringQuoteSize::Triple => 3,
        }
    }
}

#[derive(Clone, Copy)]
pub enum StringQuoteChar {
    Apostrophe,
    DoubleQuote,
}

impl StringQuoteChar {
    pub fn triple_str(&self) -> &'static str {
        match self {
            Self::Apostrophe => "'''",
            Self::DoubleQuote => "\"\"\"",
        }
    }
}

impl From<StringQuoteChar> for char {
    fn from(ch: StringQuoteChar) -> Self {
        match ch {
            StringQuoteChar::Apostrophe => '\'',
            StringQuoteChar::DoubleQuote => '"',
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0:?} is not a valid string quote character")]
pub struct StringQuoteCharConversionError(Option<char>);

impl TryFrom<Option<char>> for StringQuoteChar {
    type Error = StringQuoteCharConversionError;

    fn try_from(ch: Option<char>) -> Result<Self, StringQuoteCharConversionError> {
        match ch {
            Some('\'') => Ok(StringQuoteChar::Apostrophe),
            Some('"') => Ok(StringQuoteChar::DoubleQuote),
            _ => Err(StringQuoteCharConversionError(ch)),
        }
    }
}

#[derive(Clone)]
pub struct FStringNode {
    pub quote_char: StringQuoteChar,
    pub quote_size: StringQuoteSize,
    pub parentheses_count: usize,
    pub string_start: Option<TextPositionSnapshot>,
    // In the syntax there can be multiple format_spec's nested: {x:{y:3}}
    pub format_spec_count: usize,
    pub is_raw_string: bool,
}

impl FStringNode {
    pub fn new(
        quote_char: StringQuoteChar,
        quote_size: StringQuoteSize,
        is_raw_string: bool,
    ) -> Self {
        Self {
            quote_char,
            quote_size,
            parentheses_count: 0,
            string_start: None,
            format_spec_count: 0,
            is_raw_string,
        }
    }

    pub fn open_parentheses(&mut self) {
        self.parentheses_count += 1;
    }

    pub fn close_parentheses(&mut self) {
        if self.is_in_format_spec() {
            self.format_spec_count -= 1;
        }
        self.parentheses_count -= 1;
    }

    pub fn allow_multiline(&self) -> bool {
        self.quote_size == StringQuoteSize::Triple || self.is_in_expr()
    }

    pub fn is_in_expr(&self) -> bool {
        self.parentheses_count > self.format_spec_count
    }

    pub fn is_in_format_spec(&self) -> bool {
        !self.is_in_expr() && self.format_spec_count > 0
    }
}
