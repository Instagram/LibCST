// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

mod char_width;

use regex::Regex;
use std::fmt;

use crate::tokenizer::debug_utils::EllipsisDebug;
use char_width::NewlineNormalizedCharWidths;

pub trait TextPattern {
    fn match_len(&self, text: &str) -> Option<usize>;
}

impl TextPattern for &Regex {
    // make sure to anchor your regex with \A
    fn match_len(&self, text: &str) -> Option<usize> {
        self.find(text).map(|m| m.end())
    }
}

impl TextPattern for &str {
    // make sure to anchor your regex with \A
    fn match_len(&self, text: &str) -> Option<usize> {
        if text.starts_with(self) {
            Some(self.len())
        } else {
            None
        }
    }
}

// This is Clone, since that's needed to support async_hacks, but you probably don't usually want to
// clone. Use TextPositionSnapshot instead.
#[derive(Clone)]
pub struct TextPosition<'t> {
    text: &'t str,
    char_widths: NewlineNormalizedCharWidths<'t>,
    inner_byte_idx: usize,
    inner_char_column_number: usize,
    inner_byte_column_number: usize,
    inner_line_number: usize,
}

/// A lightweight immutable version of TextPosition that's slightly
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

impl<'t> TextPosition<'t> {
    pub fn new(text: &'t str) -> Self {
        Self {
            text,
            char_widths: NewlineNormalizedCharWidths::new(text),
            inner_byte_idx: 0,
            inner_char_column_number: 0,
            inner_byte_column_number: 0,
            inner_line_number: 1,
        }
    }

    /// Peeks at the next character. Similar to `std::iter::Peekable`, but doesn't modify our
    /// internal position counters like wrapping this in `Peekable` would.
    pub fn peek(&mut self) -> Option<<Self as Iterator>::Item> {
        self.char_widths.peek_character()
    }

    /// Matches, but does not consume TextPattern.
    ///
    /// Caution: This does not normalize `'\r'` characters, like `peek()` and `next()` do.
    pub fn matches<P: TextPattern>(&self, pattern: P) -> bool {
        let rest_of_text = &self.text[self.inner_byte_idx..];
        let match_len = pattern.match_len(rest_of_text);
        match match_len {
            Some(match_len) => {
                assert!(
                    !rest_of_text[..match_len].contains(|x| x == '\r' || x == '\n'),
                    "matches pattern must not match a newline",
                );
                true
            }
            None => false,
        }
    }

    /// Moves the iterator back one character. Panics if a newline is encountered or if we try to
    /// back up past the beginning of the text.
    pub fn backup_no_newline(&mut self) {
        if let Some(cw) = self.char_widths.previous() {
            // If we tried to back up across a newline, we'd have to recompute char_column_number,
            // which would be expensive, so it's unsupported.
            self.inner_char_column_number = self
                .inner_char_column_number
                .checked_sub(1)
                .expect("cannot back up past the beginning of a line.");
            self.inner_byte_column_number = self
                .inner_byte_column_number
                .checked_sub(cw.byte_width)
                .expect("cannot back up past the beginning of a line.");
            self.inner_byte_idx -= cw.byte_width;
        } else {
            panic!("Tried to backup past the beginning of the text.")
        }
    }

    /// Tries to consume the given TextPattern, moving the TextPosition forward. Returns false if no
    /// match was found. Does not support newlines.
    ///
    /// Panics if a newline is consumed as part of the pattern.
    pub fn consume<P: TextPattern>(&mut self, pattern: P) -> bool {
        let rest_of_text = &self.text[self.inner_byte_idx..];
        if let Some(len) = pattern.match_len(rest_of_text) {
            let new_byte_idx = self.inner_byte_idx + len;
            // Call next() a bunch of times to advance the character counters. There's no way to
            // shortcut this because we don't know how many characters are in a slice of bytes,
            // though we could use a faster algorithm that inspects multiple characters at once
            // (e.g. SIMD).
            while self.inner_byte_idx < new_byte_idx {
                // We can't support newline normalization in this API without copying the string, so
                // rather than exposing that (potentially dangerous) behavior, panic if it happens.
                assert!(
                    self.next() != Some('\n'),
                    "consume pattern must not match a newline",
                );
            }
            // this shouldn't be possible for the provided implementations of TextPattern
            debug_assert!(
                self.inner_byte_idx == new_byte_idx,
                "pattern ended on a non-character boundary",
            );
            true
        } else {
            false
        }
    }

    pub fn text(&self) -> &'t str {
        self.text
    }

    pub fn slice_from_start_pos(&self, start_pos: &TextPositionSnapshot) -> &'t str {
        &self.text[start_pos.byte_idx()..self.byte_idx()]
    }

    /// Returns the number of bytes we've traversed. This is useful for Rust code that needs to
    /// slice the input source code, since Rust slices operate on bytes and not unicode codepoints.
    pub fn byte_idx(&self) -> usize {
        self.inner_byte_idx
    }

    /// Returns the column number in terms of number of characters (unicode codepoints) past the
    /// beginning of the line. Zero-indexed.
    pub fn char_column_number(&self) -> usize {
        self.inner_char_column_number
    }

    pub fn byte_column_number(&self) -> usize {
        self.inner_byte_column_number
    }

    /// Returns the one-indexed line number.
    pub fn line_number(&self) -> usize {
        self.inner_line_number
    }
}

impl Iterator for TextPosition<'_> {
    type Item = char;

    /// Gets the next character. This has the side-effect of advancing the internal position
    /// counters.
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cw) = self.char_widths.next() {
            self.inner_byte_idx += cw.byte_width;
            match cw.character {
                '\n' => {
                    self.inner_line_number += 1;
                    self.inner_char_column_number = 0;
                    self.inner_byte_column_number = 0;
                }
                _ => {
                    self.inner_char_column_number += cw.char_width;
                    self.inner_byte_column_number += cw.byte_width;
                }
            }
            Some(cw.character)
        } else {
            None
        }
    }
}

impl fmt::Debug for TextPosition<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TextPosition")
            .field("text", &EllipsisDebug)
            .field("char_widths", &EllipsisDebug)
            .field("inner_byte_idx", &self.inner_byte_idx)
            .field("inner_char_column_number", &self.inner_char_column_number)
            .field("inner_byte_column_number", &self.inner_byte_column_number)
            .field("inner_line_number", &self.inner_line_number)
            .finish()
    }
}

impl From<&TextPosition<'_>> for TextPositionSnapshot {
    fn from(tp: &TextPosition) -> Self {
        Self {
            inner_byte_idx: tp.inner_byte_idx,
            inner_char_column_number: tp.inner_char_column_number,
            inner_line_number: tp.inner_line_number,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let mut pos = TextPosition::new("");
        assert_eq!(pos.byte_idx(), 0);
        assert_eq!(pos.char_column_number(), 0);
        assert_eq!(pos.line_number(), 1);
        assert_eq!(pos.peek(), None);
        assert!(!pos.consume(&Regex::new(r"\Awon't match").unwrap()));
        assert!(pos.consume(&Regex::new(r"\A").unwrap()));
        assert_eq!(pos.next(), None);
        // call next() again to verify that it's fused
        assert_eq!(pos.next(), None);
    }

    #[test]
    fn test_ascii() {
        let mut pos = TextPosition::new("abcdefg");

        assert_eq!(pos.peek(), Some('a'));
        assert_eq!(pos.next(), Some('a'));
        assert_eq!(pos.byte_idx(), 1);
        assert_eq!(pos.char_column_number(), 1);
        assert_eq!(pos.line_number(), 1);

        // consume a few characters with a regex
        assert!(!pos.consume(&Regex::new(r"\Awon't match").unwrap()));
        assert!(pos.consume(&Regex::new(r"\Abcd").unwrap()));
        assert_eq!(pos.byte_idx(), 4);
        assert_eq!(pos.char_column_number(), 4);
        assert_eq!(pos.line_number(), 1);

        // consume the rest of the text
        assert_eq!(pos.next(), Some('e'));
        assert_eq!(pos.next(), Some('f'));
        assert_eq!(pos.next(), Some('g'));
        assert_eq!(pos.next(), None);
        assert_eq!(pos.byte_idx(), 7);
        assert_eq!(pos.char_column_number(), 7);
        assert_eq!(pos.line_number(), 1);
    }

    #[test]
    fn test_unicode() {
        let mut pos = TextPosition::new("\u{00e9}abc");

        assert_eq!(pos.peek(), Some('\u{00e9}'));
        assert_eq!(pos.next(), Some('\u{00e9}'));
    }

    #[test]
    fn test_newline_lf() {
        let mut pos = TextPosition::new("ab\nde");

        assert_eq!(pos.next(), Some('a'));
        assert_eq!(pos.next(), Some('b'));
        assert_eq!(pos.line_number(), 1);
        assert_eq!(pos.char_column_number(), 2);

        assert_eq!(pos.next(), Some('\n'));
        assert_eq!(pos.line_number(), 2);
        assert_eq!(pos.char_column_number(), 0);

        assert_eq!(pos.next(), Some('d'));
        assert_eq!(pos.next(), Some('e'));
        assert_eq!(pos.next(), None);
        assert_eq!(pos.line_number(), 2);
        assert_eq!(pos.char_column_number(), 2);

        assert_eq!(pos.byte_idx(), 5);
    }

    #[test]
    fn test_newline_cr() {
        let mut pos = TextPosition::new("ab\rde");

        assert_eq!(pos.next(), Some('a'));
        assert_eq!(pos.next(), Some('b'));
        assert_eq!(pos.line_number(), 1);
        assert_eq!(pos.char_column_number(), 2);

        assert_eq!(pos.next(), Some('\n'));
        assert_eq!(pos.line_number(), 2);
        assert_eq!(pos.char_column_number(), 0);

        assert_eq!(pos.next(), Some('d'));
        assert_eq!(pos.next(), Some('e'));
        assert_eq!(pos.next(), None);
        assert_eq!(pos.line_number(), 2);
        assert_eq!(pos.char_column_number(), 2);

        assert_eq!(pos.byte_idx(), 5);
    }

    #[test]
    fn test_newline_cr_lf() {
        let mut pos = TextPosition::new("ab\r\nde");

        assert_eq!(pos.next(), Some('a'));
        assert_eq!(pos.next(), Some('b'));
        assert_eq!(pos.line_number(), 1);
        assert_eq!(pos.char_column_number(), 2);

        assert_eq!(pos.next(), Some('\n'));
        assert_eq!(pos.line_number(), 2);
        assert_eq!(pos.char_column_number(), 0);

        assert_eq!(pos.next(), Some('d'));
        assert_eq!(pos.next(), Some('e'));
        assert_eq!(pos.next(), None);
        assert_eq!(pos.line_number(), 2);
        assert_eq!(pos.char_column_number(), 2);

        assert_eq!(pos.byte_idx(), 6);
    }
}
