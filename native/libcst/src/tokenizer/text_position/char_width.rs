// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::str::Chars;

#[derive(Debug, Eq, PartialEq)]
pub struct CharWidth {
    pub byte_width: usize,
    pub char_width: usize,
    pub character: char,
}

/// Iterates over characters (unicode codepoints) normalizing `'\r'` and `"\r\n"` to `'\n'`. Also
/// gives the width of each character, but `'\r\n'` is counted as 2 bytes and 2 characters instead
/// of one even after being normalized to '\n'.
#[derive(Clone)]
pub struct NewlineNormalizedCharWidths<'t> {
    iter: Chars<'t>,
    text: &'t str,
    idx: usize,
}

impl<'t> NewlineNormalizedCharWidths<'t> {
    pub fn new(text: &'t str) -> Self {
        Self {
            text,
            iter: text.chars(),
            idx: 0,
        }
    }

    pub fn previous(&mut self) -> Option<<Self as Iterator>::Item> {
        // This function is called infrequently.
        let mut back_iter = self.text[..self.idx].chars();
        let result = match back_iter.next_back() {
            // Unlikely: \n, normalization *may* be needed
            Some('\n') => {
                // Peek at the previous character to see we're a `\r\n` sequence
                match back_iter.next_back() {
                    Some('\r') => Some(CharWidth {
                        byte_width: '\r'.len_utf8() + '\n'.len_utf8(),
                        char_width: 2,
                        character: '\n',
                    }),
                    _ => Some(CharWidth {
                        byte_width: '\n'.len_utf8(),
                        char_width: 1,
                        character: '\n',
                    }),
                }
            }
            // Unlikely: \r, normalization is needed
            Some('\r') => Some(CharWidth {
                byte_width: '\n'.len_utf8(),
                char_width: 1,
                character: '\n',
            }),
            // Common case: Not \r or \n, so no normalization is needed
            Some(ch) => Some(CharWidth {
                byte_width: ch.len_utf8(),
                char_width: 1,
                character: ch,
            }),
            // Unlikely: EOF
            None => None,
        };
        if let Some(r) = &result {
            self.idx -= r.byte_width;
            self.iter = self.text[self.idx..].chars();
        }
        result
    }

    pub fn peek_character(&self) -> Option<char> {
        // This function is called very frequently.
        //
        // We're not using peekable or caching here, since this should be cheap enough on it's own,
        // though benchmarking might prove otherwise.
        match self.iter.clone().next() {
            Some('\r') => Some('\n'),
            ch => ch,
        }
    }
}

impl<'t> Iterator for NewlineNormalizedCharWidths<'t> {
    type Item = CharWidth;

    fn next(&mut self) -> Option<Self::Item> {
        // This function is called very frequently.
        let result = match self.iter.next() {
            // Unlikely: \r, normalization is needed
            Some('\r') => {
                // Peek at the next character to see if it's '\n'.
                let mut speculative = self.iter.clone();
                match speculative.next() {
                    Some('\n') => {
                        self.iter = speculative;
                        Some(CharWidth {
                            byte_width: '\r'.len_utf8() + '\n'.len_utf8(),
                            char_width: 2,
                            character: '\n',
                        })
                    }
                    _ => Some(CharWidth {
                        byte_width: '\r'.len_utf8(),
                        char_width: 1,
                        character: '\n',
                    }),
                }
            }
            // Common case: Not \r, so no normalization is needed
            Some(ch) => Some(CharWidth {
                byte_width: ch.len_utf8(),
                char_width: 1,
                character: ch,
            }),
            // Unlikely: EOF
            None => None,
        };
        if let Some(r) = &result {
            self.idx += r.byte_width;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_no_newlines() {
        let mut cw = NewlineNormalizedCharWidths::new("in");

        // go forward
        assert_eq!(cw.peek_character(), Some('i'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 'i'
            })
        );
        assert_eq!(cw.peek_character(), Some('n'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 'n'
            })
        );

        // end of text
        assert_eq!(cw.peek_character(), None);
        assert_eq!(cw.next(), None);

        // go backwards
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 'n'
            })
        );
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 'i'
            })
        );

        // beginning of text
        assert_eq!(cw.previous(), None);

        // try going foward again
        assert_eq!(cw.peek_character(), Some('i'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 'i'
            })
        );
    }

    #[test]
    fn test_unicode_no_newlines() {
        // "test" with an accented 'e'
        let mut cw = NewlineNormalizedCharWidths::new("t\u{00e9}st");

        // go forward
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 't'
            })
        );
        assert_eq!(cw.peek_character(), Some('\u{00e9}'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 2,
                char_width: 1,
                character: '\u{00e9}'
            })
        );
        assert_eq!(cw.peek_character(), Some('s'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 's'
            })
        );

        // go backwards
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 's'
            })
        );
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 2,
                char_width: 1,
                character: '\u{00e9}'
            })
        );
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: 't'
            })
        );
    }

    #[test]
    fn test_newlines() {
        let mut cw = NewlineNormalizedCharWidths::new("\n\r\r\n");

        // go forward
        assert_eq!(cw.peek_character(), Some('\n'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: '\n'
            })
        );
        assert_eq!(cw.peek_character(), Some('\n'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: '\n'
            })
        );
        assert_eq!(cw.peek_character(), Some('\n'));
        assert_eq!(
            cw.next(),
            Some(CharWidth {
                byte_width: 2,
                char_width: 2,
                character: '\n'
            })
        );

        // end of text
        assert_eq!(cw.peek_character(), None);
        assert_eq!(cw.next(), None);

        // go backwards
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 2,
                char_width: 2,
                character: '\n'
            })
        );
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: '\n'
            })
        );
        assert_eq!(
            cw.previous(),
            Some(CharWidth {
                byte_width: 1,
                char_width: 1,
                character: '\n'
            })
        );

        // beginning of text
        assert_eq!(cw.previous(), None);
    }

    #[test]
    fn test_empty() {
        let mut cw = NewlineNormalizedCharWidths::new("");
        assert_eq!(cw.peek_character(), None);
        assert_eq!(cw.next(), None);
        assert_eq!(cw.previous(), None);
    }
}
