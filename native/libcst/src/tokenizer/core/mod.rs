// This implementation is Copyright (c) Meta Platforms, Inc. and affiliates.
//
// CPython 3.10.0a5 and the original C code this is based on is
// Copyright (c) 2001-2021 Python Software Foundation; All Rights Reserved
//
// Portions of this module (f-string splitting) are based on parso's tokenize.py, which is also PSF
// licensed.

/// A port of CPython's tokenizer.c to Rust, with the following significant modifications:
///
/// - PEP 263 (encoding detection) support isn't implemented. We depend on other code to do this for
///   us right now, and expect that the input is utf-8 by the time we see it.
///
/// - Removed support for tokenizing from a file handle without reading the whole file in at once.
///   This significantly complicates parsing and memory is cheap, so we require that the whole file
///   is read in and converted to a unicode string before tokenization can begin.
///
/// - Removed support for the interactive interpreter parsing mode.
///
/// - Tweaked the `translate_newlines` functionality and moved most of it into TextPosition. `\r`
///   characters are no longer removed from the input buffer, so strings may contain `\r` characters
///   that should be normalized prior to being interpreted.
///
/// - Added support for tracking more detailed position information via TextPosition. As a
///   consequence, consuming and then backing up a character (`tok_nextc`/`tok_backup`) is more
///   expensive, and we prefer to call `TextPosition::peek()` instead.
///
/// - Removed support for tokenizing type comments.
///
/// - Reduced the number of different supported token types to match what parso's tokenizer yields.
///
/// - Uses some regular expressions. Regular expression are a good fit for a tokenizer, but we don't
///   use regular expressions everywhere because we can't generate as good of error messages with
///   them.
///
/// - Added support for breaking apart f-strings into multiple tokens, matching Parso's tokenizer
///   behavior. CPython instead runs the parser recursively to parse f-strings.
///
/// Also, in general, the code is less tightly optimized. The CPython implementation is crazy
/// optimized in ways that wouldn't translate well to rust (e.g. it parses the input utf-8 buffer as
/// raw bytes instead of unicode codepoints).
///
/// The implementation should still be faster than any pure-Python implementation, and most
/// optimizations (avoiding string copies when slicing) carry over to Rust very well.
///
/// Planned (not yet implemented) features:
///
/// - Add more feature flags to more closely match the behavior of older versions of Python 3.x.
///
/// - Support for a Python 2 mode that tokenizes Python 2.7 code and fails on certain new Python 3
///   syntax that wasn't supported in 2.7.
///
/// - Maybe add back support for tokenizing type comments?
///
/// This implementation is tailored to LibCST's needs. If you're looking for a more general-purpose
/// pure-Rust Python parser, consider using [RustPython's parser][].
///
/// [RustPython's parser]: https://crates.io/crates/rustpython-parser
mod string_types;

use regex::Regex;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::convert::TryInto;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::rc::Rc;

use crate::tokenizer::{
    core::string_types::{FStringNode, StringQuoteChar, StringQuoteSize},
    operators::OPERATOR_RE,
    text_position::{TextPosition, TextPositionSnapshot},
    whitespace_parser::State as WhitespaceState,
};

/// The maximum number of indentation levels at any given point in time. CPython's tokenizer.c caps
/// this to avoid the complexity of allocating a dynamic array, but we're using a Vec, so it's not
/// necessary, but we're keeping it to maintain compatibility.
const MAX_INDENT: usize = 100;

// MAX_CHAR should be std::char::MAX once assoc_char_consts is stablized.
// https://github.com/rust-lang/rust/issues/71763
const MAX_CHAR: char = '\u{10ffff}';

thread_local! {
    static SPACE_TAB_FORMFEED_RE: Regex = Regex::new(r"\A[ \f\t]+").expect("regex");
    static ANY_NON_NEWLINE_RE: Regex = Regex::new(r"\A[^\r\n]+").expect("regex");
    static STRING_PREFIX_RE: Regex =
        Regex::new(r"\A(?i)(u|[bf]r|r[bf]|r|b|f)").expect("regex");
    static POTENTIAL_IDENTIFIER_TAIL_RE: Regex =
        Regex::new(r"\A([a-zA-Z0-9_]|[^\x00-\x7f])+").expect("regex");
    static DECIMAL_DOT_DIGIT_RE: Regex = Regex::new(r"\A\.[0-9]").expect("regex");
    static DECIMAL_TAIL_RE: Regex =
        Regex::new(r"\A[0-9](_?[0-9])*").expect("regex");
    static HEXADECIMAL_TAIL_RE: Regex =
        Regex::new(r"\A(_?[0-9a-fA-F])+").expect("regex");
    static OCTAL_TAIL_RE: Regex = Regex::new(r"\A(_?[0-7])+").expect("regex");
    static BINARY_TAIL_RE: Regex = Regex::new(r"\A(_?[01])+").expect("regex");

    /// Used to verify identifiers when there's a non-ascii character in them.
    // This changes across unicode revisions. We'd need to ship our own unicode tables to 100% match a
    // given Python version's behavior.
    static UNICODE_IDENTIFIER_RE: Regex =
        Regex::new(r"\A[\p{XID_Start}_]\p{XID_Continue}*\z").expect("regex");
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokType {
    String,
    Name,
    Number,
    Op,
    Newline,
    Indent,
    Dedent,
    Async,
    Await,
    FStringStart,
    FStringString,
    FStringEnd,
    EndMarker,
}

#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum TokError<'t> {
    #[error("inconsistent mixing of tabs and spaces")]
    TabSpace,
    #[error("too many indentation levels")]
    TooDeep,
    #[error("no matching outer block for dedent")]
    Dedent,
    #[error("unexpected characters after a line continuation")]
    LineContinuation,
    #[error("unexpected end of file after a line continuation")]
    LineContinuationEof,
    #[error("{0:?} is not a valid identifier")]
    BadIdentifier(&'t str),
    #[error("invalid decimal literal")]
    BadDecimal,
    #[error(
        "{}{}",
        "leading zeros in decimal integer literals are not permitted; use an 0o prefix for octal ",
        "integers"
    )]
    BadDecimalLeadingZeros,
    #[error("invalid hexadecimal literal")]
    BadHexadecimal,
    #[error("invalid octal literal")]
    BadOctal,
    #[error("invalid digit {0:?} in octal literal")]
    BadOctalDigit(char),
    #[error("invalid binary literal")]
    BadBinary,
    #[error("invalid digit {0:?} in binary literal")]
    BadBinaryDigit(char),
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("unterminated triple-quoted string literal")]
    UnterminatedTripleQuotedString,
    #[error("unmatched {0:?}")]
    UnmatchedClosingParen(char),
    #[error("Closing parenthesis {1:?} does not match opening parenthesis {0:?}")]
    MismatchedClosingParen(char, char),
    #[error("Closing parenthesis {1:?} does not match opening parenthesis {0:?} on line {2:}")]
    MismatchedClosingParenOnLine(char, char, usize),
    #[error("{0:?} is not a valid character in this position")]
    BadCharacter(char),
}

// Clone is used for async_hacks, which needs to speculatively look-ahead one token.
#[derive(Clone)]
pub struct TokState<'t> {
    /// The full program's source code (similar to `tok->str` or `tok->buf` in the CPython source
    /// code). We don't support reading the file line-by-line from a file handle like CPython does,
    /// so this is the whole program pre-converted to utf-8.
    pub text_pos: TextPosition<'t>,
    /// Start of the most recently returned token.
    pub start_pos: TextPositionSnapshot,
    /// True after we've encountered an error or there's no more text to process.
    done: bool,
    /// How many spaces a tab counts as (always 8)
    tab_size: usize,
    /// How many spaces a tab counts as in alt_indent_stack (always 1)
    alt_tab_size: usize,
    /// Stack of indentation levels where a tab is counted as 8 characters, used for tracking
    /// dedents. Length is current indentation level. Should never have more than MAX_INDENT
    /// entries.
    indent_stack: Vec<usize>,
    /// Used to check that tabs and spaces are not mixed.
    alt_indent_stack: Vec<usize>,
    /// Beginning of line. True if at the beginning of a new line.
    at_bol: bool,
    /// The number of bytes at the beginning of the line, as measured by consume_bol_whitespace.
    /// Used by libcst to capture (and then validate and parse) the indentation.
    pub bol_width: usize,
    /// Set by `consume_bol_whitespace`, true if the current line is blank.
    blank_line: bool,
    /// Pending intents (if > 0) or dedents (if < 0). Used when multiple tokens need to be produced
    /// at once.
    pending_indents: i32,
    /// Length is `() [] {}` parenthesis nesting level. Used to allow free continuations inside
    /// them. Stack entries are to verify that closing parenthesis match opening parenthesis.
    /// Tuple is (character, lineno).
    paren_stack: Vec<(char, usize)>,
    /// Whether we're in a continuation line.
    cont_line: bool,

    /// True if async/await aren't always keywords.
    async_hacks: bool,
    /// True if tokens are inside an 'async def' body.
    async_def: bool,
    /// Indentation level of the outermost 'async def'.
    async_def_indent: usize,
    /// True if the outermost 'async def' had at least one NEWLINE token after it.
    async_def_nl: bool,

    /// Splits f-strings into multiple tokens instead of a STRING token if true.
    ///
    /// CPython doesn't directly split f-strings in the tokenizer (and therefore doesn't support
    /// this option). Instead, when the parser encounters an f-string, it recursively re-runs the
    /// tokenizer and parser.
    ///
    /// Supporting this at the tokenizer-level is pretty nasty and adds a lot of complexity.
    /// Eventually, we should probably support this at the parser-level instead.
    split_fstring: bool,
    fstring_stack: Vec<FStringNode>,

    missing_nl_before_eof: bool,
}

pub struct TokConfig {
    /// Used in Python 3.5 and 3.6. If enabled, async/await are sometimes keywords and sometimes
    /// identifiers, depending on if they're being used in the context of an async function. This
    /// breaks async comprehensions outside of async functions.
    pub async_hacks: bool,
    pub split_fstring: bool,
    // Not currently supported:
    // type_comments: bool,
}

fn is_digit<C: Into<Option<char>>>(ch: C) -> bool {
    matches!(ch.into(), Some('0'..='9'))
}

#[derive(Debug)]
enum NumberState {
    StartDigit,
    Fraction,
    Exponent,
    Imaginary,
}

impl<'t> TokState<'t> {
    pub fn new(text: &'t str, config: &TokConfig) -> Self {
        let text_pos = TextPosition::new(text);
        let start_pos = (&text_pos).into();
        Self {
            text_pos,
            start_pos,
            done: false,
            tab_size: 8,
            alt_tab_size: 1,
            indent_stack: Vec::new(),
            alt_indent_stack: Vec::new(),
            at_bol: true,
            bol_width: 0,
            blank_line: false,
            pending_indents: 0,
            paren_stack: Vec::new(),
            cont_line: false,
            async_hacks: config.async_hacks,
            async_def: false,
            async_def_indent: 0,
            async_def_nl: false,
            split_fstring: config.split_fstring,
            fstring_stack: Vec::new(),
            missing_nl_before_eof: text.is_empty() || text.as_bytes()[text.len() - 1] != b'\n',
        }
    }

    pub fn is_parenthesized(&self) -> bool {
        !self.paren_stack.is_empty()
    }

    /// Implementation of `next()`, wrapped by next() to allow for easier error handling. Roughly
    /// equivalent to `tok_get` in the C source code.
    fn next_inner(&mut self) -> Result<TokType, TokError<'t>> {
        if self.split_fstring {
            if let Some(tos) = self.fstring_stack.last() {
                if !tos.is_in_expr() {
                    self.start_pos = (&self.text_pos).into();
                    let is_in_format_spec = tos.is_in_format_spec();
                    let is_raw_string = tos.is_raw_string;
                    if let Some(tok) =
                        self.maybe_consume_fstring_string(is_in_format_spec, is_raw_string)?
                    {
                        return Ok(tok);
                    }
                    if let Some(tok) = self.maybe_consume_fstring_end() {
                        return Ok(tok);
                    }
                }
            }
        }

        // This will never consume a token, but it may set blank_line and it may set
        // pending_indents.
        self.consume_bol_whitespace()?;

        // Return pending indents/dedents
        if let Some(t) = self.process_pending_indents() {
            self.start_pos = (&self.text_pos).into();
            return Ok(t);
        }

        self.maybe_close_async_def();

        'again: loop {
            // Skip spaces
            SPACE_TAB_FORMFEED_RE.with(|v| self.text_pos.consume(v));

            // Skip comment, unless it's a type comment
            if self.text_pos.peek() == Some('#') {
                ANY_NON_NEWLINE_RE.with(|v| self.text_pos.consume(v));
                // type_comment is not supported
            }

            // Set start of current token
            self.start_pos = (&self.text_pos).into();

            return match self.text_pos.peek() {
                // Check for EOF now
                None => {
                    if self.missing_nl_before_eof && !self.blank_line {
                        self.at_bol = true;
                        self.missing_nl_before_eof = false;
                        Ok(TokType::Newline)
                    } else {
                        let hanging_indents = self.indent_stack.len() as i32;
                        if self.pending_indents == 0 && hanging_indents != 0 {
                            // We've reached EOF but there are still pending indents not
                            // accounted for. Flush them out.
                            self.pending_indents = -hanging_indents;
                            self.indent_stack.clear();
                            self.alt_indent_stack.clear();
                            self.missing_nl_before_eof = false;
                        }
                        if let Some(t) = self.process_pending_indents() {
                            Ok(t)
                        } else {
                            Ok(TokType::EndMarker)
                        }
                    }
                }

                // Identifier (most frequent token!)
                Some('a'..='z') | Some('A'..='Z') | Some('_') | Some('\u{80}'..=MAX_CHAR) => {
                    self.consume_identifier_or_prefixed_string()
                }

                // Newline
                Some('\n') => {
                    self.text_pos.next();
                    self.at_bol = true;
                    if self.split_fstring
                        && self.fstring_stack.last().map(|node| node.allow_multiline())
                            == Some(false)
                    {
                        Err(TokError::UnterminatedString)
                    } else if self.blank_line || !self.paren_stack.is_empty() {
                        // this newline doesn't count
                        // recurse (basically `goto nextline`)
                        self.next_inner()
                    } else {
                        self.cont_line = false;
                        if self.async_def {
                            self.async_def_nl = true;
                        }
                        Ok(TokType::Newline)
                    }
                }

                // Ellipsis
                Some('.') if self.text_pos.consume("...") => {
                    return Ok(TokType::Op);
                }

                // Number starting with period
                Some('.') if DECIMAL_DOT_DIGIT_RE.with(|r| self.text_pos.matches(r)) => {
                    self.consume_number(NumberState::Fraction)
                }

                // Dot
                Some('.') => {
                    self.text_pos.next();
                    Ok(TokType::Op)
                }

                // Number
                Some('0'..='9') => self.consume_number(NumberState::StartDigit),

                // String
                Some('\'') | Some('"') => self.consume_string(),

                // Line continuation
                Some('\\') => {
                    self.text_pos.next();
                    if let Some('\n') = self.text_pos.next() {
                        if self.text_pos.peek() == None {
                            Err(TokError::LineContinuationEof)
                        } else {
                            self.cont_line = true;
                            // Read next line
                            continue 'again;
                        }
                    } else {
                        Err(TokError::LineContinuation)
                    }
                }

                Some(ch @ '(') | Some(ch @ '[') | Some(ch @ '{') => {
                    self.text_pos.next();
                    if let Some(tos) = self.fstring_stack.last_mut() {
                        tos.open_parentheses();
                    }
                    self.paren_stack.push((ch, self.text_pos.line_number()));
                    Ok(TokType::Op)
                }

                Some(closing @ ')') | Some(closing @ ']') | Some(closing @ '}') => {
                    self.text_pos.next();
                    if let Some(tos) = self.fstring_stack.last_mut() {
                        tos.close_parentheses();
                    }
                    if let Some((opening, line_number)) = self.paren_stack.pop() {
                        match (opening, closing) {
                            ('(', ')') | ('[', ']') | ('{', '}') => Ok(TokType::Op),
                            _ => {
                                if line_number != self.text_pos.line_number() {
                                    Err(TokError::MismatchedClosingParenOnLine(
                                        opening,
                                        closing,
                                        line_number,
                                    ))
                                } else {
                                    Err(TokError::MismatchedClosingParen(opening, closing))
                                }
                            }
                        }
                    } else {
                        Err(TokError::UnmatchedClosingParen(closing))
                    }
                }

                Some(':')
                    if self
                        .fstring_stack
                        .last()
                        .map(|tos| tos.parentheses_count - tos.format_spec_count == 1)
                        .unwrap_or(false) =>
                {
                    // N.B. This may capture the walrus operator and pass it to the formatter.
                    // That's intentional. PEP 572 says: "Assignment expressions inside of f-strings
                    // require parentheses."
                    //
                    // >>> f'{x:=10}'    # Valid, passes '=10' to formatter
                    let tos = self
                        .fstring_stack
                        .last_mut()
                        .expect("fstring_stack is not empty");
                    tos.format_spec_count += 1;
                    self.text_pos.next();
                    Ok(TokType::Op)
                }

                // Operator
                Some(_) if OPERATOR_RE.with(|r| self.text_pos.consume(r)) => Ok(TokType::Op),

                // Bad character
                // If nothing works, fall back to this error. CPython returns an OP in this case,
                // and then just relies on the parser to generate a generic syntax error.
                Some(ch) => Err(TokError::BadCharacter(ch)),
            };
        }
    }

    /// Consumes the whitespace (and comments) at the beginning of the line. May emit an error. Will
    /// mutate `pending_indents`, so you must check `pending_indents` after calling this.
    fn consume_bol_whitespace(&mut self) -> Result<(), TokError<'t>> {
        self.blank_line = false;
        if !self.at_bol {
            return Ok(());
        }

        let mut col = 0; // column where tab counts as 8 characters
        let mut altcol = 0; // column where tab counts as 1 character
        self.at_bol = false;
        self.bol_width = 0;

        // consume space, tab, and formfeed characters
        loop {
            match self.text_pos.peek() {
                Some(' ') => {
                    col += 1;
                    altcol += 1;
                    self.bol_width += 1;
                    self.text_pos.next();
                }
                Some('\t') => {
                    // Increment both col and altcol using different tab sizes. Tabs snap to the
                    // next multiple of self.tab_size.
                    col = (col / self.tab_size + 1) * self.tab_size;
                    // altcol will later be used for detecting mixed tabs and spaces.
                    altcol = (altcol / self.alt_tab_size + 1) * self.alt_tab_size;
                    self.bol_width += 1;
                    self.text_pos.next();
                }
                // Control-L (formfeed) for emacs users
                Some('\x0c') => {
                    col = 0;
                    altcol = 0;
                    self.bol_width += 1;
                    self.text_pos.next();
                }
                _ => {
                    break;
                }
            }
        }

        // Lines with only whitespace and/or comments and/or a line continuation
        // character shouldn't affect the indentation and are not passed to the parser
        // as NEWLINE tokens.
        self.blank_line = matches!(
            self.text_pos.peek(),
            Some('#') | Some('\n') | Some('\\') | None
        );

        if self.blank_line || !self.paren_stack.is_empty() {
            return Ok(());
        }

        let prev_col = self.indent_stack.last().unwrap_or(&0);
        match col.cmp(prev_col) {
            Ordering::Equal => {
                // No change
                if altcol != *self.alt_indent_stack.last().unwrap_or(&0) {
                    return Err(TokError::TabSpace);
                }
            }
            Ordering::Greater => {
                // col > prev_col
                // Indent -- always one
                if self.indent_stack.len() + 1 >= MAX_INDENT {
                    return Err(TokError::TooDeep);
                }
                // col > prev_col, therefore altcol > prev_altcol, unless there's badly mixed tabs
                // and spaces
                if altcol <= *self.alt_indent_stack.last().unwrap_or(&0) {
                    return Err(TokError::TabSpace);
                }
                // only emit indents if we're not at EOF
                if self.text_pos.peek().is_some() {
                    self.pending_indents += 1;
                    self.indent_stack.push(col);
                    self.alt_indent_stack.push(altcol);
                }
            }
            Ordering::Less => {
                // c < prev_col
                // Dedent -- any number, must be consistent
                while matches!(self.indent_stack.last(), Some(&ind_cols) if col < ind_cols) {
                    self.pending_indents -= 1;
                    self.indent_stack.pop();
                    self.alt_indent_stack.pop();
                }
                if col != *self.indent_stack.last().unwrap_or(&0) {
                    return Err(TokError::Dedent);
                }
                if altcol != *self.alt_indent_stack.last().unwrap_or(&0) {
                    return Err(TokError::TabSpace);
                }
            }
        }

        Ok(())
    }

    fn process_pending_indents(&mut self) -> Option<TokType> {
        if self.pending_indents != 0 {
            if self.pending_indents < 0 {
                self.pending_indents += 1;
                Some(TokType::Dedent)
            } else {
                self.pending_indents -= 1;
                Some(TokType::Indent)
            }
        } else {
            None
        }
    }

    fn maybe_close_async_def(&mut self) {
        // Check if we are closing an async function
        if self.async_def
            && !self.blank_line
            // (This is irrelevant to the rust implementation which doesn't support type_comments
            // yet, but the comment is preserved for posterity)
            // Due to some implementation artifacts of type comments, a TYPE_COMMENT at the start of
            // a function won't set an indentation level and it will produce a NEWLINE after it. To
            // avoid spuriously ending an async function due to this, wait until we have some
            // non-newline char in front of us.
            // && self.text_pos.peek() == Some('\n')
            && self.paren_stack.is_empty()
            // There was a NEWLINE after ASYNC DEF, so we're past the signature.
            && self.async_def_nl
            // Current indentation level is less than where the async function was defined
            && self.async_def_indent >= self.indent_stack.len()
        {
            self.async_def = false;
            self.async_def_indent = 0;
            self.async_def_nl = false;
        }
    }

    fn consume_identifier_or_prefixed_string(&mut self) -> Result<TokType, TokError<'t>> {
        // Process the various legal combinations of b"", r"", u"", and f"".
        if STRING_PREFIX_RE.with(|r| self.text_pos.consume(r)) {
            if let Some('"') | Some('\'') = self.text_pos.peek() {
                // We found a string, not an identifier. Bail!
                if self.split_fstring
                    && self
                        .text_pos
                        .slice_from_start_pos(&self.start_pos)
                        .contains(&['f', 'F'][..])
                {
                    return self.consume_fstring_start();
                } else {
                    return self.consume_string();
                }
            }
        } else {
            // the next character must be a potential identifier start, aka `[a-zA-Z_]|[^\x00-\x7f]`
            let first_ch = self.text_pos.next();
            debug_assert!(matches!(
                first_ch,
                Some('a'..='z') | Some('A'..='Z') | Some('_') | Some('\u{80}'..=MAX_CHAR)
            ));
        }
        POTENTIAL_IDENTIFIER_TAIL_RE.with(|r| self.text_pos.consume(r));
        let identifier_str = self.text_pos.slice_from_start_pos(&self.start_pos);
        if !verify_identifier(identifier_str) {
            // TODO: async/await
            return Err(TokError::BadIdentifier(identifier_str));
        }

        let allow_async = !self.async_hacks || self.async_def;
        match (identifier_str, allow_async) {
            ("async", true) => Ok(TokType::Async),
            ("await", true) => Ok(TokType::Await),
            ("async", false) => {
                // The current token is 'async' and async_hacks is enabled.
                // Look ahead one token to see if that is 'def'.
                // This clone is expensive, but modern code doesn't need async_hacks.
                let mut lookahead_state = self.clone();
                if lookahead_state.next_inner() == Ok(TokType::Name)
                    && lookahead_state
                        .text_pos
                        .slice_from_start_pos(&lookahead_state.start_pos)
                        == "def"
                {
                    self.async_def = true;
                    self.async_def_indent = self.indent_stack.len();
                    Ok(TokType::Async)
                } else {
                    Ok(TokType::Name)
                }
            }
            _ => Ok(TokType::Name),
        }
    }

    fn consume_number(&mut self, state: NumberState) -> Result<TokType, TokError<'t>> {
        // This is organized as a state machine. The match could also be rewritten into multiple
        // functions, but this is closer to how the C code is written (with gotos).
        match state {
            NumberState::StartDigit => {
                let start_digit_ch = self.text_pos.peek();
                debug_assert!(is_digit(start_digit_ch));

                if start_digit_ch == Some('0') {
                    self.text_pos.next();
                    match self.text_pos.peek() {
                        Some('x') | Some('X') => {
                            self.text_pos.next();
                            if !HEXADECIMAL_TAIL_RE.with(|r| self.text_pos.consume(r))
                                || self.text_pos.peek() == Some('_')
                            {
                                Err(TokError::BadHexadecimal)
                            } else {
                                Ok(TokType::Number)
                            }
                        }
                        Some('o') | Some('O') => {
                            self.text_pos.next();
                            if !OCTAL_TAIL_RE.with(|r| self.text_pos.consume(r))
                                || self.text_pos.peek() == Some('_')
                            {
                                return Err(TokError::BadOctal);
                            }
                            if let Some(next_ch) = self.text_pos.peek() {
                                if is_digit(next_ch) {
                                    return Err(TokError::BadOctalDigit(next_ch));
                                }
                            }
                            Ok(TokType::Number)
                        }
                        Some('b') | Some('B') => {
                            self.text_pos.next();
                            if !BINARY_TAIL_RE.with(|r| self.text_pos.consume(r))
                                || self.text_pos.peek() == Some('_')
                            {
                                return Err(TokError::BadBinary);
                            }
                            if let Some(next_ch) = self.text_pos.peek() {
                                if is_digit(next_ch) {
                                    return Err(TokError::BadBinaryDigit(next_ch));
                                }
                            }
                            Ok(TokType::Number)
                        }
                        _ => {
                            let mut nonzero = false;
                            // Maybe old-style octal. In any case, allow '0' as a literal
                            loop {
                                if self.text_pos.peek() == Some('_') {
                                    self.text_pos.next();
                                    if !is_digit(self.text_pos.peek()) {
                                        return Err(TokError::BadDecimal);
                                    }
                                }
                                if self.text_pos.peek() != Some('0') {
                                    break;
                                }
                                self.text_pos.next();
                            }
                            if is_digit(self.text_pos.peek()) {
                                nonzero = true;
                                self.consume_decimal_tail()?;
                            }
                            if self.text_pos.peek() == Some('.') {
                                self.consume_number(NumberState::Fraction)
                            } else if let Some('e') | Some('E') = self.text_pos.peek() {
                                self.consume_number(NumberState::Exponent)
                            } else if let Some('j') | Some('J') = self.text_pos.peek() {
                                self.consume_number(NumberState::Imaginary)
                            } else if nonzero {
                                Err(TokError::BadDecimalLeadingZeros)
                            } else {
                                Ok(TokType::Number)
                            }
                        }
                    }
                } else {
                    self.consume_decimal_tail()?;
                    if self.text_pos.peek() == Some('.') {
                        self.consume_number(NumberState::Fraction)
                    } else if let Some('e') | Some('E') = self.text_pos.peek() {
                        self.consume_number(NumberState::Exponent)
                    } else if let Some('j') | Some('J') = self.text_pos.peek() {
                        self.consume_number(NumberState::Imaginary)
                    } else {
                        Ok(TokType::Number)
                    }
                }
            }
            NumberState::Fraction => {
                let dot_ch = self.text_pos.next();
                debug_assert!(dot_ch == Some('.'));

                if is_digit(self.text_pos.peek()) {
                    self.consume_decimal_tail()?;
                }
                if let Some('e') | Some('E') = self.text_pos.peek() {
                    self.consume_number(NumberState::Exponent)
                } else if let Some('j') | Some('J') = self.text_pos.peek() {
                    self.consume_number(NumberState::Imaginary)
                } else {
                    Ok(TokType::Number)
                }
            }
            NumberState::Exponent => {
                let e_ch = self.text_pos.next();
                debug_assert!(matches!(e_ch, Some('e') | Some('E')));

                if let Some('+') | Some('-') = self.text_pos.peek() {
                    self.text_pos.next();
                    if !is_digit(self.text_pos.peek()) {
                        return Err(TokError::BadDecimal);
                    }
                } else if !is_digit(self.text_pos.peek()) {
                    // Don't consume the 'e'. It could be part of an identifier after this number.
                    self.text_pos.backup_no_newline();
                    return Ok(TokType::Number);
                }
                self.consume_decimal_tail()?;
                if let Some('j') | Some('J') = self.text_pos.peek() {
                    self.consume_number(NumberState::Imaginary)
                } else {
                    Ok(TokType::Number)
                }
            }
            NumberState::Imaginary => {
                let j_ch = self.text_pos.next();
                debug_assert!(matches!(j_ch, Some('j') | Some('J')));

                Ok(TokType::Number)
            }
        }
    }

    /// Processes a decimal tail. This is the bit after the dot or after an E in a float.
    fn consume_decimal_tail(&mut self) -> Result<(), TokError<'t>> {
        let result = DECIMAL_TAIL_RE.with(|r| self.text_pos.consume(r));
        // Assumption: If we've been called, the first character is an integer, so we must have a
        // regex match
        debug_assert!(result, "try_decimal_tail was called on a non-digit char");
        if self.text_pos.peek() == Some('_') {
            Err(TokError::BadDecimal)
        } else {
            Ok(())
        }
    }

    fn consume_open_quote(&mut self) -> (StringQuoteChar, StringQuoteSize) {
        let quote_char: StringQuoteChar = self
            .text_pos
            .peek()
            .try_into()
            .expect("the next character must be a quote when calling consume_open_quote");
        let triple_quote_pattern = quote_char.triple_str();
        let quote_size = if self.text_pos.consume(triple_quote_pattern) {
            StringQuoteSize::Triple
        } else {
            self.text_pos.next(); // consume the single character instead
            StringQuoteSize::Single
        };
        (quote_char, quote_size)
    }

    fn consume_string(&mut self) -> Result<TokType, TokError<'t>> {
        // Assumption: The opening quote has not been consumed. Leading characters (b, r, f, etc)
        // have been consumed.
        let (quote_char, quote_size) = self.consume_open_quote();
        let quote_raw = quote_char.into();

        let mut end_quote_size: usize = 0;
        let quote_usize: usize = quote_size.into();
        while end_quote_size != quote_usize {
            match (self.text_pos.next(), quote_size) {
                (None, StringQuoteSize::Triple) => {
                    return Err(TokError::UnterminatedTripleQuotedString);
                }
                (None, StringQuoteSize::Single) | (Some('\n'), StringQuoteSize::Single) => {
                    return Err(TokError::UnterminatedString);
                }
                (ch @ Some('\''), _) | (ch @ Some('"'), _) if ch == Some(quote_raw) => {
                    end_quote_size += 1;
                }
                (Some(ch), _) => {
                    end_quote_size = 0;
                    if ch == '\\' {
                        // skip escaped char
                        self.text_pos.next();
                    }
                }
            }
        }

        Ok(TokType::String)
    }

    fn consume_fstring_start(&mut self) -> Result<TokType, TokError<'t>> {
        let (quote_char, quote_size) = self.consume_open_quote();
        let is_raw_string = self
            .text_pos
            .slice_from_start_pos(&self.start_pos)
            .contains(&['r', 'R'][..]);
        self.fstring_stack
            .push(FStringNode::new(quote_char, quote_size, is_raw_string));
        Ok(TokType::FStringStart)
    }

    fn maybe_consume_fstring_string(
        &mut self,
        is_in_format_spec: bool,
        is_raw_string: bool,
    ) -> Result<Option<TokType>, TokError<'t>> {
        let allow_multiline =
            self.fstring_stack.last().map(|node| node.allow_multiline()) == Some(true);
        let mut in_named_unicode: bool = false;
        let mut ok_result = Ok(None); // value to return if we reach the end and don't error out
        'outer: loop {
            match (self.text_pos.peek(), allow_multiline) {
                (None, true) => {
                    return Err(TokError::UnterminatedTripleQuotedString);
                }
                (None, false) | (Some('\n'), false) => {
                    return Err(TokError::UnterminatedString);
                }
                (ch @ Some('\''), _) | (ch @ Some('"'), _) => {
                    // see if this actually terminates the most recent fstring
                    if let Some(node) = self.fstring_stack.last() {
                        if ch == Some(node.quote_char.into()) {
                            match node.quote_size {
                                StringQuoteSize::Single => {
                                    break 'outer;
                                }
                                StringQuoteSize::Triple => {
                                    if self.text_pos.matches(node.quote_char.triple_str()) {
                                        break 'outer;
                                    }
                                }
                            }
                        }
                    }
                    self.text_pos.next();
                }
                (Some('\\'), _) if !is_raw_string => {
                    self.text_pos.next();
                    if is_in_format_spec {
                        if let Some('{') | Some('}') = self.text_pos.peek() {
                            // don't consume { or } because we want those to be interpreted as OP
                            // tokens
                        } else {
                            // skip escaped char (e.g. \', \", or newline/line continuation)
                            self.text_pos.next();
                        }
                    } else if let Some(
                        '\n'
                        | '\\'
                        | '\''
                        | '"'
                        | 'a'
                        | 'b'
                        | 'f'
                        | 'n'
                        | 'r'
                        | 't'
                        | 'v'
                        | 'x'
                        | '0'..='9'
                        | 'N'
                        | 'u'
                        | 'U',
                    ) = self.text_pos.peek()
                    {
                        // skip escaped char
                        let next_ch = self.text_pos.next();
                        // check if this is a \N sequence
                        if let Some('N') = next_ch {
                            // swallow the next open curly brace if it exists
                            if let Some('{') = self.text_pos.peek() {
                                in_named_unicode = true;
                                self.text_pos.next();
                            }
                        }
                    }
                }
                (Some('\\'), _) if is_raw_string => {
                    self.text_pos.next();
                    // skip escaped end-of-string marker or backslash
                    if let Some('"' | '\'' | '\\') = self.text_pos.peek() {
                        self.text_pos.next();
                    }
                }
                (Some('{'), _) => {
                    if is_in_format_spec {
                        // don't actually consume the {, and generate an OP for it instead
                        break 'outer;
                    }
                    let consumed_double = self.text_pos.consume("{{");
                    if !consumed_double {
                        break 'outer;
                    }
                }
                (Some('}'), _) => {
                    if in_named_unicode {
                        in_named_unicode = false;
                        self.text_pos.next();
                    } else if is_in_format_spec {
                        // don't actually consume the }, and generate an OP for it instead
                        break 'outer;
                    } else if !self.text_pos.consume("}}") {
                        return Err(TokError::UnmatchedClosingParen('}'));
                    }
                }
                _ => {
                    self.text_pos.next();
                }
            }
            ok_result = Ok(Some(TokType::FStringString));
        }
        ok_result
    }

    fn maybe_consume_fstring_end(&mut self) -> Option<TokType> {
        let ch = self.text_pos.peek();
        if let Some(node) = self.fstring_stack.last() {
            if ch == Some(node.quote_char.into()) {
                if node.quote_size == StringQuoteSize::Triple {
                    self.text_pos.consume(node.quote_char.triple_str());
                } else {
                    self.text_pos.next(); // already matched
                }
                self.fstring_stack.pop();
                return Some(TokType::FStringEnd);
            }
        }
        None
    }
}

impl<'t> Iterator for TokState<'t> {
    type Item = Result<TokType, TokError<'t>>;

    /// Returns the next token type.
    fn next(&mut self) -> Option<Result<TokType, TokError<'t>>> {
        // This implementation wraps `next_inner`, which does the actual work.
        if self.done {
            None
        } else {
            match self.next_inner() {
                Err(err) => {
                    self.done = true;
                    Some(Err(err))
                }
                Ok(TokType::EndMarker) => {
                    self.done = true;
                    Some(Ok(TokType::EndMarker))
                }
                Ok(t) => Some(Ok(t)),
            }
        }
    }
}

/// Returns true if the given string is a valid Python 3.x identifier. Follows [PEP 3131][].
///
/// [PEP 3131]: https://www.python.org/dev/peps/pep-3131/
fn verify_identifier(name: &str) -> bool {
    // TODO: If `name` is non-ascii, must first normalize name to NFKC.
    // Common case: If the entire string is ascii, we can avoid the more expensive regex check,
    // since the tokenizer already validates ascii characters before calling us.
    name.is_ascii() || UNICODE_IDENTIFIER_RE.with(|r| r.is_match(name))
}

#[derive(Clone)]
pub struct Token<'a> {
    pub r#type: TokType,
    pub string: &'a str,
    pub start_pos: TextPositionSnapshot,
    pub end_pos: TextPositionSnapshot,
    pub whitespace_before: Rc<RefCell<WhitespaceState<'a>>>,
    pub whitespace_after: Rc<RefCell<WhitespaceState<'a>>>,
    pub relative_indent: Option<&'a str>,
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "Token({:?}, {}, start={:?}, end={:?}, relative_indent={:?}, ws_before={:?}, ws_after={:?}",
            self.r#type, self.string, self.start_pos, self.end_pos, self.relative_indent, self.whitespace_before, self.whitespace_after
        )
    }
}

// Dummy Eq implementation. We never compare Tokens like this
impl<'a> PartialEq for Token<'a> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<'a> Eq for Token<'a> {}

pub struct TokenIterator<'a> {
    previous_whitespace: Option<Rc<RefCell<WhitespaceState<'a>>>>,
    core_state: TokState<'a>,
    absolute_indents: Vec<&'a str>,
}

impl<'a> TokenIterator<'a> {
    pub fn new(module_text: &'a str, config: &TokConfig) -> Self {
        Self {
            previous_whitespace: None,
            absolute_indents: vec![],
            core_state: TokState::new(module_text, config),
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token<'a>, TokError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.core_state.next();
        next.as_ref()?;
        Some((|| {
            let tok_type = next.unwrap()?;
            let relative_indent = match tok_type {
                TokType::Indent => {
                    let end_idx = self.core_state.text_pos.byte_idx();
                    let start_idx = end_idx - self.core_state.bol_width;
                    let absolute_indent = &self.core_state.text_pos.text()[start_idx..end_idx];
                    let relative_indent =
                        if let Some(prev_absolute_indent) = self.absolute_indents.last() {
                            if let Some(ri) = absolute_indent.strip_prefix(prev_absolute_indent) {
                                ri
                            } else {
                                // TODO: return the correct exception type, improve error message
                                return Err(TokError::Dedent);
                            }
                        } else {
                            // there's no previous indent, absolute_indent is relative_indent
                            absolute_indent
                        };
                    self.absolute_indents.push(absolute_indent);
                    // HACKY: mutate and fixup the previous whitespace state
                    if let Some(ws) = self.previous_whitespace.as_mut() {
                        ws.borrow_mut().absolute_indent = absolute_indent;
                    }
                    Some(relative_indent)
                }
                TokType::Dedent => {
                    self.absolute_indents.pop();
                    // HACKY: mutate and fixup the previous whitespace state
                    if let Some(ws) = self.previous_whitespace.as_mut() {
                        ws.borrow_mut().absolute_indent =
                            self.absolute_indents.last().unwrap_or(&"");
                    }
                    None
                }
                _ => None,
            };
            let text_pos = &self.core_state.text_pos;
            let whitespace_before = self.previous_whitespace.clone().unwrap_or_default();
            let whitespace_after = match tok_type {
                TokType::Indent | TokType::Dedent | TokType::EndMarker => whitespace_before.clone(),
                _ => Rc::new(RefCell::new(WhitespaceState {
                    line: text_pos.line_number(),
                    column: text_pos.char_column_number(),
                    column_byte: text_pos.byte_column_number(),
                    byte_offset: text_pos.byte_idx(),
                    absolute_indent: self.absolute_indents.last().unwrap_or(&""),
                    is_parenthesized: self.core_state.is_parenthesized(),
                })),
            };
            self.previous_whitespace = Some(whitespace_after.clone());

            Ok(Token {
                r#type: tok_type,
                string: text_pos.slice_from_start_pos(&self.core_state.start_pos),
                start_pos: self.core_state.start_pos.clone(),
                end_pos: text_pos.into(),
                whitespace_after: whitespace_after.clone(),
                whitespace_before: whitespace_before.clone(),
                relative_indent,
            })
        })())
    }
}
