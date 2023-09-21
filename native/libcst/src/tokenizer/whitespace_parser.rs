// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use crate::nodes::{
    Comment, EmptyLine, Fakeness, Newline, ParenthesizableWhitespace, ParenthesizedWhitespace,
    SimpleWhitespace, TrailingWhitespace,
};
use memchr::{memchr2, memchr2_iter};
use thiserror::Error;

use crate::Token;

use super::TokType;

#[allow(clippy::upper_case_acronyms, clippy::enum_variant_names)]
#[derive(Error, Debug, PartialEq, Eq)]
pub enum WhitespaceError {
    #[error("WTF")]
    WTF,
    #[error("Internal error while parsing whitespace: {0}")]
    InternalError(String),
    #[error("Failed to parse mandatory trailing whitespace")]
    TrailingWhitespaceError,
}

type Result<T> = std::result::Result<T, WhitespaceError>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct State<'a> {
    pub line: usize,   // one-indexed (to match parso's behavior)
    pub column: usize, // zero-indexed (to match parso's behavior)
    pub column_byte: usize,
    pub absolute_indent: &'a str,
    pub is_parenthesized: bool,
    pub byte_offset: usize,
}

impl<'a> Default for State<'a> {
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

// TODO
pub struct Config<'a> {
    pub input: &'a str,
    pub lines: Vec<&'a str>,
    pub default_newline: &'a str,
    pub default_indent: &'a str,
}

impl<'a> Config<'a> {
    pub fn new(input: &'a str, tokens: &[Token<'a>]) -> Self {
        let mut default_indent = "    ";
        for tok in tokens {
            if tok.r#type == TokType::Indent {
                default_indent = tok.relative_indent.unwrap();
                break;
            }
        }

        let mut lines = Vec::new();
        let mut start = 0;
        let mut newline_positions = memchr2_iter(b'\n', b'\r', input.as_bytes());

        while let Some(newline_position) = newline_positions.next() {
            let newline_character = input.as_bytes()[newline_position] as char;

            let len = if newline_character == '\r'
                && input.as_bytes().get(newline_position + 1) == Some(&b'\n')
            {
                // Skip the next '\n'
                newline_positions.next();
                2
            } else {
                1
            };

            let end = newline_position + len;
            lines.push(&input[start..end]);
            start = end;
        }

        // Push the last line if it isn't terminated by a newline character
        if start < input.len() {
            lines.push(&input[start..]);
        }

        let default_newline = match lines.first().map(|line| line.as_bytes()).unwrap_or(&[]) {
            [.., b'\r', b'\n'] => "\r\n",
            [.., b'\n'] => "\n",
            [.., b'\r'] => "\r",
            _ => "\n",
        };

        Self {
            input,
            lines,
            default_newline,
            default_indent,
        }
    }

    pub fn has_trailing_newline(&self) -> bool {
        self.input.ends_with('\n')
            && !self.input.ends_with("\\\n")
            && !self.input.ends_with("\\\r\n")
    }

    fn get_line(&self, line_number: usize) -> Result<&'a str> {
        let err_fn = || {
            WhitespaceError::InternalError(format!(
                "tried to get line {} which is out of range",
                line_number
            ))
        };
        self.lines
            .get(line_number.checked_sub(1).ok_or_else(err_fn)?)
            .map(|l| &l[..])
            .ok_or_else(err_fn)
    }

    fn get_line_after_column(&self, line_number: usize, column_index: usize) -> Result<&'a str> {
        self.get_line(line_number)?
            .get(column_index..)
            .ok_or_else(|| {
                WhitespaceError::InternalError(format!(
                    "Column index {} out of range for line {}",
                    column_index, line_number
                ))
            })
    }
}

#[derive(Debug)]
enum ParsedEmptyLine<'a> {
    NoIndent,
    Line(EmptyLine<'a>),
}

fn parse_empty_line<'a>(
    config: &Config<'a>,
    state: &mut State,
    override_absolute_indent: Option<&'a str>,
) -> Result<ParsedEmptyLine<'a>> {
    let mut speculative_state = state.clone();
    if let Ok(indent) = parse_indent(config, &mut speculative_state, override_absolute_indent) {
        let whitespace = parse_simple_whitespace(config, &mut speculative_state)?;
        let comment = parse_comment(config, &mut speculative_state)?;
        if let Some(newline) = parse_newline(config, &mut speculative_state)? {
            *state = speculative_state;
            return Ok(ParsedEmptyLine::Line(EmptyLine {
                indent,
                whitespace,
                comment,
                newline,
            }));
        }
    }
    Ok(ParsedEmptyLine::NoIndent)
}

fn _parse_empty_lines<'a>(
    config: &Config<'a>,
    state: &mut State<'a>,
    override_absolute_indent: Option<&'a str>,
) -> Result<Vec<(State<'a>, EmptyLine<'a>)>> {
    let mut lines = vec![];
    loop {
        let last_state = state.clone();
        let parsed_line = parse_empty_line(config, state, override_absolute_indent)?;
        if *state == last_state {
            break;
        }
        match parsed_line {
            ParsedEmptyLine::NoIndent => break,
            ParsedEmptyLine::Line(l) => lines.push((state.clone(), l)),
        }
    }
    Ok(lines)
}

pub fn parse_empty_lines<'a>(
    config: &Config<'a>,
    state: &mut State<'a>,
    override_absolute_indent: Option<&'a str>,
) -> Result<Vec<EmptyLine<'a>>> {
    // If override_absolute_indent is Some, then we need to parse all lines up to and including the
    // last line that is indented at our level. These all belong to the footer and not to the next
    // line's leading_lines.
    //
    // We don't know what the last line with indent=True is, and there could be indent=False lines
    // interspersed with indent=True lines, so we need to speculatively parse all possible empty
    // lines, and then unwind to find the last empty line with indent=True.
    let mut speculative_state = state.clone();
    let mut lines = _parse_empty_lines(config, &mut speculative_state, override_absolute_indent)?;

    if override_absolute_indent.is_some() {
        // Remove elements from the end until we find an indented line.
        while let Some((_, empty_line)) = lines.last() {
            if empty_line.indent {
                break;
            }
            lines.pop();
        }
    }

    if let Some((final_state, _)) = lines.last() {
        // update the state to match the last line that we captured
        *state = final_state.clone();
    }

    Ok(lines.into_iter().map(|(_, e)| e).collect())
}

pub fn parse_comment<'a>(config: &Config<'a>, state: &mut State) -> Result<Option<Comment<'a>>> {
    let newline_after = config.get_line_after_column(state.line, state.column_byte)?;
    if newline_after.as_bytes().first() != Some(&b'#') {
        return Ok(None);
    }
    let comment_str = if let Some(idx) = memchr2(b'\n', b'\r', newline_after.as_bytes()) {
        &newline_after[..idx]
    } else {
        newline_after
    };
    advance_this_line(
        config,
        state,
        comment_str.chars().count(),
        comment_str.len(),
    )?;
    Ok(Some(Comment(comment_str)))
}

pub fn parse_newline<'a>(config: &Config<'a>, state: &mut State) -> Result<Option<Newline<'a>>> {
    let newline_after = config.get_line_after_column(state.line, state.column_byte)?;
    let len = match newline_after.as_bytes() {
        [b'\n', ..] => 1,
        [b'\r', b'\n', ..] => 2,
        [b'\r', ..] => 1,
        _ => 0,
    };
    if len > 0 {
        let newline_str = &newline_after[..len];
        advance_this_line(config, state, len, len)?;
        if state.column_byte != config.get_line(state.line)?.len() {
            return Err(WhitespaceError::InternalError(format!(
                "Found newline at ({}, {}) but it's not EOL",
                state.line, state.column
            )));
        }
        if state.line < config.lines.len() {
            advance_to_next_line(config, state)?;
        }
        return Ok(Some(Newline(
            if newline_str == config.default_newline {
                None
            } else {
                Some(newline_str)
            },
            Fakeness::Real,
        )));
    }

    // If we're at the end of the file but not on BOL, that means this is the fake
    // newline inserted by the tokenizer.
    if state.byte_offset == config.input.len() && state.column_byte != 0 {
        return Ok(Some(Newline(None, Fakeness::Fake)));
    }
    Ok(None)
}

pub fn parse_optional_trailing_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State,
) -> Result<Option<TrailingWhitespace<'a>>> {
    let mut speculative_state = state.clone();
    let whitespace = parse_simple_whitespace(config, &mut speculative_state)?;
    let comment = parse_comment(config, &mut speculative_state)?;
    if let Some(newline) = parse_newline(config, &mut speculative_state)? {
        *state = speculative_state;
        Ok(Some(TrailingWhitespace {
            whitespace,
            comment,
            newline,
        }))
    } else {
        Ok(None)
    }
}

pub fn parse_trailing_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State,
) -> Result<TrailingWhitespace<'a>> {
    match parse_optional_trailing_whitespace(config, state)? {
        Some(ws) => Ok(ws),
        _ => Err(WhitespaceError::TrailingWhitespaceError),
    }
}

fn parse_indent<'a>(
    config: &Config<'a>,
    state: &mut State,
    override_absolute_indent: Option<&'a str>,
) -> Result<bool> {
    let absolute_indent = override_absolute_indent.unwrap_or(state.absolute_indent);
    if state.column_byte != 0 {
        if state.column_byte == config.get_line(state.line)?.len()
            && state.line == config.lines.len()
        {
            Ok(false)
        } else {
            Err(WhitespaceError::InternalError(
                "Column should not be 0 when parsing an index".to_string(),
            ))
        }
    } else {
        Ok(
            if config
                .get_line_after_column(state.line, state.column_byte)?
                .starts_with(absolute_indent)
            {
                state.column_byte += absolute_indent.len();
                state.column += absolute_indent.chars().count();
                state.byte_offset += absolute_indent.len();
                true
            } else {
                false
            },
        )
    }
}

fn advance_to_next_line<'a>(config: &Config<'a>, state: &mut State) -> Result<()> {
    let cur_line = config.get_line(state.line)?;
    state.byte_offset += cur_line.len() - state.column_byte;
    state.column = 0;
    state.column_byte = 0;
    state.line += 1;
    Ok(())
}

fn advance_this_line<'a>(
    config: &Config<'a>,
    state: &mut State,
    char_count: usize,
    offset: usize,
) -> Result<()> {
    let cur_line = config.get_line(state.line)?;
    if cur_line.len() < state.column_byte + offset {
        return Err(WhitespaceError::InternalError(format!(
            "Tried to advance past line {}'s end",
            state.line
        )));
    }
    state.column += char_count;
    state.column_byte += offset;
    state.byte_offset += offset;
    Ok(())
}

pub fn parse_simple_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State,
) -> Result<SimpleWhitespace<'a>> {
    let capture_ws = |line, col| -> Result<&'a str> {
        let line = config.get_line_after_column(line, col)?;
        let bytes = line.as_bytes();
        let mut idx = 0;
        while idx < bytes.len() {
            match bytes[idx..] {
                [b' ' | b'\t' | b'\x0c', ..] => idx += 1,
                [b'\\', b'\r', b'\n', ..] => idx += 3,
                [b'\\', b'\r' | b'\n', ..] => idx += 2,
                _ => break,
            }
        }
        Ok(&line[..idx])
    };
    let start_offset = state.byte_offset;
    let mut prev_line: &str;
    loop {
        prev_line = capture_ws(state.line, state.column_byte)?;
        if !prev_line.contains('\\') {
            break;
        }
        advance_to_next_line(config, state)?;
    }
    advance_this_line(config, state, prev_line.chars().count(), prev_line.len())?;

    Ok(SimpleWhitespace(
        &config.input[start_offset..state.byte_offset],
    ))
}

pub fn parse_parenthesizable_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State<'a>,
) -> Result<ParenthesizableWhitespace<'a>> {
    if state.is_parenthesized {
        if let Some(ws) = parse_parenthesized_whitespace(config, state)? {
            return Ok(ParenthesizableWhitespace::ParenthesizedWhitespace(ws));
        }
    }
    parse_simple_whitespace(config, state).map(ParenthesizableWhitespace::SimpleWhitespace)
}

pub fn parse_parenthesized_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State<'a>,
) -> Result<Option<ParenthesizedWhitespace<'a>>> {
    if let Some(first_line) = parse_optional_trailing_whitespace(config, state)? {
        let empty_lines = _parse_empty_lines(config, state, None)?
            .into_iter()
            .map(|(_, line)| line)
            .collect();
        let indent = parse_indent(config, state, None)?;
        let last_line = parse_simple_whitespace(config, state)?;
        Ok(Some(ParenthesizedWhitespace {
            first_line,
            empty_lines,
            indent,
            last_line,
        }))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use crate::{tokenize, Comment, Config, Result, SimpleWhitespace};

    use super::{parse_comment, parse_simple_whitespace};

    #[test]
    fn config_mixed_newlines() -> Result<'static, ()> {
        let source = "'' % {\n'test1': '',\r  'test2': '',\r\n}";
        let tokens = tokenize(source)?;

        let config = Config::new(source, &tokens);

        assert_eq!(
            &config.lines,
            &["'' % {\n", "'test1': '',\r", "  'test2': '',\r\n", "}"]
        );

        Ok(())
    }

    fn _parse_simple_whitespace(src: &str) -> Result<SimpleWhitespace> {
        let tokens = tokenize(src)?;
        let config = Config::new(src, &tokens);
        let mut state = Default::default();
        Ok(parse_simple_whitespace(&config, &mut state)?)
    }

    #[test]
    fn simple_whitespace_line_continuations() -> Result<'static, ()> {
        assert_eq!(
            _parse_simple_whitespace("  \\\n  # foo")?,
            SimpleWhitespace("  \\\n  ")
        );

        assert_eq!(
            _parse_simple_whitespace("  \\\r  # foo")?,
            SimpleWhitespace("  \\\r  ")
        );
        assert_eq!(
            _parse_simple_whitespace("  \\\r\n  # foo")?,
            SimpleWhitespace("  \\\r\n  ")
        );

        assert_eq!(
            _parse_simple_whitespace("  \\\r\n\\\n  # foo")?,
            SimpleWhitespace("  \\\r\n\\\n  ")
        );

        Ok(())
    }

    #[test]
    fn simple_whitespace_mixed() -> Result<'static, ()> {
        assert_eq!(
            _parse_simple_whitespace(" \t\x0clol")?,
            SimpleWhitespace(" \t\x0c"),
        );

        Ok(())
    }

    fn _parse_comment(src: &str) -> Result<Option<Comment>> {
        let tokens = tokenize(src)?;
        let config = Config::new(src, &tokens);
        let mut state = Default::default();
        Ok(parse_comment(&config, &mut state)?)
    }

    #[test]
    fn single_comment() -> Result<'static, ()> {
        assert_eq!(_parse_comment("# foo\n# bar")?, Some(Comment("# foo")));
        Ok(())
    }

    #[test]
    fn comment_until_eof() -> Result<'static, ()> {
        assert_eq!(_parse_comment("#")?, Some(Comment("#")));
        Ok(())
    }

    #[test]
    fn no_comment() -> Result<'static, ()> {
        assert_eq!(_parse_comment("foo")?, None);
        assert_eq!(_parse_comment("\n")?, None);
        Ok(())
    }
}
