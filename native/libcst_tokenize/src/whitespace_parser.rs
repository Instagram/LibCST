use libcst_nodes::{
    Comment, EmptyLine, Fakeness, Newline, ParenthesizableWhitespace, ParenthesizedWhitespace,
    SimpleWhitespace, TrailingWhitespace,
};
use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

static SIMPLE_WHITESPACE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\A([ \f\t]|\\(\r\n?|\n))*").expect("regex"));
static NEWLINE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\A(\r\n?|\n)").expect("regex"));
static COMMENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\A#[^\r\n]*").expect("regex"));

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
    pub absolute_indent: &'a str,
    pub is_parenthesized: bool,
    pub byte_offset: usize,
}

impl<'a> Default for State<'a> {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
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
}

impl<'a> Config<'a> {
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

fn parse_empty_line<'a>(
    config: &Config<'a>,
    state: &mut State,
    override_absolute_indent: Option<&'a str>,
) -> Result<Option<EmptyLine<'a>>> {
    let mut speculative_state = state.clone();
    if let Ok(indent) = parse_indent(
        config,
        &mut &mut speculative_state,
        override_absolute_indent,
    ) {
        let whitespace = parse_simple_whitespace(config, &mut speculative_state)?;
        let comment = parse_comment(config, &mut speculative_state)?;
        if let Some(newline @ Newline(_, Fakeness::Real)) =
            parse_newline(config, &mut speculative_state)?
        {
            *state = speculative_state;
            return Ok(Some(EmptyLine {
                indent,
                whitespace,
                comment,
                newline,
            }));
        }
    }
    Ok(None)
}

pub fn parse_empty_lines<'a>(
    config: &Config<'a>,
    state: &mut State,
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
    let mut lines = Vec::new();
    while let Some(empty_line) =
        parse_empty_line(config, &mut speculative_state, override_absolute_indent)?
    {
        lines.push((speculative_state.clone(), empty_line));
    }

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
    if let Some(comment_match) =
        COMMENT_RE.find(config.get_line_after_column(state.line, state.column)?)
    {
        let comment_str = comment_match.as_str();
        advance_this_line(config, state, comment_str.len())?;
        return Ok(Some(Comment(comment_str)));
    }
    Ok(None)
}

pub fn parse_newline<'a>(config: &Config<'a>, state: &mut State) -> Result<Option<Newline<'a>>> {
    if let Some(newline_match) =
        NEWLINE_RE.find(config.get_line_after_column(state.line, state.column)?)
    {
        let newline_str = newline_match.as_str();
        advance_this_line(config, state, newline_str.len())?;
        if state.column != config.get_line(state.line)?.len() {
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
    if state.byte_offset == config.input.len() && state.column != 0 {
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
    if state.column != 0 {
        if state.column == config.get_line(state.line)?.len() && state.line == config.lines.len() {
            Ok(false)
        } else {
            Err(WhitespaceError::InternalError(
                "Column should not be 0 when parsing an index".to_string(),
            ))
        }
    } else {
        Ok(
            if config
                .get_line_after_column(state.line, state.column)?
                .starts_with(absolute_indent)
            {
                state.column += absolute_indent.len();
                true
            } else {
                false
            },
        )
    }
}

fn advance_to_next_line<'a>(config: &Config<'a>, state: &mut State) -> Result<()> {
    let cur_line = config.get_line(state.line)?;
    state.byte_offset += cur_line.len() - state.column;
    state.column = 0;
    state.line += 1;
    Ok(())
}

fn advance_this_line<'a>(config: &Config<'a>, state: &mut State, offset: usize) -> Result<()> {
    let cur_line = config.get_line(state.line)?;
    if cur_line.len() < state.column + offset {
        return Err(WhitespaceError::InternalError(format!(
            "Tried to advance past line {}'s end",
            state.line
        )));
    }
    state.column += offset;
    state.byte_offset += offset;
    Ok(())
}

pub fn parse_simple_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State,
) -> Result<SimpleWhitespace<'a>> {
    let capture_ws = |line, col| -> Result<&'a str> {
        Ok(SIMPLE_WHITESPACE_RE
            .find(config.get_line_after_column(line, col)?)
            .expect("SIMPLE_WHITESPACE_RE supports 0-length matches, so it must always match")
            .as_str())
    };
    let start_offset = state.byte_offset;
    let mut prev_line: &str;
    loop {
        prev_line = capture_ws(state.line, state.column)?;
        if !prev_line.contains('\\') {
            break;
        }
        advance_to_next_line(config, state)?;
    }
    advance_this_line(config, state, prev_line.len())?;

    Ok(SimpleWhitespace(
        &config.input[start_offset..state.byte_offset],
    ))
}

pub fn parse_parenthesizable_whitespace<'a>(
    config: &Config<'a>,
    state: &mut State,
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
    state: &mut State,
) -> Result<Option<ParenthesizedWhitespace<'a>>> {
    if let Some(first_line) = parse_optional_trailing_whitespace(config, state)? {
        let mut empty_lines = Vec::new();
        while let Some(empty_line) = parse_empty_line(config, state, None)? {
            empty_lines.push(empty_line);
        }
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
