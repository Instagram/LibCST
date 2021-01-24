// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

/// Parso doesn't attempt to parse (or even emit tokens for) whitespace or comments that aren't
/// syntatically important. Instead, we're just given the whitespace as a "prefix" of the token.
///
/// However, in our CST, whitespace is gathered into far more detailed objects than a simple str.
///
/// Fortunately this isn't hard for us to parse ourselves, so we just use our own hand-rolled
/// recursive descent parser.
use once_cell::sync::Lazy;
use pyo3::exceptions::PyException;
use pyo3::prelude::*;
use pyo3::types::PyTuple;
use pyo3::wrap_pyfunction;
use regex::Regex;

use crate::parser_config::BaseWhitespaceParserConfig as Config;
use crate::whitespace_state::WhitespaceState as State;

static SIMPLE_WHITESPACE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\A([ \f\t]|\\(\r\n?|\n))*").expect("regex"));
static NEWLINE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\A(\r\n?|\n)").expect("regex"));
static COMMENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\A#[^\r\n]*").expect("regex"));

py_import!(
    "libcst._nodes.whitespace",
    "SimpleWhitespace",
    get_simple_whitespace_cls
);
py_import!("libcst._nodes.whitespace", "EmptyLine", get_empty_line_cls);
py_import!("libcst._nodes.whitespace", "Comment", get_comment_cls);
py_import!("libcst._nodes.whitespace", "Newline", get_newline_cls);
py_import!(
    "libcst._nodes.whitespace",
    "TrailingWhitespace",
    get_trailing_whitespace_cls
);
py_import!(
    "libcst._nodes.whitespace",
    "ParenthesizedWhitespace",
    get_parenthesized_whitespace_cls
);

fn new_simple_whitespace<'py>(py: Python<'py>, value: &str) -> PyResult<&'py PyAny> {
    Ok(get_simple_whitespace_cls(py)?.call1((value,))?)
}

fn new_empty_line<'py>(
    py: Python<'py>,
    indent: bool,
    whitespace: &'py PyAny,
    comment: Option<&'py PyAny>,
    newline: &'py PyAny,
) -> PyResult<&'py PyAny> {
    Ok(get_empty_line_cls(py)?.call1((indent, whitespace, comment, newline))?)
}

fn new_comment<'py>(py: Python<'py>, value: &str) -> PyResult<&'py PyAny> {
    Ok(get_comment_cls(py)?.call1((value,))?)
}

fn new_newline<'py>(py: Python<'py>, value: Option<&str>) -> PyResult<&'py PyAny> {
    Ok(get_newline_cls(py)?.call1((value,))?)
}

fn new_trailing_whitespace<'py>(
    py: Python<'py>,
    whitespace: &'py PyAny,
    comment: Option<&'py PyAny>,
    newline: &'py PyAny,
) -> PyResult<&'py PyAny> {
    Ok(get_trailing_whitespace_cls(py)?.call1((whitespace, comment, newline))?)
}

fn new_parenthesized_whitespace<'py>(
    py: Python<'py>,
    first_line: &'py PyAny,
    empty_lines: Vec<&'py PyAny>,
    indent: bool,
    last_line: &'py PyAny,
) -> PyResult<&'py PyAny> {
    Ok(get_parenthesized_whitespace_cls(py)?.call1((
        first_line,
        PyTuple::new(py, empty_lines),
        indent,
        last_line,
    ))?)
}

// BEGIN PARSER ENTRYPOINTS

#[pyfunction]
pub fn parse_simple_whitespace<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<&'py PyAny> {
    let capture_ws = |line, column| -> PyResult<&str> {
        Ok(SIMPLE_WHITESPACE_RE
            .find(config.get_line_after_column(line, column)?)
            .expect("SIMPLE_WHITESPACE_RE supports 0-length matches, so it must always match")
            .as_str())
    };
    let mut prev_line = capture_ws(state.line, state.column)?;
    let mut ws = prev_line.to_string();
    while prev_line.contains('\\') {
        // continuation character
        state.line += 1;
        state.column = 0;
        prev_line = capture_ws(state.line, state.column)?;
        ws.push_str(prev_line);
    }
    state.column += prev_line.len();
    new_simple_whitespace(py, &ws[..])
}

#[pyfunction]
#[text_signature = "(config, state, *, override_absolute_indent)"]
pub fn parse_empty_lines<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
    override_absolute_indent: Option<&str>,
) -> PyResult<&'py PyTuple> {
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
        parse_empty_line(py, config, &mut speculative_state, override_absolute_indent)?
    {
        lines.push((speculative_state.clone(), empty_line));
    }

    if override_absolute_indent.is_some() {
        // Remove elements from the end until we find an indented line.
        while let Some((_, empty_line)) = lines.last() {
            if empty_line.getattr("indent")?.is_true()? {
                break;
            }
            lines.pop();
        }
    }

    if let Some((final_state, _)) = lines.last() {
        // update the state to match the last line that we captured
        *state = final_state.clone();
    }

    Ok(PyTuple::new(
        py,
        lines.iter().map(|(_, empty_line)| empty_line),
    ))
}

#[pyfunction]
pub fn parse_trailing_whitespace<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<&'py PyAny> {
    if let Some(trailing_whitespace) = parse_optional_trailing_whitespace(py, config, state)? {
        Ok(trailing_whitespace)
    } else {
        Err(PyException::new_err(concat!(
            "Internal Error: Failed to parse TrailingWhitespace. This should never ",
            "happen because a TrailingWhitespace is never optional in the grammar, ",
            "so this error should've been caught by parso first.",
        )))
    }
}

#[pyfunction]
pub fn parse_parenthesizable_whitespace<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<&'py PyAny> {
    if state.is_parenthesized {
        // First, try parenthesized (don't need speculation because it either parses or doesn't
        // modify state).
        if let Some(parenthesized_whitespace) = parse_parenthesized_whitespace(py, config, state)? {
            return Ok(parenthesized_whitespace);
        }
    }
    // It's not parenthesized, or ParenthesizedWhitespace didn't parse. Just parse and return a
    // SimpleWhitespace.
    parse_simple_whitespace(py, config, state)
}

// END PARSER ENTRYPOINTS
// BEGIN PARSER INTERNAL PRODUCTIONS

pub fn parse_empty_line<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
    override_absolute_indent: Option<&str>,
) -> PyResult<Option<&'py PyAny>> {
    // begin speculative parsing
    let mut speculative_state = state.clone();
    if let Ok(indent) = parse_indent(py, config, &mut speculative_state, override_absolute_indent) {
        let whitespace = parse_simple_whitespace(py, config, &mut speculative_state)?;
        let comment = parse_comment(py, config, &mut speculative_state)?;
        if let Some(newline) = parse_newline(py, config, &mut speculative_state)? {
            // speculative parsing succeeded
            *state = speculative_state;
            Ok(Some(new_empty_line(
                py, indent, whitespace, comment, newline,
            )?))
        } else {
            // no newline found, speculative parsing failed
            Ok(None)
        }
    } else {
        // we aren't on a new line, speculative parsing failed
        // TODO: Don't rely on a python exception for this, use a rust error type
        Ok(None)
    }
}

/// Returns true if indentation was found, otherwise False.
pub fn parse_indent(
    _py: Python,
    config: &Config,
    state: &mut State,
    override_absolute_indent: Option<&str>,
) -> PyResult<bool> {
    let absolute_indent = override_absolute_indent.unwrap_or(&state.absolute_indent[..]);
    if state.column != 0 {
        if state.column == config.get_line(state.line)?.len() && state.line == config.lines.len() {
            // we're at EOF, treat this as a failed speculative parse
            Ok(false)
        } else {
            Err(PyException::new_err(
                "Internal Error: Column should not be 0 when parsing an indent",
            ))
        }
    } else if config
        .get_line_after_column(state.line, state.column)?
        .starts_with(absolute_indent)
    {
        state.column += absolute_indent.len();
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn parse_comment<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<Option<&'py PyAny>> {
    if let Some(comment_match) =
        COMMENT_RE.find(config.get_line_after_column(state.line, state.column)?)
    {
        let comment_str = comment_match.as_str();
        state.column += comment_str.len();
        Ok(Some(new_comment(py, comment_str)?))
    } else {
        Ok(None)
    }
}

pub fn parse_newline<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<Option<&'py PyAny>> {
    // begin speculative parsing
    if let Some(newline_match) =
        NEWLINE_RE.find(config.get_line_after_column(state.line, state.column)?)
    {
        // speculative parsing succeeded
        let newline_str = newline_match.as_str();
        state.column += newline_str.len();
        if state.column != config.get_line(state.line)?.len() {
            return Err(PyException::new_err(
                "Internal Error: Found a newline, but it wasn't the EOL.",
            ));
        }
        if state.line < config.lines.len() {
            state.line += 1;
            state.column = 0;
        }
        if newline_str == config.default_newline.as_ref() {
            Ok(Some(new_newline(py, None)?))
        } else {
            Ok(Some(new_newline(py, Some(newline_str))?))
        }
    } else {
        // no newline was found, speculative parsing failed
        Ok(None)
    }
}

pub fn parse_optional_trailing_whitespace<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<Option<&'py PyAny>> {
    // begin speculative parsing
    let mut speculative_state = state.clone();
    let whitespace = parse_simple_whitespace(py, config, &mut speculative_state)?;
    let comment = parse_comment(py, config, &mut speculative_state)?;
    if let Some(newline) = parse_newline(py, config, &mut speculative_state)? {
        // speculative parsing succeeded
        *state = speculative_state;
        Ok(Some(new_trailing_whitespace(
            py, whitespace, comment, newline,
        )?))
    } else {
        // speculative parsing failed
        Ok(None)
    }
}

pub fn parse_parenthesized_whitespace<'py>(
    py: Python<'py>,
    config: &Config,
    state: &mut State,
) -> PyResult<Option<&'py PyAny>> {
    if let Some(first_line) = parse_optional_trailing_whitespace(py, config, state)? {
        let mut empty_lines = Vec::new();
        while let Some(empty_line) = parse_empty_line(py, config, state, None)? {
            empty_lines.push(empty_line);
        }
        let indent = parse_indent(py, config, state, None)?;
        let last_line = parse_simple_whitespace(py, config, state)?;
        Ok(Some(new_parenthesized_whitespace(
            py,
            first_line,
            empty_lines,
            indent,
            last_line,
        )?))
    } else {
        Ok(None)
    }
}

pub fn init_module(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse_simple_whitespace, m)?)
        .unwrap();
    m.add_function(wrap_pyfunction!(parse_empty_lines, m)?)
        .unwrap();
    m.add_function(wrap_pyfunction!(parse_trailing_whitespace, m)?)
        .unwrap();
    m.add_function(wrap_pyfunction!(parse_parenthesizable_whitespace, m)?)
        .unwrap();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_utils::py_assert_deep_equals;

    type ParseFn<'py> = dyn FnOnce(Python<'py>, &Config, &mut State) -> PyResult<&'py PyAny>;

    struct TestCase<'py, 't> {
        parser: Box<ParseFn<'py>>,
        // We could accept a Config instead of lines and default_newline, but Config is a
        // little awkward to construct from `&str`, so we do it in `.test()`.
        lines: Vec<&'t str>,
        default_newline: &'t str,
        start_state: State,
        end_state: State,
        expected_node: &'py PyAny,
    }

    impl<'py, 't> TestCase<'py, 't> {
        fn test(mut self, py: Python<'py>) {
            let config = Config {
                lines: self
                    .lines
                    .iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<_>>()
                    .into(),
                default_newline: self.default_newline.to_string().into(),
            };
            let parsed_node = (self.parser)(py, &config, &mut self.start_state)
                .unwrap()
                .into_py(py);
            py_assert_deep_equals(py, &parsed_node, &self.expected_node);
            assert_eq!(&self.start_state, &self.end_state);
        }
    }

    mod simple_whitespace {
        use super::*;

        #[test]
        fn test_empty() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_simple_whitespace),
                    lines: vec!["not whitespace\n", " another line\n"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: new_simple_whitespace(py, "").unwrap(),
                }
                .test(py)
            })
        }

        #[test]
        fn test_start_of_line() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_simple_whitespace),
                    lines: vec!["\t  <-- There's some whitespace there\n"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 1,
                        column: 3,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: new_simple_whitespace(py, "\t  ").unwrap(),
                }
                .test(py)
            })
        }

        #[test]
        fn test_end_of_line() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_simple_whitespace),
                    lines: vec!["prefix   "],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 6,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 1,
                        column: 9,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: new_simple_whitespace(py, "   ").unwrap(),
                }
                .test(py)
            })
        }

        #[test]
        fn test_line_continuation() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_simple_whitespace),
                    lines: vec!["prefix \\\n", "    \\\n", "    # suffix\n"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 6,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 3,
                        column: 4,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: new_simple_whitespace(py, " \\\n    \\\n    ").unwrap(),
                }
                .test(py)
            })
        }
    }

    mod empty_lines {
        use super::*;

        fn parse_empty_lines_no_override<'py>(
            py: Python<'py>,
            config: &Config,
            state: &mut State,
        ) -> PyResult<&'py PyAny> {
            parse_empty_lines(py, config, state, None).map(|lines| lines.into())
        }

        #[test]
        fn test_empty_list() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_empty_lines_no_override),
                    lines: vec!["this is not an empty line"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: PyTuple::new(py, Vec::<&PyAny>::new()),
                }
                .test(py)
            })
        }

        #[test]
        fn test_single_line() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_empty_lines_no_override),
                    lines: vec!["    # comment\n", "this is not an empty line\n"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "    ".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 2,
                        column: 0,
                        absolute_indent: "    ".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: PyTuple::new(
                        py,
                        vec![new_empty_line(
                            py,
                            /* indent */ true,
                            /* whitespace */ new_simple_whitespace(py, "").unwrap(),
                            /* comment */ Some(new_comment(py, "# comment").unwrap()),
                            /* newline */ new_newline(py, None).unwrap(),
                        )
                        .unwrap()],
                    ),
                }
                .test(py)
            })
        }

        #[test]
        fn test_multiple_lines() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_empty_lines_no_override),
                    lines: vec![
                        "\n",
                        "    \n",
                        "     # comment with indent and whitespace\n",
                        "# comment without indent\n",
                        "  # comment with no indent but some whitespace\n",
                    ],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "    ".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 5,
                        column: 47,
                        absolute_indent: "    ".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: PyTuple::new(
                        py,
                        vec![
                            new_empty_line(
                                py,
                                /* indent */ false,
                                new_simple_whitespace(py, "").unwrap(),
                                /* comment */ None,
                                new_newline(py, None).unwrap(),
                            )
                            .unwrap(),
                            new_empty_line(
                                py,
                                /* indent */ true,
                                new_simple_whitespace(py, "").unwrap(),
                                /* comment */ None,
                                new_newline(py, None).unwrap(),
                            )
                            .unwrap(),
                            new_empty_line(
                                py,
                                /* indent */ true,
                                new_simple_whitespace(py, " ").unwrap(),
                                Some(
                                    new_comment(py, "# comment with indent and whitespace")
                                        .unwrap(),
                                ),
                                new_newline(py, None).unwrap(),
                            )
                            .unwrap(),
                            new_empty_line(
                                py,
                                /* indent */ false,
                                new_simple_whitespace(py, "").unwrap(),
                                Some(new_comment(py, "# comment without indent").unwrap()),
                                new_newline(py, None).unwrap(),
                            )
                            .unwrap(),
                            new_empty_line(
                                py,
                                /* indent */ false,
                                new_simple_whitespace(py, "  ").unwrap(),
                                Some(
                                    new_comment(py, "# comment with no indent but some whitespace")
                                        .unwrap(),
                                ),
                                new_newline(py, None).unwrap(),
                            )
                            .unwrap(),
                        ],
                    ),
                }
                .test(py)
            })
        }

        #[test]
        fn test_non_default_newline() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_empty_lines_no_override),
                    lines: vec!["\n", "\r\n", "\r"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 0,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 3,
                        column: 1,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: PyTuple::new(
                        py,
                        vec![
                            new_empty_line(
                                py,
                                /* indent */ true,
                                new_simple_whitespace(py, "").unwrap(),
                                /* comment */ None,
                                new_newline(py, None).unwrap(), // default newline
                            )
                            .unwrap(),
                            new_empty_line(
                                py,
                                /* indent */ true,
                                new_simple_whitespace(py, "").unwrap(),
                                /* comment */ None,
                                new_newline(py, Some("\r\n")).unwrap(),
                            )
                            .unwrap(),
                            new_empty_line(
                                py,
                                /* indent */ true,
                                new_simple_whitespace(py, "").unwrap(),
                                /* comment */ None,
                                new_newline(py, Some("\r")).unwrap(),
                            )
                            .unwrap(),
                        ],
                    ),
                }
                .test(py)
            })
        }
    }

    mod trailing_whitespace {
        use super::*;

        #[test]
        fn test_with_whitespace_and_comment() {
            Python::with_gil(|py| {
                TestCase {
                    parser: Box::new(parse_trailing_whitespace),
                    lines: vec!["some code  # comment\n"],
                    default_newline: "\n",
                    start_state: State {
                        line: 1,
                        column: 9,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    end_state: State {
                        line: 1,
                        column: 21,
                        absolute_indent: "".to_string(),
                        is_parenthesized: false,
                    },
                    expected_node: new_trailing_whitespace(
                        py,
                        /* whitespace */ new_simple_whitespace(py, "  ").unwrap(),
                        /* comment */ Some(new_comment(py, "# comment").unwrap()),
                        /* newline */ new_newline(py, None).unwrap(),
                    )
                    .unwrap(),
                }
                .test(py)
            })
        }
    }
}
