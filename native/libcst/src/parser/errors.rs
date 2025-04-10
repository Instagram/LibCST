// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use crate::parser::grammar::TokVec;
use crate::tokenizer::whitespace_parser::WhitespaceError;
use crate::tokenizer::TokError;
use peg::Parse;
use thiserror::Error;

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParserError<'a> {
    #[error("tokenizer error: {0}")]
    TokenizerError(TokError<'a>, &'a str),
    #[error("parser error: {0}")]
    ParserError(
        peg::error::ParseError<<TokVec<'a> as Parse>::PositionRepr>,
        &'a str,
    ),
    #[error(transparent)]
    WhitespaceError(#[from] WhitespaceError),
    #[error("invalid operator")]
    OperatorError,
}

#[cfg(feature = "py")]
mod py_error {

    use pyo3::types::{IntoPyDict, PyAny, PyAnyMethods, PyModule};
    use pyo3::{Bound, IntoPyObject, PyErr, PyResult, Python};

    use super::ParserError;

    impl<'a> From<ParserError<'a>> for PyErr {
        fn from(e: ParserError) -> Self {
            Python::with_gil(|py| {
                let lines = match &e {
                    ParserError::TokenizerError(_, text) | ParserError::ParserError(_, text) => {
                        text.lines().collect::<Vec<_>>()
                    }
                    _ => vec![""],
                };
                let (mut line, mut col) = match &e {
                    ParserError::ParserError(err, ..) => {
                        (err.location.start_pos.line, err.location.start_pos.column)
                    }
                    _ => (0, 0),
                };
                if line + 1 > lines.len() {
                    line = lines.len() - 1;
                    col = 0;
                }
                match || -> PyResult<Bound<'_, PyAny>> {
                    let kwargs = [
                        ("message", e.to_string().into_pyobject(py)?.into_any()),
                        ("lines", lines.into_pyobject(py)?.into_any()),
                        ("raw_line", (line + 1).into_pyobject(py)?.into_any()),
                        ("raw_column", col.into_pyobject(py)?.into_any()),
                    ]
                    .into_py_dict(py)?;
                    let libcst = PyModule::import(py, "libcst")?;
                    libcst.getattr("ParserSyntaxError")?.call((), Some(&kwargs))
                }() {
                    Ok(py_err_value) => PyErr::from_value(py_err_value),
                    Err(e) => e,
                }
            })
        }
    }
}
