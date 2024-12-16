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

    use pyo3::types::{IntoPyDict, PyAnyMethods, PyModule};
    use pyo3::{IntoPy, PyErr, PyErrArguments, Python};

    use super::ParserError;

    struct Details {
        message: String,
        lines: Vec<String>,
        raw_line: u32,
        raw_column: u32,
    }

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
                let kwargs = [
                    ("message", e.to_string().into_py(py)),
                    ("lines", lines.into_py(py)),
                    ("raw_line", (line + 1).into_py(py)),
                    ("raw_column", col.into_py(py)),
                ]
                .into_py_dict_bound(py);
                let libcst =
                    PyModule::import_bound(py, "libcst").expect("libcst cannot be imported");
                PyErr::from_value_bound(
                    libcst
                        .getattr("ParserSyntaxError")
                        .expect("ParserSyntaxError not found")
                        .call((), Some(&kwargs))
                        .expect("failed to instantiate"),
                )
            })
        }
    }

    impl<'a> PyErrArguments for Details {
        fn arguments(self, py: pyo3::Python) -> pyo3::PyObject {
            [
                ("message", self.message.into_py(py)),
                ("lines", self.lines.into_py(py)),
                ("raw_line", self.raw_line.into_py(py)),
                ("raw_column", self.raw_column.into_py(py)),
            ]
            .into_py_dict_bound(py)
            .into_py(py)
        }
    }
}
