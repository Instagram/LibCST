use pyo3::types::{IntoPyDict, PyModule};
use pyo3::{IntoPy, PyErr, PyErrArguments, Python};

use crate::parser::grammar::TokVec;
use crate::tokenizer::whitespace_parser::WhitespaceError;
use crate::tokenizer::TokError;
use peg::Parse;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParserError<'a> {
    #[error("tokenizer error: {0}")]
    TokenizerError(TokError<'a>),
    #[error(transparent)]
    ParserError(#[from] peg::error::ParseError<<TokVec<'a> as Parse>::PositionRepr>),
    #[error(transparent)]
    WhitespaceError(#[from] WhitespaceError),
    #[error("invalid operator")]
    OperatorError,
}

impl<'a> From<ParserError<'a>> for PyErr {
    fn from(e: ParserError) -> Self {
        Python::with_gil(|py| {
            let kwargs = [
                ("message", e.to_string().into_py(py)),
                ("lines", vec![""].into_py(py)),
                ("raw_line", 0.into_py(py)),
                ("raw_column", 0.into_py(py)),
            ]
            .into_py_dict(py);
            let libcst = PyModule::import(py, "libcst").expect("libcst cannot be imported");
            PyErr::from_instance(
                libcst
                    .getattr("ParserSyntaxError")
                    .expect("ParserSyntaxError not found")
                    .call((), Some(kwargs))
                    .expect("failed to instantiate"),
            )
        })
    }
}

struct Details {
    message: String,
    lines: Vec<String>,
    raw_line: u32,
    raw_column: u32,
}

impl<'a> PyErrArguments for Details {
    fn arguments(self, py: pyo3::Python) -> pyo3::PyObject {
        [
            ("message", self.message.into_py(py)),
            ("lines", self.lines.into_py(py)),
            ("raw_line", self.raw_line.into_py(py)),
            ("raw_column", self.raw_column.into_py(py)),
        ]
        .into_py_dict(py)
        .into_py(py)
    }
}
