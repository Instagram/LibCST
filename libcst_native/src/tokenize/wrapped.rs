// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

/// A wrapper around crate::tokenize::core that provides `Token` structs. The `core` module only
/// gives token types alongside a mutated large and mostly opaque `TokState` struct.
///
/// This uses the data given by the core module to attach start and end positions to tokens and to
/// generate before/after `WhitespaceState` values.
use pyo3::class::basic::CompareOp;
use pyo3::exceptions::PyException;
use pyo3::prelude::*;
use pyo3::types::PyString;
use pyo3::PyObjectProtocol;

use crate::token_type::{PythonTokenTypes, TokenType as PyTokenType};
use crate::tokenize::core::{TokConfig, TokState, TokType};
use crate::whitespace_state::WhitespaceState;

#[pyclass(module = "libcst_native.tokenize")]
#[text_signature = "(type, string, start_pos, end_pos, whitespace_before, whitespace_after, relative_indent)"]
pub struct Token {
    #[pyo3(get)]
    r#type: Py<PyTokenType>,
    #[pyo3(get)]
    string: Py<PyString>,
    #[pyo3(get)]
    start_pos: PyObject, // tuple: (1-indexed line, 0-indexed column)
    #[pyo3(get)]
    end_pos: PyObject,
    #[pyo3(get)]
    whitespace_before: Py<WhitespaceState>, // WhitespaceState
    #[pyo3(get)]
    whitespace_after: Py<WhitespaceState>, // WhitespaceState
    #[pyo3(get)]
    relative_indent: PyObject, // PyString or py.None()
}

#[pymethods]
impl Token {
    // python unit tests use this constructor
    #[new]
    fn new(
        r#type: Py<PyTokenType>,
        string: Py<PyString>,
        start_pos: PyObject,
        end_pos: PyObject,
        whitespace_before: Py<WhitespaceState>,
        whitespace_after: Py<WhitespaceState>,
        relative_indent: PyObject,
    ) -> Self {
        Self {
            r#type,
            string,
            start_pos,
            end_pos,
            whitespace_before,
            whitespace_after,
            relative_indent,
        }
    }
}

#[pyproto]
impl PyObjectProtocol for Token {
    fn __repr__(&self) -> PyResult<String> {
        Python::with_gil(|py| {
            Ok(format!(
                concat!(
                    "Token({}, {}, start_pos={}, end_pos={}, ",
                    "whitespace_before=..., whitespace_after=..., ",
                    "relative_indent={})",
                ),
                self.r#type.to_object(py).as_ref(py).repr()?.to_str()?,
                self.string.to_object(py).as_ref(py).repr()?.to_str()?,
                self.start_pos.as_ref(py).repr()?.to_str()?,
                self.end_pos.as_ref(py).repr()?.to_str()?,
                self.relative_indent.as_ref(py).repr()?.to_str()?,
            ))
        })
    }

    // The python unit tests need to be able to compare WhitespaceState
    fn __richcmp__(&self, other: PyRef<Self>, op: CompareOp) -> PyResult<PyObject> {
        let gil = Python::acquire_gil();
        let py = gil.python();
        let eq = || -> PyResult<bool> {
            Ok(self
                .r#type
                .call_method1(py, "__eq__", (&(*other).r#type,))?
                .is_true(py)?
                || self
                    .string
                    .call_method1(py, "__eq__", (&(*other).string,))?
                    .is_true(py)?
                || self
                    .start_pos
                    .call_method1(py, "__eq__", (&(*other).start_pos,))?
                    .is_true(py)?
                || self
                    .end_pos
                    .call_method1(py, "__eq__", (&(*other).end_pos,))?
                    .is_true(py)?
                || self
                    .whitespace_before
                    .call_method1(py, "__eq__", (&(*other).whitespace_before,))?
                    .is_true(py)?
                || self
                    .whitespace_after
                    .call_method1(py, "__eq__", (&(*other).whitespace_after,))?
                    .is_true(py)?
                || self
                    .relative_indent
                    .call_method1(py, "__eq__", (&(*other).relative_indent,))?
                    .is_true(py)?)
        };
        match op {
            CompareOp::Eq => Ok(eq()?.into_py(py)),
            CompareOp::Ne => Ok((!eq()?).into_py(py)),
            _ => Ok(Python::NotImplemented(py)),
        }
    }
}

pub struct TokenIterator<'t> {
    previous_whitespace_state: Option<Py<WhitespaceState>>,
    core_state: TokState<'t>,
    absolute_indents: Vec<&'t str>,
}

impl<'t> TokenIterator<'t> {
    pub fn new(text: &'t str, config: &TokConfig) -> Self {
        Self {
            previous_whitespace_state: None,
            core_state: TokState::new(text, config),
            absolute_indents: Vec::new(),
        }
    }

    pub fn py_next(&mut self, py: Python) -> PyResult<Option<Token>> {
        let tok_type = if let Some(tok_result) = self.core_state.next() {
            // TODO: return the correct exception type
            tok_result.map_err(|err| PyException::new_err(format!("{}", err)))?
        } else {
            return Ok(None);
        };

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
                            return Err(PyException::new_err("Mismatched indentation"));
                        }
                    } else {
                        // there's no previous indent, absolute_indent is relative_indent
                        absolute_indent
                    };
                self.absolute_indents.push(absolute_indent);
                // HACKY: mutate and fixup the previous whitespace state
                if let Some(pws_pyobject) = &self.previous_whitespace_state {
                    let mut pws_ref = pws_pyobject.try_borrow_mut(py)?;
                    pws_ref.absolute_indent = absolute_indent.to_string();
                }
                Some(relative_indent)
            }
            TokType::Dedent => {
                self.absolute_indents.pop();
                // HACKY: mutate and fixup the previous whitespace state
                if let Some(pws_pyobject) = &self.previous_whitespace_state {
                    let mut pws_ref = pws_pyobject.try_borrow_mut(py)?;
                    pws_ref.absolute_indent =
                        self.absolute_indents.last().unwrap_or(&"").to_string();
                }
                None
            }
            _ => None,
        };

        let whitespace_before = self
            .previous_whitespace_state
            .as_ref()
            .map(|pws| pws.clone_ref(py))
            .unwrap_or(Py::new(
                py,
                WhitespaceState {
                    line: 1,
                    column: 0,
                    absolute_indent: "".to_string(),
                    is_parenthesized: false,
                },
            )?);

        let whitespace_after =
            if let TokType::Indent | TokType::Dedent | TokType::EndMarker = tok_type {
                // Don't update whitespace state for these dummy tokens. This makes it possible to
                // partially parse whitespace for IndentedBlock footers, and then parse the rest of
                // the whitespace in the following statement's leading_lines. Unfortunately, that
                // means that the indentation is either wrong for the footer comments, or for the
                // next line. We've chosen to allow it to be wrong for the IndentedBlock footer and
                // manually override the state when parsing whitespace in that particular node.
                whitespace_before.clone_ref(py)
            } else {
                Py::new(
                    py,
                    WhitespaceState {
                        line: self.core_state.text_pos.line_number(),
                        column: self.core_state.text_pos.char_column_number(),
                        absolute_indent: self.absolute_indents.last().unwrap_or(&"").to_string(),
                        is_parenthesized: self.core_state.is_parenthesized(),
                    },
                )?
            };

        // Hold onto whitespace_after, so we can use it as whitespace_before in the next node.
        self.previous_whitespace_state = Some(whitespace_after.clone_ref(py));

        Ok(Some(Token {
            r#type: PythonTokenTypes::get(py)?
                .from_toktype(tok_type)
                .clone_ref(py),
            string: PyString::new(
                py,
                self.core_state
                    .text_pos
                    .slice_from_start_pos(&self.core_state.start_pos),
            )
            .into_py(py),
            start_pos: (
                self.core_state.start_pos.line_number(),
                self.core_state.start_pos.char_column_number(),
            )
                .to_object(py),
            end_pos: (
                self.core_state.text_pos.line_number(),
                self.core_state.text_pos.char_column_number(),
            )
                .to_object(py),
            whitespace_before,
            whitespace_after,
            relative_indent: relative_indent.into_py(py),
        }))
    }
}

impl Iterator for TokenIterator<'_> {
    type Item = PyResult<Token>;

    fn next(&mut self) -> Option<PyResult<Token>> {
        Python::with_gil(|py| self.py_next(py).transpose())
    }
}
