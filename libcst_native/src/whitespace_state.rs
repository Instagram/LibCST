// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use pyo3::class::basic::CompareOp;
use pyo3::prelude::*;
use pyo3::PyObjectProtocol;

#[pyclass(module = "libcst_native.whitespace_state")]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WhitespaceState {
    #[pyo3(get, set)]
    pub line: usize, // one-indexed (to match parso's behavior)
    #[pyo3(get, set)]
    pub column: usize, // zero-indexed (to match parso's behavior)
    #[pyo3(get, set)]
    pub absolute_indent: String,
    #[pyo3(get, set)]
    pub is_parenthesized: bool,
}

impl Default for WhitespaceState {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            absolute_indent: "".to_string(),
            is_parenthesized: false,
        }
    }
}

#[pymethods]
impl WhitespaceState {
    #[new]
    fn new(line: usize, column: usize, absolute_indent: String, is_parenthesized: bool) -> Self {
        WhitespaceState {
            line,
            column,
            absolute_indent,
            is_parenthesized,
        }
    }
}

#[pyproto]
impl PyObjectProtocol for WhitespaceState {
    fn __repr__(&self) -> PyResult<String> {
        Python::with_gil(|py| {
            Ok(format!(
                "WhitespaceState({}, {}, {}, {})",
                self.line,
                self.column,
                self.absolute_indent
                    .to_object(py)
                    .as_ref(py)
                    .repr()?
                    .to_str()?,
                self.is_parenthesized
            ))
        })
    }

    // The python unit tests need to be able to compare WhitespaceState
    fn __richcmp__(&self, other: PyRef<Self>, op: CompareOp) -> PyResult<PyObject> {
        Python::with_gil(|py| {
            Ok(match op {
                CompareOp::Eq => (self == &*other).into_py(py),
                CompareOp::Ne => (self != &*other).into_py(py),
                _ => Python::NotImplemented(py),
            })
        })
    }
}

pub fn init_module(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<WhitespaceState>()?;
    Ok(())
}
