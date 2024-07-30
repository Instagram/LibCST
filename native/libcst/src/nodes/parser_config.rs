// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use pyo3::exceptions::PyIndexError;
use pyo3::prelude::*;
use pyo3::types::{IntoPyDict, PyDict, PySequence, PyString};
use pyo3::wrap_pyfunction;

use crate::py_cached::PyCached;

#[pyclass(subclass, module = "libcst_native.parser_config")]
#[text_signature = "(*, lines, default_newline)"]
pub struct BaseWhitespaceParserConfig {
    pub lines: PyCached<Vec<String>>,
    pub default_newline: PyCached<String>,
}

#[pymethods]
impl BaseWhitespaceParserConfig {
    #[new]
    fn new(lines: &PySequence, default_newline: &PyString) -> PyResult<Self> {
        // These fields will get initialized when ParserConfig.__init__ (our subclass) runs
        Ok(Self {
            lines: lines.extract()?,
            default_newline: default_newline.extract()?,
        })
    }

    #[getter]
    fn get_lines(&self, py: Python) -> PyObject {
        self.lines.to_object(py)
    }

    #[getter]
    fn get_default_newline(&self, py: Python) -> PyObject {
        self.default_newline.to_object(py)
    }
}

impl BaseWhitespaceParserConfig {
    /// Equivalent to `config.lines.unwrap()[line_number - 1]`, but it return a PyErr when we get
    /// an index that's out of range, instead of panicing.
    pub fn get_line(&self, line_number: usize) -> PyResult<&str> {
        let err_fn =
            || PyIndexError::new_err(format!("line number of {} is out of range", line_number));
        self.lines
            .get(line_number.checked_sub(1).ok_or_else(err_fn)?)
            .map(|l| &l[..])
            .ok_or_else(err_fn)
    }

    /// Equivalent to `config.get_line(line_number)[column_index..]`, but it return a PyErr when
    /// we get an column index that's out of range, instead of panicing.
    pub fn get_line_after_column(&self, line_number: usize, column_index: usize) -> PyResult<&str> {
        self.get_line(line_number)?
            .get(column_index..)
            .ok_or_else(|| {
                PyIndexError::new_err(format!("column index of {} is out of range", column_index))
            })
    }
}

// These fields are private and PyObject, since we don't currently care about using them from
// within rust.
#[pyclass(extends=BaseWhitespaceParserConfig, module="libcst_native.parser_config")]
#[text_signature = "(*, lines, encoding, default_indent, default_newline, has_trailing_newline, version, future_imports)"]
pub struct ParserConfig {
    // lines is inherited
    #[pyo3(get)]
    encoding: PyObject,
    #[pyo3(get)]
    default_indent: PyObject,
    // default_newline is inherited
    #[pyo3(get)]
    has_trailing_newline: PyObject,
    #[pyo3(get)]
    version: PyObject,
    #[pyo3(get)]
    future_imports: PyObject,
}

#[pymethods]
impl ParserConfig {
    #[new]
    fn new(
        lines: &PySequence,
        encoding: PyObject,
        default_indent: PyObject,
        default_newline: &PyString,
        has_trailing_newline: PyObject,
        version: PyObject,
        future_imports: PyObject,
    ) -> PyResult<(Self, BaseWhitespaceParserConfig)> {
        Ok((
            Self {
                encoding,
                default_indent,
                has_trailing_newline,
                version,
                future_imports,
            },
            BaseWhitespaceParserConfig::new(lines, default_newline)?,
        ))
    }
}

/// An internal helper function used by python unit tests to compare configs.
#[pyfunction]
fn parser_config_asdict<'py>(py: Python<'py>, config: PyRef<'py, ParserConfig>) -> &'py PyDict {
    let super_config: &BaseWhitespaceParserConfig = config.as_ref();
    vec![
        ("lines", super_config.lines.to_object(py)),
        ("encoding", config.encoding.clone_ref(py)),
        ("default_indent", config.default_indent.clone_ref(py)),
        (
            "default_newline",
            super_config.default_newline.to_object(py),
        ),
        (
            "has_trailing_newline",
            config.has_trailing_newline.clone_ref(py),
        ),
        ("version", config.version.clone_ref(py)),
        ("future_imports", config.future_imports.clone_ref(py)),
    ]
    .into_py_dict_bound(py)
}

pub fn init_module(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<BaseWhitespaceParserConfig>()?;
    m.add_class::<ParserConfig>()?;
    m.add_function(wrap_pyfunction!(parser_config_asdict, m)?)
        .unwrap();
    Ok(self)
}
