// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use crate::nodes::traits::py::TryIntoPy;
use pyo3::prelude::*;

#[pymodule(gil_used = false)]
#[pyo3(name = "native")]
pub fn libcst_native(m: &Bound<PyModule>) -> PyResult<()> {
    #[pyfn(m)]
    #[pyo3(signature = (source, encoding=None))]
    fn parse_module(source: String, encoding: Option<&str>) -> PyResult<PyObject> {
        Python::with_gil(|py| {
            let m = py.allow_threads(|| crate::parse_module(source.as_str(), encoding))?;
            m.try_into_py(py)
        })
    }

    #[pyfn(m)]
    fn parse_expression(source: String) -> PyResult<PyObject> {
        Python::with_gil(|py| {
            let expr = py.allow_threads(|| crate::parse_expression(source.as_str()))?;
            expr.try_into_py(py)
        })
    }

    #[pyfn(m)]
    fn parse_statement(source: String) -> PyResult<PyObject> {
        Python::with_gil(|py| {
            let stm = py.allow_threads(|| crate::parse_statement(source.as_str()))?;
            stm.try_into_py(py)
        })
    }

    Ok(())
}
