// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use crate::nodes::traits::py::TryIntoPy;
use pyo3::prelude::*;

#[pymodule]
#[pyo3(name = "native")]
pub fn libcst_native(_py: Python, m: &Bound<PyModule>) -> PyResult<()> {
    #[pyfn(m)]
    #[pyo3(signature = (source, encoding=None))]
    fn parse_module(source: String, encoding: Option<&str>) -> PyResult<PyObject> {
        let m = crate::parse_module(source.as_str(), encoding)?;
        Python::with_gil(|py| m.try_into_py(py))
    }

    #[pyfn(m)]
    fn parse_expression(source: String) -> PyResult<PyObject> {
        let expr = crate::parse_expression(source.as_str())?;
        Python::with_gil(|py| expr.try_into_py(py))
    }

    #[pyfn(m)]
    fn parse_statement(source: String) -> PyResult<PyObject> {
        let stm = crate::parse_statement(source.as_str())?;
        Python::with_gil(|py| stm.try_into_py(py))
    }

    Ok(())
}
