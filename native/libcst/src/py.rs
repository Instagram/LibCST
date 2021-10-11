use pyo3::{exceptions::PyValueError, prelude::*};

#[derive(FromPyObject)]
enum StringOrBytes {
    #[pyo3(transparent, annotation = "str")]
    String(String),
    #[pyo3(transparent, annotation = "bytes")]
    Bytes(Vec<u8>),
}

#[pymodule]
#[pyo3(name = "native")]
pub fn libcst_native(_py: Python, m: &PyModule) -> PyResult<()> {
    #[pyfn(m)]
    fn parse_module(source: StringOrBytes) -> PyResult<PyObject> {
        match source {
            StringOrBytes::String(str) => {
                let m = crate::parse_module(str.as_str())?;
                Python::with_gil(|py| Ok(m.into_py(py)))
            }
            StringOrBytes::Bytes(_) => Err(PyValueError::new_err("bytes not allowed yet")),
        }
    }

    #[pyfn(m)]
    fn parse_expression(source: String) -> PyResult<PyObject> {
        let expr = crate::parse_expression(source.as_str())?;
        Python::with_gil(|py| Ok(expr.into_py(py)))
    }

    #[pyfn(m)]
    fn parse_statement(source: String) -> PyResult<PyObject> {
        let stm = crate::parse_statement(source.as_str())?;
        Python::with_gil(|py| Ok(stm.into_py(py)))
    }

    Ok(())
}
