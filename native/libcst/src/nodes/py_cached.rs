// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use pyo3::prelude::*;
use std::convert::AsRef;
use std::ops::Deref;

/// An immutable wrapper around a rust type T and it's PyObject equivalent. Caches the conversion
/// to and from the PyObject.
pub struct PyCached<T> {
    native: T,
    py_object: PyObject,
}

impl<T> PyCached<T>
where
    T: ToPyObject,
{
    pub fn new(py: Python, native: T) -> Self {
        Self {
            py_object: native.to_object(py),
            native,
        }
    }
}

impl<'source, T> FromPyObject<'source> for PyCached<T>
where
    T: FromPyObject<'source>,
{
    fn extract(ob: &'source PyAny) -> PyResult<Self> {
        Python::with_gil(|py| {
            Ok(PyCached {
                native: ob.extract()?,
                py_object: ob.to_object(py),
            })
        })
    }
}

impl<T> IntoPy<PyObject> for PyCached<T> {
    fn into_py(self, _py: Python) -> PyObject {
        self.py_object
    }
}

impl<T> ToPyObject for PyCached<T> {
    fn to_object(&self, py: Python) -> PyObject {
        self.py_object.clone_ref(py)
    }
}

impl<T> AsRef<T> for PyCached<T> {
    fn as_ref(&self) -> &T {
        &self.native
    }
}

impl<T> Deref for PyCached<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.native
    }
}

impl<T> From<T> for PyCached<T>
where
    T: ToPyObject,
{
    fn from(val: T) -> Self {
        Python::with_gil(|py| Self::new(py, val))
    }
}
