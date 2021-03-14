// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

/// Provides python bindings to the tokenizer.
use ouroboros::self_referencing;
use pyo3::prelude::*;
use pyo3::{wrap_pyfunction, PyIterProtocol};

use crate::tokenize::core::TokConfig;
use crate::tokenize::wrapped::{Token, TokenIterator as TokenIteratorInner};

#[pyclass(module = "libcst_native.tokenize")]
pub struct TokenIterator {
    owned: TokenIteratorInnerOwned,
}

#[self_referencing]
pub struct TokenIteratorInnerOwned {
    text: String,
    #[borrows(text)]
    #[covariant]
    inner: TokenIteratorInner<'this>,
}

#[pyproto]
impl PyIterProtocol for TokenIterator {
    fn __iter__(slf: PyRef<Self>) -> PyRef<Self> {
        slf
    }

    fn __next__(mut slf: PyRefMut<Self>) -> PyResult<Option<Token>> {
        slf.owned
            .with_inner_mut(|inner| Python::with_gil(|py| inner.py_next(py)))
    }
}

#[pyfunction]
pub fn tokenize(text: String) -> TokenIterator {
    TokenIterator {
        owned: TokenIteratorInnerOwnedBuilder {
            text,
            inner_builder: |text| {
                TokenIteratorInner::new(
                    text,
                    &TokConfig {
                        async_hacks: false,
                        split_fstring: true,
                    },
                )
            },
        }
        .build(),
    }
}

pub fn init_module(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Token>()?;
    m.add_function(wrap_pyfunction!(tokenize, m)?)?;
    Ok(())
}
