// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

#[macro_use]
mod macros;

// Our submodules are all pub so that they're shown in `cargo doc`'s output.
// This crate isn't intended to be used from within rust, and won't be published to `crates.io`.
pub mod parser_config;
pub mod py_cached;
pub mod whitespace_parser;
pub mod whitespace_state;

#[cfg(any(test, doc))]
pub mod test_utils;

use pyo3::prelude::*;

#[pymodule]
fn libcst_native(py: Python, m: &PyModule) -> PyResult<()> {
    let parser_config_mod = PyModule::new(py, "parser_config")?;
    parser_config::init_module(py, parser_config_mod)?;
    m.add_submodule(parser_config_mod)?;

    let whitespace_state_mod = PyModule::new(py, "whitespace_state")?;
    whitespace_state::init_module(py, whitespace_state_mod)?;
    m.add_submodule(whitespace_state_mod)?;

    let whitespace_parser_mod = PyModule::new(py, "whitespace_parser")?;
    whitespace_parser::init_module(py, whitespace_parser_mod)?;
    m.add_submodule(whitespace_parser_mod)?;

    Ok(())
}
