// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

#[macro_use]
mod macros;

mod debug_utils;
pub mod parser;
mod parser_config;
mod py_cached;
mod token_type;
mod tokenize;
mod whitespace_parser;
mod whitespace_state;

#[cfg(any(test, doc))]
mod test_utils;

use pyo3::prelude::*;

#[pymodule]
fn libcst_native(py: Python, m: &PyModule) -> PyResult<()> {
    let parser_config_mod = PyModule::new(py, "parser_config")?;
    parser_config::init_module(py, parser_config_mod)?;
    m.add_submodule(parser_config_mod)?;

    let token_type_mod = PyModule::new(py, "token_type")?;
    token_type::init_module(py, token_type_mod)?;
    m.add_submodule(token_type_mod)?;

    let tokenize_mod = PyModule::new(py, "tokenize")?;
    tokenize::init_module(py, tokenize_mod)?;
    m.add_submodule(tokenize_mod)?;

    let whitespace_parser_mod = PyModule::new(py, "whitespace_parser")?;
    whitespace_parser::init_module(py, whitespace_parser_mod)?;
    m.add_submodule(whitespace_parser_mod)?;

    let whitespace_state_mod = PyModule::new(py, "whitespace_state")?;
    whitespace_state::init_module(py, whitespace_state_mod)?;
    m.add_submodule(whitespace_state_mod)?;

    Ok(())
}
