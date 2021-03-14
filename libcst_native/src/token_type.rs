// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use crate::tokenize::core::TokType as CoreTokType;
use pyo3::once_cell::GILOnceCell;
use pyo3::prelude::*;
use pyo3::PyObjectProtocol;

#[pyclass]
pub struct TokenType {
    #[pyo3(get)]
    pub name: &'static str,
    #[pyo3(get)]
    pub contains_syntax: bool,
}

#[pyproto]
impl PyObjectProtocol for TokenType {
    fn __str__(&self) -> &'static str {
        self.name
    }

    fn __repr__(&self) -> PyResult<String> {
        Python::with_gil(|py| {
            Ok(format!(
                "TokenType({}, contains_syntax={})",
                self.name.to_object(py).as_ref(py).repr()?.to_str()?,
                if self.contains_syntax {
                    "True"
                } else {
                    "False"
                }
            ))
        })
    }
}

impl TokenType {
    fn new(name: &'static str) -> Self {
        Self {
            name,
            contains_syntax: false,
        }
    }

    fn new_with_syntax(name: &'static str) -> Self {
        Self {
            name,
            contains_syntax: true,
        }
    }
}

static PYTHON_TOKEN_TYPES_CELL: GILOnceCell<PyResult<PythonTokenTypes>> = GILOnceCell::new();

// every field in this struct is a constant exported by this module
#[allow(non_snake_case)]
pub struct PythonTokenTypes {
    pub STRING: Py<TokenType>,
    pub NAME: Py<TokenType>,
    pub NUMBER: Py<TokenType>,
    pub OP: Py<TokenType>,
    pub NEWLINE: Py<TokenType>,
    pub INDENT: Py<TokenType>,
    pub DEDENT: Py<TokenType>,
    pub ASYNC: Py<TokenType>,
    pub AWAIT: Py<TokenType>,
    pub FSTRING_START: Py<TokenType>,
    pub FSTRING_STRING: Py<TokenType>,
    pub FSTRING_END: Py<TokenType>,
    pub ENDMARKER: Py<TokenType>,
    // unused dummy tokens for backwards compat with the parso tokenizer
    pub ERRORTOKEN: Py<TokenType>,
    pub ERROR_DEDENT: Py<TokenType>,
}

impl PythonTokenTypes {
    pub fn get(py: Python<'_>) -> PyResult<&'static Self> {
        // Use GILOnceCell to ensure that we only ever have one PyObject for each token type,
        // allowing us to compare token types by identity.
        match PYTHON_TOKEN_TYPES_CELL.get_or_init(py, || {
            Ok(Self {
                STRING: Py::new(py, TokenType::new("STRING"))?,
                NAME: Py::new(py, TokenType::new_with_syntax("NAME"))?,
                NUMBER: Py::new(py, TokenType::new("NUMBER"))?,
                OP: Py::new(py, TokenType::new_with_syntax("OP"))?,
                NEWLINE: Py::new(py, TokenType::new("NEWLINE"))?,
                INDENT: Py::new(py, TokenType::new("INDENT"))?,
                DEDENT: Py::new(py, TokenType::new("DEDENT"))?,
                ASYNC: Py::new(py, TokenType::new("ASYNC"))?,
                AWAIT: Py::new(py, TokenType::new("AWAIT"))?,
                FSTRING_START: Py::new(py, TokenType::new("FSTRING_START"))?,
                FSTRING_STRING: Py::new(py, TokenType::new("FSTRING_STRING"))?,
                FSTRING_END: Py::new(py, TokenType::new("FSTRING_END"))?,
                ENDMARKER: Py::new(py, TokenType::new("ENDMARKER"))?,
                // unused dummy tokens for backwards compat with the parso tokenizer
                ERRORTOKEN: Py::new(py, TokenType::new("ERRORTOKEN"))?,
                ERROR_DEDENT: Py::new(py, TokenType::new("ERROR_DEDENT"))?,
            })
        }) {
            Ok(tt) => Ok(&tt),
            Err(err) => Err(err.clone_ref(py)),
        }
    }

    pub fn from_toktype(&self, tok: CoreTokType) -> &Py<TokenType> {
        use CoreTokType as C;
        match tok {
            C::String => &self.STRING,
            C::Name => &self.NAME,
            C::Number => &self.NUMBER,
            C::Op => &self.OP,
            C::Newline => &self.NEWLINE,
            C::Indent => &self.INDENT,
            C::Dedent => &self.DEDENT,
            C::Async => &self.ASYNC,
            C::Await => &self.AWAIT,
            C::FStringStart => &self.FSTRING_START,
            C::FStringString => &self.FSTRING_STRING,
            C::FStringEnd => &self.FSTRING_END,
            C::EndMarker => &self.ENDMARKER,
        }
    }
}

pub fn init_module(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<TokenType>()?;
    let ptt = PythonTokenTypes::get(py)?;
    m.add("STRING", ptt.STRING.clone_ref(py))?;
    m.add("NAME", ptt.NAME.clone_ref(py))?;
    m.add("NUMBER", ptt.NUMBER.clone_ref(py))?;
    m.add("OP", ptt.OP.clone_ref(py))?;
    m.add("NEWLINE", ptt.NEWLINE.clone_ref(py))?;
    m.add("INDENT", ptt.INDENT.clone_ref(py))?;
    m.add("DEDENT", ptt.DEDENT.clone_ref(py))?;
    m.add("ASYNC", ptt.ASYNC.clone_ref(py))?;
    m.add("AWAIT", ptt.AWAIT.clone_ref(py))?;
    m.add("FSTRING_START", ptt.FSTRING_START.clone_ref(py))?;
    m.add("FSTRING_STRING", ptt.FSTRING_STRING.clone_ref(py))?;
    m.add("FSTRING_END", ptt.FSTRING_END.clone_ref(py))?;
    m.add("ENDMARKER", ptt.ENDMARKER.clone_ref(py))?;
    // unused dummy tokens for backwards compat with the parso tokenizer
    m.add("ERRORTOKEN", ptt.ERRORTOKEN.clone_ref(py))?;
    m.add("ERROR_DEDENT", ptt.ERROR_DEDENT.clone_ref(py))?;
    Ok(())
}
