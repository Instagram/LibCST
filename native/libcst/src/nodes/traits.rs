// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use crate::{
    nodes::expression::{DeflatedLeftParen, DeflatedRightParen},
    nodes::op::DeflatedComma,
    tokenizer::whitespace_parser::{Config, WhitespaceError},
    Codegen, CodegenState, EmptyLine, LeftParen, RightParen,
};
use std::ops::Deref;

pub trait WithComma<'r, 'a> {
    fn with_comma(self, comma: DeflatedComma<'r, 'a>) -> Self;
}

pub trait ParenthesizedNode<'a> {
    fn lpar(&self) -> &Vec<LeftParen<'a>>;
    fn rpar(&self) -> &Vec<RightParen<'a>>;

    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        for lpar in self.lpar() {
            lpar.codegen(state);
        }
        f(state);
        for rpar in self.rpar() {
            rpar.codegen(state);
        }
    }

    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self;
}

impl<'a, T: ParenthesizedNode<'a>> ParenthesizedNode<'a> for Box<T> {
    fn lpar(&self) -> &Vec<LeftParen<'a>> {
        self.deref().lpar()
    }
    fn rpar(&self) -> &Vec<RightParen<'a>> {
        self.deref().rpar()
    }
    fn parenthesize<F>(&self, state: &mut CodegenState<'a>, f: F)
    where
        F: FnOnce(&mut CodegenState<'a>),
    {
        self.deref().parenthesize(state, f)
    }
    fn with_parens(self, left: LeftParen<'a>, right: RightParen<'a>) -> Self {
        Self::new((*self).with_parens(left, right))
    }
}

pub trait ParenthesizedDeflatedNode<'r, 'a> {
    fn lpar(&self) -> &Vec<DeflatedLeftParen<'r, 'a>>;
    fn rpar(&self) -> &Vec<DeflatedRightParen<'r, 'a>>;

    fn with_parens(
        self,
        left: DeflatedLeftParen<'r, 'a>,
        right: DeflatedRightParen<'r, 'a>,
    ) -> Self;
}
impl<'r, 'a, T: ParenthesizedDeflatedNode<'r, 'a>> ParenthesizedDeflatedNode<'r, 'a> for Box<T> {
    fn lpar(&self) -> &Vec<DeflatedLeftParen<'r, 'a>> {
        self.deref().lpar()
    }
    fn rpar(&self) -> &Vec<DeflatedRightParen<'r, 'a>> {
        self.deref().rpar()
    }
    fn with_parens(
        self,
        left: DeflatedLeftParen<'r, 'a>,
        right: DeflatedRightParen<'r, 'a>,
    ) -> Self {
        Self::new((*self).with_parens(left, right))
    }
}

pub trait WithLeadingLines<'a> {
    fn leading_lines(&mut self) -> &mut Vec<EmptyLine<'a>>;
}

pub type Result<T> = std::result::Result<T, WhitespaceError>;

pub trait Inflate<'a>
where
    Self: Sized,
{
    type Inflated;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated>;
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Option<T> {
    type Inflated = Option<T::Inflated>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        self.map(|x| x.inflate(config)).transpose()
    }
}

impl<'a, T: Inflate<'a> + ?Sized> Inflate<'a> for Box<T> {
    type Inflated = Box<T::Inflated>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        match (*self).inflate(config) {
            Ok(a) => Ok(Box::new(a)),
            Err(e) => Err(e),
        }
    }
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Vec<T> {
    type Inflated = Vec<T::Inflated>;
    fn inflate(self, config: &Config<'a>) -> Result<Self::Inflated> {
        self.into_iter().map(|item| item.inflate(config)).collect()
    }
}
#[cfg(feature = "py")]
pub mod py {
    use pyo3::{types::PyTuple, IntoPyObjectExt, PyObject, PyResult, Python};

    // TODO: replace with upstream implementation once
    // https://github.com/PyO3/pyo3/issues/1813 is resolved
    pub trait TryIntoPy<T>: Sized {
        fn try_into_py(self, py: Python) -> PyResult<T>;
    }

    // I wish:
    // impl<PyT, T: IntoPy<PyT>> TryIntoPy<PyT> for T {
    //     fn try_into_py(self, py: Python) -> PyResult<PyT> {
    //         Ok(self.into_py(py))
    //     }
    // }

    impl TryIntoPy<PyObject> for bool {
        fn try_into_py(self, py: Python) -> PyResult<PyObject> {
            self.into_py_any(py)
        }
    }

    impl<T: TryIntoPy<PyObject>> TryIntoPy<PyObject> for Box<T>
    where
        T: TryIntoPy<PyObject>,
    {
        fn try_into_py(self, py: Python) -> PyResult<PyObject> {
            (*self).try_into_py(py)
        }
    }

    impl<T> TryIntoPy<PyObject> for Option<T>
    where
        T: TryIntoPy<PyObject>,
    {
        fn try_into_py(self, py: Python) -> PyResult<PyObject> {
            Ok(match self {
                None => py.None(),
                Some(x) => x.try_into_py(py)?,
            })
        }
    }

    impl<T> TryIntoPy<PyObject> for Vec<T>
    where
        T: TryIntoPy<PyObject>,
    {
        fn try_into_py(self, py: Python) -> PyResult<PyObject> {
            let converted = self
                .into_iter()
                .map(|x| x.try_into_py(py))
                .collect::<PyResult<Vec<_>>>()?
                .into_iter();
            PyTuple::new(py, converted)?.into_py_any(py)
        }
    }

    impl<'a> TryIntoPy<PyObject> for &'a str {
        fn try_into_py(self, py: Python) -> PyResult<PyObject> {
            self.into_py_any(py)
        }
    }
}
