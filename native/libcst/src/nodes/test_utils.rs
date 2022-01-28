// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use pyo3::prelude::*;

py_import!("libcst._nodes.deep_equals", "deep_equals", get_deep_equals);

pub fn repr_or_panic<T>(py: Python, value: T) -> String
where
    T: ToPyObject,
{
    value
        .to_object(py)
        .as_ref(py)
        .repr()
        .expect("failed to call repr")
        .extract()
        .expect("repr should've returned str")
}

pub fn py_assert_deep_equals<L, R>(py: Python, left: L, right: R)
where
    L: ToPyObject,
    R: ToPyObject,
{
    let (left, right) = (left.to_object(py), right.to_object(py));
    let equals = get_deep_equals(py)
        .expect("failed to import deep_equals")
        .call1((&left, &right))
        .expect("failed to call deep_equals")
        .extract::<bool>()
        .expect("deep_equals should return a bool");
    if !equals {
        panic!(
            "assertion failed: {} was not deeply equal to {}",
            repr_or_panic(py, &left),
            repr_or_panic(py, &right),
        );
    }
}
