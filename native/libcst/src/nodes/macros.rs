// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

/// Generates a function that lazily imports and caches a module's member. This will hold a
/// permanent reference to the imported member. Python's module cache is rarely purged though, so
/// it typically won't matter.
///
/// This cache is cheaper than looking up the module in python's module cache inspecting the
/// module's `__dict__` each time you want access to the member.
///
/// If you have multiple imports from the same module, we'll call `py.import` once for each member
/// of the module.
#[macro_export]
macro_rules! py_import {
    ( $module_name:expr, $member_name:expr, $getter_fn:ident ) => {
        paste::paste! {
            static [<IMPORT_CELL_ $getter_fn:snake:upper>]
                : pyo3::once_cell::GILOnceCell<pyo3::PyResult<pyo3::PyObject>>
                = pyo3::once_cell::GILOnceCell::new();

            fn $getter_fn<'py>(py: pyo3::Python<'py>) -> pyo3::PyResult<&'py pyo3::PyAny> {
                Ok([<IMPORT_CELL_ $getter_fn:snake:upper>].get_or_init(py, || {
                        Ok(py.import($module_name)?.get($member_name)?.to_object(py))
                    })
                    .as_ref()
                    .map_err(|err| err.clone_ref(py))?
                    .as_ref(py))
            }
        }
    };
}
