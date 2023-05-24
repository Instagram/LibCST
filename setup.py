# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from os import environ

import setuptools
from setuptools_rust import Binding, RustExtension


def no_local_scheme(version: str) -> str:
    return ""


setuptools.setup(
    setup_requires=["setuptools-rust", "setuptools_scm"],
    use_scm_version={
        "write_to": "libcst/_version.py",
        **(
            {"local_scheme": no_local_scheme}
            if "LIBCST_NO_LOCAL_SCHEME" in environ
            else {}
        ),
    },
    packages=setuptools.find_packages(),
    package_data={
        "libcst": ["py.typed"],
        "libcst.tests.pyre": ["*"],
        "libcst.codemod.tests": ["*"],
    },
    test_suite="libcst",
    rust_extensions=[
        RustExtension(
            "libcst.native",
            path="native/libcst/Cargo.toml",
            binding=Binding.PyO3,
        )
    ],
    zip_safe=False,  # for mypy compatibility https://mypy.readthedocs.io/en/latest/installed_packages.html
)
