# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from os import environ, path

import setuptools
from setuptools_rust import Binding, RustExtension


# Grab the readme so that our package stays in sync with github.
this_directory: str = path.abspath(path.dirname(__file__))
with open(path.join(this_directory, "README.rst"), encoding="utf-8") as f:
    long_description: str = f.read()


def no_local_scheme(version: str) -> str:
    return ""


setuptools.setup(
    use_scm_version={
        "write_to": "libcst/_version.py",
        **(
            {"local_scheme": no_local_scheme}
            if "LIBCST_NO_LOCAL_SCHEME" in environ
            else {}
        ),
    },
    name="libcst",
    description="A concrete syntax tree with AST-like properties for Python 3.5, 3.6, 3.7, 3.8, 3.9, and 3.10 programs.",
    long_description=long_description,
    long_description_content_type="text/x-rst",
    url="https://github.com/Instagram/LibCST",
    project_urls={
        "Changelog": "https://github.com/Instagram/LibCST/blob/main/CHANGELOG.md",
        "Documentation": "https://libcst.readthedocs.io/en/latest/",
    },
    license="MIT",
    packages=setuptools.find_packages(),
    package_data={
        "libcst": ["py.typed"],
        "libcst.tests.pyre": ["*"],
        "libcst.codemod.tests": ["*"],
    },
    test_suite="libcst",
    python_requires=">=3.7",
    setup_requires=["setuptools_scm"],
    install_requires=[dep.strip() for dep in open("requirements.txt").readlines()],
    extras_require={
        "dev": [
            dep.strip()
            for dep in open("requirements-dev.txt").readlines()
            if "=" in dep
        ],
    },
    rust_extensions=[
        RustExtension(
            "libcst.native",
            path="native/libcst/Cargo.toml",
            binding=Binding.PyO3,
        )
    ],
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Topic :: Software Development :: Libraries",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
    ],
    zip_safe=False,  # for mypy compatibility https://mypy.readthedocs.io/en/latest/installed_packages.html
)
