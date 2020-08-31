# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import importlib.util
from os import path
from typing import TYPE_CHECKING

import setuptools


if TYPE_CHECKING:
    from importlib.machinery import ModuleSpec
    from types import ModuleType

# Grab the readme so that our package stays in sync with github.
this_directory: str = path.abspath(path.dirname(__file__))
with open(path.join(this_directory, "README.rst"), encoding="utf-8") as f:
    long_description = f.read()

# Grab the version constant so that libcst.tool stays in sync with this package.
spec: "ModuleSpec" = importlib.util.spec_from_file_location(
    "version", path.join(this_directory, "libcst/_version.py")
)
version: "ModuleType" = importlib.util.module_from_spec(spec)
# pyre-ignore Pyre doesn't know about importlib entirely.
spec.loader.exec_module(version)
# pyre-ignore Pyre has no way of knowing that this constant exists.
LIBCST_VERSION = version.LIBCST_VERSION

setuptools.setup(
    name="libcst",
    description="A concrete syntax tree with AST-like properties for Python 3.5, 3.6, 3.7 and 3.8 programs.",
    long_description=long_description,
    long_description_content_type="text/x-rst",
    version=LIBCST_VERSION,
    url="https://github.com/Instagram/LibCST",
    license="MIT",
    packages=setuptools.find_packages(),
    package_data={
        "libcst": ["py.typed"],
        "libcst.tests.pyre": ["*"],
        "libcst.codemod.tests": ["*"],
    },
    test_suite="libcst",
    python_requires=">=3.6",
    install_requires=[dep.strip() for dep in open("requirements.txt").readlines()],
    extras_require={
        "dev": [dep.strip() for dep in open("requirements-dev.txt").readlines() if "=" in dep],
    },
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Topic :: Software Development :: Libraries",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
    ],
    zip_safe=False,  # for mypy compatibility https://mypy.readthedocs.io/en/latest/installed_packages.html
)
