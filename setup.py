# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from os import path

# pyre-ignore Pyre doesn't know about setuptools.
import setuptools


this_directory = path.abspath(path.dirname(__file__))
with open(path.join(this_directory, "README.rst"), encoding="utf-8") as f:
    long_description = f.read()

setuptools.setup(
    name="libcst",
    description="A concrete syntax tree with AST-like properties for Python 3.7 programs.",
    long_description=long_description,
    long_description_content_type="text/x-rst",
    version="0.1",
    url="https://github.com/Instagram/LibCST",
    license="MIT",
    packages=setuptools.find_packages(),
    test_suite="libcst",
    python_requires=">=3.6",
    install_requires=[
        "dataclasses; python_version < '3.7'",
        "parso",
        "typing_extensions",
        "typing_inspect",
    ],
    extras_require={
        "dev": [
            "black",
            "isort",
            "flake8",
            "jupyter",
            "nbsphinx",
            "pyre-check",
            "Sphinx",
            "sphinx-rtd-theme",
        ]
    },
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Topic :: Software Development :: Libraries",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
    ],
)
