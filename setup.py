# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from os import path

# pyre-ignore Pyre doesn't know about setuptools.
import setuptools


this_directory = path.abspath(path.dirname(__file__))
with open(path.join(this_directory, 'README.md'), encoding='utf-8') as f:
    long_description = f.read()

setuptools.setup(
    name="libcst",
    description="A concrete syntax tree with AST-like properties for Python 3.7.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    version="0.1.dev",
    packages=setuptools.find_packages(),
)
