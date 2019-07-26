# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable

from libcst._parser._types.config import PartialParserConfig
from libcst.testing.utils import UnitTest, data_provider


class TestConfig(UnitTest):
    @data_provider(
        {
            "empty": (lambda: PartialParserConfig(),),
            "python_version_a": (lambda: PartialParserConfig(python_version="3"),),
            "python_version_b": (lambda: PartialParserConfig(python_version="3.2"),),
            "python_version_c": (lambda: PartialParserConfig(python_version="3.2.1"),),
            "encoding": (lambda: PartialParserConfig(encoding="latin-1"),),
            "default_indent": (lambda: PartialParserConfig(default_indent="\t    "),),
            "default_newline": (lambda: PartialParserConfig(default_newline="\r\n"),),
        }
    )
    def test_valid_partial_parser_config(
        self, factory: Callable[[], PartialParserConfig]
    ) -> None:
        self.assertIsInstance(factory(), PartialParserConfig)

    @data_provider(
        {
            "python_version": (lambda: PartialParserConfig(python_version="3.2.1.0"),),
            "encoding": (lambda: PartialParserConfig(encoding="utf-42"),),
            "default_indent": (lambda: PartialParserConfig(default_indent="badinput"),),
            "default_newline": (lambda: PartialParserConfig(default_newline="\n\r"),),
        }
    )
    def test_invalid_partial_parser_config(
        self, factory: Callable[[], PartialParserConfig]
    ) -> None:
        with self.assertRaises(ValueError):
            factory()
