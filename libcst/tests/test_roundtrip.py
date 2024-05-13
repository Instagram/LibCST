# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from unittest import TestCase

from libcst import parse_module
from libcst._parser.entrypoints import is_native

fixtures: Path = Path(__file__).parent.parent.parent / "native/libcst/tests/fixtures"


class RoundTripTests(TestCase):
    def test_clean_roundtrip(self) -> None:
        if not is_native():
            self.skipTest("pure python parser doesn't work with this")
        self.assertTrue(fixtures.exists(), f"{fixtures} should exist")
        files = list(fixtures.iterdir())
        self.assertGreater(len(files), 0)
        for file in files:
            with self.subTest(file=str(file)):
                src = file.read_text(encoding="utf-8")
                mod = parse_module(src)
                self.maxDiff = None
                self.assertEqual(mod.code, src)
