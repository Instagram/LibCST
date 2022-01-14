# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import pickle

from libcst import parse_module
from libcst.testing.utils import UnitTest


class PickleTest(UnitTest):
    def test_load_and_dump(self) -> None:
        module = parse_module("5 + 3")
        self.assertTrue(module.deep_equals(pickle.loads(pickle.dumps(module))))
