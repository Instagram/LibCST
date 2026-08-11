# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from libcst._position import _CodePositionT, CodePosition, CodeRange
from libcst.testing.utils import data_provider, UnitTest


code_range = CodeRange((1, 2), (4, 5))


class PositionTest(UnitTest):
    @data_provider(
        [
            (CodePosition(1, 2), True),
            ((1, 2), True),
            (CodePosition(3, 3), True),
            ((3, 3), True),
            (CodePosition(2, 1), False),
            ((2, 1), True),
            (CodePosition(5, 4), False),
            ((5, 4), False),
        ]
    )
    def test_code_range_contains(
        self, position: _CodePositionT, expected: bool
    ) -> None:
        self.assertEqual(position in code_range, expected)
