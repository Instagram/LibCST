# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from unittest import TestCase


class TestImport(TestCase):
    def test_import_libcst(self) -> None:
        import libcst  # noqa: F401
