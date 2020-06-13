# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from typing import Optional

import libcst as cst
from libcst.metadata import BatchableMetadataProvider, MetadataWrapper
from libcst.testing.utils import UnitTest


class MetadataWrapperTest(UnitTest):
    def test_copies_tree(self) -> None:
        m = cst.parse_module("pass")
        mw = MetadataWrapper(m)
        self.assertTrue(mw.module.deep_equals(m))
        self.assertIsNot(mw.module, m)

    def test_unsafe_skip_copy(self) -> None:
        m = cst.parse_module("pass")
        mw = MetadataWrapper(m, unsafe_skip_copy=True)
        self.assertIs(mw.module, m)

    def test_equality_by_identity(self) -> None:
        m = cst.parse_module("pass")
        mw1 = MetadataWrapper(m)
        mw2 = MetadataWrapper(m)
        self.assertEqual(mw1, mw1)
        self.assertEqual(mw2, mw2)
        self.assertNotEqual(mw1, mw2)

    def test_hash_by_identity(self) -> None:
        m = cst.parse_module("pass")
        mw1 = MetadataWrapper(m)
        mw2 = MetadataWrapper(m, unsafe_skip_copy=True)
        mw3 = MetadataWrapper(m, unsafe_skip_copy=True)
        self.assertEqual(hash(mw1), hash(mw1))
        self.assertEqual(hash(mw2), hash(mw2))
        self.assertEqual(hash(mw3), hash(mw3))
        self.assertNotEqual(hash(mw1), hash(mw2))
        self.assertNotEqual(hash(mw1), hash(mw3))
        self.assertNotEqual(hash(mw2), hash(mw3))

    def test_metadata_cache(self) -> None:
        class DummyMetadataProvider(BatchableMetadataProvider[None]):
            gen_cache = tuple

        m = cst.parse_module("pass")
        mw = MetadataWrapper(m)
        with self.assertRaisesRegex(
            Exception, "Cache is required for initializing DummyMetadataProvider."
        ):
            mw.resolve(DummyMetadataProvider)

        class SimpleCacheMetadataProvider(BatchableMetadataProvider[object]):
            gen_cache = tuple

            def __init__(self, cache: object) -> None:
                super().__init__(cache)
                self.cache = cache

            def visit_Pass(self, node: cst.Pass) -> Optional[bool]:
                self.set_metadata(node, self.cache)

        cached_data = object()
        # pyre-fixme[6]: Expected `Mapping[Type[BaseMetadataProvider[object]],
        #  object]` for 2nd param but got `Dict[Type[SimpleCacheMetadataProvider],
        #  object]`.
        mw = MetadataWrapper(m, cache={SimpleCacheMetadataProvider: cached_data})
        pass_node = cst.ensure_type(mw.module.body[0], cst.SimpleStatementLine).body[0]
        self.assertEqual(
            mw.resolve(SimpleCacheMetadataProvider)[pass_node], cached_data
        )
