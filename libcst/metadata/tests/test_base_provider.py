# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import cast

import libcst as cst
from libcst import parse_module
from libcst.metadata import (
    BatchableMetadataProvider,
    MetadataWrapper,
    VisitorMetadataProvider,
)
from libcst.metadata.wrapper import _gen_batchable
from libcst.testing.utils import UnitTest


class BaseMetadataProviderTest(UnitTest):
    def test_visitor_provider(self) -> None:
        class SimpleProvider(VisitorMetadataProvider[int]):
            """
            Sets metadata on every node to 1.
            """

            def on_visit(self, node: cst.CSTNode) -> bool:
                self.set_metadata(node, 1)
                return True

        wrapper = MetadataWrapper(parse_module("pass; return"))
        module = wrapper.module
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]
        return_ = cast(cst.SimpleStatementLine, module.body[0]).body[1]

        provider = SimpleProvider()
        metadata = provider._gen(wrapper)

        # Check access on provider
        self.assertEqual(provider.get_metadata(SimpleProvider, module), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, pass_), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, return_), 1)

        # Check returned mapping
        self.assertEqual(metadata[module], 1)
        self.assertEqual(metadata[pass_], 1)
        self.assertEqual(metadata[return_], 1)

    def test_batchable_provider(self) -> None:
        class SimpleProvider(BatchableMetadataProvider[int]):
            """
            Sets metadata on every pass node to 1 and every return node to 2.
            """

            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, 1)

            def visit_Return(self, node: cst.Return) -> None:
                self.set_metadata(node, 2)

        wrapper = MetadataWrapper(parse_module("pass; return; pass"))
        module = wrapper.module
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]
        return_ = cast(cst.SimpleStatementLine, module.body[0]).body[1]
        pass_2 = cast(cst.SimpleStatementLine, module.body[0]).body[2]

        provider = SimpleProvider()
        metadata = _gen_batchable(wrapper, [provider])

        # Check access on provider
        self.assertEqual(provider.get_metadata(SimpleProvider, pass_), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, return_), 2)
        self.assertEqual(provider.get_metadata(SimpleProvider, pass_2), 1)

        # Check returned mapping
        self.assertEqual(metadata[SimpleProvider][pass_], 1)
        self.assertEqual(metadata[SimpleProvider][return_], 2)
        self.assertEqual(metadata[SimpleProvider][pass_2], 1)
