# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import cast

import libcst.nodes as cst
from libcst.batched_visitor import visit
from libcst.metadata.base_provider import (
    BatchableMetadataProvider,
    VisitorMetadataProvider,
)
from libcst.parser import parse_module
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

        module = parse_module("pass; return")
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]
        return_ = cast(cst.SimpleStatementLine, module.body[0]).body[1]

        provider = SimpleProvider()
        provider._run(module)

        self.assertEqual(provider.get_metadata(SimpleProvider, module), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, pass_), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, return_), 1)

    def test_batchable_provider(self) -> None:
        class SimpleProvider(BatchableMetadataProvider[int]):
            """
            Sets metadata on every pass node to 1 and every return node to 2.
            """

            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, 1)

            def leave_Return(self, node: cst.Return) -> None:
                self.set_metadata(node, 2)

        module = parse_module("pass; return; pass")
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]
        return_ = cast(cst.SimpleStatementLine, module.body[0]).body[1]
        pass_2 = cast(cst.SimpleStatementLine, module.body[0]).body[2]

        provider = SimpleProvider()
        visit(module, [provider])

        self.assertEqual(provider.get_metadata(SimpleProvider, pass_), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, return_), 2)
        self.assertEqual(provider.get_metadata(SimpleProvider, pass_2), 1)
