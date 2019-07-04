# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from libcst.metadata.base_provider import BaseMetadataProvider
from libcst.nodes._base import CSTNode
from libcst.parser import parse_module
from libcst.testing.utils import UnitTest


class BaseMetadataProviderTest(UnitTest):
    def test_simple_provider(self) -> None:
        class SimpleProvider(BaseMetadataProvider[int]):
            def on_visit(self, node: CSTNode) -> bool:
                self.set_metadata(node, 1)
                return True

        module = parse_module("pass")
        pass_node = module.body[0]
        provider = SimpleProvider()
        provider.run(module)

        self.assertEqual(provider.get_metadata(SimpleProvider, module), 1)
        self.assertEqual(provider.get_metadata(SimpleProvider, pass_node), 1)
