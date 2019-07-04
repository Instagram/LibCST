# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Union

from libcst._base_visitor import CSTVisitor
from libcst._removal_sentinel import RemovalSentinel
from libcst.exceptions import MetadataException
from libcst.metadata.base_provider import BaseMetadataProvider
from libcst.nodes import CSTNode, Module
from libcst.parser import parse_module
from libcst.testing.utils import UnitTest


class MetadataRunnerTest(UnitTest):
    def test_visitor_with_dependencies(self) -> None:
        class SimpleProvider(BaseMetadataProvider[int]):
            def on_visit(self, node: CSTNode) -> bool:
                self.set_metadata(node, 1)
                return True

        class DependentProvider(BaseMetadataProvider[int]):
            METADATA_DEPENDENCIES = (SimpleProvider,)

            def on_visit(self, node: CSTNode) -> bool:
                self.set_metadata(node, self.get_metadata(SimpleProvider, node) + 1)
                return True

        class DependentVisitor(CSTVisitor):
            METADATA_DEPENDENCIES = (DependentProvider, SimpleProvider)

        module = parse_module("pass")
        pass_node = module.body[0]
        visitor = DependentVisitor()
        module.visit(visitor)

        self.assertEqual(visitor.get_metadata(SimpleProvider, module), 1)
        self.assertEqual(visitor.get_metadata(DependentProvider, module), 2)
        self.assertEqual(visitor.get_metadata(SimpleProvider, pass_node), 1)
        self.assertEqual(visitor.get_metadata(DependentProvider, pass_node), 2)

    def test_provider_with_circular_dependency(self) -> None:
        class ProviderA(BaseMetadataProvider[str]):
            pass

        ProviderA.METADATA_DEPENDENCIES = (ProviderA,)

        class BadVisitor(CSTVisitor):
            METADATA_DEPENDENCIES = (ProviderA,)

        with self.assertRaisesRegex(
            MetadataException,
            "Detected circular dependency between ProviderA and ProviderA",
        ):
            Module([]).visit(BadVisitor())

    def test_self_access_metadata(self) -> None:
        test_runner = self

        class ProviderA(BaseMetadataProvider[bool]):
            def on_visit(self, node: CSTNode) -> bool:
                self.set_metadata(node, True)
                return True

            def on_leave(
                self, original_node: CSTNode, updated_node: CSTNode
            ) -> Union[CSTNode, RemovalSentinel]:
                test_runner.assertEqual(
                    self.get_metadata(type(self), original_node), True
                )
                return original_node

        class AVisitor(CSTVisitor):
            METADATA_DEPENDENCIES = (ProviderA,)

        Module([]).visit(AVisitor())

    def test_access_unset_metadata(self) -> None:
        class ProviderA(BaseMetadataProvider[bool]):
            pass

        class AVisitor(CSTVisitor):
            METADATA_DEPENDENCIES = (ProviderA,)

            def on_visit(self, node: CSTNode) -> bool:
                self.get_metadata(ProviderA, node)
                return True

        with self.assertRaises(KeyError):
            Module([]).visit(AVisitor())

    def test_access_invalid_metadata(self) -> None:
        class ProviderA(BaseMetadataProvider[bool]):
            pass

        class ProviderB(BaseMetadataProvider[bool]):
            pass

        class AVisitor(CSTVisitor):
            METADATA_DEPENDENCIES = (ProviderA,)

            def on_visit(self, node: CSTNode) -> bool:
                self.get_metadata(ProviderA, node, True)
                self.get_metadata(ProviderB, node)
                return True

        with self.assertRaisesRegex(
            KeyError, "ProviderB is not declared as a dependency from AVisitor"
        ):
            Module([]).visit(AVisitor())
