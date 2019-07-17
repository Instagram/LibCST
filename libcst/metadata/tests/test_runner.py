# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import cast

import libcst.nodes as cst
from libcst.exceptions import MetadataException
from libcst.metadata.base_provider import (
    BatchableMetadataProvider,
    VisitorMetadataProvider,
)
from libcst.parser import parse_module
from libcst.testing.utils import UnitTest
from libcst.visitors import CSTTransformer


class MetadataRunnerTest(UnitTest):
    def test_visitor_provider(self) -> None:
        """
        Sets 2 metadata entries for every node:
            SimpleProvider -> 1
            DependentProvider - > 2
        """

        class SimpleProvider(VisitorMetadataProvider[int]):
            def on_visit(self, node: cst.CSTNode) -> bool:
                self.set_metadata(node, 1)
                return True

        class DependentProvider(VisitorMetadataProvider[int]):
            METADATA_DEPENDENCIES = (SimpleProvider,)

            def on_visit(self, node: cst.CSTNode) -> bool:
                self.set_metadata(node, self.get_metadata(SimpleProvider, node) + 1)
                return True

        class DependentVisitor(CSTTransformer):
            # Declare both providers so the visitor has acesss to both types of metadata
            METADATA_DEPENDENCIES = (DependentProvider, SimpleProvider)

        module = parse_module("pass")
        stmt = module.body[0]
        visitor = DependentVisitor()
        module.visit(visitor)

        self.assertEqual(visitor.get_metadata(SimpleProvider, module), 1)
        self.assertEqual(visitor.get_metadata(DependentProvider, module), 2)
        self.assertEqual(visitor.get_metadata(SimpleProvider, stmt), 1)
        self.assertEqual(visitor.get_metadata(DependentProvider, stmt), 2)

    def test_batched_provider(self) -> None:
        """
        Sets metadata on:
            - pass: BatchedProviderA -> 1
            - return: BatchedProviderB -> "a"

        """

        class BatchedProviderA(BatchableMetadataProvider[int]):
            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, 1)

        class BatchedProviderB(BatchableMetadataProvider[str]):
            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, "a")

        class DependentVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (BatchedProviderA, BatchedProviderB)

        module = parse_module("pass")
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]
        visitor = DependentVisitor()
        module.visit(visitor)

        self.assertEqual(visitor.get_metadata(BatchedProviderA, pass_), 1)
        self.assertEqual(visitor.get_metadata(BatchedProviderB, pass_), "a")

    def test_mixed_providers(self) -> None:
        """
        Sets metadata on pass:
            BatchedProviderA -> 2
            BatchedProviderB -> 3
            DependentProvider -> 5
            DependentBatched -> 4
        """

        class SimpleProvider(VisitorMetadataProvider[int]):
            def on_visit(self, node: cst.CSTNode) -> bool:
                self.set_metadata(node, 1)
                return True

        class BatchedProviderA(BatchableMetadataProvider[int]):
            METADATA_DEPENDENCIES = (SimpleProvider,)

            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, 2)

        class BatchedProviderB(BatchableMetadataProvider[int]):
            METADATA_DEPENDENCIES = (SimpleProvider,)

            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, 3)

        class DependentProvider(VisitorMetadataProvider[int]):
            METADATA_DEPENDENCIES = (BatchedProviderA, BatchedProviderB)

            def on_visit(self, node: cst.CSTNode) -> bool:
                sum = self.get_metadata(BatchedProviderA, node, 0) + self.get_metadata(
                    BatchedProviderB, node, 0
                )
                self.set_metadata(node, sum)
                return True

        class BatchedProviderC(BatchableMetadataProvider[int]):
            METADATA_DEPENDENCIES = (BatchedProviderA,)

            def visit_Pass(self, node: cst.Pass) -> None:
                self.set_metadata(node, self.get_metadata(BatchedProviderA, node) * 2)

        class DependentVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (
                BatchedProviderA,
                BatchedProviderB,
                BatchedProviderC,
                DependentProvider,
            )

        module = parse_module("pass")
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]
        visitor = DependentVisitor()
        module.visit(visitor)

        self.assertEqual(visitor.get_metadata(BatchedProviderA, pass_), 2)
        self.assertEqual(visitor.get_metadata(BatchedProviderB, pass_), 3)
        self.assertEqual(visitor.get_metadata(BatchedProviderC, pass_), 4)
        self.assertEqual(visitor.get_metadata(DependentProvider, pass_), 5)

        # Dependent visitor set metadata on all nodes but for module it
        # defaulted to 0 because BatchedProviderA/B only set metadata on
        # pass
        self.assertEqual(visitor.get_metadata(DependentProvider, module), 0)

    def test_circular_dependency(self) -> None:
        class ProviderA(VisitorMetadataProvider[str]):
            pass

        ProviderA.METADATA_DEPENDENCIES = (ProviderA,)

        class BadVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (ProviderA,)

        with self.assertRaisesRegex(
            MetadataException, "Detected circular dependencies in ProviderA"
        ):
            cst.Module([]).visit(BadVisitor())

    def test_self_metadata(self) -> None:
        test_runner = self

        class ProviderA(VisitorMetadataProvider[bool]):
            def on_visit(self, node: cst.CSTNode) -> bool:
                self.set_metadata(node, True)
                return True

            def on_leave(self, original_node: cst.CSTNode) -> None:
                test_runner.assertEqual(
                    self.get_metadata(type(self), original_node), True
                )

        class AVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (ProviderA,)

        cst.Module([]).visit(AVisitor())

    def test_unset_metadata(self) -> None:
        class ProviderA(VisitorMetadataProvider[bool]):
            pass

        class AVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (ProviderA,)

            def on_visit(self, node: cst.CSTNode) -> bool:
                self.get_metadata(ProviderA, node)
                return True

        with self.assertRaises(KeyError):
            cst.Module([]).visit(AVisitor())

    def test_invalid_metadata(self) -> None:
        class ProviderA(VisitorMetadataProvider[bool]):
            pass

        class ProviderB(VisitorMetadataProvider[bool]):
            pass

        class AVisitor(CSTTransformer):
            METADATA_DEPENDENCIES = (ProviderA,)

            def on_visit(self, node: cst.CSTNode) -> bool:
                self.get_metadata(ProviderA, node, True)
                self.get_metadata(ProviderB, node)
                return True

        with self.assertRaisesRegex(
            KeyError, "ProviderB is not declared as a dependency from AVisitor"
        ):
            cst.Module([]).visit(AVisitor())
