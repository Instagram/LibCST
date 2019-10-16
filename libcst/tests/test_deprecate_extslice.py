# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
from typing import Set

import libcst as cst
import libcst.matchers as m
from libcst.testing.utils import UnitTest


# TODO: Remove this once we completely remove ExtSlice.
class ExtSliceDeprecatedUseTest(UnitTest):
    """
    Test that various code which refers to "SubscriptElement" as "ExtSlice" continues
    to operate as expected.
    """

    def test_deprecated_construction(self) -> None:
        module = cst.Module(
            body=[
                cst.SimpleStatementLine(
                    body=[
                        cst.Expr(
                            value=cst.Subscript(
                                value=cst.Name(value="foo"),
                                slice=[
                                    cst.ExtSlice(
                                        slice=cst.Index(value=cst.Integer(value="1"))
                                    ),
                                    cst.ExtSlice(
                                        slice=cst.Index(value=cst.Integer(value="2"))
                                    ),
                                ],
                            )
                        )
                    ]
                )
            ]
        )

        self.assertEqual(module.code, "foo[1, 2]\n")

    def test_deprecated_non_element_construction(self) -> None:
        module = cst.Module(
            body=[
                cst.SimpleStatementLine(
                    body=[
                        cst.Expr(
                            value=cst.Subscript(
                                value=cst.Name(value="foo"),
                                slice=cst.Index(value=cst.Integer(value="1")),
                            )
                        )
                    ]
                )
            ]
        )

        self.assertEqual(module.code, "foo[1]\n")

    def test_deprecated_matching(self) -> None:
        class DeprecatedDecoratorTest(m.MatcherDecoratableVisitor):
            def __init__(self) -> None:
                super().__init__()
                self.calls: Set[str] = set()

            @m.visit(m.ExtSlice())
            def _deprecated_visitor(self, node: cst.ExtSlice) -> None:
                if m.matches(node, m.ExtSlice(m.Index(m.Integer("2")))):
                    self.calls.add("called")

        module = cst.parse_module("foo[1, 2]\n")
        visitor = DeprecatedDecoratorTest()
        module.visit(visitor)
        self.assertEqual(visitor.calls, {"called"})

    def test_deprecated_visiting(self) -> None:
        class DeprecatedDecoratorTest(m.MatcherDecoratableVisitor):
            def __init__(self) -> None:
                super().__init__()
                self.visits: Set[str] = set()
                self.leaves: Set[str] = set()

            def visit_ExtSlice(self, node: cst.ExtSlice) -> None:
                self.visits.add(
                    "node "
                    + cst.ensure_type(
                        cst.ensure_type(node.slice, cst.Index).value, cst.Integer
                    ).value
                )

            def visit_ExtSlice_slice(self, node: cst.ExtSlice) -> None:
                self.visits.add(
                    "attr "
                    + cst.ensure_type(
                        cst.ensure_type(node.slice, cst.Index).value, cst.Integer
                    ).value
                )

            def leave_ExtSlice(self, original_node: cst.ExtSlice) -> None:
                self.leaves.add(
                    "node "
                    + cst.ensure_type(
                        cst.ensure_type(original_node.slice, cst.Index).value,
                        cst.Integer,
                    ).value
                )

            def leave_ExtSlice_slice(self, original_node: cst.ExtSlice) -> None:
                self.leaves.add(
                    "attr "
                    + cst.ensure_type(
                        cst.ensure_type(original_node.slice, cst.Index).value,
                        cst.Integer,
                    ).value
                )

        module = cst.parse_module("foo[1, 2]\n")
        visitor = DeprecatedDecoratorTest()
        module.visit(visitor)
        self.assertEqual(visitor.visits, {"node 1", "node 2", "attr 1", "attr 2"})
        self.assertEqual(visitor.leaves, {"node 1", "node 2", "attr 1", "attr 2"})
