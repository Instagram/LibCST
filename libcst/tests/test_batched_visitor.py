# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import cast
from unittest.mock import Mock

import libcst as cst
from libcst import BatchableCSTVisitor, parse_module, visit_batched
from libcst.testing.utils import UnitTest


class BatchedVisitorTest(UnitTest):
    def test_simple(self) -> None:
        mock = Mock()

        class ABatchable(BatchableCSTVisitor):
            def visit_Pass(self, node: cst.Pass) -> None:
                mock.visited_a()
                object.__setattr__(node, "a_attr", True)

        class BBatchable(BatchableCSTVisitor):
            def visit_Pass(self, node: cst.Pass) -> None:
                mock.visited_b()
                object.__setattr__(node, "b_attr", 1)

        module = visit_batched(parse_module("pass"), [ABatchable(), BBatchable()])
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]

        # Check properties were set
        self.assertEqual(object.__getattribute__(pass_, "a_attr"), True)
        self.assertEqual(object.__getattribute__(pass_, "b_attr"), 1)

        # Check that each visitor was only called once
        mock.visited_a.assert_called_once()
        mock.visited_b.assert_called_once()

    def test_all_visits(self) -> None:
        mock = Mock()

        class Batchable(BatchableCSTVisitor):
            def visit_Pass(self, node: cst.Pass) -> None:
                mock.visit_Pass()
                object.__setattr__(node, "visit_Pass", True)

            def visit_Pass_semicolon(self, node: cst.Pass) -> None:
                mock.visit_Pass_semicolon()
                object.__setattr__(node, "visit_Pass_semicolon", True)

            def leave_Pass_semicolon(self, node: cst.Pass) -> None:
                mock.leave_Pass_semicolon()
                object.__setattr__(node, "leave_Pass_semicolon", True)

            def leave_Pass(self, original_node: cst.Pass) -> None:
                mock.leave_Pass()
                object.__setattr__(original_node, "leave_Pass", True)

        module = visit_batched(parse_module("pass"), [Batchable()])
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]

        # Check properties were set
        self.assertEqual(object.__getattribute__(pass_, "visit_Pass"), True)
        self.assertEqual(object.__getattribute__(pass_, "leave_Pass"), True)
        self.assertEqual(object.__getattribute__(pass_, "visit_Pass_semicolon"), True)
        self.assertEqual(object.__getattribute__(pass_, "leave_Pass_semicolon"), True)

        # Check that each visitor was only called once
        mock.visit_Pass.assert_called_once()
        mock.leave_Pass.assert_called_once()
        mock.visit_Pass_semicolon.assert_called_once()
        mock.leave_Pass_semicolon.assert_called_once()
