# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import cast

import libcst.nodes as cst
from libcst.batched_visitor import BatchableCSTVisitor, visit
from libcst.parser import parse_module
from libcst.testing.utils import UnitTest


class BatchedVisitorTest(UnitTest):
    def test(self) -> None:
        class ABatchable(BatchableCSTVisitor):
            def visit_Pass(self, node: cst.Pass) -> None:
                object.__setattr__(node, "a_attr", True)

        class BBatchable(BatchableCSTVisitor):
            def visit_Pass(self, node: cst.Pass) -> None:
                object.__setattr__(node, "b_attr", 1)

        module = parse_module("pass")
        pass_ = cast(cst.SimpleStatementLine, module.body[0]).body[0]

        visit(module, [ABatchable(), BBatchable()])

        self.assertEqual(object.__getattribute__(pass_, "a_attr"), True)
        self.assertEqual(object.__getattribute__(pass_, "b_attr"), 1)
