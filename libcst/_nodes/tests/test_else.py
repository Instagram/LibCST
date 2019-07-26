# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Optional

import libcst as cst
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class ElseTest(CSTNodeTest):
    @data_provider(
        (
            (cst.Else(cst.SimpleStatementSuite((cst.Pass(),))), "else: pass\n"),
            (
                cst.Else(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_before_colon=cst.SimpleWhitespace("  "),
                ),
                "else  : pass\n",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)
