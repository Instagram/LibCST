# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class NumberTest(CSTNodeTest):
    @data_provider(
        (
            # Simple number
            (cst.Number(cst.Integer("5")), "5", parse_expression),
            # Negted number
            (
                cst.Number(operator=cst.Minus(), number=cst.Integer("5")),
                "-5",
                parse_expression,
            ),
            # In parenthesis
            (
                cst.Number(
                    lpar=(cst.LeftParen(),),
                    operator=cst.Minus(),
                    number=cst.Integer("5"),
                    rpar=(cst.RightParen(),),
                ),
                "(-5)",
                parse_expression,
            ),
            (
                cst.Number(
                    lpar=(cst.LeftParen(),),
                    operator=cst.Minus(),
                    number=cst.Integer(
                        "5", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                    rpar=(cst.RightParen(),),
                ),
                "(-(5))",
                parse_expression,
            ),
            (
                cst.UnaryOperation(
                    operator=cst.Minus(),
                    expression=cst.Number(
                        operator=cst.Minus(), number=cst.Integer("5")
                    ),
                ),
                "--5",
                parse_expression,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
    ) -> None:
        self.validate_node(node, code, parser)

    @data_provider(
        (
            (
                lambda: cst.Number(cst.Integer("5"), lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Number(cst.Integer("5"), rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
