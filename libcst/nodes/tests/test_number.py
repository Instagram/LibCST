# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodePosition
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
                CodePosition((1, 0), (1, 2)),
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
                CodePosition((1, 1), (1, 3)),
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
                CodePosition((1, 1), (1, 5)),
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
                CodePosition((1, 0), (1, 3)),
            ),
            # TODO: add test cases for "((5))" and "(+((5)))"
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
        position: Optional[CodePosition] = None,
    ) -> None:
        self.validate_node(node, code, parser, expected_position=position)

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
