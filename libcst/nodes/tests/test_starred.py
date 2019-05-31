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


class StarredTest(CSTNodeTest):
    @data_provider(
        (
            # Simple starred expression
            (cst.Starred(cst.Name("foo")), "*foo", parse_expression),
            # In parenthesis
            (
                cst.Starred(
                    lpar=(cst.LeftParen(),),
                    expression=cst.Name("foo"),
                    rpar=(cst.RightParen(),),
                ),
                "(*foo)",
                None,
            ),
            # Verify spacing
            (
                cst.Starred(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    expression=cst.Name("foo"),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                    whitespace_after_star=cst.SimpleWhitespace(" "),
                ),
                "( * foo )",
                None,
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
                lambda: cst.Starred(cst.Name("foo"), lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Starred(cst.Name("foo"), rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
