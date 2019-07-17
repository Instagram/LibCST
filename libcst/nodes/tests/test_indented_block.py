# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional, TypeVar, Union

import libcst.nodes as cst
from libcst._removal_sentinel import RemovalSentinel
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider
from libcst.visitors import CSTTransformer


_CSTNodeT = TypeVar("_CSTNodeT", bound=cst.CSTNode)


class IfStatementRemovalVisitor(CSTTransformer):
    def on_leave(
        self, original_node: _CSTNodeT, updated_node: _CSTNodeT
    ) -> Union[_CSTNodeT, RemovalSentinel]:
        if isinstance(updated_node, cst.If):
            return RemovalSentinel.REMOVE
        else:
            return updated_node


class IndentedBlockTest(CSTNodeTest):
    @data_provider(
        (
            (
                cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                "\n    pass\n",
                None,
            ),
            (
                cst.IndentedBlock(
                    (cst.SimpleStatementLine((cst.Pass(),)),), indent="\t"
                ),
                "\n\tpass\n",
                None,
            ),
            (
                cst.IndentedBlock(
                    (cst.SimpleStatementLine((cst.Pass(),)),),
                    header=cst.TrailingWhitespace(
                        whitespace=cst.SimpleWhitespace("  "),
                        comment=cst.Comment("# header comment"),
                    ),
                ),
                "  # header comment\n    pass\n",
                None,
            ),
            (
                cst.IndentedBlock(
                    (cst.SimpleStatementLine((cst.Pass(),)),),
                    footer=(cst.EmptyLine(comment=cst.Comment("# footer comment")),),
                ),
                "\n    pass\n    # footer comment\n",
                None,
            ),
            (
                cst.IndentedBlock(
                    (cst.SimpleStatementLine((cst.Pass(),)),),
                    footer=(
                        cst.EmptyLine(
                            whitespace=cst.SimpleWhitespace("    "),
                            comment=cst.Comment("# footer comment"),
                        ),
                    ),
                ),
                "\n    pass\n        # footer comment\n",
                None,
            ),
            (
                cst.IndentedBlock(
                    (
                        cst.SimpleStatementLine((cst.Continue(),)),
                        cst.SimpleStatementLine((cst.Pass(),)),
                    )
                ),
                "\n    continue\n    pass\n",
                None,
            ),
            # Basic parsing test
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                ),
                "if conditional:\n    pass\n",
                parse_statement,
            ),
            # Multi-level parsing test
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.IndentedBlock(
                        (
                            cst.SimpleStatementLine((cst.Pass(),)),
                            cst.If(
                                cst.Name("other_conditional"),
                                cst.IndentedBlock(
                                    (cst.SimpleStatementLine((cst.Pass(),)),)
                                ),
                            ),
                        )
                    ),
                ),
                "if conditional:\n    pass\n    if other_conditional:\n        pass\n",
                parse_statement,
            ),
            # Inconsistent indentation parsing test
            (
                cst.If(
                    cst.Name("conditional"),
                    cst.IndentedBlock(
                        (
                            cst.SimpleStatementLine((cst.Pass(),)),
                            cst.If(
                                cst.Name("other_conditional"),
                                cst.IndentedBlock(
                                    (cst.SimpleStatementLine((cst.Pass(),)),),
                                    indent="        ",
                                ),
                            ),
                        )
                    ),
                ),
                "if conditional:\n    pass\n    if other_conditional:\n            pass\n",
                parse_statement,
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
            (lambda: cst.IndentedBlock(()), "at least one"),
            (
                lambda: cst.IndentedBlock(
                    (cst.SimpleStatementLine((cst.Pass(),)),), indent=""
                ),
                "non-zero width indent",
            ),
            (
                lambda: cst.IndentedBlock(
                    (cst.SimpleStatementLine((cst.Pass(),)),),
                    indent="this isn't valid whitespace!",
                ),
                "only whitespace",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)

    def test_removal_creates_pass(self) -> None:
        original = cst.IndentedBlock(
            (cst.If(cst.Name("conditional"), cst.SimpleStatementSuite((cst.Break(),))),)
        )
        expected = cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),))

        self.assertEqual(original.visit(IfStatementRemovalVisitor()), expected)
