# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_module
from libcst.testing.utils import data_provider


class ModuleTest(CSTNodeTest):
    @data_provider(
        (
            # simplest possible program
            (cst.Module((cst.SimpleStatementLine((cst.Pass(),)),)), "pass\n"),
            # test default_newline
            (
                cst.Module(
                    (cst.SimpleStatementLine((cst.Pass(),)),), default_newline="\r"
                ),
                "pass\r",
            ),
            # test header/footer
            (
                cst.Module(
                    (cst.SimpleStatementLine((cst.Pass(),)),),
                    header=(cst.EmptyLine(comment=cst.Comment("# header")),),
                    footer=(cst.EmptyLine(comment=cst.Comment("# footer")),),
                ),
                "# header\npass\n# footer\n",
            ),
            # test has_trailing_newline
            (
                cst.Module(
                    (cst.SimpleStatementLine((cst.Pass(),)),),
                    has_trailing_newline=False,
                ),
                "pass",
            ),
            # an empty file
            (cst.Module((), has_trailing_newline=False), ""),
            # a file with only comments
            (
                cst.Module(
                    (),
                    header=(
                        cst.EmptyLine(comment=cst.Comment("# nothing to see here")),
                    ),
                ),
                "# nothing to see here\n",
            ),
            # TODO: test default_indent
        )
    )
    def test_code_and_bytes_properties(self, module: cst.Module, expected: str) -> None:
        self.assertEqual(module.code, expected)
        self.assertEqual(module.bytes, expected.encode("utf-8"))

    @data_provider(
        (
            (cst.Module(()), cst.Newline(), "\n"),
            (cst.Module((), default_newline="\r\n"), cst.Newline(), "\r\n"),
            # has_trailing_newline has no effect on code_for_node
            (cst.Module((), has_trailing_newline=False), cst.Newline(), "\n"),
            # TODO: test default_indent
        )
    )
    def test_code_for_node(
        self, module: cst.Module, node: cst.CSTNode, expected: str
    ) -> None:
        self.assertEqual(module.code_for_node(node), expected)

    @data_provider(
        {
            "empty_program": {
                "code": "",
                "expected": cst.Module([], has_trailing_newline=False),
            },
            "empty_program_with_newline": {
                "code": "\n",
                "expected": cst.Module([], has_trailing_newline=True),
            },
            "empty_program_with_comments": {
                "code": "# some comment\n",
                "expected": cst.Module(
                    [], header=[cst.EmptyLine(comment=cst.Comment("# some comment"))]
                ),
            },
            "simple_pass": {
                "code": "pass\n",
                "expected": cst.Module([cst.SimpleStatementLine([cst.Pass()])]),
            },
            "simple_pass_with_header_footer": {
                "code": "# header\npass # trailing\n# footer\n",
                "expected": cst.Module(
                    [
                        cst.SimpleStatementLine(
                            [cst.Pass()],
                            trailing_whitespace=cst.TrailingWhitespace(
                                whitespace=cst.SimpleWhitespace(" "),
                                comment=cst.Comment("# trailing"),
                            ),
                        )
                    ],
                    header=[cst.EmptyLine(comment=cst.Comment("# header"))],
                    footer=[cst.EmptyLine(comment=cst.Comment("# footer"))],
                ),
            },
        }
    )
    def test_parser(self, *, code: str, expected: cst.Module) -> None:
        self.assertEqual(parse_module(code), expected)
