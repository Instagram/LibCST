# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst._helpers import ensure_type
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class RaiseConstructionTest(CSTNodeTest):
    @data_provider(
        (
            # Simple raise
            (cst.Raise(), "raise"),
            # Raise exception
            (cst.Raise(cst.Call(cst.Name("Exception"))), "raise Exception()"),
            # Raise exception from cause
            (
                cst.Raise(cst.Call(cst.Name("Exception")), cst.From(cst.Name("cause"))),
                "raise Exception() from cause",
            ),
            # Whitespace oddities test
            (
                cst.Raise(
                    cst.Call(
                        cst.Name("Exception"),
                        lpar=(cst.LeftParen(),),
                        rpar=(cst.RightParen(),),
                    ),
                    cst.From(
                        cst.Name(
                            "cause", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                        ),
                        whitespace_before_from=cst.SimpleWhitespace(""),
                        whitespace_after_from=cst.SimpleWhitespace(""),
                    ),
                    whitespace_after_raise=cst.SimpleWhitespace(""),
                ),
                "raise(Exception())from(cause)",
            ),
            (
                cst.Raise(
                    cst.Call(cst.Name("Exception")),
                    cst.From(
                        cst.Name("cause"),
                        whitespace_before_from=cst.SimpleWhitespace(""),
                    ),
                ),
                "raise Exception()from cause",
            ),
            # Whitespace rendering test
            (
                cst.Raise(
                    exc=cst.Call(cst.Name("Exception")),
                    cause=cst.From(
                        cst.Name("cause"),
                        whitespace_before_from=cst.SimpleWhitespace("  "),
                        whitespace_after_from=cst.SimpleWhitespace("  "),
                    ),
                    whitespace_after_raise=cst.SimpleWhitespace("  "),
                ),
                "raise  Exception()  from  cause",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            # Validate construction
            (
                lambda: cst.Raise(cause=cst.From(cst.Name("cause"))),
                "Must have an 'exc' when specifying 'clause'. on Raise",
            ),
            # Validate whitespace handling
            (
                lambda: cst.Raise(
                    cst.Call(cst.Name("Exception")),
                    whitespace_after_raise=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'raise'",
            ),
            (
                lambda: cst.Raise(
                    cst.Name("exc"),
                    cst.From(
                        cst.Name("cause"),
                        whitespace_before_from=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space before 'from'",
            ),
            (
                lambda: cst.Raise(
                    cst.Name("exc"),
                    cst.From(
                        cst.Name("cause"),
                        whitespace_after_from=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space after 'from'",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class RaiseParsingTest(CSTNodeTest):
    @data_provider(
        (
            # Simple raise
            (cst.Raise(), "raise"),
            # Raise exception
            (
                cst.Raise(
                    cst.Call(cst.Name("Exception")),
                    whitespace_after_raise=cst.SimpleWhitespace(" "),
                ),
                "raise Exception()",
            ),
            # Raise exception from cause
            (
                cst.Raise(
                    cst.Call(cst.Name("Exception")),
                    cst.From(
                        cst.Name("cause"),
                        whitespace_before_from=cst.SimpleWhitespace(" "),
                        whitespace_after_from=cst.SimpleWhitespace(" "),
                    ),
                    whitespace_after_raise=cst.SimpleWhitespace(" "),
                ),
                "raise Exception() from cause",
            ),
            # Whitespace oddities test
            (
                cst.Raise(
                    cst.Call(
                        cst.Name("Exception"),
                        lpar=(cst.LeftParen(),),
                        rpar=(cst.RightParen(),),
                    ),
                    cst.From(
                        cst.Name(
                            "cause", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                        ),
                        whitespace_before_from=cst.SimpleWhitespace(""),
                        whitespace_after_from=cst.SimpleWhitespace(""),
                    ),
                    whitespace_after_raise=cst.SimpleWhitespace(""),
                ),
                "raise(Exception())from(cause)",
            ),
            (
                cst.Raise(
                    cst.Call(cst.Name("Exception")),
                    cst.From(
                        cst.Name("cause"),
                        whitespace_before_from=cst.SimpleWhitespace(""),
                        whitespace_after_from=cst.SimpleWhitespace(" "),
                    ),
                    whitespace_after_raise=cst.SimpleWhitespace(" "),
                ),
                "raise Exception()from cause",
            ),
            # Whitespace rendering test
            (
                cst.Raise(
                    exc=cst.Call(cst.Name("Exception")),
                    cause=cst.From(
                        cst.Name("cause"),
                        whitespace_before_from=cst.SimpleWhitespace("  "),
                        whitespace_after_from=cst.SimpleWhitespace("  "),
                    ),
                    whitespace_after_raise=cst.SimpleWhitespace("  "),
                ),
                "raise  Exception()  from  cause",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(
            node,
            code,
            lambda code: ensure_type(
                parse_statement(code), cst.SimpleStatementLine
            ).body[0],
            expected_position=position,
        )
