# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class ComparisonTest(CSTNodeTest):
    @data_provider(
        (
            # Simple comparison statements
            (
                cst.Comparison(
                    cst.Name("foo"),
                    (
                        cst.ComparisonTarget(
                            cst.LessThan(), cst.Number(cst.Integer("5"))
                        ),
                    ),
                ),
                "foo < 5",
            ),
            (
                cst.Comparison(
                    cst.Name("foo"),
                    (
                        cst.ComparisonTarget(
                            cst.NotEqual(), cst.Number(cst.Integer("5"))
                        ),
                    ),
                ),
                "foo != 5",
            ),
            (
                cst.Comparison(
                    cst.Name("foo"), (cst.ComparisonTarget(cst.Is(), cst.Name("True")),)
                ),
                "foo is True",
            ),
            (
                cst.Comparison(
                    cst.Name("foo"),
                    (cst.ComparisonTarget(cst.IsNot(), cst.Name("False")),),
                ),
                "foo is not False",
            ),
            (
                cst.Comparison(
                    cst.Name("foo"), (cst.ComparisonTarget(cst.In(), cst.Name("bar")),)
                ),
                "foo in bar",
            ),
            (
                cst.Comparison(
                    cst.Name("foo"),
                    (cst.ComparisonTarget(cst.NotIn(), cst.Name("bar")),),
                ),
                "foo not in bar",
            ),
            # Comparison with parens
            (
                cst.Comparison(
                    lpar=(cst.LeftParen(),),
                    left=cst.Name("foo"),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.NotIn(), comparator=cst.Name("bar")
                        ),
                    ),
                    rpar=(cst.RightParen(),),
                ),
                "(foo not in bar)",
            ),
            (
                cst.Comparison(
                    left=cst.Name(
                        "foo", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.NotIn(
                                whitespace_before=cst.SimpleWhitespace(""),
                                whitespace_after=cst.SimpleWhitespace(""),
                            ),
                            comparator=cst.Name(
                                "bar", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            ),
                        ),
                    ),
                ),
                "(foo)not in(bar)",
            ),
            # Valid expressions that look like they shouldn't parse
            (
                cst.Comparison(
                    left=cst.Number(cst.Integer("5")),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.NotIn(
                                whitespace_before=cst.SimpleWhitespace("")
                            ),
                            comparator=cst.Name("bar"),
                        ),
                    ),
                ),
                "5not in bar",
            ),
            # Validate that spacing works properly
            (
                cst.Comparison(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    left=cst.Name("foo"),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.NotIn(
                                whitespace_before=cst.SimpleWhitespace("  "),
                                whitespace_between=cst.SimpleWhitespace("  "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                            comparator=cst.Name("bar"),
                        ),
                    ),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "( foo  not  in  bar )",
            ),
            # Do some complex nodes
            (
                cst.Comparison(
                    left=cst.Name("baz"),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.Equal(),
                            comparator=cst.Comparison(
                                lpar=(cst.LeftParen(),),
                                left=cst.Name("foo"),
                                comparisons=(
                                    cst.ComparisonTarget(
                                        operator=cst.NotIn(), comparator=cst.Name("bar")
                                    ),
                                ),
                                rpar=(cst.RightParen(),),
                            ),
                        ),
                    ),
                ),
                "baz == (foo not in bar)",
            ),
            (
                cst.Comparison(
                    left=cst.Name("a"),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.GreaterThan(), comparator=cst.Name("b")
                        ),
                        cst.ComparisonTarget(
                            operator=cst.GreaterThan(), comparator=cst.Name("c")
                        ),
                    ),
                ),
                "a > b > c",
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        self.validate_node(node, code, parse_expression)

    @data_provider(
        (
            (
                lambda: cst.Comparison(
                    cst.Name("foo"),
                    # pyre-fixme[6]: Expected `BaseExpression` for 2nd param but got
                    #  `Integer`.
                    (cst.ComparisonTarget(cst.LessThan(), cst.Integer("5")),),
                    lpar=(cst.LeftParen(),),
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.Comparison(
                    cst.Name("foo"),
                    # pyre-fixme[6]: Expected `BaseExpression` for 2nd param but got
                    #  `Integer`.
                    (cst.ComparisonTarget(cst.LessThan(), cst.Integer("5")),),
                    rpar=(cst.RightParen(),),
                ),
                "right paren without left paren",
            ),
            (
                lambda: cst.Comparison(cst.Name("foo"), ()),
                "at least one ComparisonTarget",
            ),
            (
                lambda: cst.Comparison(
                    left=cst.Name("foo"),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.NotIn(
                                whitespace_before=cst.SimpleWhitespace("")
                            ),
                            comparator=cst.Name("bar"),
                        ),
                    ),
                ),
                "at least one space around comparison operator",
            ),
            (
                lambda: cst.Comparison(
                    left=cst.Name("foo"),
                    comparisons=(
                        cst.ComparisonTarget(
                            operator=cst.NotIn(
                                whitespace_after=cst.SimpleWhitespace("")
                            ),
                            comparator=cst.Name("bar"),
                        ),
                    ),
                ),
                "at least one space around comparison operator",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
