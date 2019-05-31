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


class SubscriptTest(CSTNodeTest):
    @data_provider(
        (
            # Simple subscript expression
            (
                cst.Subscript(cst.Name("foo"), cst.Index(cst.Number(cst.Integer("5")))),
                "foo[5]",
                True,
            ),
            # Test creation of subscript with slice/extslice.
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        upper=cst.Number(cst.Integer("2")),
                        step=cst.Number(cst.Integer("3")),
                    ),
                ),
                "foo[1:2:3]",
                False,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    (
                        cst.ExtSlice(
                            cst.Slice(
                                lower=cst.Number(cst.Integer("1")),
                                upper=cst.Number(cst.Integer("2")),
                                step=cst.Number(cst.Integer("3")),
                            )
                        ),
                        cst.ExtSlice(cst.Index(cst.Number(cst.Integer("5")))),
                    ),
                ),
                "foo[1:2:3, 5]",
                False,
            ),
            # Test parsing of subscript with slice/extslice.
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        first_colon=cst.Colon(),
                        upper=cst.Number(cst.Integer("2")),
                        second_colon=cst.Colon(),
                        step=cst.Number(cst.Integer("3")),
                    ),
                ),
                "foo[1:2:3]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    (
                        cst.ExtSlice(
                            cst.Slice(
                                lower=cst.Number(cst.Integer("1")),
                                first_colon=cst.Colon(),
                                upper=cst.Number(cst.Integer("2")),
                                second_colon=cst.Colon(),
                                step=cst.Number(cst.Integer("3")),
                            ),
                            comma=cst.Comma(),
                        ),
                        cst.ExtSlice(cst.Index(cst.Number(cst.Integer("5")))),
                    ),
                ),
                "foo[1:2:3,5]",
                True,
            ),
            # Some more wild slice creations
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        upper=cst.Number(cst.Integer("2")),
                    ),
                ),
                "foo[1:2]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(lower=cst.Number(cst.Integer("1")), upper=None),
                ),
                "foo[1:]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(lower=None, upper=cst.Number(cst.Integer("2"))),
                ),
                "foo[:2]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        upper=None,
                        step=cst.Number(cst.Integer("3")),
                    ),
                ),
                "foo[1::3]",
                False,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=None, upper=None, step=cst.Number(cst.Integer("3"))
                    ),
                ),
                "foo[::3]",
                False,
            ),
            # Some more wild slice parsings
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        upper=cst.Number(cst.Integer("2")),
                    ),
                ),
                "foo[1:2]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(lower=cst.Number(cst.Integer("1")), upper=None),
                ),
                "foo[1:]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(lower=None, upper=cst.Number(cst.Integer("2"))),
                ),
                "foo[:2]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        upper=None,
                        second_colon=cst.Colon(),
                        step=cst.Number(cst.Integer("3")),
                    ),
                ),
                "foo[1::3]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=None,
                        upper=None,
                        second_colon=cst.Colon(),
                        step=cst.Number(cst.Integer("3")),
                    ),
                ),
                "foo[::3]",
                True,
            ),
            # Valid list clone operations rendering
            (
                cst.Subscript(cst.Name("foo"), cst.Slice(lower=None, upper=None)),
                "foo[:]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=None, upper=None, second_colon=cst.Colon(), step=None
                    ),
                ),
                "foo[::]",
                True,
            ),
            # Valid list clone operations parsing
            (
                cst.Subscript(cst.Name("foo"), cst.Slice(lower=None, upper=None)),
                "foo[:]",
                True,
            ),
            (
                cst.Subscript(
                    cst.Name("foo"),
                    cst.Slice(
                        lower=None, upper=None, second_colon=cst.Colon(), step=None
                    ),
                ),
                "foo[::]",
                True,
            ),
            # In parenthesis
            (
                cst.Subscript(
                    lpar=(cst.LeftParen(),),
                    value=cst.Name("foo"),
                    slice=cst.Index(cst.Number(cst.Integer("5"))),
                    rpar=(cst.RightParen(),),
                ),
                "(foo[5])",
                True,
            ),
            # Verify spacing
            (
                cst.Subscript(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    value=cst.Name("foo"),
                    lbracket=cst.LeftSquareBracket(
                        whitespace_after=cst.SimpleWhitespace(" ")
                    ),
                    slice=cst.Index(cst.Number(cst.Integer("5"))),
                    rbracket=cst.RightSquareBracket(
                        whitespace_before=cst.SimpleWhitespace(" ")
                    ),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                    whitespace_after_value=cst.SimpleWhitespace(" "),
                ),
                "( foo [ 5 ] )",
                True,
            ),
            (
                cst.Subscript(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    value=cst.Name("foo"),
                    lbracket=cst.LeftSquareBracket(
                        whitespace_after=cst.SimpleWhitespace(" ")
                    ),
                    slice=cst.Slice(
                        lower=cst.Number(cst.Integer("1")),
                        first_colon=cst.Colon(
                            whitespace_before=cst.SimpleWhitespace(" "),
                            whitespace_after=cst.SimpleWhitespace(" "),
                        ),
                        upper=cst.Number(cst.Integer("2")),
                        second_colon=cst.Colon(
                            whitespace_before=cst.SimpleWhitespace(" "),
                            whitespace_after=cst.SimpleWhitespace(" "),
                        ),
                        step=cst.Number(cst.Integer("3")),
                    ),
                    rbracket=cst.RightSquareBracket(
                        whitespace_before=cst.SimpleWhitespace(" ")
                    ),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                    whitespace_after_value=cst.SimpleWhitespace(" "),
                ),
                "( foo [ 1 : 2 : 3 ] )",
                True,
            ),
            (
                cst.Subscript(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    value=cst.Name("foo"),
                    lbracket=cst.LeftSquareBracket(
                        whitespace_after=cst.SimpleWhitespace(" ")
                    ),
                    slice=(
                        cst.ExtSlice(
                            slice=cst.Slice(
                                lower=cst.Number(cst.Integer("1")),
                                first_colon=cst.Colon(
                                    whitespace_before=cst.SimpleWhitespace(" "),
                                    whitespace_after=cst.SimpleWhitespace(" "),
                                ),
                                upper=cst.Number(cst.Integer("2")),
                                second_colon=cst.Colon(
                                    whitespace_before=cst.SimpleWhitespace(" "),
                                    whitespace_after=cst.SimpleWhitespace(" "),
                                ),
                                step=cst.Number(cst.Integer("3")),
                            ),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.ExtSlice(slice=cst.Index(cst.Number(cst.Integer("5")))),
                    ),
                    rbracket=cst.RightSquareBracket(
                        whitespace_before=cst.SimpleWhitespace(" ")
                    ),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                    whitespace_after_value=cst.SimpleWhitespace(" "),
                ),
                "( foo [ 1 : 2 : 3 ,  5 ] )",
                True,
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str, check_parsing: bool) -> None:
        if check_parsing:
            self.validate_node(node, code, parse_expression)
        else:
            self.validate_node(node, code)

    @data_provider(
        (
            (
                lambda: cst.Subscript(
                    cst.Name("foo"),
                    cst.Index(cst.Number(cst.Integer("5"))),
                    lpar=(cst.LeftParen(),),
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.Subscript(
                    cst.Name("foo"),
                    cst.Index(cst.Number(cst.Integer("5"))),
                    rpar=(cst.RightParen(),),
                ),
                "right paren without left paren",
            ),
            (lambda: cst.Subscript(cst.Name("foo"), ()), "empty ExtSlice"),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
