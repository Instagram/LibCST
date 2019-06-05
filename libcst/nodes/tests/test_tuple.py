# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class TupleTest(CSTNodeTest):
    @data_provider(
        (
            # zero-element tuple
            (cst.Tuple([]), "()"),
            # one-element tuple
            (cst.Tuple([cst.Element(cst.Name("single_element"))]), "(single_element,)"),
            # two-element tuple
            (
                cst.Tuple([cst.Element(cst.Name("one")), cst.Element(cst.Name("two"))]),
                "(one, two)",
            ),
            # remove parenthesis
            (
                cst.Tuple(
                    [cst.Element(cst.Name("one")), cst.Element(cst.Name("two"))],
                    lpar=[],
                    rpar=[],
                ),
                "one, two",
            ),
            # add extra parenthesis
            (
                cst.Tuple(
                    [cst.Element(cst.Name("one")), cst.Element(cst.Name("two"))],
                    lpar=[cst.LeftParen(), cst.LeftParen()],
                    rpar=[cst.RightParen(), cst.RightParen()],
                ),
                "((one, two))",
            ),
            # starred element
            (
                cst.Tuple(
                    [cst.Element(cst.Name("one")), cst.StarredElement(cst.Name("two"))]
                ),
                "(one, *two)",
            ),
            # custom comma on Element
            (
                cst.Tuple(
                    [
                        cst.Element(cst.Name("one")),
                        cst.Element(cst.Name("two"), comma=cst.Comma()),
                    ]
                ),
                "(one, two,)",
            ),
            # custom comma on StarredElement
            (
                cst.Tuple(
                    [
                        cst.Element(cst.Name("one")),
                        cst.StarredElement(cst.Name("two"), comma=cst.Comma()),
                    ]
                ),
                "(one, *two,)",
            ),
            # custom parenthesis on StarredElement
            (
                cst.Tuple(
                    [
                        cst.StarredElement(
                            cst.Name("abc"),
                            lpar=[cst.LeftParen()],
                            rpar=[cst.RightParen()],
                        )
                    ]
                ),
                "((*abc),)",
            ),
            # custom whitespace on Element
            (
                cst.Tuple(
                    [
                        cst.Element(cst.Name("one")),
                        cst.Element(
                            cst.Name("two"), whitespace_after=cst.SimpleWhitespace("  ")
                        ),
                    ],
                    lpar=[],
                    rpar=[],  # rpar can't own the trailing whitespace if it's not there
                ),
                "one, two  ",
            ),
            # custom whitespace on StarredElement
            (
                cst.Tuple(
                    [
                        cst.Element(cst.Name("one")),
                        cst.StarredElement(
                            cst.Name("two"),
                            whitespace_before_value=cst.SimpleWhitespace("  "),
                            whitespace_after=cst.SimpleWhitespace("    "),
                            lpar=[cst.LeftParen()],
                            rpar=[cst.RightParen()],
                        ),
                    ],
                    lpar=[],
                    rpar=[],  # rpar can't own the trailing whitespace if it's not there
                ),
                "one, (*  two)    ",
            ),
            # missing spaces around tuple, okay with parenthesis
            (
                cst.For(
                    target=cst.Tuple(
                        [cst.Element(cst.Name("k")), cst.Element(cst.Name("v"))]
                    ),
                    iter=cst.Name("abc"),
                    body=cst.SimpleStatementSuite([cst.Pass()]),
                    whitespace_after_for=cst.SimpleWhitespace(""),
                    whitespace_before_in=cst.SimpleWhitespace(""),
                ),
                "for(k, v)in abc: pass\n",
            ),
            # no spaces around tuple, but using values that are parenthesized
            (
                cst.For(
                    target=cst.Tuple(
                        [
                            cst.Element(
                                cst.Name(
                                    "k", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                                )
                            ),
                            cst.Element(
                                cst.Name(
                                    "v", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                                )
                            ),
                        ],
                        lpar=[],
                        rpar=[],
                    ),
                    iter=cst.Name("abc"),
                    body=cst.SimpleStatementSuite([cst.Pass()]),
                    whitespace_after_for=cst.SimpleWhitespace(""),
                    whitespace_before_in=cst.SimpleWhitespace(""),
                ),
                "for(k), (v)in abc: pass\n",
            ),
            # starred elements are safe to use without a space before them
            (
                cst.For(
                    target=cst.Tuple(
                        [cst.StarredElement(cst.Name("foo"))], lpar=[], rpar=[]
                    ),
                    iter=cst.Name("bar"),
                    body=cst.SimpleStatementSuite([cst.Pass()]),
                    whitespace_after_for=cst.SimpleWhitespace(""),
                ),
                "for*foo, in bar: pass\n",
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        self.validate_node(node, code)

    @data_provider(
        (
            (
                lambda: cst.Tuple([], lpar=[], rpar=[]),
                "A zero-length tuple must be wrapped in parentheses.",
            ),
            (
                lambda: cst.Tuple(
                    [cst.Element(cst.Name("mismatched"))],
                    lpar=[cst.LeftParen(), cst.LeftParen()],
                    rpar=[cst.RightParen()],
                ),
                "unbalanced parens",
            ),
            (
                lambda: cst.For(
                    target=cst.Tuple([cst.Element(cst.Name("el"))], lpar=[], rpar=[]),
                    iter=cst.Name("it"),
                    body=cst.SimpleStatementSuite([cst.Pass()]),
                    whitespace_after_for=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space after 'for' keyword.",
            ),
            (
                lambda: cst.For(
                    target=cst.Tuple([cst.Element(cst.Name("el"))], lpar=[], rpar=[]),
                    iter=cst.Name("it"),
                    body=cst.SimpleStatementSuite([cst.Pass()]),
                    whitespace_before_in=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space before 'in' keyword.",
            ),
            # an additional check for StarredElement, since it's a separate codepath
            (
                lambda: cst.For(
                    target=cst.Tuple(
                        [cst.StarredElement(cst.Name("el"))], lpar=[], rpar=[]
                    ),
                    iter=cst.Name("it"),
                    body=cst.SimpleStatementSuite([cst.Pass()]),
                    whitespace_before_in=cst.SimpleWhitespace(""),
                ),
                "Must have at least one space before 'in' keyword.",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
