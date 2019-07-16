# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Any, Callable

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class SimpleCompTest(CSTNodeTest):
    @data_provider(
        [
            # simple GeneratorExp
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"), cst.CompFor(target=cst.Name("b"), iter=cst.Name("c"))
                ),
                "code": "(a for b in c)",
            },
            # simple ListComp
            {
                "node": cst.ListComp(
                    cst.Name("a"), cst.CompFor(target=cst.Name("b"), iter=cst.Name("c"))
                ),
                "code": "[a for b in c]",
            },
            # async GeneratorExp
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        asynchronous=cst.Asynchronous(),
                    ),
                ),
                "code": "(a async for b in c)",
            },
            # a generator doesn't have to own it's own parenthesis
            {
                "node": cst.Call(
                    cst.Name("func"),
                    [
                        cst.Arg(
                            cst.GeneratorExp(
                                cst.Name("a"),
                                cst.CompFor(target=cst.Name("b"), iter=cst.Name("c")),
                                lpar=[],
                                rpar=[],
                            )
                        )
                    ],
                ),
                "code": "func(a for b in c)",
            },
            # add a few 'if' clauses
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        ifs=[
                            cst.CompIf(cst.Name("d")),
                            cst.CompIf(cst.Name("e")),
                            cst.CompIf(cst.Name("f")),
                        ],
                    ),
                ),
                "code": "(a for b in c if d if e if f)",
            },
            # nested/inner for-in clause
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        inner_for_in=cst.CompFor(
                            target=cst.Name("d"), iter=cst.Name("e")
                        ),
                    ),
                ),
                "code": "(a for b in c for d in e)",
            },
            # nested/inner for-in clause with an 'if' clause
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        ifs=[cst.CompIf(cst.Name("d"))],
                        inner_for_in=cst.CompFor(
                            target=cst.Name("e"), iter=cst.Name("f")
                        ),
                    ),
                ),
                "code": "(a for b in c if d for e in f)",
            },
            # custom whitespace
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        ifs=[
                            cst.CompIf(
                                cst.Name("d"),
                                whitespace_before=cst.SimpleWhitespace("\t"),
                                whitespace_before_test=cst.SimpleWhitespace("\t\t"),
                            )
                        ],
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after_for=cst.SimpleWhitespace("   "),
                        whitespace_before_in=cst.SimpleWhitespace("    "),
                        whitespace_after_in=cst.SimpleWhitespace("     "),
                    ),
                    lpar=[cst.LeftParen(whitespace_after=cst.SimpleWhitespace("\f"))],
                    rpar=[
                        cst.RightParen(whitespace_before=cst.SimpleWhitespace("\f\f"))
                    ],
                ),
                "code": "(\fa  for   b    in     c\tif\t\td\f\f)",
            },
            # custom whitespace around ListComp's brackets
            {
                "node": cst.ListComp(
                    cst.Name("a"),
                    cst.CompFor(target=cst.Name("b"), iter=cst.Name("c")),
                    lbracket=cst.LeftSquareBracket(
                        whitespace_after=cst.SimpleWhitespace("\t")
                    ),
                    rbracket=cst.RightSquareBracket(
                        whitespace_before=cst.SimpleWhitespace("\t\t")
                    ),
                    lpar=[cst.LeftParen(whitespace_after=cst.SimpleWhitespace("\f"))],
                    rpar=[
                        cst.RightParen(whitespace_before=cst.SimpleWhitespace("\f\f"))
                    ],
                ),
                "code": "(\f[\ta for b in c\t\t]\f\f)",
            },
            # no whitespace between elements
            {
                "node": cst.GeneratorExp(
                    cst.Name("a", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]),
                    cst.CompFor(
                        target=cst.Name(
                            "b", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                        ),
                        iter=cst.Name(
                            "c", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                        ),
                        ifs=[
                            cst.CompIf(
                                cst.Name(
                                    "d", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                                ),
                                whitespace_before=cst.SimpleWhitespace(""),
                                whitespace_before_test=cst.SimpleWhitespace(""),
                            )
                        ],
                        inner_for_in=cst.CompFor(
                            target=cst.Name(
                                "e", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                            ),
                            iter=cst.Name(
                                "f", lpar=[cst.LeftParen()], rpar=[cst.RightParen()]
                            ),
                            whitespace_before=cst.SimpleWhitespace(""),
                            whitespace_after_for=cst.SimpleWhitespace(""),
                            whitespace_before_in=cst.SimpleWhitespace(""),
                            whitespace_after_in=cst.SimpleWhitespace(""),
                        ),
                        whitespace_before=cst.SimpleWhitespace(""),
                        whitespace_after_for=cst.SimpleWhitespace(""),
                        whitespace_before_in=cst.SimpleWhitespace(""),
                        whitespace_after_in=cst.SimpleWhitespace(""),
                    ),
                    lpar=[cst.LeftParen()],
                    rpar=[cst.RightParen()],
                ),
                "code": "((a)for(b)in(c)if(d)for(e)in(f))",
            },
            # no whitespace before/after GeneratorExp is valid
            {
                "node": cst.Comparison(
                    cst.GeneratorExp(
                        cst.Name("a"),
                        cst.CompFor(target=cst.Name("b"), iter=cst.Name("c")),
                    ),
                    [
                        cst.ComparisonTarget(
                            cst.Is(
                                whitespace_before=cst.SimpleWhitespace(""),
                                whitespace_after=cst.SimpleWhitespace(""),
                            ),
                            cst.GeneratorExp(
                                cst.Name("d"),
                                cst.CompFor(target=cst.Name("e"), iter=cst.Name("f")),
                            ),
                        )
                    ],
                ),
                "code": "(a for b in c)is(d for e in f)",
            },
            # no whitespace before/after ListComp is valid
            {
                "node": cst.Comparison(
                    cst.ListComp(
                        cst.Name("a"),
                        cst.CompFor(target=cst.Name("b"), iter=cst.Name("c")),
                    ),
                    [
                        cst.ComparisonTarget(
                            cst.Is(
                                whitespace_before=cst.SimpleWhitespace(""),
                                whitespace_after=cst.SimpleWhitespace(""),
                            ),
                            cst.ListComp(
                                cst.Name("d"),
                                cst.CompFor(target=cst.Name("e"), iter=cst.Name("f")),
                            ),
                        )
                    ],
                ),
                "code": "[a for b in c]is[d for e in f]",
            },
        ]
    )
    def test_valid(self, **kwargs: Any) -> None:
        self.validate_node(**kwargs)

    @data_provider(
        (
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(target=cst.Name("b"), iter=cst.Name("c")),
                    lpar=[cst.LeftParen(), cst.LeftParen()],
                    rpar=[cst.RightParen()],
                ),
                "unbalanced parens",
            ),
            (
                lambda: cst.ListComp(
                    cst.Name("a"),
                    cst.CompFor(target=cst.Name("b"), iter=cst.Name("c")),
                    lpar=[cst.LeftParen(), cst.LeftParen()],
                    rpar=[cst.RightParen()],
                ),
                "unbalanced parens",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        whitespace_before=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space before 'for' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        asynchronous=cst.Asynchronous(),
                        whitespace_before=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space before 'async' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        whitespace_after_for=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space after 'for' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        whitespace_before_in=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space before 'in' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        whitespace_after_in=cst.SimpleWhitespace(""),
                    ),
                ),
                "Must have at least one space after 'in' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        ifs=[
                            cst.CompIf(
                                cst.Name("d"),
                                whitespace_before=cst.SimpleWhitespace(""),
                            )
                        ],
                    ),
                ),
                "Must have at least one space before 'if' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        ifs=[
                            cst.CompIf(
                                cst.Name("d"),
                                whitespace_before_test=cst.SimpleWhitespace(""),
                            )
                        ],
                    ),
                ),
                "Must have at least one space after 'if' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        inner_for_in=cst.CompFor(
                            target=cst.Name("d"),
                            iter=cst.Name("e"),
                            whitespace_before=cst.SimpleWhitespace(""),
                        ),
                    ),
                ),
                "Must have at least one space before 'for' keyword.",
            ),
            (
                lambda: cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        target=cst.Name("b"),
                        iter=cst.Name("c"),
                        inner_for_in=cst.CompFor(
                            target=cst.Name("d"),
                            iter=cst.Name("e"),
                            asynchronous=cst.Asynchronous(),
                            whitespace_before=cst.SimpleWhitespace(""),
                        ),
                    ),
                ),
                "Must have at least one space before 'async' keyword.",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
