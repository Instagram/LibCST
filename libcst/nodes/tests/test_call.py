# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class CallTest(CSTNodeTest):
    @data_provider(
        (
            # Simple call
            (cst.Call(cst.Name("foo")), "foo()", parse_expression),
            (
                cst.Call(
                    cst.Name("foo"), whitespace_before_args=cst.SimpleWhitespace(" ")
                ),
                "foo( )",
                parse_expression,
            ),
            # Call with attribute dereference
            (
                cst.Call(cst.Attribute(cst.Name("foo"), cst.Name("bar"))),
                "foo.bar()",
                parse_expression,
            ),
            # Positional arguments render test
            (
                cst.Call(cst.Name("foo"), (cst.Arg(cst.Number(cst.Integer("1"))),)),
                "foo(1)",
                None,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(cst.Number(cst.Integer("1"))),
                        cst.Arg(cst.Number(cst.Integer("2"))),
                        cst.Arg(cst.Number(cst.Integer("3"))),
                    ),
                ),
                "foo(1, 2, 3)",
                None,
            ),
            # Positional arguments parse test
            (
                cst.Call(
                    cst.Name("foo"), (cst.Arg(value=cst.Number(cst.Integer("1"))),)
                ),
                "foo(1)",
                parse_expression,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            value=cst.Number(cst.Integer("1")),
                            whitespace_after_arg=cst.SimpleWhitespace(" "),
                        ),
                    ),
                    whitespace_after_func=cst.SimpleWhitespace(" "),
                    whitespace_before_args=cst.SimpleWhitespace(" "),
                ),
                "foo ( 1 )",
                parse_expression,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            value=cst.Number(cst.Integer("1")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                    ),
                    whitespace_after_func=cst.SimpleWhitespace(" "),
                    whitespace_before_args=cst.SimpleWhitespace(" "),
                ),
                "foo ( 1, )",
                parse_expression,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            value=cst.Number(cst.Integer("1")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            value=cst.Number(cst.Integer("2")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(value=cst.Number(cst.Integer("3"))),
                    ),
                ),
                "foo(1, 2, 3)",
                parse_expression,
            ),
            # Keyword arguments render test
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            keyword=cst.Name("one"), value=cst.Number(cst.Integer("1"))
                        ),
                    ),
                ),
                "foo(one = 1)",
                None,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            keyword=cst.Name("one"), value=cst.Number(cst.Integer("1"))
                        ),
                        cst.Arg(
                            keyword=cst.Name("two"), value=cst.Number(cst.Integer("2"))
                        ),
                        cst.Arg(
                            keyword=cst.Name("three"),
                            value=cst.Number(cst.Integer("3")),
                        ),
                    ),
                ),
                "foo(one = 1, two = 2, three = 3)",
                None,
            ),
            # Keyword arguments parser test
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            keyword=cst.Name("one"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("1")),
                        ),
                    ),
                ),
                "foo(one = 1)",
                parse_expression,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            keyword=cst.Name("one"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("1")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("two"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("2")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("three"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("3")),
                        ),
                    ),
                ),
                "foo(one = 1, two = 2, three = 3)",
                parse_expression,
            ),
            # Iterator expansion render test
            (
                cst.Call(cst.Name("foo"), (cst.Arg(star="*", value=cst.Name("one")),)),
                "foo(*one)",
                None,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(star="*", value=cst.Name("one")),
                        cst.Arg(star="*", value=cst.Name("two")),
                        cst.Arg(star="*", value=cst.Name("three")),
                    ),
                ),
                "foo(*one, *two, *three)",
                None,
            ),
            # Iterator expansion parser test
            (
                cst.Call(cst.Name("foo"), (cst.Arg(star="*", value=cst.Name("one")),)),
                "foo(*one)",
                parse_expression,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            star="*",
                            value=cst.Name("one"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="*",
                            value=cst.Name("two"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(star="*", value=cst.Name("three")),
                    ),
                ),
                "foo(*one, *two, *three)",
                parse_expression,
            ),
            # Dictionary expansion render test
            (
                cst.Call(cst.Name("foo"), (cst.Arg(star="**", value=cst.Name("one")),)),
                "foo(**one)",
                None,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(star="**", value=cst.Name("one")),
                        cst.Arg(star="**", value=cst.Name("two")),
                        cst.Arg(star="**", value=cst.Name("three")),
                    ),
                ),
                "foo(**one, **two, **three)",
                None,
            ),
            # Dictionary expansion parser test
            (
                cst.Call(cst.Name("foo"), (cst.Arg(star="**", value=cst.Name("one")),)),
                "foo(**one)",
                parse_expression,
            ),
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            star="**",
                            value=cst.Name("one"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="**",
                            value=cst.Name("two"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(star="**", value=cst.Name("three")),
                    ),
                ),
                "foo(**one, **two, **three)",
                parse_expression,
            ),
            # Complicated mingling rules render test
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(value=cst.Name("pos1")),
                        cst.Arg(star="*", value=cst.Name("list1")),
                        cst.Arg(value=cst.Name("pos2")),
                        cst.Arg(value=cst.Name("pos3")),
                        cst.Arg(star="*", value=cst.Name("list2")),
                        cst.Arg(value=cst.Name("pos4")),
                        cst.Arg(star="*", value=cst.Name("list3")),
                        cst.Arg(
                            keyword=cst.Name("kw1"), value=cst.Number(cst.Integer("1"))
                        ),
                        cst.Arg(star="*", value=cst.Name("list4")),
                        cst.Arg(
                            keyword=cst.Name("kw2"), value=cst.Number(cst.Integer("2"))
                        ),
                        cst.Arg(star="*", value=cst.Name("list5")),
                        cst.Arg(
                            keyword=cst.Name("kw3"), value=cst.Number(cst.Integer("3"))
                        ),
                        cst.Arg(star="**", value=cst.Name("dict1")),
                        cst.Arg(
                            keyword=cst.Name("kw4"), value=cst.Number(cst.Integer("4"))
                        ),
                        cst.Arg(star="**", value=cst.Name("dict2")),
                    ),
                ),
                "foo(pos1, *list1, pos2, pos3, *list2, pos4, *list3, kw1 = 1, *list4, kw2 = 2, *list5, kw3 = 3, **dict1, kw4 = 4, **dict2)",
                None,
            ),
            # Complicated mingling rules parser test
            (
                cst.Call(
                    cst.Name("foo"),
                    (
                        cst.Arg(
                            value=cst.Name("pos1"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="*",
                            value=cst.Name("list1"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            value=cst.Name("pos2"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            value=cst.Name("pos3"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="*",
                            value=cst.Name("list2"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            value=cst.Name("pos4"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="*",
                            value=cst.Name("list3"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("kw1"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("1")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="*",
                            value=cst.Name("list4"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("kw2"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("2")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="*",
                            value=cst.Name("list5"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("kw3"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("3")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="**",
                            value=cst.Name("dict1"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("kw4"),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("4")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(star="**", value=cst.Name("dict2")),
                    ),
                ),
                "foo(pos1, *list1, pos2, pos3, *list2, pos4, *list3, kw1 = 1, *list4, kw2 = 2, *list5, kw3 = 3, **dict1, kw4 = 4, **dict2)",
                parse_expression,
            ),
            # Test whitespace
            (
                cst.Call(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    func=cst.Name("foo"),
                    whitespace_after_func=cst.SimpleWhitespace(" "),
                    whitespace_before_args=cst.SimpleWhitespace(" "),
                    args=(
                        cst.Arg(
                            keyword=None,
                            value=cst.Name("pos1"),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.Arg(
                            star="*",
                            whitespace_after_star=cst.SimpleWhitespace("  "),
                            keyword=None,
                            value=cst.Name("list1"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            keyword=cst.Name("kw1"),
                            equal=cst.AssignEqual(
                                whitespace_before=cst.SimpleWhitespace(""),
                                whitespace_after=cst.SimpleWhitespace(""),
                            ),
                            value=cst.Number(cst.Integer("1")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            star="**",
                            keyword=None,
                            whitespace_after_star=cst.SimpleWhitespace(" "),
                            value=cst.Name("dict1"),
                            whitespace_after_arg=cst.SimpleWhitespace(" "),
                        ),
                    ),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                "( foo ( pos1 ,  *  list1, kw1=1, ** dict1 ) )",
                parse_expression,
                CodeRange.create((1, 2), (1, 43)),
            ),
            # Test args
            (
                cst.Arg(
                    star="*",
                    whitespace_after_star=cst.SimpleWhitespace("  "),
                    keyword=None,
                    value=cst.Name("list1"),
                    comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                ),
                "*  list1, ",
                None,
                CodeRange.create((1, 0), (1, 8)),
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
        position: Optional[CodeRange] = None,
    ) -> None:
        self.validate_node(node, code, parser, expected_position=position)

    @data_provider(
        (
            # Basic expression parenthesizing tests.
            (
                lambda: cst.Call(func=cst.Name("foo"), lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Call(func=cst.Name("foo"), rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            # Test that we handle keyword stuff correctly.
            (
                lambda: cst.Call(
                    func=cst.Name("foo"),
                    args=(
                        cst.Arg(
                            equal=cst.AssignEqual(), value=cst.SimpleString("'baz'")
                        ),
                    ),
                ),
                "Must have a keyword when specifying an AssignEqual",
            ),
            # Test that we separate *, ** and keyword args correctly
            (
                lambda: cst.Call(
                    func=cst.Name("foo"),
                    args=(
                        cst.Arg(
                            star="*",
                            keyword=cst.Name("bar"),
                            value=cst.SimpleString("'baz'"),
                        ),
                    ),
                ),
                "Cannot specify a star and a keyword together",
            ),
            # Test for expected star inputs only
            (
                lambda: cst.Call(
                    func=cst.Name("foo"),
                    # pyre-fixme[6]: Expected `Union[typing_extensions.Literal[''],
                    #  typing_extensions.Literal['*'],
                    #  typing_extensions.Literal['**']]` for 1st param but got
                    #  `typing_extensions.Literal['***']`.
                    args=(cst.Arg(star="***", value=cst.SimpleString("'baz'")),),
                ),
                r"Must specify either '', '\*' or '\*\*' for star",
            ),
            # Test ordering exceptions
            (
                lambda: cst.Call(
                    func=cst.Name("foo"),
                    args=(
                        cst.Arg(star="**", value=cst.Name("bar")),
                        cst.Arg(star="*", value=cst.Name("baz")),
                    ),
                ),
                "Cannot have iterable argument unpacking after keyword argument unpacking",
            ),
            (
                lambda: cst.Call(
                    func=cst.Name("foo"),
                    args=(
                        cst.Arg(star="**", value=cst.Name("bar")),
                        cst.Arg(value=cst.Name("baz")),
                    ),
                ),
                "Cannot have positional argument after keyword argument unpacking",
            ),
            (
                lambda: cst.Call(
                    func=cst.Name("foo"),
                    args=(
                        cst.Arg(
                            keyword=cst.Name("arg"), value=cst.SimpleString("'baz'")
                        ),
                        cst.Arg(value=cst.SimpleString("'bar'")),
                    ),
                ),
                "Cannot have positional argument after keyword argument",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
