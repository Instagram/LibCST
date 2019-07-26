# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class ClassDefCreationTest(CSTNodeTest):
    @data_provider(
        (
            # Simple classdef
            (
                cst.ClassDef(cst.Name("Foo"), cst.SimpleStatementSuite((cst.Pass(),))),
                "class Foo: pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    rpar=cst.RightParen(),
                ),
                "class Foo(): pass\n",
            ),
            # Positional arguments render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    bases=(cst.Arg(cst.Name("obj")),),
                ),
                "class Foo(obj): pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    bases=(
                        cst.Arg(cst.Name("Bar")),
                        cst.Arg(cst.Name("Baz")),
                        cst.Arg(cst.Name("object")),
                    ),
                ),
                "class Foo(Bar, Baz, object): pass\n",
            ),
            # Keyword arguments render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    keywords=(
                        cst.Arg(keyword=cst.Name("metaclass"), value=cst.Name("Bar")),
                    ),
                ),
                "class Foo(metaclass = Bar): pass\n",
            ),
            # Iterator expansion render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    bases=(cst.Arg(star="*", value=cst.Name("one")),),
                ),
                "class Foo(*one): pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    bases=(
                        cst.Arg(star="*", value=cst.Name("one")),
                        cst.Arg(star="*", value=cst.Name("two")),
                        cst.Arg(star="*", value=cst.Name("three")),
                    ),
                ),
                "class Foo(*one, *two, *three): pass\n",
            ),
            # Dictionary expansion render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    keywords=(cst.Arg(star="**", value=cst.Name("one")),),
                ),
                "class Foo(**one): pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    keywords=(
                        cst.Arg(star="**", value=cst.Name("one")),
                        cst.Arg(star="**", value=cst.Name("two")),
                        cst.Arg(star="**", value=cst.Name("three")),
                    ),
                ),
                "class Foo(**one, **two, **three): pass\n",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            # Basic parenthesis tests.
            (
                lambda: cst.ClassDef(
                    name=cst.Name("Foo"),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                ),
                "Do not mix concrete LeftParen/RightParen with MaybeSentinel",
            ),
            (
                lambda: cst.ClassDef(
                    name=cst.Name("Foo"),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                    rpar=cst.RightParen(),
                ),
                "Do not mix concrete LeftParen/RightParen with MaybeSentinel",
            ),
            # Whitespace validation
            (
                lambda: cst.ClassDef(
                    name=cst.Name("Foo"),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_class=cst.SimpleWhitespace(""),
                ),
                "at least one space between 'class' and name",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class ClassDefParserTest(CSTNodeTest):
    @data_provider(
        (
            # Simple classdef
            (
                cst.ClassDef(cst.Name("Foo"), cst.SimpleStatementSuite((cst.Pass(),))),
                "class Foo: pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    rpar=cst.RightParen(),
                ),
                "class Foo(): pass\n",
            ),
            # Positional arguments render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    bases=(cst.Arg(cst.Name("obj")),),
                    rpar=cst.RightParen(),
                ),
                "class Foo(obj): pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    bases=(
                        cst.Arg(
                            cst.Name("Bar"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(
                            cst.Name("Baz"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.Arg(cst.Name("object")),
                    ),
                    rpar=cst.RightParen(),
                ),
                "class Foo(Bar, Baz, object): pass\n",
            ),
            # Keyword arguments render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    keywords=(
                        cst.Arg(
                            keyword=cst.Name("metaclass"),
                            equal=cst.AssignEqual(),
                            value=cst.Name("Bar"),
                        ),
                    ),
                    rpar=cst.RightParen(),
                ),
                "class Foo(metaclass = Bar): pass\n",
            ),
            # Iterator expansion render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    bases=(cst.Arg(star="*", value=cst.Name("one")),),
                    rpar=cst.RightParen(),
                ),
                "class Foo(*one): pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    bases=(
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
                    rpar=cst.RightParen(),
                ),
                "class Foo(*one, *two, *three): pass\n",
            ),
            # Dictionary expansion render test
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    keywords=(cst.Arg(star="**", value=cst.Name("one")),),
                    rpar=cst.RightParen(),
                ),
                "class Foo(**one): pass\n",
            ),
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    keywords=(
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
                    rpar=cst.RightParen(),
                ),
                "class Foo(**one, **two, **three): pass\n",
            ),
            # Decorator render tests
            (
                cst.ClassDef(
                    cst.Name("Foo"),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    decorators=(cst.Decorator(cst.Name("foo")),),
                    lpar=cst.LeftParen(),
                    rpar=cst.RightParen(),
                ),
                "@foo\nclass Foo(): pass\n",
            ),
            (
                cst.ClassDef(
                    leading_lines=(
                        cst.EmptyLine(),
                        cst.EmptyLine(comment=cst.Comment("# leading comment 1")),
                    ),
                    decorators=(
                        cst.Decorator(cst.Name("foo"), leading_lines=()),
                        cst.Decorator(
                            cst.Name("bar"),
                            leading_lines=(
                                cst.EmptyLine(
                                    comment=cst.Comment("# leading comment 2")
                                ),
                            ),
                        ),
                        cst.Decorator(
                            cst.Name("baz"),
                            leading_lines=(
                                cst.EmptyLine(
                                    comment=cst.Comment("# leading comment 3")
                                ),
                            ),
                        ),
                    ),
                    lines_after_decorators=(
                        cst.EmptyLine(comment=cst.Comment("# class comment")),
                    ),
                    name=cst.Name("Foo"),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                    lpar=cst.LeftParen(),
                    rpar=cst.RightParen(),
                ),
                "\n# leading comment 1\n@foo\n# leading comment 2\n@bar\n# leading comment 3\n@baz\n# class comment\nclass Foo(): pass\n",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, parse_statement, expected_position=position)
