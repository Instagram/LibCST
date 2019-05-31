# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest, DummyIndentedBlock
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class FunctionDefCreationTest(CSTNodeTest):
    @data_provider(
        (
            # Simple function definition without any arguments or return
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(): pass\n",
            ),
            # Functiondef with a return annotation
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    returns=cst.Annotation(cst.Name("str")),
                ),
                "def foo() -> str: pass\n",
            ),
            # Async function definition.
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(),
                ),
                "async def foo(): pass\n",
            ),
            # Async function definition with annotation.
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(),
                    returns=cst.Annotation(cst.Name("int")),
                ),
                "async def foo() -> int: pass\n",
            ),
            # Test basic positional params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(cst.Param(cst.Name("bar")), cst.Param(cst.Name("baz")))
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(bar, baz): pass\n",
            ),
            # Typed positional params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(cst.Name("bar"), cst.Annotation(cst.Name("str"))),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(bar: str, baz: int): pass\n",
            ),
            # Test basic positional default params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("bar"), default=cst.SimpleString('"one"')
                            ),
                            cst.Param(
                                cst.Name("baz"), default=cst.Number(cst.Integer("5"))
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(bar = "one", baz = 5): pass\n',
            ),
            # Typed positional default params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(cst.Name("int")),
                                default=cst.Number(cst.Integer("5")),
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(bar: str = "one", baz: int = 5): pass\n',
            ),
            # Mixed positional and default params.
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(cst.Name("bar"), cst.Annotation(cst.Name("str"))),
                        ),
                        default_params=(
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(cst.Name("int")),
                                default=cst.Number(cst.Integer("5")),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(bar: str, baz: int = 5): pass\n",
            ),
            # Test kwonly params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"), default=cst.SimpleString('"one"')
                            ),
                            cst.Param(cst.Name("baz")),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(*, bar = "one", baz): pass\n',
            ),
            # Typed kwonly params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"two"'),
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(*, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed params and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(cst.Name("first")),
                            cst.Param(cst.Name("second")),
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"two"'),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first, second, *, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed default_params and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("first"), default=cst.Number(cst.Float("1.0"))
                            ),
                            cst.Param(
                                cst.Name("second"), default=cst.Number(cst.Float("1.5"))
                            ),
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"two"'),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first = 1.0, second = 1.5, *, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed params, default_params, and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(cst.Name("first")),
                            cst.Param(cst.Name("second")),
                        ),
                        default_params=(
                            cst.Param(
                                cst.Name("third"), default=cst.Number(cst.Float("1.0"))
                            ),
                            cst.Param(
                                cst.Name("fourth"), default=cst.Number(cst.Float("1.5"))
                            ),
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"two"'),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first, second, third = 1.0, fourth = 1.5, *, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Test star_arg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(star_arg=cst.Param(cst.Name("params"))),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(*params): pass\n",
            ),
            # Typed star_arg, include kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(
                            cst.Name("params"), cst.Annotation(cst.Name("str"))
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"two"'),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(*params: str, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed params default_params, star_arg and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(cst.Name("first")),
                            cst.Param(cst.Name("second")),
                        ),
                        default_params=(
                            cst.Param(
                                cst.Name("third"), default=cst.Number(cst.Float("1.0"))
                            ),
                            cst.Param(
                                cst.Name("fourth"), default=cst.Number(cst.Float("1.5"))
                            ),
                        ),
                        star_arg=cst.Param(
                            cst.Name("params"), cst.Annotation(cst.Name("str"))
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"one"'),
                            ),
                            cst.Param(cst.Name("baz"), cst.Annotation(cst.Name("int"))),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(cst.Name("str")),
                                default=cst.SimpleString('"two"'),
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first, second, third = 1.0, fourth = 1.5, *params: str, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Test star_arg and star_kwarg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(star_kwarg=cst.Param(cst.Name("kwparams"))),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(**kwparams): pass\n",
            ),
            # Test star_arg and kwarg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(cst.Name("params")),
                        star_kwarg=cst.Param(cst.Name("kwparams")),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(*params, **kwparams): pass\n",
            ),
            # Test typed star_arg and star_kwarg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(
                            cst.Name("params"), cst.Annotation(cst.Name("str"))
                        ),
                        star_kwarg=cst.Param(
                            cst.Name("kwparams"), cst.Annotation(cst.Name("int"))
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(*params: str, **kwparams: int): pass\n",
            ),
            # Test decorators
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (cst.Decorator(cst.Name("bar")),),
                ),
                "@bar\ndef foo(): pass\n",
            ),
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (
                        cst.Decorator(
                            cst.Call(
                                cst.Name("bar"),
                                (
                                    cst.Arg(cst.Name("baz")),
                                    cst.Arg(cst.SimpleString("'123'")),
                                ),
                            )
                        ),
                    ),
                ),
                "@bar(baz, '123')\ndef foo(): pass\n",
            ),
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (
                        cst.Decorator(
                            cst.Call(
                                cst.Name("bar"), (cst.Arg(cst.SimpleString("'123'")),)
                            )
                        ),
                        cst.Decorator(
                            cst.Call(
                                cst.Name("baz"), (cst.Arg(cst.SimpleString("'456'")),)
                            )
                        ),
                    ),
                ),
                "@bar('123')\n@baz('456')\ndef foo(): pass\n",
            ),
            # Test indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.FunctionDef(
                        cst.Name("foo"),
                        cst.Parameters(),
                        cst.SimpleStatementSuite((cst.Pass(),)),
                        (cst.Decorator(cst.Name("bar")),),
                    ),
                ),
                "    @bar\n    def foo(): pass\n",
            ),
            # With an indented body
            (
                DummyIndentedBlock(
                    "    ",
                    cst.FunctionDef(
                        cst.Name("foo"),
                        cst.Parameters(),
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                        (cst.Decorator(cst.Name("bar")),),
                    ),
                ),
                "    @bar\n    def foo():\n        pass\n",
            ),
            # Leading lines
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    leading_lines=(
                        cst.EmptyLine(comment=cst.Comment("# leading comment")),
                    ),
                ),
                "# leading comment\ndef foo(): pass\n",
            ),
            # Inner whitespace
            (
                cst.FunctionDef(
                    leading_lines=(
                        cst.EmptyLine(),
                        cst.EmptyLine(
                            comment=cst.Comment("# What an amazing decorator")
                        ),
                    ),
                    decorators=(
                        cst.Decorator(
                            whitespace_after_at=cst.SimpleWhitespace(" "),
                            decorator=cst.Call(
                                func=cst.Name("bar"),
                                whitespace_after_func=cst.SimpleWhitespace(" "),
                                whitespace_before_args=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    lines_after_decorators=(
                        cst.EmptyLine(comment=cst.Comment("# What a great function")),
                    ),
                    asynchronous=cst.Asynchronous(
                        whitespace_after=cst.SimpleWhitespace("  ")
                    ),
                    whitespace_after_def=cst.SimpleWhitespace("  "),
                    name=cst.Name("foo"),
                    whitespace_after_name=cst.SimpleWhitespace("  "),
                    whitespace_before_params=cst.SimpleWhitespace("  "),
                    params=cst.Parameters(),
                    returns=cst.Annotation(
                        whitespace_before_indicator=cst.SimpleWhitespace("  "),
                        whitespace_after_indicator=cst.SimpleWhitespace("  "),
                        annotation=cst.Name("str"),
                    ),
                    whitespace_before_colon=cst.SimpleWhitespace(" "),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "\n# What an amazing decorator\n@ bar (  )\n# What a great function\nasync  def  foo  (  )  ->  str : pass\n",
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        self.validate_node(node, code)

    @data_provider(
        (
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Cannot have parens around Name",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(
                        whitespace_after=cst.SimpleWhitespace("")
                    ),
                ),
                "one space after Asynchronous",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    whitespace_after_def=cst.SimpleWhitespace(""),
                ),
                "one space between 'def' and name",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_kwarg=cst.Param(cst.Name("bar"), equal=cst.AssignEqual())
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Must have a default when specifying an AssignEqual.",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(star_kwarg=cst.Param(cst.Name("bar"), star="***")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                r"Must specify either '', '\*' or '\*\*' for star.",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("bar"), default=cst.SimpleString('"one"')
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Cannot have defaults for params",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(default_params=(cst.Param(cst.Name("bar")),)),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Must have defaults for default_params",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(star_arg=cst.ParamStar()),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Must have at least one kwonly param if ParamStar is used.",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(params=(cst.Param(cst.Name("bar"), star="*"),)),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Expecting a star prefix of ''",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("bar"),
                                default=cst.SimpleString('"one"'),
                                star="*",
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Expecting a star prefix of ''",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        kwonly_params=(cst.Param(cst.Name("bar"), star="*"),)
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "Expecting a star prefix of ''",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(star_arg=cst.Param(cst.Name("bar"), star="**")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                r"Expecting a star prefix of '\*'",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(star_kwarg=cst.Param(cst.Name("bar"), star="*")),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                r"Expecting a star prefix of '\*\*'",
            ),
            # Validate decorator name semantics
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (
                        cst.Decorator(
                            cst.Name(
                                "bar", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                            )
                        ),
                    ),
                ),
                "Cannot have parens around decorator in a Decorator",
            ),
            # Validate annotations
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    returns=cst.Annotation(cst.Name("str"), indicator=":"),
                ),
                "return Annotation must be denoted with a '->'",
            ),
            (
                lambda: cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(cst.Name("int"), indicator="->"),
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "param Annotation must be denoted with a ':'",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class FunctionDefParserTest(CSTNodeTest):
    @data_provider(
        (
            # Simple function definition without any arguments or return
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(): pass\n",
            ),
            # Functiondef with a return annotation
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    returns=cst.Annotation(
                        cst.Name("str"),
                        indicator="->",
                        whitespace_before_indicator=cst.SimpleWhitespace(" "),
                    ),
                ),
                "def foo() -> str: pass\n",
            ),
            # Async function definition.
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(),
                ),
                "async def foo(): pass\n",
            ),
            # Async function definition with annotation.
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    asynchronous=cst.Asynchronous(),
                    returns=cst.Annotation(
                        cst.Name("int"),
                        indicator="->",
                        whitespace_before_indicator=cst.SimpleWhitespace(" "),
                    ),
                ),
                "async def foo() -> int: pass\n",
            ),
            # Test basic positional params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("bar"),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(cst.Name("baz"), star=""),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(bar, baz): pass\n",
            ),
            # Typed positional params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                star="",
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(bar: str, baz: int): pass\n",
            ),
            # Test basic positional default params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("bar"),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Integer("5")),
                                star="",
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(bar = "one", baz = 5): pass\n',
            ),
            # Typed positional default params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Integer("5")),
                                star="",
                            ),
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(bar: str = "one", baz: int = 5): pass\n',
            ),
            # Mixed positional and default params.
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        default_params=(
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Integer("5")),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(bar: str, baz: int = 5): pass\n",
            ),
            # Test kwonly params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.ParamStar(),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(cst.Name("baz"), default=None, star=""),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(*, bar = "one", baz): pass\n',
            ),
            # Typed kwonly params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.ParamStar(),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"two"'),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(*, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed params and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("first"),
                                annotation=None,
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("second"),
                                annotation=None,
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        star_arg=cst.ParamStar(),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"two"'),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first, second, *, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed default_params and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        default_params=(
                            cst.Param(
                                cst.Name("first"),
                                annotation=None,
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Float("1.0")),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("second"),
                                annotation=None,
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Float("1.5")),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        star_arg=cst.ParamStar(),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"two"'),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first = 1.0, second = 1.5, *, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed params, default_params, and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("first"),
                                annotation=None,
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("second"),
                                annotation=None,
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        default_params=(
                            cst.Param(
                                cst.Name("third"),
                                annotation=None,
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Float("1.0")),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("fourth"),
                                annotation=None,
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Float("1.5")),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        star_arg=cst.ParamStar(),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"two"'),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first, second, third = 1.0, fourth = 1.5, *, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Test star_arg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(
                            cst.Name("params"), annotation=None, default=None, star="*"
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(*params): pass\n",
            ),
            # Typed star_arg, include kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(
                            cst.Name("params"),
                            cst.Annotation(
                                cst.Name("str"),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            default=None,
                            star="*",
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"two"'),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(*params: str, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Mixed params default_params, star_arg and kwonly_params
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        params=(
                            cst.Param(
                                cst.Name("first"),
                                annotation=None,
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("second"),
                                annotation=None,
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        default_params=(
                            cst.Param(
                                cst.Name("third"),
                                annotation=None,
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Float("1.0")),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("fourth"),
                                annotation=None,
                                equal=cst.AssignEqual(),
                                default=cst.Number(cst.Float("1.5")),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                        ),
                        star_arg=cst.Param(
                            cst.Name("params"),
                            cst.Annotation(
                                cst.Name("str"),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            default=None,
                            star="*",
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        kwonly_params=(
                            cst.Param(
                                cst.Name("bar"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"one"'),
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("baz"),
                                cst.Annotation(
                                    cst.Name("int"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                default=None,
                                star="",
                                comma=cst.Comma(
                                    whitespace_after=cst.SimpleWhitespace(" ")
                                ),
                            ),
                            cst.Param(
                                cst.Name("biz"),
                                cst.Annotation(
                                    cst.Name("str"),
                                    indicator=":",
                                    whitespace_before_indicator=cst.SimpleWhitespace(
                                        ""
                                    ),
                                ),
                                equal=cst.AssignEqual(),
                                default=cst.SimpleString('"two"'),
                                star="",
                            ),
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                'def foo(first, second, third = 1.0, fourth = 1.5, *params: str, bar: str = "one", baz: int, biz: str = "two"): pass\n',
            ),
            # Test star_arg and star_kwarg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_kwarg=cst.Param(
                            cst.Name("kwparams"),
                            annotation=None,
                            default=None,
                            star="**",
                        )
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(**kwparams): pass\n",
            ),
            # Test star_arg and kwarg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(
                            cst.Name("params"),
                            annotation=None,
                            default=None,
                            star="*",
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        star_kwarg=cst.Param(
                            cst.Name("kwparams"),
                            annotation=None,
                            default=None,
                            star="**",
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(*params, **kwparams): pass\n",
            ),
            # Test typed star_arg and star_kwarg
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(
                        star_arg=cst.Param(
                            cst.Name("params"),
                            cst.Annotation(
                                cst.Name("str"),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            default=None,
                            star="*",
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        star_kwarg=cst.Param(
                            cst.Name("kwparams"),
                            cst.Annotation(
                                cst.Name("int"),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            default=None,
                            star="**",
                        ),
                    ),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "def foo(*params: str, **kwparams: int): pass\n",
            ),
            # Test decorators
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (cst.Decorator(cst.Name("bar")),),
                ),
                "@bar\ndef foo(): pass\n",
            ),
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (
                        cst.Decorator(
                            cst.Call(
                                cst.Name("bar"),
                                (
                                    cst.Arg(
                                        cst.Name("baz"),
                                        keyword=None,
                                        comma=cst.Comma(
                                            whitespace_after=cst.SimpleWhitespace(" ")
                                        ),
                                    ),
                                    cst.Arg(cst.SimpleString("'123'"), keyword=None),
                                ),
                            )
                        ),
                    ),
                ),
                "@bar(baz, '123')\ndef foo(): pass\n",
            ),
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    (
                        cst.Decorator(
                            cst.Call(
                                cst.Name("bar"),
                                (cst.Arg(cst.SimpleString("'123'"), keyword=None),),
                            )
                        ),
                        cst.Decorator(
                            cst.Call(
                                cst.Name("baz"),
                                (cst.Arg(cst.SimpleString("'456'"), keyword=None),),
                            )
                        ),
                    ),
                ),
                "@bar('123')\n@baz('456')\ndef foo(): pass\n",
            ),
            # Leading lines
            (
                cst.FunctionDef(
                    cst.Name("foo"),
                    cst.Parameters(),
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    leading_lines=(
                        cst.EmptyLine(comment=cst.Comment("# leading comment")),
                    ),
                ),
                "# leading comment\ndef foo(): pass\n",
            ),
            # Inner whitespace
            (
                cst.FunctionDef(
                    leading_lines=(
                        cst.EmptyLine(),
                        cst.EmptyLine(
                            comment=cst.Comment("# What an amazing decorator")
                        ),
                    ),
                    decorators=(
                        cst.Decorator(
                            whitespace_after_at=cst.SimpleWhitespace(" "),
                            decorator=cst.Call(
                                func=cst.Name("bar"),
                                whitespace_after_func=cst.SimpleWhitespace(" "),
                                whitespace_before_args=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    lines_after_decorators=(
                        cst.EmptyLine(comment=cst.Comment("# What a great function")),
                    ),
                    asynchronous=cst.Asynchronous(
                        whitespace_after=cst.SimpleWhitespace("  ")
                    ),
                    whitespace_after_def=cst.SimpleWhitespace("  "),
                    name=cst.Name("foo"),
                    whitespace_after_name=cst.SimpleWhitespace("  "),
                    whitespace_before_params=cst.SimpleWhitespace("  "),
                    params=cst.Parameters(),
                    returns=cst.Annotation(
                        whitespace_before_indicator=cst.SimpleWhitespace("  "),
                        whitespace_after_indicator=cst.SimpleWhitespace("  "),
                        annotation=cst.Name("str"),
                        indicator="->",
                    ),
                    whitespace_before_colon=cst.SimpleWhitespace(" "),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                ),
                "\n# What an amazing decorator\n@ bar (  )\n# What a great function\nasync  def  foo  (  )  ->  str : pass\n",
            ),
        )
    )
    def test_valid(self, node: cst.CSTNode, code: str) -> None:
        self.validate_node(node, code, parse_statement)
