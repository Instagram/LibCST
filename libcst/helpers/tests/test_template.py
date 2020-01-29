# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict

import os
from textwrap import dedent

import libcst as cst
from libcst.helpers import (
    parse_template_expression,
    parse_template_module,
    parse_template_statement,
)
from libcst.testing.utils import UnitTest


class TemplateTest(UnitTest):
    def dedent(self, code: str) -> str:
        lines = dedent(code).split(os.linesep)
        if not lines[0].strip():
            lines = lines[1:]
        if not lines[-1].strip():
            lines = [
                *lines[:-1],
                os.linesep,
            ]
        return os.linesep.join(lines)

    def code(self, node: cst.CSTNode) -> str:
        return cst.Module([]).code_for_node(node)

    def test_simple_module(self) -> None:
        module = parse_template_module(
            self.dedent(
                """
                from {module} import {obj}

                def foo() -> {obj}:
                    return {obj}()
                """
            ),
            module=cst.Name("foo"),
            obj=cst.Name("Bar"),
        )
        self.assertEqual(
            module.code,
            self.dedent(
                """
                from foo import Bar

                def foo() -> Bar:
                    return Bar()
                """
            ),
        )

    def test_simple_statement(self) -> None:
        statement = parse_template_statement(
            "assert {test}, {msg}\n",
            test=cst.Name("True"),
            msg=cst.SimpleString('"Somehow True is no longer True..."'),
        )
        self.assertEqual(
            self.code(statement), 'assert True, "Somehow True is no longer True..."\n',
        )

    def test_simple_expression(self) -> None:
        expression = parse_template_expression(
            "{a} + {b} + {c}",
            a=cst.Name("one"),
            b=cst.Name("two"),
            c=cst.BinaryOperation(
                lpar=(cst.LeftParen(),),
                left=cst.Name("three"),
                operator=cst.Multiply(),
                right=cst.Name("four"),
                rpar=(cst.RightParen(),),
            ),
        )
        self.assertEqual(
            self.code(expression), "one + two + (three * four)",
        )

    def test_annotation(self) -> None:
        # Test that we can insert an annotation expression normally.
        statement = parse_template_statement(
            "x: {type} = {val}", type=cst.Name("int"), val=cst.Integer("5"),
        )
        self.assertEqual(
            self.code(statement), "x: int = 5\n",
        )

        # Test that we can insert an annotation node as a special case.
        statement = parse_template_statement(
            "x: {type} = {val}",
            type=cst.Annotation(cst.Name("int")),
            val=cst.Integer("5"),
        )
        self.assertEqual(
            self.code(statement), "x: int = 5\n",
        )

    def test_assign_target(self) -> None:
        # Test that we can insert an assignment target normally.
        statement = parse_template_statement(
            "{a} = {b} = {val}",
            a=cst.Name("first"),
            b=cst.Name("second"),
            val=cst.Integer("5"),
        )
        self.assertEqual(
            self.code(statement), "first = second = 5\n",
        )

        # Test that we can insert an assignment target as a special case.
        statement = parse_template_statement(
            "{a} = {b} = {val}",
            a=cst.AssignTarget(cst.Name("first")),
            b=cst.AssignTarget(cst.Name("second")),
            val=cst.Integer("5"),
        )
        self.assertEqual(
            self.code(statement), "first = second = 5\n",
        )

    def test_parameters(self) -> None:
        # Test that we can insert a parameter into a function def normally.
        statement = parse_template_statement(
            "def foo({arg}): pass", arg=cst.Name("bar"),
        )
        self.assertEqual(
            self.code(statement), "def foo(bar): pass\n",
        )

        # Test that we can insert a parameter as a special case.
        statement = parse_template_statement(
            "def foo({arg}): pass", arg=cst.Param(cst.Name("bar")),
        )
        self.assertEqual(
            self.code(statement), "def foo(bar): pass\n",
        )

        # Test that we can insert a parameters list as a special case.
        statement = parse_template_statement(
            "def foo({args}): pass",
            args=cst.Parameters((cst.Param(cst.Name("bar")),),),
        )
        self.assertEqual(
            self.code(statement), "def foo(bar): pass\n",
        )

        # Test filling out multiple parameters
        statement = parse_template_statement(
            "def foo({args}): pass",
            args=cst.Parameters(
                params=(cst.Param(cst.Name("bar")), cst.Param(cst.Name("baz")),),
                star_kwarg=cst.Param(cst.Name("rest")),
            ),
        )
        self.assertEqual(
            self.code(statement), "def foo(bar, baz, **rest): pass\n",
        )

    def test_args(self) -> None:
        # Test that we can insert an argument into a function call normally.
        statement = parse_template_expression(
            "foo({arg1}, {arg2})", arg1=cst.Name("bar"), arg2=cst.Name("baz"),
        )
        self.assertEqual(
            self.code(statement), "foo(bar, baz)",
        )

        # Test that we can insert an argument as a special case.
        statement = parse_template_expression(
            "foo({arg1}, {arg2})",
            arg1=cst.Arg(cst.Name("bar")),
            arg2=cst.Arg(cst.Name("baz")),
        )
        self.assertEqual(
            self.code(statement), "foo(bar, baz)",
        )

    def test_statement(self) -> None:
        # Test that we can insert various types of statements into a
        # statement list.
        module = parse_template_module(
            "{statement1}\n{statement2}\n{statement3}\n",
            statement1=cst.If(
                test=cst.Name("foo"), body=cst.SimpleStatementSuite((cst.Pass(),),),
            ),
            statement2=cst.SimpleStatementLine((cst.Expr(cst.Call(cst.Name("bar"))),),),
            statement3=cst.Pass(),
        )
        self.assertEqual(
            module.code, "if foo: pass\nbar()\npass\n",
        )
