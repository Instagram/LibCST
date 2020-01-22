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
