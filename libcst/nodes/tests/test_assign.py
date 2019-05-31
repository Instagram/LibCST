# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class AssignTest(CSTNodeTest):
    @data_provider(
        (
            # Simple assignment creation case.
            (
                cst.Assign(
                    (cst.AssignTarget(cst.Name("foo")),), cst.Number(cst.Integer("5"))
                ),
                "foo = 5",
                None,
            ),
            # Multiple targets creation
            (
                cst.Assign(
                    (
                        cst.AssignTarget(cst.Name("foo")),
                        cst.AssignTarget(cst.Name("bar")),
                    ),
                    cst.Number(cst.Integer("5")),
                ),
                "foo = bar = 5",
                None,
            ),
            # Whitespace test for creating nodes
            (
                cst.Assign(
                    (
                        cst.AssignTarget(
                            cst.Name("foo"),
                            whitespace_before_equal=cst.SimpleWhitespace(""),
                            whitespace_after_equal=cst.SimpleWhitespace(""),
                        ),
                    ),
                    cst.Number(cst.Integer("5")),
                ),
                "foo=5",
                None,
            ),
            # Simple assignment parser case.
            (
                cst.SimpleStatementLine(
                    (
                        cst.Assign(
                            (cst.AssignTarget(cst.Name("foo")),),
                            cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo = 5\n",
                parse_statement,
            ),
            # Multiple targets parser
            (
                cst.SimpleStatementLine(
                    (
                        cst.Assign(
                            (
                                cst.AssignTarget(cst.Name("foo")),
                                cst.AssignTarget(cst.Name("bar")),
                            ),
                            cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo = bar = 5\n",
                parse_statement,
            ),
            # Whitespace test parser
            (
                cst.SimpleStatementLine(
                    (
                        cst.Assign(
                            (
                                cst.AssignTarget(
                                    cst.Name("foo"),
                                    whitespace_before_equal=cst.SimpleWhitespace(""),
                                    whitespace_after_equal=cst.SimpleWhitespace(""),
                                ),
                            ),
                            cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo=5\n",
                parse_statement,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
    ) -> None:
        self.validate_node(node, code, parser)

    @data_provider(
        (
            (
                lambda: cst.Assign(targets=(), value=cst.Number(cst.Integer("5"))),
                "at least one AssignTarget",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class AnnAssignTest(CSTNodeTest):
    @data_provider(
        (
            # Simple assignment creation case.
            (
                cst.AnnAssign(
                    cst.Name("foo"),
                    cst.Annotation(cst.Name("str")),
                    cst.Number(cst.Integer("5")),
                ),
                "foo: str = 5",
                None,
            ),
            # Annotation creation without assignment
            (
                cst.AnnAssign(cst.Name("foo"), cst.Annotation(cst.Name("str"))),
                "foo: str",
                None,
            ),
            # Complex annotation creation
            (
                cst.AnnAssign(
                    cst.Name("foo"),
                    cst.Annotation(
                        cst.Subscript(cst.Name("Optional"), cst.Index(cst.Name("str")))
                    ),
                    cst.Number(cst.Integer("5")),
                ),
                "foo: Optional[str] = 5",
                None,
            ),
            # Simple assignment parser case.
            (
                cst.SimpleStatementLine(
                    (
                        cst.AnnAssign(
                            target=cst.Name("foo"),
                            annotation=cst.Annotation(
                                annotation=cst.Name("str"),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo: str = 5\n",
                parse_statement,
            ),
            # Annotation without assignment
            (
                cst.SimpleStatementLine(
                    (
                        cst.AnnAssign(
                            target=cst.Name("foo"),
                            annotation=cst.Annotation(
                                annotation=cst.Name("str"),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            value=None,
                        ),
                    )
                ),
                "foo: str\n",
                parse_statement,
            ),
            # Complex annotation
            (
                cst.SimpleStatementLine(
                    (
                        cst.AnnAssign(
                            target=cst.Name("foo"),
                            annotation=cst.Annotation(
                                annotation=cst.Subscript(
                                    cst.Name("Optional"), cst.Index(cst.Name("str"))
                                ),
                                indicator=":",
                                whitespace_before_indicator=cst.SimpleWhitespace(""),
                            ),
                            equal=cst.AssignEqual(),
                            value=cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo: Optional[str] = 5\n",
                parse_statement,
            ),
            # Whitespace test
            (
                cst.AnnAssign(
                    target=cst.Name("foo"),
                    annotation=cst.Annotation(
                        annotation=cst.Subscript(
                            cst.Name("Optional"), cst.Index(cst.Name("str"))
                        ),
                        whitespace_before_indicator=cst.SimpleWhitespace(" "),
                        whitespace_after_indicator=cst.SimpleWhitespace("  "),
                    ),
                    equal=cst.AssignEqual(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    value=cst.Number(cst.Integer("5")),
                ),
                "foo :  Optional[str]  =  5",
                None,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.AnnAssign(
                            target=cst.Name("foo"),
                            annotation=cst.Annotation(
                                annotation=cst.Subscript(
                                    cst.Name("Optional"), cst.Index(cst.Name("str"))
                                ),
                                whitespace_before_indicator=cst.SimpleWhitespace(" "),
                                indicator=":",
                                whitespace_after_indicator=cst.SimpleWhitespace("  "),
                            ),
                            equal=cst.AssignEqual(
                                whitespace_before=cst.SimpleWhitespace("  "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                            value=cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo :  Optional[str]  =  5\n",
                parse_statement,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
    ) -> None:
        self.validate_node(node, code, parser)

    @data_provider(
        (
            (
                lambda: cst.AnnAssign(
                    target=cst.Name("foo"),
                    annotation=cst.Annotation(cst.Name("str")),
                    equal=cst.AssignEqual(),
                    value=None,
                ),
                "Must have a value when specifying an AssignEqual.",
            ),
            (
                lambda: cst.AnnAssign(
                    target=cst.Name("foo"),
                    annotation=cst.Annotation(cst.Name("str"), "->"),
                    value=cst.Number(cst.Integer("5")),
                ),
                "must be denoted with a ':'",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class AugAssignTest(CSTNodeTest):
    @data_provider(
        (
            # Simple assignment constructor case.
            (
                cst.AugAssign(
                    cst.Name("foo"), cst.AddAssign(), cst.Number(cst.Integer("5"))
                ),
                "foo += 5",
                None,
            ),
            (
                cst.AugAssign(cst.Name("bar"), cst.MultiplyAssign(), cst.Name("foo")),
                "bar *= foo",
                None,
            ),
            # Whitespace constructor test
            (
                cst.AugAssign(
                    target=cst.Name("foo"),
                    operator=cst.LeftShiftAssign(
                        whitespace_before=cst.SimpleWhitespace("  "),
                        whitespace_after=cst.SimpleWhitespace("  "),
                    ),
                    value=cst.Number(cst.Integer("5")),
                ),
                "foo  <<=  5",
                None,
            ),
            # Simple assignment parser case.
            (
                cst.SimpleStatementLine(
                    (
                        cst.AugAssign(
                            cst.Name("foo"),
                            cst.AddAssign(),
                            cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo += 5\n",
                parse_statement,
            ),
            (
                cst.SimpleStatementLine(
                    (
                        cst.AugAssign(
                            cst.Name("bar"), cst.MultiplyAssign(), cst.Name("foo")
                        ),
                    )
                ),
                "bar *= foo\n",
                parse_statement,
            ),
            # Whitespace parser test
            (
                cst.SimpleStatementLine(
                    (
                        cst.AugAssign(
                            target=cst.Name("foo"),
                            operator=cst.LeftShiftAssign(
                                whitespace_before=cst.SimpleWhitespace("  "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                            value=cst.Number(cst.Integer("5")),
                        ),
                    )
                ),
                "foo  <<=  5\n",
                parse_statement,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
    ) -> None:
        self.validate_node(node, code, parser)
