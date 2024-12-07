# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
# pyre-strict
import builtins

import libcst as cst
from libcst import matchers as m
from libcst.codemod import VisitorBasedCodemodCommand
from libcst.codemod.visitors._add_imports import AddImportsVisitor
from libcst.helpers import ensure_type
from libcst.metadata.name_provider import QualifiedNameProvider


def node_with_qname(expected: str) -> m.MatchMetadataIfTrue:
    return m.MatchMetadataIfTrue(
        QualifiedNameProvider,
        lambda qnames: any(qname.name == expected for qname in qnames),
    )


annotation = m.SimpleStatementLine(body=[m.AtLeastN(m.AnnAssign(), n=1)])
dataclass_ref: m.MatchMetadataIfTrue = node_with_qname("dataclasses.dataclass")
constant_expressions: list[str] = [
    name for name in dir(builtins) if name.capitalize()[0] == name[0]
]
literal: m.BaseMatcherNode = (
    m.Integer()
    | m.Float()
    | m.Imaginary()
    | m.SimpleString()
    | m.ConcatenatedString()
    | m.FormattedString()
)

field_call = m.Call(func=node_with_qname("dataclasses.field"))

default_arg = m.Call(
    args=[
        m.AtLeastN(
            m.Arg(
                keyword=m.Name("default"),
                value=m.SaveMatchedNode(m.DoNotCare(), "default_value"),
            ),
            n=1,
        )
    ],
)


def wrap_in_default_factory(expr: cst.BaseExpression) -> cst.Arg:
    return cst.Arg(
        keyword=cst.Name("default_factory"),
        value=cst.Lambda(params=cst.Parameters(), body=expr),
    )


class DataclassDefaultFactoryCodemod(VisitorBasedCodemodCommand):
    """
    Converts dataclass fields with mutable default values to use default_factory.

    For example:
        @dataclass
        class Foo:
            x: list = []  # Mutable default, bad practice

    Becomes:
        @dataclass
        class Foo:
            x: list = field(default_factory=lambda: [])  # Better practice
    """

    METADATA_DEPENDENCIES = (QualifiedNameProvider,)

    def is_immutable(self, expr: cst.BaseExpression) -> bool:
        return self.matches(
            expr,
            m.OneOf(
                literal,
                *[m.Name(expr) for expr in constant_expressions],
            ),
        )

    @m.leave(
        m.ClassDef(
            decorators=[m.AtLeastN(m.Decorator(dataclass_ref), n=1)],
            body=m.IndentedBlock(body=[m.AtLeastN(annotation, n=1)]),
        )
    )
    def handle_class(
        self, original_node: cst.ClassDef, updated_node: cst.ClassDef
    ) -> cst.ClassDef:
        new_body: list[cst.BaseStatement] = []
        for line in ensure_type(original_node.body, cst.IndentedBlock).body:
            if not self.matches(line, annotation):
                new_body.append(line)
                continue
            new_line_body: list[cst.BaseSmallStatement] = []
            for stmt in ensure_type(line, cst.SimpleStatementLine).body:
                if not isinstance(stmt, cst.AnnAssign):
                    new_line_body.append(stmt)
                    continue
                new_line_body.append(self.handle_annotation(stmt))
            new_body.append(line.with_changes(body=new_line_body))

        return updated_node.with_changes(
            body=updated_node.body.with_changes(body=new_body)
        )

    def handle_annotation(self, annotation: cst.AnnAssign) -> cst.AnnAssign:
        if annotation.value is None or self.is_immutable(annotation.value):
            return annotation

        if not self.matches(annotation.value, field_call):
            AddImportsVisitor.add_needed_import(self.context, "dataclasses", "field")
            return annotation.with_changes(
                value=cst.Call(
                    func=cst.Name("field"),
                    args=[wrap_in_default_factory(annotation.value)],
                )
            )

        # we found field(...) on the RHS
        if (match := self.extract(annotation.value, default_arg)) is None:
            # no default= kwarg, nothing to do
            return annotation
        default = ensure_type(match["default_value"], cst.BaseExpression)
        if self.is_immutable(default):
            return annotation
        # rebuild the args for field(), dropping default= and adding default_factory=
        new_args: list[cst.Arg] = []
        for arg in ensure_type(annotation.value, cst.Call).args:
            if arg.keyword is None or arg.keyword.value != "default":
                new_args.append(arg)
                continue
            new_args.append(wrap_in_default_factory(default))
        return annotation.with_changes(
            value=annotation.value.with_changes(args=new_args)
        )
