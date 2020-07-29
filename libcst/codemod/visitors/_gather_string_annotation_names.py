# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Set, Union, cast

import libcst as cst
import libcst.matchers as m
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareVisitor
from libcst.metadata import QualifiedNameProvider


FUNCS_CONSIDERED_AS_STRING_ANNOTATIONS = {"typing.TypeVar"}
ANNOTATION_MATCHER = m.Annotation() | m.Call(
    metadata=m.MatchMetadataIfTrue(
        QualifiedNameProvider,
        lambda qualnames: any(
            qn.name in FUNCS_CONSIDERED_AS_STRING_ANNOTATIONS for qn in qualnames
        ),
    )
)


class GatherNamesFromStringAnnotationsVisitor(ContextAwareVisitor):
    METADATA_DEPENDENCIES = (QualifiedNameProvider,)

    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)

        self.names: Set[str] = set()

    @m.call_if_inside(ANNOTATION_MATCHER)
    @m.visit(m.ConcatenatedString())
    def handle_any_string(
        self, node: Union[cst.SimpleString, cst.ConcatenatedString]
    ) -> None:
        value = node.evaluated_value
        if value is None:
            return
        mod = cst.parse_module(value)
        extracted_nodes = m.extractall(
            mod,
            m.Name(value=m.SaveMatchedNode(m.DoNotCare(), "name"))
            | m.SaveMatchedNode(m.Attribute(), "attribute"),
        )
        # This captures a bit more than necessary. For attributes, we capture the inner
        # Name twice.
        names = {
            cast(str, values["name"]) for values in extracted_nodes if "name" in values
        } | {
            name
            for values in extracted_nodes
            if "attribute" in values
            for name, _ in cst.metadata.scope_provider._gen_dotted_names(
                cast(cst.Attribute, values["attribute"])
            )
        }
        self.names.update(names)

    @m.call_if_inside(ANNOTATION_MATCHER)
    @m.call_if_not_inside(m.ConcatenatedString())
    @m.visit(m.SimpleString())
    def handle_simple_string(self, node: cst.SimpleString) -> None:
        self.handle_any_string(node)
