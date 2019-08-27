# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from enum import Enum, auto
from typing import Optional

import libcst as cst
from libcst.metadata.base_provider import BatchableMetadataProvider


class ExpressionContext(Enum):
    LOAD = auto()
    STORE = auto()
    DEL = auto()


NODE_TYPES_HAVE_CONTEXT = {
    cst.Attribute,
    cst.Subscript,
    cst.StarredElement,
    cst.StarredDictElement,
    cst.Name,
    cst.List,
    cst.Tuple,
}


class ExpressionContextProvider(BatchableMetadataProvider[Optional[ExpressionContext]]):
    def __init__(self) -> None:
        super().__init__()

    def visit_Assign(self, node: cst.Assign) -> None:
        for target in node.targets:
            self.set_metadata(target.target, ExpressionContext.STORE)

    def visit_Del(self, node: cst.Del) -> None:
        self.set_metadata(node.target, ExpressionContext.DEL)

    def _set_metadata_as_load_if_not_set(self, node: cst.CSTNode) -> None:
        if self.get_metadata(ExpressionContextProvider, node, None) is None:
            self.set_metadata(node, ExpressionContext.LOAD)

    def leave_Attribute(self, node: cst.Attribute) -> None:
        self._set_metadata_as_load_if_not_set(node)
        self.set_metadata(node.attr, self.get_metadata(ExpressionContextProvider, node))

    def leave_Subscript(self, node: cst.Subscript) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_StarredDictElement(self, node: cst.StarredDictElement) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_StarredElement(self, node: cst.StarredElement) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_Name(self, node: cst.Name) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_List(self, node: cst.List) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_Tuple(self, node: cst.Tuple) -> None:
        self._set_metadata_as_load_if_not_set(node)
        for element in node.elements:
            if any(
                isinstance(element.value, node_type)
                for node_type in NODE_TYPES_HAVE_CONTEXT
            ):
                self.set_metadata(
                    element.value, self.get_metadata(ExpressionContextProvider, node)
                )
