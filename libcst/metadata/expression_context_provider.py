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
    """Used in :class:`ExpressionContextProvider` to represent context of a variable
    reference. """

    #: Load the value of a variable reference.
    #:
    #: >>> libcst.MetadataWrapper(libcst.parse_module("a")).resolve(libcst.ExpressionContextProvider)
    #: mappingproxy({Name(
    #:                   value='a',
    #:                   lpar=[],
    #:                   rpar=[],
    #:               ): <ExpressionContext.LOAD: 1>})
    LOAD = auto()

    #: Store a value to a variable reference by :class:`~libcst.Assign` (``=``),
    #: :class:`~libcst.AugAssign` (e.g. ``+=``, ``-=``, etc), or
    #: :class:`~libcst.AnnAssign`.
    #:
    #: >>> libcst.MetadataWrapper(libcst.parse_module("a = b")).resolve(libcst.ExpressionContextProvider)
    #: mappingproxy({Name(
    #:               value='a',
    #:               lpar=[],
    #:               rpar=[],
    #:           ): <ExpressionContext.STORE: 2>, Name(
    #:               value='b',
    #:               lpar=[],
    #:               rpar=[],
    #:           ): <ExpressionContext.LOAD: 1>})
    STORE = auto()

    #: Delete value of a variable reference by ``del``.
    #:
    #: >>> libcst.MetadataWrapper(libcst.parse_module("del a")).resolve(libcst.ExpressionContextProvider)
    #: mappingproxy({Name(
    #:                   value='a',
    #:                   lpar=[],
    #:                   rpar=[],
    #:               ): < ExpressionContext.DEL: 3 >})
    DEL = auto()


NODE_TYPES_HAVE_CONTEXT = (
    cst.Attribute,
    cst.Subscript,
    cst.StarredElement,
    cst.Name,
    cst.List,
    cst.Tuple,
)


class ExpressionContextProvider(BatchableMetadataProvider[Optional[ExpressionContext]]):
    """
    Generate :class:`ExpressionContext` metadata for the following node types:
    :class:`~libcst.Attribute`, :class:`~libcst.Subscript`,
    :class:`~libcst.StarredElement` , :class:`~libcst.Name`, :class:`~libcst.List`,
    :class:`~libcst.Tuple`. The provided context mimics the `expr_context
    <https://docs.python.org/3/library/ast.html>`__ in ast.

    Three context types :attr:`ExpressionContext.STORE`,
    :attr:`ExpressionContext.LOAD` and :attr:`ExpressionContext.DEL` are provided.
    """

    def _check_type_and_set_metadata(
        self, node: cst.CSTNode, context: Optional[ExpressionContext]
    ) -> None:
        if context is not None and isinstance(node, NODE_TYPES_HAVE_CONTEXT):
            self.set_metadata(node, context)

    def visit_Assign(self, node: cst.Assign) -> None:
        for target in node.targets:
            self._check_type_and_set_metadata(target.target, ExpressionContext.STORE)

    def visit_AnnAssign(self, node: cst.AnnAssign) -> None:
        self._check_type_and_set_metadata(node.target, ExpressionContext.STORE)

    def visit_AugAssign(self, node: cst.AugAssign) -> None:
        self._check_type_and_set_metadata(node.target, ExpressionContext.STORE)

    def visit_AsName(self, node: cst.AsName) -> Optional[bool]:
        self._check_type_and_set_metadata(node.name, ExpressionContext.STORE)

    def visit_CompFor(self, node: cst.CompFor) -> Optional[bool]:
        self._check_type_and_set_metadata(node.target, ExpressionContext.STORE)

    def visit_Del(self, node: cst.Del) -> None:
        self.set_metadata(node.target, ExpressionContext.DEL)

    def _set_metadata_as_load_if_not_set(self, node: cst.CSTNode) -> None:
        if self.get_metadata(ExpressionContextProvider, node, None) is None:
            self.set_metadata(node, ExpressionContext.LOAD)

    def leave_Attribute(self, node: cst.Attribute) -> None:
        self._set_metadata_as_load_if_not_set(node)
        self._check_type_and_set_metadata(
            node.attr, self.get_metadata(ExpressionContextProvider, node)
        )

    def leave_Subscript(self, node: cst.Subscript) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_StarredElement(self, node: cst.StarredElement) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_Name(self, node: cst.Name) -> None:
        self._set_metadata_as_load_if_not_set(node)

    def leave_List(self, node: cst.List) -> None:
        self._set_metadata_as_load_if_not_set(node)
        for element in node.elements:
            self._check_type_and_set_metadata(
                element.value, self.get_metadata(ExpressionContextProvider, node)
            )

    def leave_Tuple(self, node: cst.Tuple) -> None:
        self._set_metadata_as_load_if_not_set(node)
        for element in node.elements:
            self._check_type_and_set_metadata(
                element.value, self.get_metadata(ExpressionContextProvider, node)
            )
