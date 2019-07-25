# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import TYPE_CHECKING, TypeVar, Union

from libcst._removal_sentinel import RemovalSentinel
from libcst.metadata._interface import _MetadataInterface


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst.nodes._base import CSTNode  # noqa: F401


CSTVisitorT = Union["CSTTransformer", "CSTVisitor"]
CSTNodeT = TypeVar("CSTNodeT", bound="CSTNode")


class CSTTransformer(_MetadataInterface):
    """
    The low-level base visitor class for traversing a CST and creating an
    updated copy of the original CST.
    """

    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns `True` if children should be visited, and returns `False` otherwise.
        """
        visit_func = getattr(self, f"visit_{type(node).__name__}", None)
        if visit_func is not None:
            retval = visit_func(node)
        else:
            retval = True
        # Don't visit children IFF the visit function returned False.
        return False if retval is False else True

    def on_leave(
        self, original_node: CSTNodeT, updated_node: CSTNodeT
    ) -> Union[CSTNodeT, RemovalSentinel]:
        """
        Called every time we leave a node, after we've visited its children.

        A RemovalSentinel indicates that the node should be removed from its parent.
        This is not always possible, and may raise an exception.
        """
        leave_func = getattr(self, f"leave_{type(original_node).__name__}", None)
        if leave_func is not None:
            updated_node = leave_func(original_node, updated_node)

        return updated_node


class CSTVisitor(_MetadataInterface):
    """
    The low-level base visitor class for traversing a CST.
    """

    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns `True` if children should be visited, and returns `False` otherwise.
        """
        visit_func = getattr(self, f"visit_{type(node).__name__}", None)
        if visit_func is not None:
            retval = visit_func(node)
        else:
            retval = True
        # Don't visit children IFF the visit function returned False.
        return False if retval is False else True

    def on_leave(self, original_node: CSTNodeT) -> None:
        """
        Called every time we leave a node, after we've visited its children.
        """
        leave_func = getattr(self, f"leave_{type(original_node).__name__}", None)
        if leave_func is not None:
            leave_func(original_node)
