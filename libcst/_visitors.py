# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import TYPE_CHECKING, TypeVar, Union

from libcst._removal_sentinel import RemovalSentinel
from libcst.metadata.dependent import _MetadataDependent


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst._nodes._base import CSTNode  # noqa: F401


CSTVisitorT = Union["CSTTransformer", "CSTVisitor"]
CSTNodeT = TypeVar("CSTNodeT", bound="CSTNode")


class CSTTransformer(_MetadataDependent):
    """
    The low-level base visitor class for traversing a CST and creating an
    updated copy of the original CST. This should be used in conjunction with
    the :func:`~libcst.CSTNode.visit` method on a :class:`~libcst.CSTNode` to
    visit each element in a tree starting with that node, and possibly returning
    a new node in its place.

    When visiting nodes using a :class:`CSTTransformer`, the return value of
    :func:`~libcst.CSTNode.visit` will be a new tree with any changes made in
    :func:`~libcst.CSTTransformer.on_leave` calls reflected in its children.
    """

    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns ``True`` if children should be visited, and returns ``False``
        otherwise.
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
        Called every time we leave a node, after we've visited its children. If
        the :func:`~libcst.CSTTransformer.on_visit` function for this node returns
        ``False``, this function will not be called on that node.

        ``original_node`` is guaranteed to be the same node as is passed to
        :func:`~libcst.CSTTransformer.on_visit`, so it is safe to do state-based
        checks using the ``is`` operator. Modifications should always be performed
        on the ``updated_node`` so as to not overwrite changes made by child
        visits.

        Returning a :class:`~libcst.RemovalSentinel` indicates that the node
        should be removed from its parent. This is not always possible, and
        may raise an exception if this node is required.
        """
        leave_func = getattr(self, f"leave_{type(original_node).__name__}", None)
        if leave_func is not None:
            updated_node = leave_func(original_node, updated_node)

        return updated_node


class CSTVisitor(_MetadataDependent):
    """
    The low-level base visitor class for traversing a CST. This should be used in
    conjunction with the :func:`~libcst.CSTNode.visit` method on a
    :class:`~libcst.CSTNode` to visit each element in a tree starting with that
    node. Unlike :class:`CSTTransformer`, instances of this class cannot modify
    the tree.

    When visiting nodes using a :class:`CSTVisitor`, the return value of
    :func:`~libcst.CSTNode.visit` will equal the passed in tree.
    """

    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns ``True`` if children should be visited, and returns ``False``
        otherwise.
        """
        visit_func = getattr(self, f"visit_{type(node).__name__}", None)
        if visit_func is not None:
            retval = visit_func(node)
        else:
            retval = True
        # Don't visit children IFF the visit function returned False.
        return False if retval is False else True

    def on_leave(self, original_node: "CSTNode") -> None:
        """
        Called every time we leave a node, after we've visited its children. If
        the :func:`~libcst.CSTVisitor.on_visit` function for this node returns
        ``False``, this function will not be called on that node.
        """
        leave_func = getattr(self, f"leave_{type(original_node).__name__}", None)
        if leave_func is not None:
            leave_func(original_node)
