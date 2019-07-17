# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from abc import ABC
from typing import TYPE_CHECKING, ClassVar, Sequence, Type, TypeVar, Union

from libcst._removal_sentinel import RemovalSentinel


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst.nodes._base import CSTNode
    from libcst.metadata.base_provider import BaseMetadataProvider


CSTVisitorT = Union["CSTTransformer", "CSTVisitor"]
CSTNodeT = TypeVar("CSTNodeT", bound="CSTNode")
_T = TypeVar("_T")

_THROWS_SENTINEL = object()


class _MetadataInterface(ABC):
    """
    The low-level base visitor class.

    This shouldn't be used directly, instead we provide two subclasses below.
    """

    METADATA_DEPENDENCIES: ClassVar[Sequence[Type["BaseMetadataProvider[object]"]]] = ()

    @classmethod
    def get_metadata(
        cls,
        key: Type["BaseMetadataProvider[_T]"],
        node: CSTNodeT,
        default: _T = _THROWS_SENTINEL,
    ) -> _T:
        """
        Gets metadata provided by the [key] provider if it is accessible from
        this vistor. Metadata is accessible if [key] is the same as [cls] or
        if [key] is in METADATA_DEPENDENCIES.
        """
        if key not in cls.METADATA_DEPENDENCIES and key is not cls:
            raise KeyError(
                f"{key.__name__} is not declared as a dependency from {cls.__name__}"
            )

        try:
            return node._metadata[key]
        except KeyError as err:
            if default is not _THROWS_SENTINEL:
                return default
            else:
                raise err


class CSTTransformer(_MetadataInterface):
    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns `True` if children should be visited, and returns `False` otherwise.
        """
        visit_func = getattr(self, f"visit_{type(node).__name__}", None)
        if visit_func is not None:
            visit_func(node)
        return True

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
    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns `True` if children should be visited, and returns `False` otherwise.
        """
        visit_func = getattr(self, f"visit_{type(node).__name__}", None)
        if visit_func is not None:
            visit_func(node)
        return True

    def on_leave(self, original_node: CSTNodeT) -> None:
        """
        Called every time we leave a node, after we've visited its children.
        """
        leave_func = getattr(self, f"leave_{type(original_node).__name__}", None)
        if leave_func is not None:
            leave_func(original_node)
