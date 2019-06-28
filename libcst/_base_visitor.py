# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from abc import ABC
from typing import TYPE_CHECKING, Any, Type, TypeVar, Union, cast

from libcst._removal_sentinel import RemovalSentinel


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst.nodes._base import CSTNode
    from libcst.metadata.base_provider import BaseMetadataProvider


# RemovalSentinel is re-exported
__all__ = ["CSTVisitor", "RemovalSentinel"]


CSTNodeT = TypeVar("CSTNodeT", bound="CSTNode")
_T = TypeVar("_T")


class CSTVisitor(ABC):
    """
    The low-level base visitor class.

    This shouldn't be used directly, instead we should provide a more user-friendly
    subclass.
    """

    def on_visit(self, node: "CSTNode") -> bool:
        """
        Called every time a node is visited, before we've visited its children.

        Returns `True` if children should be visited, and returns `False` otherwise.
        """
        return True

    def on_leave(
        self, original_node: CSTNodeT, updated_node: CSTNodeT
    ) -> Union[CSTNodeT, RemovalSentinel]:
        """
        Called every time we leave a node, after we've visited its children.

        A RemovalSentinel indicates that the node should be removed from its parent.
        This is not always possible, and may raise an exception.
        """
        return updated_node

    @classmethod
    def get_metadata(cls, key: Type["BaseMetadataProvider[_T]"], node: CSTNodeT) -> _T:
        """
        Gets metadata provided by the [key] provider if it is accessible from
        this vistor. Metadata is accessible if [key] is the same as [cls] or
        if [key] is in METADATA_DEPENDENCIES.
        """
        # TODO: runtime checks that metadata is available in this visitor

        # pyre-fixme[33]: Given annotation cannot be `Any`.
        return cast(Any, node).__metadata__[key]
