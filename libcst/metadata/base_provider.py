# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Any, Generic, TypeVar, cast

import libcst.nodes as cst
from libcst._base_visitor import CSTVisitor


_T = TypeVar("_T")


class BaseMetadataProvider(CSTVisitor, Generic[_T]):
    """
    Base class for metadata providers to subclass off of.
    """

    def leave_Module(
        self, orig_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        # TODO: We may need to change the behavior of CSTVisitor or create a
        # subclass to make sure MetaDataProviders don't mutate the tree.

        # A metadata provider can't modify the tree.
        # This ensures no copying is done (which would erase metadata).
        assert orig_node.deep_equals(updated_node)
        return orig_node

    def generate(self, module: cst.Module) -> None:
        """
        Convenience method to run metadata provider over a module.
        """
        module.visit(self)

    @classmethod
    def set_metadata(cls, node: cst.CSTNode, value: _T) -> None:
        # pyre-fixme[33]: Explicit annotation for `typing.cast` cannot be `Any`.
        cast(Any, node).__metadata__[cls] = value
