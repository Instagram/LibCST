# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Generic, TypeVar

import libcst.nodes as cst
from libcst.batched_visitor import BatchableCSTVisitor
from libcst.exceptions import MetadataException
from libcst.metadata._interface import _MetadataInterface
from libcst.visitors import CSTVisitor


_T_co = TypeVar("_T_co", covariant=True)


# We can't use an ABCMeta here, because of metaclass conflicts
class BaseMetadataProvider(_MetadataInterface, Generic[_T_co]):
    """
    Abstract base class for all metadata providers.
    """

    def _run(self, module: cst.Module) -> None:
        """
        Method to override to run metadata provider over a module. This method
        is not intended to be called directly but will be used by the
        metadata runner when a visitor declares metadata dependencies.
        """
        ...

    @classmethod
    # pyre-ignore[35]: Parameter type cannot be covariant. Pyre can't
    # detect that this method is not mutating the Provider class.
    def set_metadata(cls, node: cst.CSTNode, value: _T_co) -> None:
        node._metadata[cls] = value


class BatchableMetadataProvider(BatchableCSTVisitor, BaseMetadataProvider[_T_co]):
    """
    Base class for batchable visitor metadata providers.
    """

    def _run(self, module: cst.Module) -> None:
        """
        Batchable providers are not intended to be called directly.
        """
        raise MetadataException(
            "BatchableMetadataProvider should not be called directly."
        )


class VisitorMetadataProvider(CSTVisitor, BaseMetadataProvider[_T_co]):
    """
    Base class for non-batchable visitor metadata providers.
    """

    def _run(self, module: cst.Module) -> None:
        """
        Convenience method to run metadata provider over a module.
        """
        module.visit(self)
