# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Generic, Iterable, TypeVar, cast

import libcst as cst
from libcst._batched_visitor import (
    BatchableCSTVisitor,
    _BatchedCSTVisitor,
    _get_visitor_methods,
)
from libcst._exceptions import MetadataException
from libcst._nodes._module import _ModuleSelfT as _ModuleT
from libcst._visitors import CSTVisitor
from libcst.metadata._interface import _MetadataInterface


_T_co = TypeVar("_T_co", covariant=True)


# We can't use an ABCMeta here, because of metaclass conflicts
class BaseMetadataProvider(_MetadataInterface, Generic[_T_co]):
    """
    Abstract base class for all metadata providers.
    """

    def _run(self, module: _ModuleT) -> _ModuleT:
        """
        Returns the given module with metadata from this provider.

        This is a hook for metadata runner and should not be called directly.
        Any implementation of this method should not handle any dependencies
        declared by this provider and should not have any side effects besides
        setting metadata computed by this provider.
        """
        ...

    @classmethod
    # pyre-ignore[35]: Parameter type cannot be covariant. Pyre can't
    # detect that this method is not mutating the Provider class.
    def set_metadata(cls, node: cst.CSTNode, value: _T_co) -> None:
        """
        Stores given metadata from this provider on the given node.
        """
        node._metadata[cls] = value


class VisitorMetadataProvider(CSTVisitor, BaseMetadataProvider[_T_co]):
    """
    Extend this to compute metadata with a non-batchable visitor.
    """

    def _run(self, module: _ModuleT) -> _ModuleT:
        """
        Returns the given module with metadata from this provider.
        """
        # Cast is safe as metadata providers should never mutate the tree
        return cast(_ModuleT, module._visit_impl(self))


class BatchableMetadataProvider(BatchableCSTVisitor, BaseMetadataProvider[_T_co]):
    """
    Extend this to compute metadata with a batchable visitor.
    """

    def _run(self, module: _ModuleT) -> _ModuleT:
        """
        Batchable providers are resolved using [_run_batchable].
        """
        raise MetadataException("BatchableMetadataProvider cannot be called directly.")


def _run_batchable(
    module: _ModuleT, providers: Iterable[BatchableMetadataProvider[object]]
) -> _ModuleT:
    """
    Returns the given module with metadata from the given batchable providers.
    """

    visitor_methods = _get_visitor_methods(providers)
    batched_visitor = _BatchedCSTVisitor(visitor_methods)
    # Cast is safe as metadata providers should never mutate the tree
    return cast(_ModuleT, module._visit_impl(batched_visitor))
