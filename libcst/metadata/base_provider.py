# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from types import MappingProxyType
from typing import (
    TYPE_CHECKING,
    Any,
    Generic,
    Iterable,
    Mapping,
    MutableMapping,
    Type,
    TypeVar,
    cast,
)

from libcst._batched_visitor import BatchableCSTVisitor
from libcst._exceptions import MetadataException
from libcst._visitors import CSTVisitor
from libcst.metadata.dependent import (
    _T as _MetadataT,
    _UNDEFINED_DEFAULT,
    _MetadataDependent,
)


if TYPE_CHECKING:
    from libcst._nodes._base import CSTNode
    from libcst._nodes._module import Module, _ModuleSelfT as _ModuleT
    from libcst.metadata.wrapper import MetadataWrapper


ProviderT = Type["BaseMetadataProvider[Any]"]
_T = TypeVar("_T")


# We can't use an ABCMeta here, because of metaclass conflicts
class BaseMetadataProvider(_MetadataDependent, Generic[_T]):
    """
    Abstract base class for all metadata providers.
    """

    _computed: MutableMapping["CSTNode", _T]

    def __init__(self) -> None:
        super().__init__()
        self._computed = {}

    # TODO: remove this
    def _run(self, module: "_ModuleT") -> "_ModuleT":
        """
        Returns the given module with metadata from this provider.

        This is a hook for metadata runner and should not be called directly.
        Any implementation of this method should not handle any dependencies
        declared by this provider and should not have any side effects besides
        setting metadata computed by this provider.
        """
        ...

    def _gen(self, wrapper: "MetadataWrapper") -> Mapping["CSTNode", _T]:
        """
        Returns the given module with metadata from this provider.

        This is a hook for metadata resolver and should not be called directly.
        """

        self._computed = {}
        # Resolve metadata dependencies for this provider
        with self.resolve(wrapper):
            self._gen_impl(wrapper.module)

        # Copy into a mapping proxy to ensure immutability
        return MappingProxyType(dict(self._computed))

    def _gen_impl(self, module: "Module") -> None:
        """
        Override this method to compute metadata using set_metadata.
        """
        ...

    def set_metadata(self, node: "CSTNode", value: _T) -> None:
        """
        Maps the given node to a metadata value.
        """
        node._metadata[type(self)] = value  # TODO: remove this
        self._computed[node] = value

    def get_metadata(
        self,
        key: Type["BaseMetadataProvider[_MetadataT]"],
        node: "CSTNode",
        default: _MetadataT = _UNDEFINED_DEFAULT,
    ) -> _MetadataT:
        """
        Override to query from self._computed in addition to self.metadata.
        """
        if key is type(self):
            if default is not _UNDEFINED_DEFAULT:
                return cast(_MetadataT, self._computed.get(node, default))
            else:
                return cast(_MetadataT, self._computed[node])

        return super().get_metadata(key, node, default)


class VisitorMetadataProvider(CSTVisitor, BaseMetadataProvider[_T]):
    """
    Extend this to compute metadata with a non-batchable visitor.
    """

    # TODO: remove this
    def _run(self, module: "_ModuleT") -> "_ModuleT":
        """
        Returns the given module with metadata from this provider.
        """
        return module.visit(self, use_compatible=False)

    def _gen_impl(self, module: "_ModuleT") -> None:
        module.visit(self, use_compatible=False)


class BatchableMetadataProvider(BatchableCSTVisitor, BaseMetadataProvider[_T]):
    """
    Extend this to compute metadata with a batchable visitor.
    """

    # TODO: remove this
    def _run(self, module: "_ModuleT") -> "_ModuleT":
        """
        Batchable providers are resolved using [_run_batchable].
        """
        raise MetadataException("BatchableMetadataProvider cannot be called directly.")

    def _gen_impl(self, module: "Module") -> None:
        """
        Batchables providers are run through _run_batchable] so no
        implementation should be provided in _gen_impl.
        """
        pass


def _gen_batchable(
    wrapper: "MetadataWrapper",
    # pyre-fixme[2]: Parameter `providers` must have a type that does not contain `Any`
    providers: Iterable[BatchableMetadataProvider[Any]],
) -> Mapping[ProviderT, Mapping["CSTNode", object]]:
    """
    Returns the given module with metadata from the given batchable providers.

    Does not compute dependencies declared by providers.
    """
    # Resolve metadata dependencies and do batched visit
    wrapper.visit_batched(providers)

    # Make immutable metadata mapping
    # pyre-ignore[7]
    return {type(p): MappingProxyType(dict(p._computed)) for p in providers}
