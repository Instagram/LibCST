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
from libcst._visitors import CSTVisitor
from libcst.metadata.dependent import (
    _T as _MetadataT,
    _UNDEFINED_DEFAULT,
    MetadataDependent,
)


if TYPE_CHECKING:
    from libcst._nodes._base import CSTNode
    from libcst._nodes._module import Module, _ModuleSelfT as _ModuleT
    from libcst.metadata.wrapper import MetadataWrapper


ProviderT = Type["BaseMetadataProvider[Any]"]
_T = TypeVar("_T")


# We can't use an ABCMeta here, because of metaclass conflicts
class BaseMetadataProvider(MetadataDependent, Generic[_T]):
    """
    The low-level base class for all metadata providers. This class should be
    extended for metadata providers that are not visitor-based.

    This class is generic. A subclass of ``BaseMetadataProvider[T]`` will
    provider metadata of type ``T``.
    """

    #: Cache of metadata computed by this provider
    _computed: MutableMapping["CSTNode", _T]

    def __init__(self) -> None:
        super().__init__()
        self._computed = {}

    def _gen(self, wrapper: "MetadataWrapper") -> Mapping["CSTNode", _T]:
        """
        Resolves and returns metadata mapping for the module in ``wrapper``.

        This method is used by the metadata resolver and should not be called
        directly.
        """

        self._computed = {}
        # Resolve metadata dependencies for this provider
        with self.resolve(wrapper):
            self._gen_impl(wrapper.module)

        # Copy into a mapping proxy to ensure immutability
        return MappingProxyType(dict(self._computed))

    def _gen_impl(self, module: "Module") -> None:
        """
        Override this method with a metadata computation implementation.
        """
        ...

    def set_metadata(self, node: "CSTNode", value: _T) -> None:
        """
        Record a metadata value ``value`` for ``node``.
        """
        self._computed[node] = value

    def get_metadata(
        self,
        key: Type["BaseMetadataProvider[_MetadataT]"],
        node: "CSTNode",
        default: _MetadataT = _UNDEFINED_DEFAULT,
    ) -> _MetadataT:
        """
        The same method as :func:`~libcst.MetadataDependent.get_metadata` except
        metadata is accessed from ``self._computed`` in addition to ``self.metadata``.
        See :func:`~libcst.MetadataDependent.get_metadata`.
        """
        if key is type(self):
            if default is not _UNDEFINED_DEFAULT:
                return cast(_MetadataT, self._computed.get(node, default))
            else:
                return cast(_MetadataT, self._computed[node])

        return super().get_metadata(key, node, default)


class VisitorMetadataProvider(CSTVisitor, BaseMetadataProvider[_T]):
    """
    The low-level base class for all non-batchable visitor-based metadata
    providers. Inherits from :class:`~libcst.CSTVisitor`.

    This class is generic. A subclass of ``VisitorMetadataProvider[T]`` will
    provider metadata of type ``T``.
    """

    def _gen_impl(self, module: "_ModuleT") -> None:
        module.visit(self)


class BatchableMetadataProvider(BatchableCSTVisitor, BaseMetadataProvider[_T]):
    """
    The low-level base class for all batchable visitor-based metadata providers.
    Batchable providers should be preferred when possible as they are more
    efficient to run compared to non-batchable visitor-based providers.
    Inherits from :class:`~libcst.BatchableCSTVisitor`.

    This class is generic. A subclass of ``BatchableMetadataProvider[T]`` will
    provider metadata of type ``T``.
    """

    def _gen_impl(self, module: "Module") -> None:
        """
        Batchables providers are resolved through _gen_batchable] so no
        implementation should be provided in _gen_impl.
        """
        pass


def _gen_batchable(
    wrapper: "MetadataWrapper",
    # pyre-fixme[2]: Parameter `providers` must have a type that does not contain `Any`
    providers: Iterable[BatchableMetadataProvider[Any]],
) -> Mapping[ProviderT, Mapping["CSTNode", object]]:
    """
    Returns map of metadata mappings from resolving ``providers`` on ``wrapper``.
    """
    wrapper.visit_batched(providers)

    # Make immutable metadata mapping
    # pyre-ignore[7]
    return {type(p): MappingProxyType(dict(p._computed)) for p in providers}
