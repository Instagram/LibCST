# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import inspect
from abc import ABC
from contextlib import contextmanager
from typing import (
    TYPE_CHECKING,
    ClassVar,
    Collection,
    Iterator,
    Mapping,
    Type,
    TypeVar,
    cast,
)


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst._nodes._base import CSTNode  # noqa: F401
    from libcst.metadata.base_provider import (  # noqa: F401
        BaseMetadataProvider,
        ProviderT,
    )
    from libcst.metadata.wrapper import MetadataWrapper  # noqa: F401


_T = TypeVar("_T")

_UNDEFINED_DEFAULT = object()


class _MetadataDependent(ABC):
    """
    Base class for all types that declare required metadata dependencies.

    By default, metadata dependencies are computed upon visiting a Module node.
    An exception will be raised if a visitor that declares metadata is used to
    directly visit a node of type other than Module.
    """

    # pyre-ignore[4]: Attribute `metadata` of class
    # `libcst.metadata.dependent._MetadataDependent` must have a type that
    # does not contain `Any`.
    metadata: Mapping["ProviderT", Mapping["CSTNode", object]]

    METADATA_DEPENDENCIES: ClassVar[Collection["ProviderT"]] = ()

    def __init__(self) -> None:
        self.metadata = {}

    @classmethod
    def get_inherited_dependencies(cls) -> Collection["ProviderT"]:
        """
        Compute and cache all metadata dependencies from mro chain.
        """
        try:
            # pyre-fixme[16]: use a hidden attribute to cache the property
            return cls._INHERITED_METADATA_DEPENDENCIES_CACHE
        except AttributeError:
            dependencies = set()
            for c in inspect.getmro(cls):
                if issubclass(c, _MetadataDependent):
                    dependencies.update(c.METADATA_DEPENDENCIES)
            cls._INHERITED_METADATA_DEPENDENCIES_CACHE = frozenset(dependencies)
            # pyre-fixme[16]: use a hidden attribute to cache the property
            return cls._INHERITED_METADATA_DEPENDENCIES_CACHE

    @contextmanager
    def resolve(self, wrapper: "MetadataWrapper") -> Iterator[None]:
        self.metadata = wrapper.resolve_many(self.get_inherited_dependencies())
        yield
        self.metadata = {}

    def get_metadata(
        self,
        key: Type["BaseMetadataProvider[_T]"],
        node: "CSTNode",
        default: _T = _UNDEFINED_DEFAULT,
    ) -> _T:
        """
        Gets metadata provided by the [key] provider if it is accessible from
        this vistor. Metadata is accessible if [key] is the same as [cls] or
        if [key] is in METADATA_DEPENDENCIES.
        """
        if key not in self.get_inherited_dependencies():
            raise KeyError(
                f"{key.__name__} is not declared as a dependency from {type(self).__name__}"
            )

        if default is not _UNDEFINED_DEFAULT:
            return cast(_T, self.metadata[key].get(node, default))
        else:
            return cast(_T, self.metadata[key][node])
