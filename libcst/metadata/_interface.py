# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import inspect
from abc import ABC
from typing import TYPE_CHECKING, ClassVar, Collection, Type, TypeVar


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst._nodes._base import CSTNode  # noqa: F401
    from libcst.metadata.base_provider import BaseMetadataProvider  # noqa: F401


_T = TypeVar("_T")

_UNDEFINED_DEFAULT = object()


class _MetadataInterface(ABC):
    """
    Base class for all types that declare required metadata dependencies.

    By default, metadata dependencies are computed upon visiting a Module node.
    An exception will be raised if a visitor that declares metadata is used to
    directly visit a node of type other than Module.
    """

    METADATA_DEPENDENCIES: ClassVar[
        Collection[Type["BaseMetadataProvider[object]"]]
    ] = ()

    @property
    def INHERITED_METADATA_DEPENDENCIES(
        self
    ) -> Collection[Type["BaseMetadataProvider[object]"]]:
        """
        Compute and cache all metadata dependencies from mro chain.
        """
        try:
            # pyre-fixme[16]: use a hidden attribute to cache the property
            # TODO: can simplify this function by using functools.cached_property
            # when it becomes available in Python 3.8
            dependencies = self.INHERITED_METADATA_DEPENDENCIES_CACHE
        except AttributeError:
            dependencies_mut = set()
            for c in inspect.getmro(type(self)):
                if issubclass(c, _MetadataInterface):
                    dependencies_mut.update(c.METADATA_DEPENDENCIES)
            self.INHERITED_METADATA_DEPENDENCIES_CACHE = frozenset(dependencies_mut)
            # pyre-fixme[16]: use a hidden attribute to cache the property
            dependencies = self.INHERITED_METADATA_DEPENDENCIES_CACHE

        return dependencies

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
        if key not in self.INHERITED_METADATA_DEPENDENCIES and key is not type(self):
            raise KeyError(
                f"{key.__name__} is not declared as a dependency from {type(self).__name__}"
            )

        try:
            return node._metadata[key]
        except KeyError as err:
            if default is not _UNDEFINED_DEFAULT:
                return default
            else:
                raise err
