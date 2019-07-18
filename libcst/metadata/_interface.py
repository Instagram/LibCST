# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from abc import ABC
from typing import TYPE_CHECKING, ClassVar, Sequence, Type, TypeVar


if TYPE_CHECKING:
    # Circular dependency for typing reasons only
    from libcst.nodes._base import CSTNode  # noqa: F401
    from libcst.metadata.base_provider import BaseMetadataProvider  # noqa: F401


_T = TypeVar("_T")

_UNDEFINED_DEFAULT = object()


class _MetadataInterface(ABC):
    """
    Abstract base class for all types that declare required metadata dependencies.

    By default, metadata dependencies are computed upon visiting a Module node.
    An exception will be raised if a visitor that declares metadata is used to
    directly visit a node of type other than Module.
    """

    METADATA_DEPENDENCIES: ClassVar[Sequence[Type["BaseMetadataProvider[object]"]]] = ()

    @classmethod
    def get_metadata(
        cls,
        key: Type["BaseMetadataProvider[_T]"],
        node: "CSTNode",
        default: _T = _UNDEFINED_DEFAULT,
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
            if default is not _UNDEFINED_DEFAULT:
                return default
            else:
                raise err
