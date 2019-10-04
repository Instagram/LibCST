# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from contextlib import ExitStack
from dataclasses import dataclass, field
from typing import (
    TYPE_CHECKING,
    Collection,
    Iterable,
    Mapping,
    MutableMapping,
    Optional,
    Type,
    TypeVar,
    cast,
)

from libcst._batched_visitor import BatchableCSTVisitor, VisitorMethod, visit_batched
from libcst.metadata._resolver import _resolve_impl


if TYPE_CHECKING:
    from libcst._nodes.base import CSTNode  # noqa: F401
    from libcst._nodes.module import Module  # noqa: F401
    from libcst.metadata.base_provider import (  # noqa: F401
        BaseMetadataProvider,
        ProviderT,
    )
    from libcst._visitors import CSTVisitorT  # noqa: F401


_T = TypeVar("_T")


@dataclass(frozen=True)
class MetadataWrapper:
    """
    A wrapper around a :class:`~libcst.Module` that stores associated metadata
    for that module. When a :class:`MetadataWrapper` is constructed over
    a module, the wrapper will store a deep copy of the original module. This
    means ``MetadataWrapper(module).module == module`` is ``False``.
    """

    module: "Module"
    _metadata: MutableMapping["ProviderT", Mapping["CSTNode", object]] = field(
        init=False, default_factory=dict
    )

    def __post_init__(self) -> None:
        # Ensure that module is safe to use by copying the module to remove
        # any duplicate nodes.
        object.__setattr__(self, "module", self.module.deep_clone())

    def resolve(
        self, provider: Type["BaseMetadataProvider[_T]"]
    ) -> Mapping["CSTNode", _T]:
        """
        Returns a copy of the metadata mapping computed by ``provider``.
        """
        if provider in self._metadata:
            metadata = self._metadata[provider]
        else:
            metadata = self.resolve_many([provider])[provider]

        # pyre-ignore Pyre doesn't recognize "CSTNode" in this contxt.
        return cast(Mapping["CSTNode", _T], metadata)

    def resolve_many(
        self, providers: Collection["ProviderT"]
    ) -> Mapping["ProviderT", Mapping["CSTNode", object]]:
        """
        Returns a copy of the map of metadata mapping computed by each provider
        in ``providers``.

        The returned map does not contain any metadata from undeclared metadata
        dependencies that ``providers`` has.
        """
        _resolve_impl(self, providers)

        # Only return what what declared in providers
        return {k: self._metadata[k] for k in providers}

    def visit(self, visitor: "CSTVisitorT") -> "Module":
        """
        Convenience method to resolve metadata before performing a traversal over
        ``self.module`` with ``visitor``. See :func:`~libcst.Module.visit`.
        """
        with visitor.resolve(self):
            return self.module.visit(visitor)

    def visit_batched(
        self,
        visitors: Iterable[BatchableCSTVisitor],
        before_visit: Optional[VisitorMethod] = None,
        after_leave: Optional[VisitorMethod] = None,
    ) -> "CSTNode":
        """
        Convenience method to resolve metadata before performing a traversal over
        ``self.module`` with ``visitors``. See :func:`~libcst.visit_batched`.
        """
        with ExitStack() as stack:
            # Resolve dependencies of visitors
            for v in visitors:
                stack.enter_context(v.resolve(self))

            return visit_batched(self.module, visitors, before_visit, after_leave)
