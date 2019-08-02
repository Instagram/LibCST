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
    from libcst._nodes._base import CSTNode  # noqa: F401
    from libcst._nodes._module import Module  # noqa: F401
    from libcst.metadata.base_provider import (  # noqa: F401
        BaseMetadataProvider,
        ProviderT,
    )
    from libcst._visitors import CSTVisitorT  # noqa: F401


_T = TypeVar("_T")


@dataclass(frozen=True)
class MetadataWrapper:
    module: "Module"
    _metadata: MutableMapping["ProviderT", Mapping["CSTNode", object]] = field(
        init=False, default_factory=dict
    )

    def __post_init__(self) -> None:
        # Ensure that module is safe to use by copying
        # module is never mutated after this point which allows a
        # ModuleMetadata instance to be reuse as all metadata
        # computation should be deterministic
        object.__setattr__(self, "module", self.module.deep_clone())

    def resolve(
        self, provider: Type["BaseMetadataProvider[_T]"]
    ) -> Mapping["CSTNode", _T]:
        """
        Returns a copy of a node-metadata map.
        """
        if provider in self._metadata:
            metadata = self._metadata[provider]
        else:
            metadata = self.resolve_many([provider])[provider]

        # pyre-ignore[31]
        return cast(Mapping["CSTNode", _T], metadata)

    def resolve_many(
        self, providers: Collection["ProviderT"]
    ) -> Mapping["ProviderT", Mapping["CSTNode", object]]:
        """
        Returns a copy of the internal map of provider to
        node-metadata maps.
        """
        _resolve_impl(self, providers)

        # Only return what what declared in providers
        return {k: self._metadata[k] for k in providers}

    def visit(self, visitor: "CSTVisitorT") -> "Module":
        """
        Convenience function for visitors to resolve metadata before
        performing a visit pass.

        This basically hoists existing behavior out of Module.visit.
        """
        with visitor.resolve(self):
            return self.module.visit(visitor, use_compatible=False)

    def visit_batched(
        self,
        visitors: Iterable[BatchableCSTVisitor],
        before_visit: Optional[VisitorMethod] = None,
        after_leave: Optional[VisitorMethod] = None,
    ) -> "CSTNode":
        with ExitStack() as stack:
            # Resolve dependencies of visitors
            for v in visitors:
                stack.enter_context(v.resolve(self))

            return visit_batched(
                self.module, visitors, before_visit, after_leave, use_compatible=False
            )
