# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import TYPE_CHECKING, Collection, MutableSet

from libcst._exceptions import MetadataException
from libcst.metadata.base_provider import (
    BatchableMetadataProvider,
    ProviderT,
    _gen_batchable,
)


if TYPE_CHECKING:
    from libcst.metadata.wrapper import MetadataWrapper  # noqa: F401


def _gather_providers(
    providers: Collection[ProviderT], gathered: MutableSet[ProviderT]
) -> MutableSet[ProviderT]:
    for P in providers:
        if P not in gathered:
            gathered.add(P)
            _gather_providers(P.METADATA_DEPENDENCIES, gathered)
    return gathered


def _resolve_impl(wrapper: "MetadataWrapper", providers: Collection[ProviderT]) -> None:
    """
    Returns a copy of the module that contains all metadata dependencies
    declared by the visitor.
    """
    providers = set(providers) - set(wrapper._metadata.keys())
    remaining = _gather_providers(providers, set())

    completed = set()
    while len(remaining) > 0:
        batchable = set()

        for P in remaining:
            if set(P.METADATA_DEPENDENCIES).issubset(completed):
                if issubclass(P, BatchableMetadataProvider):
                    batchable.add(P)
                else:
                    wrapper._metadata[P] = P()._gen(wrapper)
                    completed.add(P)

        metadata_batch = _gen_batchable(wrapper, [p() for p in batchable])
        wrapper._metadata.update(metadata_batch)
        completed |= batchable

        if len(completed) == 0 and len(batchable) == 0:
            # remaining must be non-empty at this point
            names = ", ".join([P.__name__ for P in remaining])
            raise MetadataException(f"Detected circular dependencies in {names}")

        remaining -= completed
