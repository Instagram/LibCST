# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import MutableSet, Type

import libcst.nodes as cst
from libcst.batched_visitor import visit as batched_visit
from libcst.exceptions import MetadataException
from libcst.metadata._interface import _MetadataInterface
from libcst.metadata.base_provider import (
    BaseMetadataProvider,
    BatchableMetadataProvider,
)


ProviderT = Type[BaseMetadataProvider[object]]


class _MetadataRunner:
    """
    Helper class for resolving metadata dependencies.
    """

    def __init__(self) -> None:
        self.providers: MutableSet[ProviderT] = set()
        self.satisfied: MutableSet[ProviderT] = set()

    def gather_providers(self, root: ProviderT) -> None:
        if root not in self.providers:
            self.providers.add(root)
            for dep in root.METADATA_DEPENDENCIES:
                self.gather_providers(dep)


def run(module: cst.Module, root: _MetadataInterface) -> None:
    """
    Called by Module.visit to resolve metadata dependencies before performing
    a visitor pass.
    """

    runner = _MetadataRunner()
    for dep in root.METADATA_DEPENDENCIES:
        runner.gather_providers(dep)

    while len(runner.providers) > 0:
        completed = set()
        batchable = set()

        for P in runner.providers:
            if set(P.METADATA_DEPENDENCIES).issubset(runner.satisfied):
                if issubclass(P, BatchableMetadataProvider):
                    batchable.add(P)
                else:
                    P()._run(module)
                    completed.add(P)

        batched_visit(module, [p() for p in batchable])
        runner.providers -= completed | batchable
        runner.satisfied |= completed | batchable

        if len(completed) == 0 and len(batchable) == 0:
            # runner.providers must be non-empty at this point
            names = ", ".join([P.__name__ for P in runner.providers])
            raise MetadataException(f"Detected circular dependencies in {names}")
