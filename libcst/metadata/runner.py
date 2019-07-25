# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import MutableSet, Type

from libcst.exceptions import MetadataException
from libcst.metadata.base_provider import (
    BaseMetadataProvider,
    BatchableMetadataProvider,
    _run_batchable,
)
from libcst.nodes._module import _ModuleSelfT as _ModuleT
from libcst.visitors import CSTVisitorT


ProviderT = Type[BaseMetadataProvider[object]]


class _MetadataRunner:
    def __init__(self) -> None:
        self.providers: MutableSet[ProviderT] = set()
        self.satisfied: MutableSet[ProviderT] = set()

    def gather_providers(self, root: ProviderT) -> None:
        if root not in self.providers:
            self.providers.add(root)
            for dep in root.METADATA_DEPENDENCIES:
                self.gather_providers(dep)

    # TODO: decouple this from visit_Module. require users to first
    # resolve a module + MetadataInterface and then
    @staticmethod
    def resolve(module: _ModuleT, visitor: CSTVisitorT) -> _ModuleT:
        """
        Returns a copy of the module that contains all metadata dependencies
        declared by the visitor.
        """

        if len(visitor.INHERITED_METADATA_DEPENDENCIES) == 0:
            return module

        # We need to deep clone to ensure that there are no duplicate nodes
        # TODO: this is disabled to not break lint and will no longer
        # be necessary when we refactor the metadata API
        # module = module.deep_clone()

        runner = _MetadataRunner()
        for dep in visitor.INHERITED_METADATA_DEPENDENCIES:
            runner.gather_providers(dep)

        while len(runner.providers) > 0:
            completed = set()
            batchable = set()

            for P in runner.providers:
                if set(P.METADATA_DEPENDENCIES).issubset(runner.satisfied):
                    if issubclass(P, BatchableMetadataProvider):
                        batchable.add(P)
                    else:
                        module = P()._run(module)
                        completed.add(P)

            module = _run_batchable(module, [p() for p in batchable])

            runner.providers -= completed | batchable
            runner.satisfied |= completed | batchable

            if len(completed) == 0 and len(batchable) == 0:
                # runner.providers must be non-empty at this point
                names = ", ".join([P.__name__ for P in runner.providers])
                raise MetadataException(f"Detected circular dependencies in {names}")

        return module
