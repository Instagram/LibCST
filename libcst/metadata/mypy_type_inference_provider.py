# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import Dict, List, Mapping, Optional, TYPE_CHECKING

import libcst as cst
from libcst._position import CodeRange
from libcst.helpers import calculate_module_and_package
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.position_provider import PositionProvider

try:
    import mypy

    MYPY_INSTALLED = True
except ImportError:
    MYPY_INSTALLED = False


if TYPE_CHECKING:
    import mypy.nodes

    import libcst.metadata.mypy_utils


def raise_on_mypy_non_installed() -> None:
    if not MYPY_INSTALLED:
        raise RuntimeError("mypy is not installed, please install it")


class MypyTypeInferenceProvider(
    BatchableMetadataProvider["libcst.metadata.mypy_utils.MypyType"]
):
    """
    Access inferred type annotation through `mypy <http://mypy-lang.org/>`_.
    """

    METADATA_DEPENDENCIES = (PositionProvider,)

    @classmethod
    def gen_cache(
        cls, root_path: Path, paths: List[str], timeout: Optional[int] = None
    ) -> Mapping[
        str, Optional["libcst.metadata.mypy_utils.MypyTypeInferenceProviderCache"]
    ]:
        raise_on_mypy_non_installed()

        import mypy.build
        import mypy.main

        from libcst.metadata.mypy_utils import MypyTypeInferenceProviderCache

        targets, options = mypy.main.process_options(paths)
        options.preserve_asts = True
        options.fine_grained_incremental = True
        options.use_fine_grained_cache = True
        mypy_result = mypy.build.build(targets, options=options)
        cache = {}
        for path in paths:
            module = calculate_module_and_package(str(root_path), path).name
            cache[path] = MypyTypeInferenceProviderCache(
                module_name=module,
                mypy_file=mypy_result.graph[module].tree,
            )
        return cache

    def __init__(
        self,
        cache: Optional["libcst.metadata.mypy_utils.MypyTypeInferenceProviderCache"],
    ) -> None:
        from libcst.metadata.mypy_utils import CodeRangeToMypyNodesBinder

        super().__init__(cache)
        self._mypy_node_locations: Dict[CodeRange, "mypy.nodes.Node"] = {}
        if cache is None:
            return
        code_range_to_mypy_nodes_binder = CodeRangeToMypyNodesBinder(cache.module_name)
        code_range_to_mypy_nodes_binder.visit_mypy_file(cache.mypy_file)
        self._mypy_node_locations = code_range_to_mypy_nodes_binder.locations

    def _parse_metadata(self, node: cst.CSTNode) -> None:
        range = self.get_metadata(PositionProvider, node)
        if range in self._mypy_node_locations:
            self.set_metadata(node, self._mypy_node_locations.get(range))

    def visit_Name(self, node: cst.Name) -> Optional[bool]:
        self._parse_metadata(node)

    def visit_Attribute(self, node: cst.Attribute) -> Optional[bool]:
        self._parse_metadata(node)

    def visit_Call(self, node: cst.Call) -> Optional[bool]:
        self._parse_metadata(node)
