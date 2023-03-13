# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import Collection, List, Mapping, Optional

import libcst as cst
from libcst.metadata.base_provider import BatchableMetadataProvider


class FilePathProvider(BatchableMetadataProvider[Collection[Path]]):
    """
    Provides the path to the current file on disk as metadata for the root
    :class:`~libcst.Module` node. Requires a :class:`~libcst.metadata.FullRepoManager`.
    If an absolute path to the repo root is given, or file paths are absolute, then the
    resulting metadata will contain absolute paths. Otherwise, the metadata will only
    contain relative paths matching those given to the repo manager.

    Example usage:

    .. code:: python

        class CustomVisitor(CSTVisitor):
            METADATA_DEPENDENCIES = [FilePathProvider]

            path: pathlib.Path

            def visit_Module(self, node: libcst.Module) -> None:
                self.path = self.get_metadata(FilePathProvider, node)

    .. code::

        >>> mgr = FullRepoManager(".", {"libcst/_types.py"}, {FilePathProvider})
        >>> wrapper = mgr.get_metadata_wrapper_for_path("libcst/_types.py")
        >>> fqnames = wrapper.resolve(FilePathProvider)
        >>> {type(k): v for k, v in wrapper.resolve(FilePathProvider).items()}
        {<class 'libcst._nodes.module.Module'>: PosixPath('libcst/_types.py')}

    """

    @classmethod
    def gen_cache(
        cls, root_path: Path, paths: List[str], timeout: Optional[int] = None
    ) -> Mapping[str, Path]:
        cache = {path: root_path / path for path in paths}
        return cache

    def __init__(self, cache: Path) -> None:
        super().__init__(cache)
        self.path: Path = cache

    def visit_Module(self, node: cst.Module) -> Optional[bool]:
        self.set_metadata(node, self.path)
        return False
