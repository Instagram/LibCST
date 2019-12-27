# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import json
import subprocess
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Callable,
    Dict,
    List,
    Mapping,
    Optional,
    Sequence,
    Tuple,
)

from mypy_extensions import TypedDict

import libcst as cst
from libcst.metadata.type_inference_provider import (
    InferredType,
    PyreData,
    TypeInferenceProvider,
)
from libcst.metadata.wrapper import MetadataWrapper


if TYPE_CHECKING:
    from libcst.metadata.base_provider import ProviderT


class FullRepoManager:
    def __init__(
        self,
        repo_root_dir: str,
        paths: List[str],
        providers: List["ProviderT"],
        timeout: int = 5,
    ) -> None:
        self.root_path: Path = Path(repo_root_dir)
        self._cache: Dict["ProviderT", Mapping[str, object]] = {}
        self._timeout = timeout
        self._providers = providers
        self._paths = paths

    def _get_cache_handler(self, provider: "ProviderT") -> Optional[Callable]:
        maps: Dict["ProviderT", Callable[[List[str]], Mapping[str, object]]] = {
            TypeInferenceProvider: self._handle_pyre_cache
        }
        return maps.get(provider)

    def _handle_pyre_cache(self, paths: List[str]) -> Mapping[str, object]:
        params = ",".join(f"path='{self.root_path/path}'" for path in paths)
        cmd = f'''pyre query "types({params})"'''
        try:
            stdout, stderr, return_code = run_command(cmd, timeout=self._timeout)
        except subprocess.TimeoutExpired as exc:
            raise exc

        if return_code != 0:
            raise Exception(f"stderr:\n {stderr}\nstdout:\n {stdout}")
        try:
            resp = json.loads(stdout)["response"]
        except Exception as e:
            raise Exception(f"{e}\n\nstderr:\n {stderr}\nstdout:\n {stdout}")
        return {path: _process_pyre_data(data) for path, data in zip(paths, resp)}

    def _resolve_cache(self) -> None:
        if not self._cache:
            cache: Dict["ProviderT", Mapping[str, object]] = {}
            for provider in self._providers:
                handler = self._get_cache_handler(provider)
                if handler:
                    cache[provider] = handler(self._paths)
            self._cache = cache

    def get_metadata_wrapper_for_path(self, path: str) -> MetadataWrapper:
        self._resolve_cache()
        module = cst.parse_module((self.root_path / path).read_text())
        cache = {
            provider: data
            for provider, files in self._cache.items()
            for _path, data in files.items()
            if _path == path
        }
        return MetadataWrapper(module, True, cache)


def run_command(command: str, timeout: Optional[int] = None) -> Tuple[str, str, int]:
    process = subprocess.Popen(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True
    )
    stdout, stderr = process.communicate(timeout=timeout)
    return stdout.decode(), stderr.decode(), process.returncode


def _sort_by_position(data: InferredType) -> Tuple[int, int, int, int]:
    start = data["location"]["start"]
    stop = data["location"]["stop"]
    return start["line"], start["column"], stop["line"], stop["column"]


class RawPyreData(TypedDict):
    path: str
    types: Sequence[InferredType]


def _process_pyre_data(data: RawPyreData) -> PyreData:
    return {"types": sorted(data["types"], key=_sort_by_position)}
