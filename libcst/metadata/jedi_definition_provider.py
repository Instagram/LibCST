# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from threading import RLock
from typing import (
    cast,
    Dict,
    List,
    Mapping,
    NamedTuple,
    Optional,
    Protocol,
    Sequence,
    Tuple,
)

import libcst as cst
from libcst._position import CodePosition
from libcst.metadata.base_provider import VisitorMetadataProvider
from libcst.metadata.position_provider import PositionProvider


_JEDI_LOCK: RLock = RLock()


class JediDefinition(NamedTuple):
    """
    An immutable snapshot of a definition returned by Jedi.

    The type field is Jedi's definition kind (such as class or function),
    not a Python type annotation. The full_name, module_path, and position
    fields can be absent, notably for builtins. Positions use one-based
    lines and zero-based Unicode character columns.
    """

    name: str
    full_name: Optional[str]
    type: str
    module_name: str
    module_path: Optional[Path]
    position: Optional[CodePosition]


class _JediName(Protocol):
    name: str
    full_name: Optional[str]
    type: str
    module_name: str
    module_path: Optional[Path]
    line: Optional[int]
    column: Optional[int]


class _JediScript(Protocol):
    def infer(self, line: int, column: int) -> Sequence[_JediName]: ...


class _JediCache(NamedTuple):
    root_path: Path
    path: Path


class JediDefinitionProvider(VisitorMetadataProvider[Tuple[JediDefinition, ...]]):
    """
    Resolve definitions with the optional Jedi dependency.

    Install with pip install "libcst[jedi]". Uses Jedi's Script.infer for
    Name nodes and the final identifier of Attribute nodes. A Call whose
    function is a Name or Attribute receives the same definitions as its
    function. This describes the called definition, not the call's return type.
    Other call expressions do not receive metadata.

    Inference follows assignments and imports: a variable holding an integer
    may resolve to builtins.int, not its assignment. Results can be empty or
    contain several candidates. They are snapshots, not live Jedi objects or
    complete type annotations. Resolution and syntax support depend on Jedi,
    its environment, and the project's import paths.

    Use FullRepoManager to provide file paths, or pass a per-file result of
    gen_cache to MetadataWrapper. The cache contains only paths and can be
    passed between processes. Each metadata resolution analyzes the wrapper's
    current module text, including edits that have not been saved to disk.
    Imported modules are read by Jedi. Jedi is initialized lazily when resolving
    this provider; importing LibCST does not require it. The FullRepoManager
    timeout does not limit in-process Jedi inference.
    """

    METADATA_DEPENDENCIES = (PositionProvider,)

    @classmethod
    def gen_cache(
        cls,
        root_path: Path,
        paths: List[str],
        *,
        timeout: Optional[int] = None,
        use_pyproject_toml: bool = False,
    ) -> Mapping[str, _JediCache]:
        root = root_path.resolve()
        return {path: _JediCache(root, (root / path).resolve()) for path in paths}

    def __init__(self, cache: _JediCache) -> None:
        super().__init__(cache)
        self._root_path: Path = cache.root_path
        self._path: Path = cache.path
        self._script: Optional[_JediScript] = None
        self._definitions: Dict[CodePosition, Tuple[JediDefinition, ...]] = {}

    def _gen_impl(self, module: cst.Module) -> None:
        try:
            import jedi
        except ModuleNotFoundError as error:
            if error.name != "jedi":
                raise
            raise ImportError(
                'JediDefinitionProvider requires Jedi: install "libcst[jedi]".'
            ) from error
        # Jedi uses process-global caches and mutable parser state. Serialize
        # whole resolutions, including snapshots, across provider instances.
        with _JEDI_LOCK:
            self._definitions.clear()
            self._script = cast(
                _JediScript,
                jedi.Script(
                    code=module.code,
                    path=self._path,
                    project=jedi.Project(path=self._root_path),
                ),
            )
            try:
                module.visit(self)
            finally:
                self._script = None

    def _infer(self, name: cst.Name) -> Tuple[JediDefinition, ...]:
        # Query at the identifier's end, a cursor position within its token.
        # An Attribute's start instead points at its receiver.
        position = self.get_metadata(PositionProvider, name).end
        if position not in self._definitions:
            definitions: List[JediDefinition] = []
            script = self._script
            assert script is not None
            for definition in script.infer(position.line, position.column):
                location = (
                    CodePosition(definition.line, definition.column)
                    if definition.line is not None and definition.column is not None
                    else None
                )
                definitions.append(
                    JediDefinition(
                        name=definition.name,
                        full_name=definition.full_name,
                        type=definition.type,
                        module_name=definition.module_name,
                        module_path=definition.module_path,
                        position=location,
                    )
                )
            self._definitions[position] = tuple(definitions)
        return self._definitions[position]

    def visit_Name(self, node: cst.Name) -> None:
        self.set_metadata(node, self._infer(node))

    def visit_Attribute(self, node: cst.Attribute) -> None:
        self.set_metadata(node, self._infer(node.attr))

    def visit_Call(self, node: cst.Call) -> None:
        func = node.func
        if isinstance(func, cst.Name):
            self.set_metadata(node, self._infer(func))
        elif isinstance(func, cst.Attribute):
            self.set_metadata(node, self._infer(func.attr))
