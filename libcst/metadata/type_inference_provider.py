# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Dict, Optional, Sequence

from mypy_extensions import TypedDict

import libcst as cst
from libcst._position import CodePosition, CodeRange
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.position_provider import PositionProvider


class Position(TypedDict):
    line: int
    column: int


class Location(TypedDict):
    path: str
    start: Position
    stop: Position


class InferredType(TypedDict):
    location: Location
    annotation: str


class Position(TypedDict):
    line: int
    column: int


class Location(TypedDict):
    path: str
    start: Position
    stop: Position


class InferredType(TypedDict):
    location: Location
    annotation: str


class PyreData(TypedDict):
    types: Sequence[InferredType]


class TypeInferenceProvider(BatchableMetadataProvider[str]):
    """
    Access inferred type annotation through `Pyre Query API <https://pyre-check.org/docs/querying-pyre.html>`_.
    It requires `setup watchman <https://pyre-check.org/docs/watchman-integration.html>`_
    and start pyre server by running ``pyre`` command.
    The inferred type is a string of `type annotation <https://docs.python.org/3/library/typing.html>`_.
    E.g. ``typing.List[libcst._nodes.expression.Name]``
    is the inferred type of name ``n`` in expression ``n = [cst.Name("")]``.
    All name references use the fully qualified name regardless how the names are imported.
    (e.g. ``import libcst; libcst.Name`` and ``import libcst as cst; cst.Name`` refer to the same name.)
    Pyre infers the type of :class:`~libcst.Name`, :class:`~libcst.Attribute` and :class:`~libcst.Call` nodes.
    The inter process communication to Pyre server is managed by :class:`~libcst.metadata.FullRepoManager`.
    """

    METADATA_DEPENDENCIES = (PositionProvider,)
    is_cache_required = True

    def __init__(self, cache: PyreData) -> None:
        super().__init__(cache)
        lookup: Dict[CodeRange, str] = {}
        for item in cache["types"]:
            location = item["location"]
            start = location["start"]
            end = location["stop"]
            lookup[
                CodeRange(
                    start=CodePosition(start["line"], start["column"]),
                    end=CodePosition(end["line"], end["column"]),
                )
            ] = item["annotation"]
        self.lookup: Dict[CodeRange, str] = lookup

    def _parse_metadata(self, node: cst.CSTNode) -> None:
        range = self.get_metadata(PositionProvider, node)
        if range in self.lookup:
            self.set_metadata(node, self.lookup.pop(range))

    def visit_Name(self, node: cst.Name) -> Optional[bool]:
        self._parse_metadata(node)

    def visit_Attribute(self, node: cst.Attribute) -> Optional[bool]:
        self._parse_metadata(node)

    def visit_Call(self, node: cst.Call) -> Optional[bool]:
        self._parse_metadata(node)
