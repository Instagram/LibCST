# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Dict, List, Optional

from mypy_extensions import TypedDict

import libcst as cst
from libcst._position import CodePosition, CodeRange
from libcst.metadata import PositionProvider
from libcst.metadata.base_provider import BatchableMetadataProvider


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


class TypeInferenceProvider(BatchableMetadataProvider[str]):
    METADATA_DEPENDENCIES = (PositionProvider,)
    is_cache_required = True

    def __init__(self, cache: List[InferredType]) -> None:
        super().__init__(cache)
        lookup: Dict[CodeRange, str] = {}
        for item in cache:
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
