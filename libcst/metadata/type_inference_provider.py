from typing import Optional

import libcst as cst
from libcst import BatchableMetadataProvider
from libcst.metadata import PositionProvider


class TypeInferenceProvider(BatchableMetadataProvider[str]):
    METADATA_DEPENDENCIES = (PositionProvider,)
    is_cache_required = True

    def __init__(self, cache: object):
        super().__init__(cache)
        lookup = {}
        for item in cache:
            location = item["location"]
            start = location["start"]
            end = location["stop"]
            lookup[(start["line"], start["column"], end["line"], end["column"])] = item[
                "annotation"
            ]
        self.lookup = lookup

    def _parse_metadata(self, node: cst.CSTNode) -> None:
        range = self.get_metadata(PositionProvider, node)
        key = (range.start.line, range.start.column, range.end.line, range.end.column)
        if key in self.lookup:
            self.set_metadata(node, self.lookup.pop(key))

    def visit_Name(self, node: cst.Name) -> Optional[bool]:
        self._parse_metadata(node)

    def visit_Attribute(self, node: cst.Attribute) -> Optional[bool]:
        self._parse_metadata(node)

    def visit_Call(self, node: cst.Call) -> Optional[bool]:
        self._parse_metadata(node)
