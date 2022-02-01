# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, FrozenSet, Mapping, Sequence

from libcst._parser.parso.utils import PythonVersionInfo

class BaseWhitespaceParserConfig:
    def __new__(
        cls,
        *,
        lines: Sequence[str],
        default_newline: str,
    ) -> BaseWhitespaceParserConfig: ...
    lines: Sequence[str]
    default_newline: str

class ParserConfig(BaseWhitespaceParserConfig):
    def __new__(
        cls,
        *,
        lines: Sequence[str],
        encoding: str,
        default_indent: str,
        default_newline: str,
        has_trailing_newline: bool,
        version: PythonVersionInfo,
        future_imports: FrozenSet[str],
    ) -> BaseWhitespaceParserConfig: ...
    # lines is inherited
    encoding: str
    default_indent: str
    # default_newline is inherited
    has_trailing_newline: bool
    version: PythonVersionInfo
    future_imports: FrozenSet[str]

def parser_config_asdict(config: ParserConfig) -> Mapping[str, Any]: ...
