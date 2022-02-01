# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Optional, Sequence, Union

from libcst._nodes.whitespace import (
    EmptyLine,
    Newline,
    ParenthesizedWhitespace,
    SimpleWhitespace,
    TrailingWhitespace,
)
from libcst._parser.types.config import BaseWhitespaceParserConfig as Config
from libcst._parser.types.whitespace_state import WhitespaceState as State

def parse_simple_whitespace(config: Config, state: State) -> SimpleWhitespace: ...
def parse_empty_lines(
    config: Config,
    state: State,
    *,
    override_absolute_indent: Optional[str] = None,
) -> Sequence[EmptyLine]: ...
def parse_trailing_whitespace(config: Config, state: State) -> TrailingWhitespace: ...
def parse_parenthesizable_whitespace(
    config: Config, state: State
) -> Union[SimpleWhitespace, ParenthesizedWhitespace]: ...
