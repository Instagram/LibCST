# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Iterator, Optional, Tuple

from libcst_native import token_type, whitespace_state

class Token:
    def __new__(
        cls,
        type: token_type.TokenType,
        string: str,
        start_pos: Tuple[int, int],
        end_pos: Tuple[int, int],
        whitespace_before: whitespace_state.WhitespaceState,
        whitespace_after: whitespace_state.WhitespaceState,
        relative_indent: Optional[str],
    ) -> Token: ...
    type: token_type.TokenType
    string: str
    start_pos: Tuple[int, int]
    end_pos: Tuple[int, int]
    whitespace_before: whitespace_state.WhitespaceState
    whitespace_after: whitespace_state.WhitespaceState
    relative_indent: Optional[str]

def tokenize(text: str) -> Iterator[Token]: ...
