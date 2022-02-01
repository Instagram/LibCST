# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class WhitespaceState:
    def __new__(
        cls, line: int, column: int, absolute_indent: str, is_parenthesized: bool
    ) -> WhitespaceState: ...
    line: int  # one-indexed (to match parso's behavior)
    column: int  # zero-indexed (to match parso's behavior)
    # What to look for when executing `_parse_indent`.
    absolute_indent: str
    is_parenthesized: bool
