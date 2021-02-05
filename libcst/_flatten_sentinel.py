# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys


# PEP 585
if sys.version_info < (3, 9):
    from typing import Iterable, Sequence
else:
    from collections.abc import Iterable, Sequence

from libcst._types import CSTNodeT_co


class FlattenSentinel(Sequence[CSTNodeT_co]):
    nodes: Sequence[CSTNodeT_co]

    def __init__(self, nodes: Iterable[CSTNodeT_co]) -> None:
        self.nodes = tuple(nodes)

    def __getitem__(self, idx: int) -> CSTNodeT_co:
        return self.nodes[idx]

    def __len__(self) -> int:
        return len(self.nodes)
