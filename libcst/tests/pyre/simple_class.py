# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Sequence


class Item:
    def __init__(self, n: int):
        self.number = n


class ItemCollector:
    def get_items(self, n: int) -> Sequence[Item]:
        return [Item() for i in range(n)]


collector = ItemCollector()
items = collector.get_items()
for item in items:
    item.number
