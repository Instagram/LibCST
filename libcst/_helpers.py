# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Type

from libcst._visitors import CSTNodeT


def ensure_type(node: object, nodetype: Type[CSTNodeT]) -> CSTNodeT:
    if not isinstance(node, nodetype):
        raise Exception(
            f"Expected a {nodetype.__name__} bot got a {node.__class__.__name__}!"
        )
    return node
