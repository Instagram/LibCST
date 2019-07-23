# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Type, TypeVar

import libcst.nodes as libcst


def get_fully_qualified_name(node: libcst.BaseExpression) -> str:
    if isinstance(node, libcst.Name):
        return node.value
    elif isinstance(node, libcst.Attribute):
        return get_fully_qualified_name(node.value) + "." + node.attr.value
    else:
        raise Exception(f"Invalid node type {type(node)}!")


_CSTNodeT = TypeVar("_CSTNodeT", bound=libcst.CSTNode)


def ensure_type(node: object, nodetype: Type[_CSTNodeT]) -> _CSTNodeT:
    if not isinstance(node, nodetype):
        raise Exception(
            f"Expected a {nodetype.__name__} bot got a {node.__class__.__name__}!"
        )
    return node
