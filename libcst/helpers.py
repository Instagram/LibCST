# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import libcst.nodes as libcst


def get_fully_qualified_name(node: libcst.BaseExpression) -> str:
    if isinstance(node, libcst.Name):
        return node.value
    elif isinstance(node, libcst.Attribute):
        return get_fully_qualified_name(node.value) + "." + node.attr.value
    else:
        raise Exception(f"Invalid node type {type(node)}!")
