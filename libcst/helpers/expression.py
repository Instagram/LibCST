# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from typing import Optional, Union

import libcst as cst


def get_full_name_for_node(node: Union[str, cst.CSTNode]) -> Optional[str]:
    """Return a dot concatenated full name for str, :class:`~libcst.Name`, :class:`~libcst.Attribute`.
    :class:`~libcst.Call`, :class:`~libcst.Subscript`, :class:`~libcst.FunctionDef`, :class:`~libcst.ClassDef`.
    Return ``None`` for not supported Node.
    """
    if isinstance(node, cst.Name):
        return node.value
    elif isinstance(node, str):
        return node
    elif isinstance(node, cst.Attribute):
        return f"{get_full_name_for_node(node.value)}.{node.attr.value}"
    elif isinstance(node, cst.Call):
        return get_full_name_for_node(node.func)
    elif isinstance(node, cst.Subscript):
        return get_full_name_for_node(node.value)
    elif isinstance(node, (cst.FunctionDef, cst.ClassDef)):
        return get_full_name_for_node(node.name)
    return None
