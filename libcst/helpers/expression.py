# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from typing import Optional, Type, Union

import libcst as cst
from libcst._types import CSTNodeT


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


def ensure_type(node: object, nodetype: Type[CSTNodeT]) -> CSTNodeT:
    """
    Takes any python object, and a LibCST :class:`~libcst.CSTNode` subclass and
    refines the type of the python object. This is most useful when you already
    know that a particular object is a certain type but your type checker is not
    convinced. Note that this does an instance check for you and raises an
    exception if it is not the right type, so this should be used in situations
    where you are sure of the type given previous checks.
    """

    if not isinstance(node, nodetype):
        raise Exception(
            f"Expected a {nodetype.__name__} bot got a {node.__class__.__name__}!"
        )
    return node
