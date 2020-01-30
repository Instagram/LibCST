# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from typing import Type

from libcst._types import CSTNodeT


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
            f"Expected a {nodetype.__name__} but got a {node.__class__.__name__}!"
        )
    return node
