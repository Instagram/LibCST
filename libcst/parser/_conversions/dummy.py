# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, List, Sequence, Union

import libcst.nodes as cst
from libcst.nodes._base import CSTNode
from libcst.parser._types.config import ParserConfig
from libcst.parser._types.partials import WithLeadingWhitespace
from libcst.parser._types.token import Token
from libcst.parser._whitespace_parser import parse_parenthesizable_whitespace


def make_dummy_node(config: ParserConfig, children: Sequence[Any]) -> Any:
    wrapped_children: List[Union[CSTNode, str]] = []

    for i, child in enumerate(children):
        if isinstance(child, Token):
            if i > 0:
                # Leading whitespace for dummy is owned by the parent, so only
                # add raw whitespace if this is isn't the first node.
                wrapped_children.append(
                    parse_parenthesizable_whitespace(config, child.whitespace_before)
                )
            # Add ourselves unconditionally.
            wrapped_children.append(child.string)
        elif isinstance(child, WithLeadingWhitespace):
            if i > 0:
                # Leading whitespace for dummy is owned by the parent, so only
                # add parsed whitespace if this isn't the first node.
                wrapped_children.append(
                    parse_parenthesizable_whitespace(config, child.whitespace_before)
                )
            # Add ourselves unconditionally.
            wrapped_children.append(child.value)
        else:
            wrapped_children.append(child)

    if hasattr(children[0], "whitespace_before"):
        return WithLeadingWhitespace(
            cst.DummyNode(children=wrapped_children), children[0].whitespace_before
        )
    else:
        return cst.DummyNode(children=wrapped_children)
