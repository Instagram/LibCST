# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Union

from libcst._nodes.internal import CodeRange
from libcst._nodes.module import _ModuleSelfT as _ModuleT
from libcst.metadata.base_provider import BaseMetadataProvider


PositionProvider = Union["BasicPositionProvider", "SyntacticPositionProvider"]


class BasicPositionProvider(BaseMetadataProvider[CodeRange]):
    """
    Generates basic line and column metadata. Basic position is defined by the
    start and ending bounds of a node including all whitespace owned by that node.
    """

    def _gen_impl(self, module: _ModuleT) -> None:
        module.code_for_node(module, provider=self)


class SyntacticPositionProvider(BasicPositionProvider):
    """
    Generates syntactic line and column metadata. Syntactic position is defined
    by the start and ending bounds of a node ignoring most instances of leading
    and trailing whitespace when it is not syntactically significant.
    """
