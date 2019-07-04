# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import libcst.nodes as cst
from libcst.metadata.base_provider import BaseMetadataProvider
from libcst.nodes._internal import CodeRange


class BasicPositionProvider(BaseMetadataProvider[CodeRange]):
    """
    Generates basic line and column metadata. Basic position is
    defined by the start and ending bounds of a node including all whitespace
    owned by that node.
    """

    def run(self, module: cst.Module) -> None:
        """
        Override default generate behavior as position information is
        calculated through codegen instead of a standard visitor.
        """
        module.code_for_node(module, provider=self.__class__)


class SyntacticPositionProvider(BasicPositionProvider):
    """
    Generates Syntactic line and column metadata. Syntactic position is
    defined by the start and ending bounds of a node ignoring most instances
    of leading and trailing whitespace when it is not syntactically significant.
    """
