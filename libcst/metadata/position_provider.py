# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Union

from libcst._nodes.internal import CodeRange
from libcst._nodes.module import _ModuleSelfT as _ModuleT
from libcst.metadata.base_provider import BaseMetadataProvider


_PositionProviderUnion = Union[
    "WhitespaceInclusivePositionProvider", "PositionProvider"
]


class WhitespaceInclusivePositionProvider(BaseMetadataProvider[CodeRange]):
    """
    Generates line and column metadata.

    The start and ending bounds of the positions produced by this provider include all
    whitespace owned by the node.
    """

    def _gen_impl(self, module: _ModuleT) -> None:
        module.code_for_node(module, provider=self)


class PositionProvider(WhitespaceInclusivePositionProvider):
    """
    Generates line and column metadata.

    These positions are defined by the start and ending bounds of a node ignoring most
    instances of leading and trailing whitespace when it is not syntactically
    significant.

    The positions provided by this provider should eventually match the positions used
    by Pyre_ for equivalent nodes.

    .. _Pyre: https://github.com/facebook/pyre-check
    """


# DEPRECATED: These names are here for backwards compatibility and will be removed in
# the future.
BasicPositionProvider = WhitespaceInclusivePositionProvider
SyntacticPositionProvider = PositionProvider
