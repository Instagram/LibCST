# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Generic, TypeVar

import libcst.nodes as cst
from libcst.visitors import CSTVisitor


_T_co = TypeVar("_T_co", covariant=True)


class BaseMetadataProvider(CSTVisitor, Generic[_T_co]):
    """
    Base class for visitor-based metadata providers.
    """

    def run(self, module: cst.Module) -> None:
        """
        Convenience method to run metadata provider over a module.
        """
        module.visit(self)

    @classmethod
    # pyre-ignore[35]: Parameter type cannot be covariant. Pyre can't
    # detect that this method is not mutating the Provider class.
    def set_metadata(cls, node: cst.CSTNode, value: _T_co) -> None:
        node._metadata[cls] = value
