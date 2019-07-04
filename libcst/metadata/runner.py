# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import TYPE_CHECKING

from libcst.exceptions import MetadataException


if TYPE_CHECKING:
    from libcst._base_visitor import CSTVisitor
    from libcst.nodes import Module


def run(module: "Module", visitor: "CSTVisitor") -> None:
    """
    Called by Module.visit to generate metadata dependencies before performing
    a visitor pass.
    """
    for Provider in visitor.METADATA_DEPENDENCIES:
        if Provider in module._remaining_dependencies:
            raise MetadataException(
                f"Detected circular dependency between {type(visitor).__name__} and {Provider.__name__}"
            )

        if Provider not in module._satisfied_dependencies:
            module._remaining_dependencies.add(Provider)
            Provider().run(module)
            module._satisfied_dependencies.add(Provider)
            module._remaining_dependencies.remove(Provider)
