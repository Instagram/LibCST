# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import TYPE_CHECKING, TypeVar


if TYPE_CHECKING:
    from libcst._nodes.base import CSTNode  # noqa: F401


CSTNodeT = TypeVar("CSTNodeT", bound="CSTNode")
