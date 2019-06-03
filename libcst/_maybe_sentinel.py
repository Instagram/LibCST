# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from enum import Enum, auto


class MaybeSentinel(Enum):
    """
    A MaybeSentinal value is used as the default constructor for some attributes to
    denote that on generating code we should optionally include this element in order
    to generate valid code.
    """

    DEFAULT = auto()

    def __repr__(self) -> str:
        return str(self)
