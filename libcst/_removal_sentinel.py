# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
"""
Used and re-exported by visitor. This is hoisted into a separate module to avoid some
circular dependencies in the definition of CSTNode.
"""

from enum import Enum, auto


class RemovalSentinel(Enum):
    """
    A RemovalSentinel value should be returned by a `on_leave` method when we want to
    remove that child from its parent.

    The parent's _visit_and_replace_children method should make a best-effort to remove
    the child, but may raise an exception when removing the child doesn't make sense, or
    could change the semantics in an unexpected way. E.g. a function with no name
    doesn't make sense.

    In we can't automatically remove the child, the developer should instead remove the
    child by constructing a new parent in the parent's on_leave call.

    We use this instead of `None` to force developers to be more explicit about
    deletions, because `None` is the default return value for a function with no return
    statement.

    In the future, we may extend this to support other forms of removal, but that's
    unlikely.
    """

    REMOVE = auto()
