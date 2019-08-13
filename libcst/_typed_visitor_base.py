# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import TYPE_CHECKING, Any, Callable, TypeVar, cast


if TYPE_CHECKING:
    from libcst._typed_visitor import CSTTypedBaseFunctions  # noqa: F401

T = TypeVar("T")


def mark_no_op(
    f: Callable[["CSTTypedBaseFunctions", T], None]
) -> Callable[["CSTTypedBaseFunctions", T], None]:
    """
    Annotates stubs with a field to indicate they should not be collected
    by BatchableCSTVisitor.get_visitors() to reduce function call
    overhead when running a batched visitor pass.
    """

    cast(Any, f)._is_no_op = True
    return f
