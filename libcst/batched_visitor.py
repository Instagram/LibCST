# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import inspect
from typing import Callable, Iterable, List, Mapping, MutableMapping, Optional

import libcst.nodes as cst
from libcst.matchers import CSTMatchers
from libcst.metadata._interface import _MetadataInterface
from libcst.visitors import CSTNodeT, CSTVisitor


VisitorMethod = Callable[[cst.CSTNode], None]
_VisitorMethodCollection = Mapping[str, List[VisitorMethod]]


class BatchableCSTVisitor(CSTMatchers, _MetadataInterface):
    """
    Extend this class for each type of batched operation you want to perform.
    """

    def get_visitors(self) -> Mapping[str, VisitorMethod]:
        """
        Returns a mapping of all the visit_* and leave_* methods defined by
        this visitor, excluding all empty stubs.
        """

        methods = inspect.getmembers(
            self,
            lambda m: (
                inspect.ismethod(m)
                and (m.__name__.startswith("visit_") or m.__name__.startswith("leave_"))
                and not getattr(m, "_is_no_op", False)
            ),
        )

        # TODO: verify all visitor methods reference valid node classes.
        # for name, __ in methods:
        #     pass

        return dict(methods)


def visit(
    module: cst.CSTNode,
    visitors: Iterable[BatchableCSTVisitor],
    before_visit: Optional[VisitorMethod] = None,
    after_leave: Optional[VisitorMethod] = None,
) -> None:
    """
    Create and run a batched visitor composed of [visitors] over [module].

    [before_visit] and [after_leave] are provided as optional hooks to
    execute before visit_* and after leave_* methods are executed by the
    batched visitor.
    """

    visitor_methods = _get_visitor_methods(visitors)
    batched_visitor = _BatchedCSTVisitor(
        visitor_methods, before_visit=before_visit, after_leave=after_leave
    )
    module.visit(batched_visitor)


def _get_visitor_methods(
    batchable_visitors: Iterable[BatchableCSTVisitor]
) -> _VisitorMethodCollection:
    visitor_methods: MutableMapping[str, List[VisitorMethod]] = {}
    for bv in batchable_visitors:
        for name, fn in bv.get_visitors().items():
            visitor_methods.setdefault(name, []).append(fn)
    return visitor_methods


class _BatchedCSTVisitor(CSTVisitor, _MetadataInterface):

    visitor_methods: _VisitorMethodCollection
    before_visit: Optional[VisitorMethod]
    after_leave: Optional[VisitorMethod]

    def __init__(
        self,
        visitor_methods: _VisitorMethodCollection,
        *,
        before_visit: Optional[VisitorMethod] = None,
        after_leave: Optional[VisitorMethod] = None,
    ) -> None:
        self.visitor_methods = visitor_methods
        self.before_visit = before_visit
        self.after_leave = after_leave

    def on_visit(self, node: CSTNodeT) -> bool:
        """
        Call appropriate visit methods on node before visiting children.
        """
        if self.before_visit is not None:
            self.before_visit(node)
        type_name = type(node).__name__
        for v in self.visitor_methods.get(f"visit_{type_name}", []):
            v(node)
        return True

    def on_leave(self, original_node: CSTNodeT) -> None:
        """
        Call appropriate leave methods on node after visiting children.
        """
        type_name = type(original_node).__name__
        for v in self.visitor_methods.get(f"leave_{type_name}", []):
            v(original_node)
        if self.after_leave is not None:
            self.after_leave(original_node)
