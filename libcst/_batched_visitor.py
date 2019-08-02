# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import inspect
from typing import (
    TYPE_CHECKING,
    Callable,
    Collection,
    Iterable,
    List,
    Mapping,
    MutableMapping,
    Optional,
    cast,
)

from libcst._typed_visitor_base import CSTTypedVisitorFunctions
from libcst._visitors import CSTNodeT, CSTVisitor
from libcst.metadata.dependent import _MetadataDependent


if TYPE_CHECKING:
    from libcst._nodes._base import CSTNode  # noqa: F401
    from libcst.metadata.base_provider import ProviderT  # noqa: F401


VisitorMethod = Callable[["CSTNode"], None]
_VisitorMethodCollection = Mapping[str, List[VisitorMethod]]


class BatchableCSTVisitor(CSTTypedVisitorFunctions, _MetadataDependent):
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


def visit_batched(
    node: CSTNodeT,
    visitors: Iterable[BatchableCSTVisitor],
    before_visit: Optional[VisitorMethod] = None,
    after_leave: Optional[VisitorMethod] = None,
    use_compatible: bool = True,  # TODO: remove this
) -> CSTNodeT:
    """
    Returns the result of running all visitors [visitors] over [node].

    [before_visit] and [after_leave] are provided as optional hooks to
    execute before visit_* and after leave_* methods are executed by the
    batched visitor.
    """
    # TODO: remove compatiblity hack
    if use_compatible:
        from libcst._nodes._module import Module

        if isinstance(node, Module):
            from contextlib import ExitStack
            from libcst.metadata.wrapper import MetadataWrapper

            wrapper = MetadataWrapper(node)
            with ExitStack() as stack:
                # Resolve dependencies of visitors
                for v in visitors:
                    stack.enter_context(v.resolve(wrapper))

                batched_visitor = make_batched(visitors, before_visit, after_leave)
                return cast(
                    CSTNodeT,
                    wrapper.module.visit(batched_visitor, use_compatible=False),
                )

        batched_visitor = make_batched(visitors, before_visit, after_leave)
        return cast(CSTNodeT, node.visit(batched_visitor))
    # end compatible

    batched_visitor = make_batched(visitors, before_visit, after_leave)
    return cast(CSTNodeT, node.visit(batched_visitor, use_compatible=False))


def _get_visitor_methods(
    batchable_visitors: Iterable[BatchableCSTVisitor]
) -> _VisitorMethodCollection:
    visitor_methods: MutableMapping[str, List[VisitorMethod]] = {}
    for bv in batchable_visitors:
        for name, fn in bv.get_visitors().items():
            visitor_methods.setdefault(name, []).append(fn)
    return visitor_methods


def _get_visitor_dependencies(
    batchable_visitors: Iterable[BatchableCSTVisitor]
) -> Collection["ProviderT"]:
    dependencies = set()
    for visitor in batchable_visitors:
        dependencies.update(visitor.METADATA_DEPENDENCIES)

    return dependencies


def make_batched(
    visitors: Iterable[BatchableCSTVisitor],
    before_visit: Optional[VisitorMethod] = None,
    after_leave: Optional[VisitorMethod] = None,
) -> "_BatchedCSTVisitor":
    """
    Make a batched visitor out of [visitors].

    [before_visit] and [after_leave] are provided as optional hooks to
    execute before visit_* and after leave_* methods are executed by the
    batched visitor.
    """
    visitor_methods = _get_visitor_methods(visitors)
    return _BatchedCSTVisitor(
        visitor_methods, before_visit=before_visit, after_leave=after_leave
    )


class _BatchedCSTVisitor(CSTVisitor):

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
        super().__init__()
        self.visitor_methods = visitor_methods
        self.before_visit = before_visit
        self.after_leave = after_leave

    def on_visit(self, node: "CSTNode") -> bool:
        """
        Call appropriate visit methods on node before visiting children.
        """
        if self.before_visit is not None:
            self.before_visit(node)
        type_name = type(node).__name__
        for v in self.visitor_methods.get(f"visit_{type_name}", []):
            v(node)
        return True

    def on_leave(self, original_node: "CSTNode") -> None:
        """
        Call appropriate leave methods on node after visiting children.
        """
        type_name = type(original_node).__name__
        for v in self.visitor_methods.get(f"leave_{type_name}", []):
            v(original_node)
        if self.after_leave is not None:
            self.after_leave(original_node)
