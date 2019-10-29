# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict

import textwrap
from contextlib import ExitStack
from typing import (
    TYPE_CHECKING,
    Collection,
    Iterable,
    Mapping,
    MutableMapping,
    Optional,
    Type,
    TypeVar,
    cast,
)

from libcst._batched_visitor import BatchableCSTVisitor, VisitorMethod, visit_batched
from libcst.metadata._resolver import _resolve_impl


if TYPE_CHECKING:
    from libcst._nodes.base import CSTNode  # noqa: F401
    from libcst._nodes.module import Module  # noqa: F401
    from libcst.metadata.base_provider import (  # noqa: F401
        BaseMetadataProvider,
        ProviderT,
    )
    from libcst._visitors import CSTVisitorT  # noqa: F401


_T = TypeVar("_T")


class MetadataWrapper:
    """
    A wrapper around a :class:`~libcst.Module` that stores associated metadata
    for that module.

    When a :class:`MetadataWrapper` is constructed over a module, the wrapper will
    store a deep copy of the original module. This means
    ``MetadataWrapper(module).module == module`` is ``False``.

    This copying operation ensures that a node will never appear twice (by identity) in
    the same tree. This allows us to uniquely look up metadata for a node based on a
    node's identity.
    """

    __slots__ = ["__module", "_metadata"]

    __module: "Module"
    _metadata: MutableMapping["ProviderT", Mapping["CSTNode", object]]

    def __init__(self, module: "Module", unsafe_skip_copy: bool = False) -> None:
        """
        :param module: The module to wrap. This is deeply copied by default.
        :param unsafe_skip_copy: When true, this skips the deep cloning of the module.
            This can provide a small performance benefit, but you should only use this
            if you know that there are no duplicate nodes in your tree (e.g. this
            module came from the parser).
        """
        # Ensure that module is safe to use by copying the module to remove
        # any duplicate nodes.
        if not unsafe_skip_copy:
            module = module.deep_clone()
        self.__module = module
        self._metadata = {}

    def __repr__(self) -> str:
        return f"MetadataWrapper(\n{textwrap.indent(repr(self.module), ' ' * 4)},\n)"

    @property
    def module(self) -> "Module":
        """
        The module that's wrapped by this MetadataWrapper. By default, this is a deep
        copy of the passed in module.

        ::

            mw = ModuleWrapper(module)
            # Because `mw.module is not module`, you probably want to do visit and do
            # your analysis on `mw.module`, not `module`.
            mw.module.visit(DoSomeAnalysisVisitor)
        """
        # use a property getter to enforce that this is a read-only variable
        return self.__module

    def resolve(
        self, provider: Type["BaseMetadataProvider[_T]"]
    ) -> Mapping["CSTNode", _T]:
        """
        Returns a copy of the metadata mapping computed by ``provider``.
        """
        if provider in self._metadata:
            metadata = self._metadata[provider]
        else:
            metadata = self.resolve_many([provider])[provider]

        # pyre-ignore Pyre doesn't recognize "CSTNode" in this contxt.
        return cast(Mapping["CSTNode", _T], metadata)

    def resolve_many(
        self, providers: Collection["ProviderT"]
    ) -> Mapping["ProviderT", Mapping["CSTNode", object]]:
        """
        Returns a copy of the map of metadata mapping computed by each provider
        in ``providers``.

        The returned map does not contain any metadata from undeclared metadata
        dependencies that ``providers`` has.
        """
        _resolve_impl(self, providers)

        # Only return what what declared in providers
        return {k: self._metadata[k] for k in providers}

    def visit(self, visitor: "CSTVisitorT") -> "Module":
        """
        Convenience method to resolve metadata before performing a traversal over
        ``self.module`` with ``visitor``. See :func:`~libcst.Module.visit`.
        """
        with visitor.resolve(self):
            return self.module.visit(visitor)

    def visit_batched(
        self,
        visitors: Iterable[BatchableCSTVisitor],
        before_visit: Optional[VisitorMethod] = None,
        after_leave: Optional[VisitorMethod] = None,
    ) -> "CSTNode":
        """
        Convenience method to resolve metadata before performing a traversal over
        ``self.module`` with ``visitors``. See :func:`~libcst.visit_batched`.
        """
        with ExitStack() as stack:
            # Resolve dependencies of visitors
            for v in visitors:
                stack.enter_context(v.resolve(self))

            return visit_batched(self.module, visitors, before_visit, after_leave)
