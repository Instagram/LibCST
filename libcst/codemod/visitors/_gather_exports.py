# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from typing import Set

import libcst
import libcst.matchers as m
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareVisitor
from libcst.helpers import ensure_type, get_full_name_for_node


class GatherExportsVisitor(ContextAwareVisitor):
    """
    Gathers all explicit exports in a module and stores them as attributes on the
    instance. Intended to be instantiated and passed to a :class:`~libcst.Module`
    :meth:`~libcst.CSTNode.visit` method in order to gather up information about
    exports specified in an ``__all__`` variable inside a module.

    After visiting a module the following attributes will be populated:

     explicit_exported_objects
      A sequence of strings representing objects that the module exports
      directly. Note that when ``__all__`` is absent, this attribute does not
      store default exported objects by name.

    For more information on ``__all__``, please see Python's `Modules Documentation
    <https://docs.python.org/3/tutorial/modules.html>`_.
    """

    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)
        # Track any re-exported objects in an __all__ reference
        self.explicit_exported_objects: Set[str] = set()

        # Presumably at some point in the future it would be useful to grab
        # a list of all implicitly exported objects. That would go here as
        # well and would follow Python's rule for importing objects that
        # do not start with an underscore. Because of that, I named the above
        # `explicit_exported_objects` instead of just `exported_objects` so
        # that we have a reasonable place to put implicit objects in the future.

        # Internal bookkeeping
        self._in_assignment: int = 0
        self._in_list: int = 0

    def visit_AnnAssign(self, node: libcst.AnnAssign) -> bool:
        target = get_full_name_for_node(node.target)
        if target == "__all__":
            self._in_assignment += 1
            return True
        return False

    def leave_AnnAssign(self, original_node: libcst.AnnAssign) -> None:
        self._in_assignment -= 1

    def visit_Assign(self, node: libcst.Assign) -> bool:
        for target_node in node.targets:
            target = get_full_name_for_node(target_node.target)
            if target == "__all__":
                self._in_assignment += 1
                return True
        return False

    def leave_Assign(self, original_node: libcst.Assign) -> None:
        self._in_assignment -= 1

    def visit_List(self, node: libcst.List) -> bool:
        self._in_list += 1
        # Only visit list/set entries when we're in an __all__
        # assignment. We gate also by internal counters, so this
        # is simply an optimization.
        return self._in_assignment == 1 and self._in_list == 1

    def leave_List(self, original_node: libcst.List) -> None:
        self._in_list -= 1

    def visit_Tuple(self, node: libcst.Tuple) -> bool:
        self._in_list += 1
        # Only visit list/set entries when we're in an __all__
        # assignment. We gate also by internal counters, so this
        # is simply an optimization.
        return self._in_assignment == 1 and self._in_list == 1

    def leave_Tuple(self, original_node: libcst.Tuple) -> None:
        self._in_list -= 1

    def visit_Set(self, node: libcst.Set) -> bool:
        # Only visit list/set entries when we're in an __all__
        # assignment. We gate also by internal counters, so this
        # is simply an optimization.
        self._in_list += 1
        return self._in_assignment == 1 and self._in_list == 1

    def leave_Set(self, original_node: libcst.Set) -> None:
        self._in_list -= 1

    def visit_Element(self, node: libcst.Element) -> bool:
        # See if this is a entry that is a string.
        extraction = self.extract(
            node, m.Element(m.SaveMatchedNode(m.SimpleString(), "string"))
        )
        if extraction:
            string = ensure_type(extraction["string"], libcst.SimpleString)
            self.explicit_exported_objects.add(string.evaluated_value)

        # Don't need to visit children
        return False
