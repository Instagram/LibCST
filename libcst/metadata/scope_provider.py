# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import abc
import builtins
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass
from typing import Dict, Iterator, List, MutableMapping, Optional, Tuple, Type, Union

import libcst as cst
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.expression_context_provider import (
    ExpressionContext,
    ExpressionContextProvider,
)


@dataclass(frozen=True)
class Access:
    node: cst.Name
    scope: "Scope"


class BaseAssignment(abc.ABC):
    name: str
    scope: "Scope"
    __accesses: List[Access]

    def __init__(self, name: str, scope: "Scope") -> None:
        self.name = name
        self.scope = scope
        self.__accesses = []

    def record_access(self, access: Access) -> None:
        self.__accesses.append(access)

    @property
    def accesses(self) -> Tuple[Access, ...]:
        # we don't want to publicly expose the mutable version of this
        return tuple(self.__accesses)


class Assignment(BaseAssignment):
    node: cst.CSTNode

    def __init__(self, name: str, scope: "Scope", node: cst.CSTNode) -> None:
        self.node = node
        super().__init__(name, scope)


class BuiltinAssignmemt(BaseAssignment):
    pass


class Scope(abc.ABC):
    parent: "Scope"
    globals: "GlobalScope"
    _assignments: MutableMapping[str, List[BaseAssignment]]

    def __init__(self, parent: "Scope") -> None:
        super().__init__()
        self.parent = parent
        self.globals = parent.globals
        self._assignments = defaultdict(list)

    def record_assignment(self, name: str, node: cst.CSTNode) -> None:
        self._assignments[name].append(Assignment(name=name, scope=self, node=node))

    def _getitem_from_self_or_parent(self, name: str) -> Tuple[BaseAssignment, ...]:
        """Overridden by ClassScope to hide it's assignments from child scopes."""
        return self[name]

    def _record_assignment_as_parent(self, name: str, node: cst.CSTNode) -> None:
        """Overridden by ClassScope to forward 'nonlocal' assignments from child scopes."""
        self.record_assignment(name, node)

    def __contains__(self, name: str) -> bool:
        return len(self[name]) > 0

    @abc.abstractmethod
    def __getitem__(self, name: str) -> Tuple[BaseAssignment, ...]:
        ...

    @abc.abstractmethod
    def record_global_overwrite(self, name: str) -> None:
        ...


class GlobalScope(Scope):
    def __init__(self) -> None:
        self.globals: Scope = self  # must be defined before Scope.__init__ is called
        super().__init__(parent=self)

    def __getitem__(self, name: str) -> Tuple[BaseAssignment, ...]:
        if hasattr(builtins, name):
            if not any(
                isinstance(i, BuiltinAssignmemt) for i in self._assignments[name]
            ):
                self._assignments[name].append(BuiltinAssignmemt(name, self))
        return tuple(self._assignments[name])

    def record_global_overwrite(self, name: str) -> None:
        pass


class LocalScope(Scope, abc.ABC):
    _scope_overwrites: Dict[str, Scope]

    def __init__(self, parent: Scope) -> None:
        super().__init__(parent)
        self._scope_overwrites = {}

    def record_nonlocal_overwrite(self, name: str) -> None:
        self._scope_overwrites[name] = self.parent

    def record_global_overwrite(self, name: str) -> None:
        self._scope_overwrites[name] = self.globals

    def record_assignment(self, name: str, node: cst.CSTNode) -> None:
        if name in self._scope_overwrites:
            self._scope_overwrites[name]._record_assignment_as_parent(name, node)
        else:
            super().record_assignment(name, node)

    def __getitem__(self, name: str) -> Tuple[BaseAssignment, ...]:
        if name in self._scope_overwrites:
            return self._scope_overwrites[name]._getitem_from_self_or_parent(name)
        if name in self._assignments:
            return tuple(self._assignments[name])
        else:
            return self.parent._getitem_from_self_or_parent(name)


class FunctionScope(LocalScope):
    pass


class ClassScope(LocalScope):
    def _record_assignment_as_parent(self, name: str, node: cst.CSTNode) -> None:
        """
        Forward the assignment to parent.

            def outer_fn():
                v = ...  # outer_fn's declaration
                class InnerCls:
                    v = ...  # shadows outer_fn's declaration
                    def inner_fn():
                        nonlocal v
                        v = ...  # this should actually refer to outer_fn's declaration
                                 # and not to InnerCls's, because InnerCls's scope is
                                 # hidden from its children.

        """
        self.parent._record_assignment_as_parent(name, node)

    def _getitem_from_self_or_parent(self, name: str) -> Tuple[BaseAssignment, ...]:
        """
        Class variables are only accessible using ClassName.attribute, cls.attribute, or
        self.attribute in child scopes. They cannot be accessed with their bare names.
        """
        return self.parent._getitem_from_self_or_parent(name)


class ComprehensionScope(LocalScope):
    """
    Comprehensions and generator expressions create their own scope. For example, in

        [i for i in range(10)]

    the variable `i` shouldn't leak outside the generator. This wasn't always true in
    py2, but it is in py3.

    Guido explains some of the scoping rules here:
    http://python-history.blogspot.com/2010/06/from-list-comprehensions-to-generator.html

    TODO: Assignment expressions (Python 3.8) will complicate ComprehensionScopes,
    and will require us to handle such assignments as non-local.
    https://www.python.org/dev/peps/pep-0572/#scope-of-the-target
    """

    pass


class ScopeVisitor(cst.CSTVisitor):
    def __init__(self, provider: "ScopeProvider") -> None:
        self.provider: ScopeProvider = provider
        self.scope: Scope = GlobalScope()
        self.__deferred_accesses: List[Access] = []

    @contextmanager
    def _new_scope(self, kind: Type[Scope]) -> Iterator[None]:
        parent_scope = self.scope
        self.scope = kind(parent_scope)
        try:
            yield
        finally:
            self.scope = parent_scope

    @contextmanager
    def _switch_scope(self, scope: Scope) -> Iterator[None]:
        current_scope = self.scope
        self.scope = scope
        try:
            yield
        finally:
            self.scope = current_scope

    def _visit_import_alike(self, node: Union[cst.Import, cst.ImportFrom]) -> bool:
        names = node.names
        if not isinstance(names, cst.ImportStar):
            # make sure node.names is Sequence[ImportAlias]
            for name in names:
                if name.asname is not None:
                    name_value = cst.ensure_type(name.asname.name, cst.Name).value
                else:
                    name_node = name.name
                    while isinstance(name_node, cst.Attribute):
                        # the value of Attribute in import alike can only be either Name or Attribute
                        name_node = name_node.value
                    if isinstance(name_node, cst.Name):
                        name_value = name_node.value
                    else:
                        raise Exception(
                            f"Unexpected ImportAlias name value: {name_node}"
                        )

                self.scope.record_assignment(name_value, node)
        return False

    def visit_Import(self, node: cst.Import) -> Optional[bool]:
        return self._visit_import_alike(node)

    def visit_ImportFrom(self, node: cst.ImportFrom) -> Optional[bool]:
        return self._visit_import_alike(node)

    def visit_Name(self, node: cst.Name) -> Optional[bool]:
        context = self.provider.get_metadata(ExpressionContextProvider, node)
        if context == ExpressionContext.STORE:
            self.scope.record_assignment(node.value, node)
        elif context == ExpressionContext.LOAD:
            self.__deferred_accesses.append(Access(node, self.scope))

    def visit_FunctionDef(self, node: cst.FunctionDef) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        with self._new_scope(FunctionScope):
            node.params.visit(self)
            node.body.visit(self)

        for decorator in node.decorators:
            decorator.visit(self)
        if node.returns:
            node.returns.visit(self)
        return False

    def visit_Lambda(self, node: cst.Lambda) -> Optional[bool]:
        with self._new_scope(FunctionScope):
            node.params.visit(self)
            node.body.visit(self)
        return False

    def visit_Param(self, node: cst.Param) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        with self._switch_scope(self.scope.parent):
            for field in [node.default, node.annotation]:
                if field:
                    field.visit(self)
        return False

    def visit_Arg_keyword(self, node: cst.Arg) -> None:
        if node.keyword is not None:
            self.scope.record_assignment(node.keyword.value, node)

    def visit_ClassDef(self, node: cst.ClassDef) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        for decorator in node.decorators:
            decorator.visit(self)
        for base in node.bases:
            base.visit(self)
        for keyword in node.keywords:
            keyword.visit(self)

        with self._new_scope(ClassScope):
            for statement in node.body.body:
                statement.visit(self)
        return False

    def visit_Global(self, node: cst.Global) -> Optional[bool]:
        for name_item in node.names:
            self.scope.record_global_overwrite(name_item.name.value)
        return False

    def infer_accesses(self) -> None:
        for access in self.__deferred_accesses:
            for assignment in access.scope[access.node.value]:
                assignment.record_access(access)
        self.__deferred_accesses = []

    def on_leave(self, original_node: cst.CSTNode) -> None:
        # will this always been called? What if the node wasn't visited? This should be tested.
        # ideas: a metadata provider for parent node.
        self.provider.set_metadata(original_node, self.scope)
        super().on_leave(original_node)


class ScopeProvider(BatchableMetadataProvider[Optional[Scope]]):
    METADATA_DEPENDENCIES = (ExpressionContextProvider,)

    def __init__(self) -> None:
        super().__init__()

    def visit_Module(self, node: cst.Module) -> Optional[bool]:
        visitor = ScopeVisitor(self)
        node.visit(visitor)
        visitor.infer_accesses()
