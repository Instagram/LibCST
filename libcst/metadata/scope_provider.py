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
from libcst._add_slots import add_slots
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.expression_context_provider import (
    ExpressionContext,
    ExpressionContextProvider,
)


@add_slots
@dataclass(frozen=True)
class Access:
    """
    An Access records an access of an assignment.
    """

    #: The name node of the access. A name is an access when the expression context is
    #: :attr:`ExpressionContext.LOAD`.
    node: cst.Name

    #: The scope of the access. Note that a access could be in a child scope of its assignment.
    scope: "Scope"


class BaseAssignment(abc.ABC):
    """Abstract base class of :class:`Assignment` and :class:`BuitinAssignment`."""

    #: The name of assignment.
    name: str

    #: The scope associates to assignment.
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
        """Return all accesses of the assignment."""
        # we don't want to publicly expose the mutable version of this
        return tuple(self.__accesses)


class Assignment(BaseAssignment):
    """An assignment records the name, CSTNode and its accesses."""

    #: The node of assignment, it could be a :class:`~libcst.Import`, :class:`~libcst.ImportFrom`,
    #: :class:`~libcst.Name`, :class:`~libcst.FunctionDef`, or :class:`~libcst.ClassDef`.
    node: cst.CSTNode

    def __init__(self, name: str, scope: "Scope", node: cst.CSTNode) -> None:
        self.node = node
        super().__init__(name, scope)


class BuiltinAssignment(BaseAssignment):
    """
    A BuiltinAssignment represents an value provide by Python as a builtin, including
    `functions <https://docs.python.org/3/library/functions.html>`_,
    `constants <https://docs.python.org/3/library/constants.html>`_, and
    `types <https://docs.python.org/3/library/stdtypes.html>`_.
    """

    pass


class Scope(abc.ABC):
    """
    Base class of all scope classes. Scope object stores assignments from imports,
    variable assignments, function definition or class definition.
    A scope has a parent scope which represents the inheritance relationship. That means
    an assignment in parent scope is viewable to the child scope and the child scope may
    overwrites the assignment by using the same name.
    Use ``name in scope`` to check whether a name is viewable in the scope.
    Use ``scope[name]`` to retrieve all viewable assignments in the scope.
    """

    #: Parent scope. Note the parent scope of a GlobalScope is itself.
    parent: "Scope"

    #: Refers to the GlobalScope.
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

    @abc.abstractmethod
    def record_nonlocal_overwrite(self, name: str) -> None:
        ...


class GlobalScope(Scope):
    """
    A GlobalScope is the scope of module. All module level assignments are recorded in GlobalScope.
    """

    def __init__(self) -> None:
        self.globals: Scope = self  # must be defined before Scope.__init__ is called
        super().__init__(parent=self)

    def __getitem__(self, name: str) -> Tuple[BaseAssignment, ...]:
        if hasattr(builtins, name):
            if not any(
                isinstance(i, BuiltinAssignment) for i in self._assignments[name]
            ):
                self._assignments[name].append(BuiltinAssignment(name, self))
        return tuple(self._assignments[name])

    def record_global_overwrite(self, name: str) -> None:
        pass

    def record_nonlocal_overwrite(self, name: str) -> None:
        raise NotImplementedError("nonlocal declaration not allowed at module level")


class LocalScope(Scope, abc.ABC):
    _scope_overwrites: Dict[str, Scope]

    def __init__(self, parent: Scope) -> None:
        super().__init__(parent)
        self._scope_overwrites = {}

    def record_global_overwrite(self, name: str) -> None:
        self._scope_overwrites[name] = self.globals

    def record_nonlocal_overwrite(self, name: str) -> None:
        self._scope_overwrites[name] = self.parent

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
    """
    When a function is defined, it creates a FunctionScope.
    """

    pass


class ClassScope(LocalScope):
    """
    When a class is defined, it creates a ClassScope.
    """

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

    The variable ``i`` is only viewable within the ComprehensionScope.
    """

    # TODO: Assignment expressions (Python 3.8) will complicate ComprehensionScopes,
    # and will require us to handle such assignments as non-local.
    # https://www.python.org/dev/peps/pep-0572/#scope-of-the-target
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

        # visit remaining attributes
        if isinstance(node, cst.Import):
            remaining_attrs = [node.semicolon, node.whitespace_after_import]
        else:
            remaining_attrs = [node.semicolon, node.whitespace_after_import]

        for attr in remaining_attrs:
            if isinstance(attr, cst.CSTNode):
                attr.visit(self)

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
        self.provider.set_metadata(node.name, self.scope)

        with self._new_scope(FunctionScope):
            node.params.visit(self)
            node.body.visit(self)

            # visit remaining attributes
            for attr in [
                node.asynchronous,
                node.leading_lines,
                node.lines_after_decorators,
                node.whitespace_after_def,
                node.whitespace_after_name,
                node.whitespace_before_params,
                node.whitespace_before_colon,
            ]:
                if isinstance(attr, cst.CSTNode):
                    attr.visit(self)

        for decorator in node.decorators:
            decorator.visit(self)
        if node.returns:
            node.returns.visit(self)

        return False

    def visit_Lambda(self, node: cst.Lambda) -> Optional[bool]:
        with self._new_scope(FunctionScope):
            node.params.visit(self)
            node.body.visit(self)

            # visit remaining attributes
            for attr in [
                node.colon,
                node.lpar,
                node.rpar,
                node.whitespace_after_lambda,
            ]:
                if isinstance(attr, cst.CSTNode):
                    attr.visit(self)
        return False

    def visit_Param(self, node: cst.Param) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        self.provider.set_metadata(node.name, self.scope)
        with self._switch_scope(self.scope.parent):
            for field in [node.default, node.annotation]:
                if field:
                    field.visit(self)

        # visit remaining attributes
        for attr in [
            node.equal,
            node.comma,
            node.star,
            node.whitespace_after_star,
            node.whitespace_after_param,
        ]:
            if isinstance(attr, cst.CSTNode):
                attr.visit(self)
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

            # visit remaining attributes
            for attr in [
                node.lpar,
                node.rpar,
                node.leading_lines,
                node.lines_after_decorators,
                node.whitespace_after_class,
                node.whitespace_after_name,
                node.whitespace_before_colon,
            ]:
                if isinstance(attr, cst.CSTNode):
                    attr.visit(self)
        return False

    def visit_Global(self, node: cst.Global) -> Optional[bool]:
        for name_item in node.names:
            self.scope.record_global_overwrite(name_item.name.value)

        # visit remaining attributes
        for attr in [node.whitespace_after_global, node.semicolon]:
            if isinstance(attr, cst.CSTNode):
                attr.visit(self)
        return False

    def visit_Nonlocal(self, node: cst.Nonlocal) -> Optional[bool]:
        for name_item in node.names:
            self.scope.record_nonlocal_overwrite(name_item.name.value)

        # visit remaining attributes
        for attr in [node.whitespace_after_nonlocal, node.semicolon]:
            if isinstance(attr, cst.CSTNode):
                attr.visit(self)
        return False

    def visit_ListComp(self, node: cst.ListComp) -> Optional[bool]:
        return self._visit_comp_alike(node)

    def visit_SetComp(self, node: cst.SetComp) -> Optional[bool]:
        return self._visit_comp_alike(node)

    def visit_DictComp(self, node: cst.DictComp) -> Optional[bool]:
        return self._visit_comp_alike(node)

    def visit_GeneratorExp(self, node: cst.GeneratorExp) -> Optional[bool]:
        return self._visit_comp_alike(node)

    def _visit_comp_alike(
        self, node: Union[cst.ListComp, cst.SetComp, cst.DictComp, cst.GeneratorExp]
    ) -> bool:
        """
        Cheat sheet: `[elt for target in iter if ifs]`

        Terminology:
            target: The variable or pattern we're storing each element of the iter in.
            iter: The thing we're iterating over.
            ifs: A list of conditions provided
            elt: The value that will be computed and "yielded" each time the loop
                iterates. For most comprehensions, this is just the `node.elt`, but
                DictComp has `key` and `value`, which behave like `node.elt` would.


        Nested Comprehension: ``[a for b in c for a in b]`` is a "nested" ListComp.
        The outer iterator is in ``node.for_in`` and the inner iterator is in
        ``node.for_in.inner_for_in``.


        The first comprehension object's iter in generators is evaluated
        outside of the ComprehensionScope. Every other comprehension's iter is
        evaluated inside the ComprehensionScope. Even though that doesn't seem very sane,
        but that appears to be how it works.

            non_flat = [ [1,2,3], [4,5,6], [7,8]
            flat = [y for x in non_flat for y in x]  # this works fine

            # This will give a "NameError: name 'x' is not defined":
            flat = [y for x in x for y in x]
            # x isn't defined, because the first iter is evaluted outside the scope.

            # This will give an UnboundLocalError, indicating that the second
            # comprehension's iter value is evaluated inside the scope as its elt.
            # UnboundLocalError: local variable 'y' referenced before assignment
            flat = [y for x in non_flat for y in y]
        """
        for_in = node.for_in
        for_in.iter.visit(self)
        self.provider.set_metadata(for_in, self.scope)
        with self._new_scope(ComprehensionScope):
            for_in.target.visit(self)
            for condition in for_in.ifs:
                condition.visit(self)
            if for_in.inner_for_in:
                for_in.inner_for_in.visit(self)
            if isinstance(node, cst.DictComp):
                node.key.visit(self)
                node.value.visit(self)
            else:
                node.elt.visit(self)

            if isinstance(node, cst.ListComp):
                remaining_attrs = [node.lbracket, node.rbracket, node.lpar, node.rpar]
            elif isinstance(node, cst.SetComp):
                remaining_attrs = [node.lbrace, node.rbrace, node.lpar, node.rpar]
            elif isinstance(node, cst.DictComp):
                remaining_attrs = [
                    node.lbrace,
                    node.rbrace,
                    node.lpar,
                    node.rpar,
                    node.whitespace_before_colon,
                    node.whitespace_after_colon,
                ]
            else:  # cst.GeneratorExp
                remaining_attrs = [node.lpar, node.rpar]

            for attr in remaining_attrs:
                if isinstance(attr, cst.CSTNode):
                    attr.visit(self)
        return False

    def infer_accesses(self) -> None:
        for access in self.__deferred_accesses:
            for assignment in access.scope[access.node.value]:
                assignment.record_access(access)
        self.__deferred_accesses = []

    def on_leave(self, original_node: cst.CSTNode) -> None:
        self.provider.set_metadata(original_node, self.scope)
        super().on_leave(original_node)


class ScopeProvider(BatchableMetadataProvider[Optional[Scope]]):
    """
     ScopeProvider traverses the entire module and creates the scope inheritance
    structure. It provides the scope of name assignment and accesses. It is useful for
    more advanced static analysis. E.g. given a :class:`~libcst.FunctionDef`
    node, we can check the type of its Scope to figure out whether it is a class method
    (:class:`ClassScope`) or a regular function (:class:`GlobalScope`).
    """

    METADATA_DEPENDENCIES = (ExpressionContextProvider,)

    def __init__(self) -> None:
        super().__init__()

    def visit_Module(self, node: cst.Module) -> Optional[bool]:
        visitor = ScopeVisitor(self)
        node.visit(visitor)
        visitor.infer_accesses()
