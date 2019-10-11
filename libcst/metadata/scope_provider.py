# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import abc
import builtins
import warnings
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass
from enum import Enum, auto
from typing import (
    Collection,
    Dict,
    Iterator,
    List,
    Mapping,
    MutableMapping,
    Optional,
    Set,
    Tuple,
    Type,
    Union,
)

import libcst as cst
from libcst._add_slots import add_slots
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.expression_context_provider import (
    ExpressionContext,
    ExpressionContextProvider,
)


class Access:
    """
    An Access records an access of an assignment.

    .. note::
       This scope analysis only analyzes access via a :class:`~libcst.Name` or  a :class:`~libcst.Name`
       node embedded in other node like :class:`~libcst.Call` or :class:`~libcst.Attribute`.
       It doesn't support type annontation using :class:`~libcst.SimpleString` literal for forward
       references. E.g. in this example, the ``"Tree"`` isn't parsed as as an access::

           class Tree:
               def __new__(cls) -> "Tree":
                   ...
    """

    #: The name node of the access. A name is an access when the expression context is
    #: :attr:`ExpressionContext.LOAD`.
    node: cst.Name

    #: The scope of the access. Note that a access could be in a child scope of its assignment.
    scope: "Scope"

    __assignments: Set["BaseAssignment"]

    def __init__(self, node: cst.Name, scope: "Scope") -> None:
        self.node = node
        self.scope = scope
        self.__assignments = set()

    def __hash__(self) -> int:
        return id(self)

    @property
    def referents(self) -> Collection["BaseAssignment"]:
        """Return all assignments of the access."""
        return self.__assignments

    def record_assignment(self, assignment: "BaseAssignment") -> None:
        self.__assignments.add(assignment)


class BaseAssignment(abc.ABC):
    """Abstract base class of :class:`Assignment` and :class:`BuitinAssignment`."""

    #: The name of assignment.
    name: str

    #: The scope associates to assignment.
    scope: "Scope"
    __accesses: Set[Access]

    def __init__(self, name: str, scope: "Scope") -> None:
        self.name = name
        self.scope = scope
        self.__accesses = set()

    def record_access(self, access: Access) -> None:
        self.__accesses.add(access)

    @property
    def references(self) -> Collection[Access]:
        """Return all accesses of the assignment."""
        # we don't want to publicly expose the mutable version of this
        return self.__accesses

    @property
    def accesses(self) -> Tuple[Access, ...]:
        """Return all accesses of the assignment.

        .. warning::
           Deprecated: This will be removed soon. Please use
           :attr:`~libcst.metadata.BaseAssignment.references` instead!
        """
        # we don't want to publicly expose the mutable version of this
        warnings.warn(
            "This will be removed soon. Please use `.references` instead!",
            DeprecationWarning,
        )
        return tuple(self.__accesses)

    def __hash__(self) -> int:
        return id(self)


class Assignment(BaseAssignment):
    """An assignment records the name, CSTNode and its accesses."""

    #: The node of assignment, it could be a :class:`~libcst.Import`, :class:`~libcst.ImportFrom`,
    #: :class:`~libcst.Name`, :class:`~libcst.FunctionDef`, or :class:`~libcst.ClassDef`.
    node: cst.CSTNode

    def __init__(self, name: str, scope: "Scope", node: cst.CSTNode) -> None:
        self.node = node
        super().__init__(name, scope)


# even though we don't override the constructor.
class BuiltinAssignment(BaseAssignment):
    """
    A BuiltinAssignment represents an value provide by Python as a builtin, including
    `functions <https://docs.python.org/3/library/functions.html>`_,
    `constants <https://docs.python.org/3/library/constants.html>`_, and
    `types <https://docs.python.org/3/library/stdtypes.html>`_.
    """

    pass


class Assignments:
    """A container to provide all assignments in a scope."""

    def __init__(self, assignments: Mapping[str, Collection[BaseAssignment]]) -> None:
        self._assignments = assignments

    def __iter__(self) -> Iterator[BaseAssignment]:
        """Iterate through all assignments by ``for i in scope.assignments``."""
        for assignments in self._assignments.values():
            for assignment in assignments:
                yield assignment

    def __getitem__(self, node: Union[str, cst.CSTNode]) -> Collection[BaseAssignment]:
        """Get assignments given a name str or :class:`~libcst.CSTNode` by ``scope.assignments[node]``"""
        name = _NameUtil.get_name_for(node)
        return set(self._assignments[name]) if name in self._assignments else set()

    def __contains__(self, node: Union[str, cst.CSTNode]) -> bool:
        """Check if a name str or :class:`~libcst.CSTNode` has any assignment by ``node in scope.assignments``"""
        return len(self[node]) > 0


class Accesses:
    """A container to provide all accesses in a scope."""

    def __init__(self, accesses: Mapping[str, Collection[Access]]) -> None:
        self._accesses = accesses

    def __iter__(self) -> Iterator[Access]:
        """Iterate through all accesses by ``for i in scope.accesses``."""
        for accesses in self._accesses.values():
            for access in accesses:
                yield access

    def __getitem__(self, node: Union[str, cst.CSTNode]) -> Collection[Access]:
        """Get accesses given a name str or :class:`~libcst.CSTNode` by ``scope.accesses[node]``"""
        name = _NameUtil.get_name_for(node)
        return self._accesses[name] if name in self._accesses else set()

    def __contains__(self, node: Union[str, cst.CSTNode]) -> bool:
        """Check if a name str or :class:`~libcst.CSTNode` has any access by ``node in scope.accesses``"""
        return len(self[node]) > 0


class QualifiedNameSource(Enum):
    IMPORT = auto()
    BUILTIN = auto()
    LOCAL = auto()


@add_slots
@dataclass(frozen=True)
class QualifiedName:
    #: Qualified name, e.g. ``a.b.c`` or ``fn.<locals>.var``.
    name: str

    #: Source of the name, either :attr:`QualifiedNameSource.IMPORT`, :attr:`QualifiedNameSource.BUILTIN`
    #: or :attr:`QualifiedNameSource.LOCAL`.
    source: QualifiedNameSource


class _NameUtil:
    @staticmethod
    def get_full_name_for(node: Union[str, cst.CSTNode]) -> Optional[str]:
        if isinstance(node, cst.Name):
            return node.value
        elif isinstance(node, str):
            return node
        elif isinstance(node, cst.Attribute):
            return f"{_NameUtil.get_full_name_for(node.value)}.{node.attr.value}"
        elif isinstance(node, cst.Call):
            return _NameUtil.get_full_name_for(node.func)
        elif isinstance(node, cst.Subscript):
            return _NameUtil.get_full_name_for(node.value)
        elif isinstance(node, (cst.FunctionDef, cst.ClassDef)):
            return _NameUtil.get_full_name_for(node.name)
        return None

    @staticmethod
    def get_name_for(node: Union[str, cst.CSTNode]) -> Optional[str]:
        """A helper function to retrieve simple name str from a CSTNode or str"""
        if isinstance(node, cst.Name):
            return node.value
        elif isinstance(node, str):
            return node
        elif isinstance(node, cst.Call):
            return _NameUtil.get_name_for(node.func)
        elif isinstance(node, cst.Subscript):
            return _NameUtil.get_name_for(node.value)
        elif isinstance(node, (cst.FunctionDef, cst.ClassDef)):
            return _NameUtil.get_name_for(node.name)
        return None

    @staticmethod
    def find_qualified_name_for_import_alike(
        assignment_node: Union[cst.Import, cst.ImportFrom], name_parts: List[str]
    ) -> Set[QualifiedName]:
        module = ""
        results = set()
        if isinstance(assignment_node, cst.ImportFrom):
            module_attr = assignment_node.module
            if module_attr:
                # TODO: for relative import, keep the relative Dot in the qualified name
                module = _NameUtil.get_full_name_for(module_attr)
        import_names = assignment_node.names
        if not isinstance(import_names, cst.ImportStar):
            for name in import_names:
                real_name = _NameUtil.get_full_name_for(name.name)
                as_name = real_name
                if name and name.asname:
                    name_asname = name.asname
                    if name_asname:
                        as_name = cst.ensure_type(name_asname.name, cst.Name).value
                if as_name == name_parts[0]:
                    if module:
                        real_name = f"{module}.{real_name}"
                    if real_name:
                        results.add(
                            QualifiedName(
                                ".".join([real_name, *name_parts[1:]]),
                                QualifiedNameSource.IMPORT,
                            )
                        )
        return results

    @staticmethod
    def find_qualified_name_for_non_import(
        assignment: Assignment, name_parts: List[str]
    ) -> Set[QualifiedName]:
        scope = assignment.scope
        name_prefixes = []
        while scope:
            if isinstance(scope, ClassScope):
                name_prefixes.append(scope.name)
            elif isinstance(scope, FunctionScope):
                name_prefixes.append(f"{scope.name}.<locals>")
            elif isinstance(scope, GlobalScope):
                break
            elif isinstance(scope, ComprehensionScope):
                name_prefixes.append("<comprehension>")
            else:
                raise Exception(f"Unexpected Scope: {scope}")
            scope = scope.parent

        return {
            QualifiedName(
                ".".join([*reversed(name_prefixes), *name_parts]),
                QualifiedNameSource.LOCAL,
            )
        }


class Scope(abc.ABC):
    """
    Base class of all scope classes. Scope object stores assignments from imports,
    variable assignments, function definition or class definition.
    A scope has a parent scope which represents the inheritance relationship. That means
    an assignment in parent scope is viewable to the child scope and the child scope may
    overwrites the assignment by using the same name.

    Use ``name in scope`` to check whether a name is viewable in the scope.
    Use ``scope[name]`` to retrieve all viewable assignments in the scope.

    .. note::
       This scope analysis module only analyzes local variable names and it doesn't handle
       attribute names; for example, given ``a.b.c = 1``, local variable name ``a`` is recorded
       as an assignment instead of ``c`` or ``a.b.c``. To analyze the assignment/access of
       arbitrary object attributes, we leave the job to type inference metadata provider
       coming in the future.
    """

    #: Parent scope. Note the parent scope of a GlobalScope is itself.
    parent: "Scope"

    #: Refers to the GlobalScope.
    globals: "GlobalScope"
    _assignments: MutableMapping[str, Set[BaseAssignment]]
    _accesses: MutableMapping[str, Set[Access]]

    def __init__(self, parent: "Scope") -> None:
        super().__init__()
        self.parent = parent
        self.globals = parent.globals
        self._assignments = defaultdict(set)
        self._accesses = defaultdict(set)

    def record_assignment(self, name: str, node: cst.CSTNode) -> None:
        self._assignments[name].add(Assignment(name=name, scope=self, node=node))

    def record_access(self, name: str, access: Access) -> None:
        self._accesses[name].add(access)

    def _getitem_from_self_or_parent(self, name: str) -> Tuple[BaseAssignment, ...]:
        """Overridden by ClassScope to hide it's assignments from child scopes."""
        return self[name]

    def _record_assignment_as_parent(self, name: str, node: cst.CSTNode) -> None:
        """Overridden by ClassScope to forward 'nonlocal' assignments from child scopes."""
        self.record_assignment(name, node)

    def __contains__(self, name: str) -> bool:
        """ Check if the name str exist in current scope by ``name in scope``. """
        return len(self[name]) > 0

    @abc.abstractmethod
    def __getitem__(self, name: str) -> Tuple[BaseAssignment, ...]:
        """ Get assignments given a name str by ``scope[name]``. """
        ...

    def __hash__(self) -> int:
        return id(self)

    @abc.abstractmethod
    def record_global_overwrite(self, name: str) -> None:
        ...

    @abc.abstractmethod
    def record_nonlocal_overwrite(self, name: str) -> None:
        ...

    def get_qualified_names_for(
        self, node: Union[str, cst.CSTNode]
    ) -> Collection[QualifiedName]:
        """ Get all :class:`~libcst.metadata.QualifiedName` in current scope given a
        :class:`~libcst.CSTNode`.
        The source of a qualified name can be either :attr:`QualifiedNameSource.IMPORT`,
        :attr:`QualifiedNameSource.BUILTIN` or :attr:`QualifiedNameSource.LOCAL`.
        Given the following example, ``c`` has qualified name ``a.b.c`` with source ``IMPORT``,
        ``f`` has qualified name ``Cls.f`` with source ``LOCAL``, ``a`` has qualified name
        ``Cls.f.<locals>.a``, ``i`` has qualified name ``Cls.f.<locals>.<comprehension>.i``,
        and the builtin ``int`` has qualified name ``builtins.int`` with source ``BUILTIN``::

            from a.b import c
            class Cls:
                def f(self) -> "c":
                    c()
                    a = int("1")
                    [i for i in c()]

        We extends `PEP-3155 <https://www.python.org/dev/peps/pep-3155/>`_
        (defines ``__qualname__`` for class and function only; function namespace is followed
        by a ``<locals>``) to provide qualified name for all :class:`~libcst.CSTNode`
        recorded by :class:`~libcst.metadata.Assignment` and :class:`~libcst.metadata.Access`.
        The namespace of a comprehension (:class:`~libcst.ListComp`, :class:`~libcst.SetComp`,
        :class:`~libcst.DictComp`) is represented with ``<comprehension>``.

        An imported name may be used for type annotation with :class:`~libcst.SimpleString` and
        currently resolving the qualified given :class:`~libcst.SimpleString` is not supported
        considering it could be a complex type annotation in the string which is hard to
        resolve, e.g. ``List[Union[int, str]]``.
        """
        results = set()
        full_name = _NameUtil.get_full_name_for(node)
        if full_name is None:
            return results
        parts = full_name.split(".")
        for assignment in self[parts[0]]:
            if isinstance(assignment, Assignment):
                assignment_node = assignment.node
                if isinstance(assignment_node, (cst.Import, cst.ImportFrom)):
                    results |= _NameUtil.find_qualified_name_for_import_alike(
                        assignment_node, parts
                    )
                else:
                    results |= _NameUtil.find_qualified_name_for_non_import(
                        assignment, parts
                    )
            elif isinstance(assignment, BuiltinAssignment):
                results.add(
                    QualifiedName(
                        f"builtins.{assignment.name}", QualifiedNameSource.BUILTIN
                    )
                )
        return results

    @property
    def assignments(self) -> Assignments:
        """Return an :class:`~libcst.metadata.Assignments` contains all assignmens in current scope."""
        return Assignments(self._assignments)

    @property
    def accesses(self) -> Accesses:
        """Return an :class:`~libcst.metadata.Accesses` contains all accesses in current scope."""
        return Accesses(self._accesses)


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
                self._assignments[name].add(BuiltinAssignment(name, self))
        return tuple(self._assignments[name])

    def record_global_overwrite(self, name: str) -> None:
        pass

    def record_nonlocal_overwrite(self, name: str) -> None:
        raise NotImplementedError("nonlocal declaration not allowed at module level")


class LocalScope(Scope, abc.ABC):
    _scope_overwrites: Dict[str, Scope]

    #: Name of function. Used as qualified name.
    name: Optional[str]

    def __init__(self, parent: Scope, name: Optional[str] = None) -> None:
        super().__init__(parent)
        self.name = name
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


# even though we don't override the constructor.
class FunctionScope(LocalScope):
    """
    When a function is defined, it creates a FunctionScope.
    """

    pass


# even though we don't override the constructor.
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


# even though we don't override the constructor.
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
    def _new_scope(
        self, kind: Type[LocalScope], name: Optional[str] = None
    ) -> Iterator[None]:
        parent_scope = self.scope
        self.scope = kind(parent_scope, name)
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
                asname = name.asname
                if asname is not None:
                    name_value = cst.ensure_type(asname.name, cst.Name).value
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
        # not all Name have ExpressionContext
        context = self.provider.get_metadata(ExpressionContextProvider, node, None)
        if context == ExpressionContext.STORE:
            self.scope.record_assignment(node.value, node)
        elif context == ExpressionContext.LOAD:
            access = Access(node, self.scope)
            self.__deferred_accesses.append(access)
            self.scope.record_access(node.value, access)

    def visit_FunctionDef(self, node: cst.FunctionDef) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        self.provider.set_metadata(node.name, self.scope)

        with self._new_scope(FunctionScope, _NameUtil.get_full_name_for(node.name)):
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
        returns = node.returns
        if returns:
            returns.visit(self)

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
        keyword = node.keyword
        if keyword is not None:
            self.scope.record_assignment(keyword.value, node)

    def visit_ClassDef(self, node: cst.ClassDef) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        for decorator in node.decorators:
            decorator.visit(self)
        for base in node.bases:
            base.visit(self)
        for keyword in node.keywords:
            keyword.visit(self)

        with self._new_scope(ClassScope, _NameUtil.get_full_name_for(node.name)):
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
            inner_for_in = for_in.inner_for_in
            if inner_for_in:
                inner_for_in.visit(self)
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
                access.record_assignment(assignment)
        self.__deferred_accesses = []

    def on_leave(self, original_node: cst.CSTNode) -> None:
        self.provider.set_metadata(original_node, self.scope)
        super().on_leave(original_node)


class ScopeProvider(BatchableMetadataProvider[Optional[Scope]]):
    """
    :class:`ScopeProvider` traverses the entire module and creates the scope inheritance
    structure. It provides the scope of name assignment and accesses. It is useful for
    more advanced static analysis. E.g. given a :class:`~libcst.FunctionDef`
    node, we can check the type of its Scope to figure out whether it is a class method
    (:class:`ClassScope`) or a regular function (:class:`GlobalScope`).
    """

    METADATA_DEPENDENCIES = (ExpressionContextProvider,)

    def visit_Module(self, node: cst.Module) -> Optional[bool]:
        visitor = ScopeVisitor(self)
        node.visit(visitor)
        visitor.infer_accesses()


class QualifiedNameVisitor(cst.CSTVisitor):
    def __init__(self, provider: "QualifiedNameProvider") -> None:
        self.provider: QualifiedNameProvider = provider

    def on_visit(self, node: cst.CSTNode) -> bool:
        scope = self.provider.get_metadata(ScopeProvider, node, None)
        if scope:
            self.provider.set_metadata(node, scope.get_qualified_names_for(node))
        else:
            self.provider.set_metadata(node, {})
        super().on_visit(node)
        return True


class QualifiedNameProvider(
    BatchableMetadataProvider[Optional[Collection[QualifiedName]]]
):
    """
    Compute possible qualified names of a variable CSTNode
    (extends `PEP-3155 <https://www.python.org/dev/peps/pep-3155/>`_).
    It uses the
    :func:`~libcst.metadata.Scope.get_qualified_names_for` underlying to get qualified names.
    Multiple qualified names may be returned, such as when we have conditional imports or an
    import shadows another. E.g., the provider finds ``a.b``, ``d.e`` and
    ``f.g`` as possible qualified names of ``c``::

        >>> wrapper = MetadataWrapper(
        >>>     cst.parse_module(dedent(
        >>>     '''
        >>>         if something:
        >>>             from a import b as c
        >>>         elif otherthing:
        >>>             from d import e as c
        >>>         else:
        >>>             from f import g as c
        >>>         c()
        >>>     '''
        >>>     ))
        >>> )
        >>> call = wrapper.module.body[1].body[0].value
        >>> wrapper.resolve(QualifiedNameProvider)[call],
        {
            QualifiedName(name="a.b", source=QualifiedNameSource.IMPORT),
            QualifiedName(name="d.e", source=QualifiedNameSource.IMPORT),
            QualifiedName(name="f.g", source=QualifiedNameSource.IMPORT),
        }

    For qualified name of a variable in a function or a comprehension, please refer
    :func:`~libcst.metadata.Scope.get_qualified_names_for` for more detail.
    """

    METADATA_DEPENDENCIES = (ScopeProvider,)

    def visit_Module(self, node: cst.Module) -> Optional[bool]:
        visitor = QualifiedNameVisitor(self)
        node.visit(visitor)
