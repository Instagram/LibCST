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
from libcst import ensure_type
from libcst._add_slots import add_slots
from libcst._metadata_dependent import MetadataDependent
from libcst.helpers import get_full_name_for_node
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.expression_context_provider import (
    ExpressionContext,
    ExpressionContextProvider,
)


@add_slots
@dataclass(frozen=False)
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

    #: The node of the access. A name is an access when the expression context is
    #: :attr:`ExpressionContext.LOAD`. This is usually the name node representing the
    #: access, except for dotted imports, when it might be the attribute that
    #: represents the most specific part of the imported symbol.
    node: Union[cst.Name, cst.Attribute]

    #: The scope of the access. Note that a access could be in a child scope of its
    #: assignment.
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

    def record_assignments(self, assignments: Set["BaseAssignment"]) -> None:
        self.__assignments |= assignments


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

    def record_accesses(self, accesses: Set[Access]) -> None:
        self.__accesses |= accesses

    @property
    def references(self) -> Collection[Access]:
        """Return all accesses of the assignment."""
        # we don't want to publicly expose the mutable version of this
        return self.__accesses

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
        assignment_node: Union[cst.Import, cst.ImportFrom], full_name: str
    ) -> Set[QualifiedName]:
        module = ""
        results = set()
        if isinstance(assignment_node, cst.ImportFrom):
            module_attr = assignment_node.module
            if module_attr:
                # TODO: for relative import, keep the relative Dot in the qualified name
                module = get_full_name_for_node(module_attr)
        import_names = assignment_node.names
        if not isinstance(import_names, cst.ImportStar):
            for name in import_names:
                real_name = get_full_name_for_node(name.name)
                if not real_name:
                    continue
                # real_name can contain `.` for dotted imports
                # for these we want to find the longest prefix that matches full_name
                parts = real_name.split(".")
                real_names = [".".join(parts[:i]) for i in range(len(parts), 0, -1)]
                for real_name in real_names:
                    as_name = real_name
                    if module:
                        real_name = f"{module}.{real_name}"
                    if name and name.asname:
                        eval_alias = name.evaluated_alias
                        if eval_alias is not None:
                            as_name = eval_alias
                    if full_name.startswith(as_name):
                        remaining_name = full_name.split(as_name)[1].lstrip(".")
                        results.add(
                            QualifiedName(
                                f"{real_name}.{remaining_name}"
                                if remaining_name
                                else real_name,
                                QualifiedNameSource.IMPORT,
                            )
                        )
                        break
        return results

    @staticmethod
    def find_qualified_name_for_non_import(
        assignment: Assignment, remaining_name: str
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

        parts = [*reversed(name_prefixes)]
        if remaining_name:
            parts.append(remaining_name)
        return {QualifiedName(".".join(parts), QualifiedNameSource.LOCAL)}


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

    def _getitem_from_self_or_parent(self, name: str) -> Set[BaseAssignment]:
        """Overridden by ClassScope to hide it's assignments from child scopes."""
        return self[name]

    def _contains_in_self_or_parent(self, name: str) -> bool:
        """Overridden by ClassScope to hide it's assignments from child scopes."""
        return name in self

    def _record_assignment_as_parent(self, name: str, node: cst.CSTNode) -> None:
        """Overridden by ClassScope to forward 'nonlocal' assignments from child scopes."""
        self.record_assignment(name, node)

    @abc.abstractmethod
    def __contains__(self, name: str) -> bool:
        """ Check if the name str exist in current scope by ``name in scope``. """
        ...

    @abc.abstractmethod
    def __getitem__(self, name: str) -> Set[BaseAssignment]:
        """
        Get assignments given a name str by ``scope[name]``.

        .. note::
           *Why does it return a list of assignments given a name instead of just one assignment?*

           Many programming languages differentiate variable declaration and assignment.
           Further, those programming languages often disallow duplicate declarations within
           the same scope, and will often hoist the declaration (without its assignment) to
           the top of the scope. These design decisions make static analysis much easier,
           because it's possible to match a name against its single declaration for a given scope.

           As an example, the following code would be valid in JavaScript::

               function fn() {
                 console.log(value);  // value is defined here, because the declaration is hoisted, but is currently 'undefined'.
                 var value = 5;  // A function-scoped declaration.
               }
               fn();  // prints 'undefined'.

           In contrast, Python's declaration and assignment are identical and are not hoisted::

               if conditional_value:
                   value = 5
               elif other_conditional_value:
                   value = 10
               print(value)  # possibly valid, depending on conditional execution

           This code may throw a ``NameError`` if both conditional values are falsy.
           It also means that depending on the codepath taken, the original declaration
           could come from either ``value = ...`` assignment node.
           As a result, instead of returning a single declaration,
           we're forced to return a collection of all of the assignments we think could have
           defined a given name by the time a piece of code is executed.
           For the above example, value would resolve to a set of both assignments.
        """
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
        full_name = get_full_name_for_node(node)
        if full_name is None:
            return results
        assignments = set()
        parts = full_name.split(".")
        for i in range(len(parts), 0, -1):
            prefix = ".".join(parts[:i])
            if prefix in self:
                assignments = self[prefix]
                break
        for assignment in assignments:
            if isinstance(assignment, Assignment):
                assignment_node = assignment.node
                if isinstance(assignment_node, (cst.Import, cst.ImportFrom)):
                    results |= _NameUtil.find_qualified_name_for_import_alike(
                        assignment_node, full_name
                    )
                else:
                    results |= _NameUtil.find_qualified_name_for_non_import(
                        assignment, full_name
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

    def __contains__(self, name: str) -> bool:
        return hasattr(builtins, name) or (
            name in self._assignments and len(self._assignments[name]) > 0
        )

    def __getitem__(self, name: str) -> Set[BaseAssignment]:
        if hasattr(builtins, name):
            if not any(
                isinstance(i, BuiltinAssignment) for i in self._assignments[name]
            ):
                self._assignments[name].add(BuiltinAssignment(name, self))
        return self._assignments[name]

    def record_global_overwrite(self, name: str) -> None:
        pass

    def record_nonlocal_overwrite(self, name: str) -> None:
        raise NotImplementedError("nonlocal declaration not allowed at module level")


class LocalScope(Scope, abc.ABC):
    _scope_overwrites: Dict[str, Scope]

    #: Name of function. Used as qualified name.
    name: Optional[str]

    #: The :class:`~libcst.CSTNode` node defines the current scope.
    node: cst.CSTNode

    def __init__(
        self, parent: Scope, node: cst.CSTNode, name: Optional[str] = None
    ) -> None:
        super().__init__(parent)
        self.name = name
        self.node = node
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

    def __contains__(self, name: str) -> bool:
        if name in self._scope_overwrites:
            return name in self._scope_overwrites[name]
        if name in self._assignments:
            return len(self._assignments[name]) > 0
        return self.parent._contains_in_self_or_parent(name)

    def __getitem__(self, name: str) -> Set[BaseAssignment]:
        if name in self._scope_overwrites:
            return self._scope_overwrites[name]._getitem_from_self_or_parent(name)
        if name in self._assignments:
            return self._assignments[name]
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

    def _getitem_from_self_or_parent(self, name: str) -> Set[BaseAssignment]:
        """
        Class variables are only accessible using ClassName.attribute, cls.attribute, or
        self.attribute in child scopes. They cannot be accessed with their bare names.
        """
        return self.parent._getitem_from_self_or_parent(name)

    def _contains_in_self_or_parent(self, name: str) -> bool:
        """
        See :meth:`_getitem_from_self_or_parent`
        """
        return self.parent._contains_in_self_or_parent(name)


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


# Generates dotted names from an Attribute or Name node:
# Attribute(value=Name(value="a"), attr=Name(value="b")) -> ("a.b", "a")
# each string has the corresponding CSTNode attached to it
def _gen_dotted_names(
    node: Union[cst.Attribute, cst.Name]
) -> Iterator[Tuple[str, Union[cst.Attribute, cst.Name]]]:
    if isinstance(node, cst.Name):
        yield node.value, node
    else:
        value = node.value
        if not isinstance(value, (cst.Attribute, cst.Name)):
            # this is not an import
            return
        name_values = _gen_dotted_names(value)
        try:
            next_name, next_node = next(name_values)
        except StopIteration:
            return
        else:
            yield f"{next_name}.{node.attr.value}", node
            yield next_name, next_node
            yield from name_values


class ScopeVisitor(cst.CSTVisitor):
    # since it's probably not useful. That can makes this visitor cleaner.
    def __init__(self, provider: "ScopeProvider") -> None:
        self.provider: ScopeProvider = provider
        self.scope: Scope = GlobalScope()
        self.__deferred_accesses: List[Tuple[Access, Optional[cst.Attribute]]] = []
        self.__top_level_attribute: Optional[cst.Attribute] = None

    @contextmanager
    def _new_scope(
        self, kind: Type[LocalScope], node: cst.CSTNode, name: Optional[str] = None
    ) -> Iterator[None]:
        parent_scope = self.scope
        self.scope = kind(parent_scope, node, name)
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
        if isinstance(names, cst.ImportStar):
            return False

        # make sure node.names is Sequence[ImportAlias]
        for name in names:
            asname = name.asname
            if asname is not None:
                name_values = _gen_dotted_names(cst.ensure_type(asname.name, cst.Name))
            else:
                name_values = _gen_dotted_names(name.name)

            for name_value, _ in name_values:
                self.scope.record_assignment(name_value, node)
        return False

    def visit_Import(self, node: cst.Import) -> Optional[bool]:
        return self._visit_import_alike(node)

    def visit_ImportFrom(self, node: cst.ImportFrom) -> Optional[bool]:
        return self._visit_import_alike(node)

    def visit_Attribute(self, node: cst.Attribute) -> Optional[bool]:
        if self.__top_level_attribute is None:
            self.__top_level_attribute = node
        node.value.visit(self)  # explicitly not visiting attr
        if self.__top_level_attribute is node:
            self.__top_level_attribute = None
        return False

    def visit_Name(self, node: cst.Name) -> Optional[bool]:
        # not all Name have ExpressionContext
        context = self.provider.get_metadata(ExpressionContextProvider, node, None)
        if context == ExpressionContext.STORE:
            self.scope.record_assignment(node.value, node)
        elif context in (ExpressionContext.LOAD, ExpressionContext.DEL):
            access = Access(node, self.scope)
            self.__deferred_accesses.append((access, self.__top_level_attribute))

    def visit_FunctionDef(self, node: cst.FunctionDef) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        self.provider.set_metadata(node.name, self.scope)

        with self._new_scope(FunctionScope, node, get_full_name_for_node(node.name)):
            node.params.visit(self)
            node.body.visit(self)

        for decorator in node.decorators:
            decorator.visit(self)
        returns = node.returns
        if returns:
            returns.visit(self)

        return False

    def visit_Lambda(self, node: cst.Lambda) -> Optional[bool]:
        with self._new_scope(FunctionScope, node):
            node.params.visit(self)
            node.body.visit(self)
        return False

    def visit_Param(self, node: cst.Param) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        self.provider.set_metadata(node.name, self.scope)
        with self._switch_scope(self.scope.parent):
            for field in [node.default, node.annotation]:
                if field:
                    field.visit(self)

        return False

    def visit_Arg(self, node: cst.Arg) -> bool:
        # The keyword of Arg is neither an Assignment nor an Access and we explicitly don't visit it.
        value = node.value
        if value:
            value.visit(self)
        return False

    def visit_ClassDef(self, node: cst.ClassDef) -> Optional[bool]:
        self.scope.record_assignment(node.name.value, node)
        for decorator in node.decorators:
            decorator.visit(self)
        for base in node.bases:
            base.visit(self)
        for keyword in node.keywords:
            keyword.visit(self)

        with self._new_scope(ClassScope, node, get_full_name_for_node(node.name)):
            for statement in node.body.body:
                statement.visit(self)
        return False

    def visit_Global(self, node: cst.Global) -> Optional[bool]:
        for name_item in node.names:
            self.scope.record_global_overwrite(name_item.name.value)
        return False

    def visit_Nonlocal(self, node: cst.Nonlocal) -> Optional[bool]:
        for name_item in node.names:
            self.scope.record_nonlocal_overwrite(name_item.name.value)
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
        with self._new_scope(ComprehensionScope, node):
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
        return False

    def infer_accesses(self) -> None:
        # Aggregate access with the same name and batch add with set union as an optimization.
        # In worst case, all accesses (m) and assignments (n) refer to the same name,
        # the time complexity is O(m x n), this optimizes it as O(m + n).
        scope_name_accesses = defaultdict(set)
        for (access, enclosing_attribute) in self.__deferred_accesses:
            name = ensure_type(access.node, cst.Name).value
            if enclosing_attribute is not None:
                # if _gen_dotted_names doesn't generate any values, fall back to
                # the original name node above
                for name, node in _gen_dotted_names(enclosing_attribute):
                    if name in access.scope:
                        access.node = node
                        break

            scope_name_accesses[(access.scope, name)].add(access)
            access.record_assignments(access.scope[name])
            access.scope.record_access(name, access)

        for (scope, name), accesses in scope_name_accesses.items():
            for assignment in scope[name]:
                assignment.record_accesses(accesses)

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

    Scope metadata is available for most node types other than formatting information nodes
    (whitespace, parentheses, etc.).
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
            self.provider.set_metadata(node, set())
        super().on_visit(node)
        return True


class QualifiedNameProvider(BatchableMetadataProvider[Collection[QualifiedName]]):
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

    @staticmethod
    def has_name(
        visitor: MetadataDependent, node: cst.CSTNode, name: Union[str, QualifiedName]
    ) -> bool:
        """Check if any of qualified name has the str name or :class:`~libcst.metadata.QualifiedName` name."""
        qualified_names = visitor.get_metadata(QualifiedNameProvider, node, set())
        if isinstance(name, str):
            return any(qn.name == name for qn in qualified_names)
        else:
            return any(qn == name for qn in qualified_names)
