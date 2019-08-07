# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields, replace
from typing import (
    TYPE_CHECKING,
    Any,
    List,
    MutableMapping,
    Sequence,
    Type,
    TypeVar,
    Union,
    cast,
)

from libcst._nodes._internal import CodegenState, CodePosition, CodeRange
from libcst._removal_sentinel import RemovalSentinel
from libcst._type_enforce import is_value_of_type
from libcst._visitors import CSTTransformer, CSTVisitor, CSTVisitorT


if TYPE_CHECKING:
    from libcst.metadata.base_provider import BaseMetadataProvider  # noqa: F401


_T = TypeVar("_T")
_CSTMetadataMapping = MutableMapping[Type["BaseMetaDataProvider[_T]"], _T]

_CSTNodeSelfT = TypeVar("_CSTNodeSelfT", bound="CSTNode")
_EMPTY_SEQUENCE: Sequence["CSTNode"] = ()


class CSTValidationError(SyntaxError):
    pass


class CSTCodegenError(SyntaxError):
    pass


class _ChildrenCollectionVisitor(CSTVisitor):
    def __init__(self) -> None:
        self.children: List[CSTNode] = []

    def on_visit(self, node: "CSTNode") -> bool:
        self.children.append(node)
        return False  # Don't include transitive children


class _NOOPVisitor(CSTTransformer):
    pass


def _pretty_repr(value: object) -> str:
    if not isinstance(value, str) and isinstance(value, Sequence):
        return _pretty_repr_sequence(value)
    else:
        return repr(value)


def _pretty_repr_sequence(seq: Sequence[object]) -> str:
    if len(seq) == 0:
        return "[]"
    else:
        return "\n".join(["[", *[f"{_indent(repr(el))}," for el in seq], "]"])


def _indent(value: str) -> str:
    return "\n".join(f"    {l}" for l in value.split("\n"))


@dataclass(frozen=True)
class CSTNode(ABC):
    # TODO: remove this field once lint is ported
    _metadata: MutableMapping[Type["BaseMetaDataProvider[_T]"], _T] = field(
        default_factory=dict, init=False, repr=False, compare=False
    )

    def __post_init__(self) -> None:
        # PERF: It might make more sense to move validation work into the visitor, which
        # would allow us to avoid validating the tree when parsing a file.
        self._validate()

    @classmethod
    def __init_subclass__(cls, **kwargs: Any) -> None:
        """
        HACK: Add our implementation of `__repr__`, `__hash__`, and `__eq__` to the
        class's __dict__ to prevent dataclass from generating it's own `__repr__`,
        `__hash__`, and `__eq__`.

        The alternative is to require each implementation of a node to remember to add
        `repr=False, eq=False`, which is more error-prone.
        """
        super().__init_subclass__(**kwargs)

        if "__repr__" not in cls.__dict__:
            cls.__repr__ = CSTNode.__repr__
        if "__eq__" not in cls.__dict__:
            cls.__eq__ = CSTNode.__eq__
        if "__hash__" not in cls.__dict__:
            cls.__hash__ = CSTNode.__hash__

    def _validate(self) -> None:
        """
        Override this to perform runtime validation of a newly created node.

        The function is called during `__init__`. It should check for possible mistakes
        that wouldn't be caught by a static type checker.

        If you can't use a static type checker, and want to perform a runtime validation
        of this node's types, use `validate_types` instead.
        """
        pass

    def validate_types_shallow(self) -> None:
        """
        Compares the type annotations on a node's fields with those field's actual
        values at runtime. Raises a TypeError is a mismatch is found.

        Only validates the current node, not any of it's children. For a recursive
        version, see :func:`validate_types_deep`.

        If you're using a static type checker (highly recommended), this is useless.
        However, if your code doesn't use a static type checker, or if you're unable to
        statically type your code for some reason, you can use this method to help
        validate your tree.

        Some (non-typing) validation is done unconditionally during the construction of
        a node. That validation does not overlap with the work that
        :func:`validate_types_deep` does.
        """
        for f in fields(self):
            # TODO: remove this
            if f.name == "_metadata":  # skip typechecking metadata field
                continue

            value = getattr(self, f.name)
            if not is_value_of_type(value, f.type):
                raise TypeError(
                    f"Expected an instance of {f.type!r} on "
                    + f"{type(self).__name__}'s '{f.name}' field, but instead got "
                    + f"an instance of {type(value)!r}"
                )

    def validate_types_deep(self) -> None:
        """
        Like :func:`validate_types_shallow`, but recursively validates the whole tree.
        """
        self.validate_types_shallow()
        for ch in self.children:
            ch.validate_types_deep()

    @property
    def children(self) -> Sequence["CSTNode"]:
        """
        The immediate (not transitive) child CSTNodes of the current node. Various
        properties on the nodes, such as string values, will not be visited if they are
        not a subclass of CSTNode.

        Iterable properties of the node (e.g. an IndentedBlock's body) will be flattened
        into the children's sequence.

        The children will always be returned in the same order that they appear
        lexically in the code.
        """

        # We're hooking into _visit_and_replace_children, which means that our current
        # implementation is slow. We may need to rethink and/or cache this if it becomes
        # a frequently accessed property.
        #
        # This probably won't be called frequently, because most child access will
        # probably through visit, or directly through named property access, not through
        # children.

        visitor = _ChildrenCollectionVisitor()
        self._visit_and_replace_children(visitor)
        return visitor.children

    # TODO: remove compatibility hack
    def visit(
        self: _CSTNodeSelfT, visitor: CSTVisitorT, use_compatible: bool = True
    ) -> Union[_CSTNodeSelfT, RemovalSentinel]:
        """
        Visits the current node, its children, and all transitive children using
        the given visitor's callbacks.
        """
        # visit self
        should_visit_children = visitor.on_visit(self)

        # TODO: provide traversal where children are not replaced
        # visit children (optionally)
        if should_visit_children:
            # It's not possible to define `_visit_and_replace_children` with the correct
            # return type in any sane way, so we're using this cast. See the
            # explanation above the declaration of `_visit_and_replace_children`.
            with_updated_children = cast(
                _CSTNodeSelfT, self._visit_and_replace_children(visitor)
            )
        else:
            with_updated_children = self

        if isinstance(visitor, CSTVisitor):
            visitor.on_leave(self)
            leave_result = self
        else:
            leave_result = visitor.on_leave(self, with_updated_children)

        # validate return type of the user-defined `visitor.on_leave` method
        if not isinstance(leave_result, (CSTNode, RemovalSentinel)):
            raise Exception(
                f"Expected a node of type CSTNode or a RemovalSentinel, "
                + f"but got a return value of {type(leave_result).__name__}"
            )

        # TODO: Run runtime typechecks against updated nodes

        return leave_result

    # The return type of `_visit_and_replace_children` is `CSTNode`, not
    # `_CSTNodeSelfT`. This is because pyre currently doesn't have a way to annotate
    # classes as final. https://mypy.readthedocs.io/en/latest/final_attrs.html
    #
    # The issue is that any reasonable implementation of `_visit_and_replace_children`
    # needs to refer to the class' own constructor:
    #
    #   class While(CSTNode):
    #       def _visit_and_replace_children(self, visitor: CSTVisitorT) -> While:
    #           return While(...)
    #
    # You'll notice that because this implementation needs to call the `While`
    # constructor, the return type is also `While`. This function is a valid subtype of
    # `Callable[[CSTVisitorT], CSTNode]`.
    #
    # It is not a valid subtype of `Callable[[CSTVisitorT], _CSTNodeSelfT]`. That's
    # because the return type of this function wouldn't be valid for any subclasses.
    # In practice, that's not an issue, because we don't have any subclasses of `While`,
    # but there's no way to tell pyre that without a `@final` annotation.
    #
    # Instead, we're just relying on an unchecked call to `cast()` in the `visit`
    # method.
    @abstractmethod
    def _visit_and_replace_children(self, visitor: CSTVisitorT) -> "CSTNode":
        """
        Intended to be overridden by subclasses to provide a low-level hook for the
        visitor API.

        Don't call this directly. Instead, use `visitor.visit_and_replace_node` or
        `visitor.visit_and_replace_module`. If you need list of children, access the
        `children` property instead.

        The general expectation is that children should be visited in the order in which
        they appear lexically.
        """
        ...

    def _is_removable(self) -> bool:
        """
        Intended to be overridden by nodes that will be iterated over inside
        Module and IndentedBlock. Returning true signifies that this node is
        essentially useless and can be dropped when doing a visit across it.
        """
        return False

    @abstractmethod
    def _codegen_impl(self, state: CodegenState) -> None:
        ...

    def _codegen(self, state: CodegenState, **kwargs: Any) -> None:
        start = CodePosition(state.line, state.column)
        self._codegen_impl(state, **kwargs)
        end = CodePosition(state.line, state.column)
        state.record_position(self, CodeRange(start, end))

    def with_changes(self: _CSTNodeSelfT, **changes: Any) -> _CSTNodeSelfT:
        """
        A convenience method for performing mutation-like operations on immutable nodes.
        Creates a new object of the same type, replacing fields with values from the
        supplied keyword arguments.

        For example, to update the test of an if conditional, you could do::

            def leave_If(self, old_node: cst.If) -> cst.If:
                new_node = old_node.with_changes(test=new_conditional)
                return new_node

        ``new_node`` will have the same ``body``, ``orelse``, and whitespace fields as
        ``old_node``, but with the updated ``test`` field.

        The accepted arguments match the arguments given to ``__init__``, however there
        are no required or positional arguments.

        TODO: This API is untyped. There's probably no sane way to type it using pyre's
        current feature-set, but we should still think about ways to type this or a
        similar API in the future.
        """
        return replace(self, **changes)

    def deep_clone(self: _CSTNodeSelfT) -> _CSTNodeSelfT:
        """
        Recursively clone the entire tree. The created tree is a new tree has the same
        representation but different identity.

        >>> tree = cst.parse_expression("1+2")

        >>> tree.deep_clone() == tree
        False

        >>> tree == tree
        True

        >>> tree.deep_equals(tree.deep_clone())
        True
        """
        return cast(_CSTNodeSelfT, self.visit(_NOOPVisitor(), use_compatible=False))

    def deep_equals(self, other: "CSTNode") -> bool:
        """
        Recursively inspects the entire tree under ``self`` and ``other`` to determine if
        the two trees are equal by representation instead of identity (``==``).
        """
        from libcst._nodes._deep_equals import deep_equals as deep_equals_impl

        return deep_equals_impl(self, other)

    def __eq__(self: _CSTNodeSelfT, other: _CSTNodeSelfT) -> bool:
        """
        CSTNodes are only treated as equal by identity. This matches the behavior of
        CPython's AST nodes.

        If you actually want to compare the value instead of the identity of the current
        node with another, use `node.deep_equals`. Because `deep_equals` must traverse
        the entire tree, it can have an unexpectedly large time complexity.

        We're not exposing value equality as the default behavior because of
        `deep_equals`'s large time complexity.
        """
        return self is other

    def __hash__(self) -> int:
        # Equality of nodes is based on identity, so the hash should be too.
        return id(self)

    def __repr__(self) -> str:
        if len(fields(self)) == 0:
            return f"{type(self).__name__}()"

        lines = []
        lines.append(f"{type(self).__name__}(")
        for f in fields(self):
            key = f.name
            if key[0] != "_":
                value = getattr(self, key)
                lines.append(_indent(f"{key}={_pretty_repr(value)},"))
        lines.append(")")
        return "\n".join(lines)


class BaseLeaf(CSTNode, ABC):
    @property
    def children(self) -> Sequence[CSTNode]:
        # override this with an optimized implementation
        return _EMPTY_SEQUENCE

    def _visit_and_replace_children(
        self: _CSTNodeSelfT, visitor: CSTVisitorT
    ) -> _CSTNodeSelfT:
        return self


class BaseValueToken(BaseLeaf, ABC):
    """
    Represents the subset of nodes that only contain a value. Not all tokens from the
    tokenizer will exist as BaseValueTokens. In places where the token is always a
    constant value (e.g. a COLON token), the token's value will be implicitly folded
    into the parent CSTNode, and hard-coded into the implementation of _codegen.
    """

    value: str

    def _codegen_impl(self, state: CodegenState) -> None:
        state.add_token(self.value)
