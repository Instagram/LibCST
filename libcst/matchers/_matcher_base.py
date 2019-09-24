# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import collections.abc
import re
from dataclasses import fields
from enum import Enum, auto  # noqa: IG29: We don't want to depend on distillery
from typing import (
    AnyStr,
    Callable,
    Generic,
    List,
    NoReturn,
    Optional,
    Pattern,
    Sequence,
    TypeVar,
    Union,
    cast,
)

import libcst
from libcst import MaybeSentinel, RemovalSentinel


class DoNotCareSentinel(Enum):
    """
    A sentinel that is used in matcher classes to indicate that a caller
    does not care what this value is. We recommend that you do not use this
    directly, and instead use the :func:`DoNotCare` helper.
    """

    DEFAULT = auto()

    def __repr__(self) -> str:
        return "DoNotCare()"


_MatcherT = TypeVar("_MatcherT", covariant=True)
_CallableT = TypeVar("_CallableT", bound="Callable", covariant=True)
_BaseMatcherNodeSelfT = TypeVar("_BaseMatcherNodeSelfT", bound="BaseMatcherNode")
_OtherNodeT = TypeVar("_OtherNodeT")


class BaseMatcherNode:
    """
    Base class that all generated matchers subclass from. :class:`OneOf` and
    :class:`AllOf` also subclass from this in order to allow them to be used in
    any place that a generated matcher is allowed.
    """

    def __or__(
        self: _BaseMatcherNodeSelfT, other: _OtherNodeT
    ) -> "OneOf[Union[_BaseMatcherNodeSelfT, _OtherNodeT]]":
        # Without a cast, pyre thinks that the below OneOf is type OneOf[object]
        # even though it has the types passed into it.
        return cast(
            OneOf[Union[_BaseMatcherNodeSelfT, _OtherNodeT]], OneOf(self, other)
        )

    def __and__(
        self: _BaseMatcherNodeSelfT, other: _OtherNodeT
    ) -> "AllOf[Union[_BaseMatcherNodeSelfT, _OtherNodeT]]":
        # Without a cast, pyre thinks that the below AllOf is type AllOf[object]
        # even though it has the types passed into it.
        return cast(
            AllOf[Union[_BaseMatcherNodeSelfT, _OtherNodeT]], AllOf(self, other)
        )

    def __invert__(self: _BaseMatcherNodeSelfT) -> "_BaseMatcherNodeSelfT":
        return cast(_BaseMatcherNodeSelfT, InverseOf(self))


def DoNotCare() -> DoNotCareSentinel:
    """
    Used when you want to match exactly one node, but you do not care what node it is.
    Useful inside sequences such as function def and argument matchers. You do not
    need to use this for attributes since :func:`DoNotCare` is already the default.
    """
    return DoNotCareSentinel.DEFAULT


class OneOf(Generic[_MatcherT], BaseMatcherNode):
    """
    Matcher node that matches any one of its options. Useful when you want to match
    against one of several options for a single node. You can also construct a
    :class:`OneOf` matcher by using Python's or operator (``|``) with generated
    matcher classes.
    """

    def __init__(self, *options: Union[_MatcherT, "OneOf[_MatcherT]"]) -> None:
        actual_options: List[_MatcherT] = []
        for option in options:
            if isinstance(option, AllOf):
                raise Exception("Cannot use AllOf and OneOf in combination!")
            elif isinstance(option, OneOf):
                actual_options.extend(option.options)
            else:
                actual_options.append(option)

        if len(actual_options) < 2:
            raise Exception("Must provide at least two options to OneOf!")
        self.options: Sequence[_MatcherT] = actual_options

    def __or__(self, other: _OtherNodeT) -> "OneOf[Union[_MatcherT, _OtherNodeT]]":
        # Without a cast, pyre thinks that the below OneOf is type OneOf[object]
        # even though it has the types passed into it.
        return cast(OneOf[Union[_MatcherT, _OtherNodeT]], OneOf(self, other))

    def __and__(self, other: _OtherNodeT) -> NoReturn:
        raise Exception("Cannot use AllOf and OneOf in combination!")

    def __invert__(self) -> "AllOf[_MatcherT]":
        # Invert using De Morgan's Law so we don't have to complicate types.
        return cast(AllOf[_MatcherT], AllOf(*[DoesNotMatch(m) for m in self.options]))

    def __repr__(self) -> str:
        return f"OneOf({', '.join([repr(o) for o in self.options])})"


class AllOf(Generic[_MatcherT], BaseMatcherNode):
    """
    Matcher node that matches all of its options. Useful when you want to match
    against a concrete matcher and a :class:`MatchIfTrue` at the same time. You
    can also construct a :class:`AllOf` matcher by using Python's and operator
    (``&``) with generated matcher classes.
    """

    def __init__(self, *options: Union[_MatcherT, "AllOf[_MatcherT]"]) -> None:
        actual_options: List[_MatcherT] = []
        for option in options:
            if isinstance(option, OneOf):
                raise Exception("Cannot use AllOf and OneOf in combination!")
            elif isinstance(option, AllOf):
                actual_options.extend(option.options)
            else:
                actual_options.append(option)

        if len(actual_options) < 2:
            raise Exception("Must provide at least two options to AllOf!")
        self.options: Sequence[_MatcherT] = actual_options

    def __or__(self, other: _OtherNodeT) -> NoReturn:
        raise Exception("Cannot use AllOf and OneOf in combination!")

    def __and__(self, other: _OtherNodeT) -> "AllOf[Union[_MatcherT, _OtherNodeT]]":
        # Without a cast, pyre thinks that the below AllOf is type AllOf[object]
        # even though it has the types passed into it.
        return cast(AllOf[Union[_MatcherT, _OtherNodeT]], AllOf(self, other))

    def __invert__(self) -> "OneOf[_MatcherT]":
        # Invert using De Morgan's Law so we don't have to complicate types.
        return cast(OneOf[_MatcherT], OneOf(*[DoesNotMatch(m) for m in self.options]))

    def __repr__(self) -> str:
        return f"AllOf({', '.join([repr(o) for o in self.options])})"


class InverseOf(Generic[_MatcherT]):
    """
    Matcher node that inverts the match of its child. You can also construct a
    :class:`InverseOf` matcher by using Python's bitwise invert operator
    (``~``) with generated matcher classes.
    """

    def __init__(self, matcher: _MatcherT) -> None:
        self.matcher = matcher

    def __or__(self, other: _OtherNodeT) -> "OneOf[Union[_MatcherT, _OtherNodeT]]":
        # Without a cast, pyre thinks that the below OneOf is type OneOf[object]
        # even though it has the types passed into it.
        return cast(OneOf[Union[_MatcherT, _OtherNodeT]], OneOf(self, other))

    def __and__(self, other: _OtherNodeT) -> "AllOf[Union[_MatcherT, _OtherNodeT]]":
        # Without a cast, pyre thinks that the below AllOf is type AllOf[object]
        # even though it has the types passed into it.
        return cast(AllOf[Union[_MatcherT, _OtherNodeT]], AllOf(self, other))

    def __getattr__(self, key: str) -> object:
        # We lie about types to make InverseOf appear transparent. So, its conceivable
        # that somebody might try to dereference an attribute on the _MatcherT wrapped
        # node and become surprised that it doesn't work.
        return getattr(self.matcher, key)

    def __invert__(self) -> _MatcherT:
        return self.matcher

    def __repr__(self) -> str:
        return f"DoesNotMatch({repr(self.matcher)})"


class MatchIfTrue(Generic[_CallableT]):
    """
    Matcher node that matches if its child callable returns ``True``.
    """

    def __init__(self, func: _CallableT) -> None:
        # Without a cast, pyre thinks that self.func is not a function, even though
        # it recognizes that it is a _CallableT bound to Callable.
        self.func: Callable[..., bool] = cast(Callable[..., bool], func)

    def __or__(
        self, other: _OtherNodeT
    ) -> "OneOf[Union[MatchIfTrue[_CallableT], _OtherNodeT]]":
        # Without a cast, pyre thinks that the below OneOf is type OneOf[object]
        # even though it has the types passed into it.
        return cast(
            OneOf[Union[MatchIfTrue[_CallableT], _OtherNodeT]], OneOf(self, other)
        )

    def __and__(
        self, other: _OtherNodeT
    ) -> "AllOf[Union[MatchIfTrue[_CallableT], _OtherNodeT]]":
        # Without a cast, pyre thinks that the below AllOf is type AllOf[object]
        # even though it has the types passed into it.
        return cast(
            AllOf[Union[MatchIfTrue[_CallableT], _OtherNodeT]], AllOf(self, other)
        )

    def __invert__(self) -> "MatchIfTrue[_CallableT]":
        # Construct a wrapped version of MatchIfTrue for typing simplicity.
        # Without the cast, pyre doesn't seem to think the lambda is valid.
        return MatchIfTrue(cast(_CallableT, lambda val: not self.func(val)))

    def __repr__(self) -> str:
        return f"MatchIfTrue({repr(self.func)})"


def MatchRegex(
    regex: Union[str, Pattern[AnyStr]]
) -> MatchIfTrue[Callable[[str], bool]]:
    """
    Used as a convenience wrwapper to :class:`MatchIfTrue` which allows for
    matching against a regex. This should be used against attributes which
    are strings, to allow matching regular expressions. An example would be
    ``m.Name(m.MatchRegex(r'[A-Za-z]+'))``.
    """

    def _match_func(value: object) -> bool:
        if isinstance(value, str):
            # pyre-ignore Pyre doesn't think a 'Pattern' can be passed to fullmatch.
            return bool(re.fullmatch(regex, value))
        else:
            return False

    return MatchIfTrue(_match_func)


class _BaseWildcardNode:
    """
    A typing-only class for internal helpers in this module to be able to
    specify that they take a wildcard node type.
    """

    pass


class AtLeastN(Generic[_MatcherT], _BaseWildcardNode):
    """
    Matcher node that matches N or more of a particular node in a sequence.
    When specifying a matcher, each of the nodes that :class:`AtLeastN` matches
    must match that matcher. When not specifying a matcher, this acts like a
    :func:`DoNotCare` that consumes N or more nodes.
    """

    def __init__(
        self,
        matcher: Union[_MatcherT, DoNotCareSentinel] = DoNotCareSentinel.DEFAULT,
        *,
        n: int,
    ) -> None:
        if n < 0:
            raise Exception(f"{self.__class__.__name__} n attribute must be positive")
        self.n: int = n
        self.matcher: Union[_MatcherT, DoNotCareSentinel] = matcher

    def __or__(self, other: object) -> NoReturn:
        raise Exception(f"AtLeastN cannot be used in a OneOf matcher")

    def __and__(self, other: object) -> NoReturn:
        raise Exception(f"AtLeastN cannot be used in an AllOf matcher")

    def __invert__(self) -> NoReturn:
        raise Exception("Cannot invert an AtLeastN matcher!")

    def __repr__(self) -> str:
        if self.n == 0:
            return f"ZeroOrMore({repr(self.matcher)})"
        else:
            return f"AtLeastN({repr(self.matcher)}, n={self.n})"


def ZeroOrMore(
    matcher: Union[_MatcherT, DoNotCareSentinel] = DoNotCareSentinel.DEFAULT
) -> AtLeastN[Union[_MatcherT, DoNotCareSentinel]]:
    """
    Used as a convenience wrapper to :class:`AtLeastN```(n=0)``. Use this when you
    want to consume any number of nodes in a sequence.
    """
    return cast(AtLeastN[Union[_MatcherT, DoNotCareSentinel]], AtLeastN(matcher, n=0))


class AtMostN(Generic[_MatcherT], _BaseWildcardNode):
    """
    Matcher node that must matches N or less of a particular type in a sequence.
    When specifying a matcher, each of the nodes must match that matcher. When
    not specifying a matcher, this acts like a :func:`DoNotCare` that consumes at
    most N nodes.
    """

    def __init__(
        self,
        matcher: Union[_MatcherT, DoNotCareSentinel] = DoNotCareSentinel.DEFAULT,
        *,
        n: int,
    ) -> None:
        if n < 0:
            raise Exception(f"{self.__class__.__name__} n attribute must be positive")
        self.n: int = n
        self.matcher: Union[_MatcherT, DoNotCareSentinel] = matcher

    def __or__(self, other: object) -> NoReturn:
        raise Exception(f"AtMostN cannot be used in a OneOf matcher")

    def __and__(self, other: object) -> NoReturn:
        raise Exception(f"AtMostN cannot be used in an AllOf matcher")

    def __invert__(self) -> NoReturn:
        raise Exception("Cannot invert an AtMostN matcher!")

    def __repr__(self) -> str:
        if self.n == 1:
            return f"ZeroOrOne({repr(self.matcher)})"
        else:
            return f"AtMostN({repr(self.matcher)}, n={self.n})"


def ZeroOrOne(
    matcher: Union[_MatcherT, DoNotCareSentinel] = DoNotCareSentinel.DEFAULT
) -> AtMostN[Union[_MatcherT, DoNotCareSentinel]]:
    """
    Used as a convenience wrapper to :class:`AtMostN```(n=1)``. This is effectively a
    maybe clause.
    """
    return cast(AtMostN[Union[_MatcherT, DoNotCareSentinel]], AtMostN(matcher, n=1))


def DoesNotMatch(obj: _OtherNodeT) -> _OtherNodeT:
    """
    Used as a way of specifying that the containing matcher's logic should be
    inverted.
    """

    # This type is a complete, dirty lie, but there's no way to recursively apply
    # a parameter to each type inside a Union that may be in a _OtherNodeT.
    # However, given the way InverseOf works (it will unwrap itself if
    # inverted again), and the way we apply De Morgan's law for OneOf and AllOf,
    # this lie ends up getting us correct typing. Anywhere a node is valid, using
    # DoesNotMatch(node) is also valid.
    #
    # ~MatchIfTrue is still MatchIfTrue
    # ~OneOf[x] is AllOf[~x]
    # ~AllOf[x] is OneOf[~x]
    # ~~x is x
    #
    # So, under all circumstances, since OneOf/AllOf are both allowed in every
    # instance, and given that inverting MatchIfTrue is still MatchIfTrue,
    # and inverting an inverted value returns us the original, its clear that
    # there are no operations we can possibly do that bring us outside of the
    # types specified in the generated matchers as long as we lie that DoesNotMatch
    # returns the value passed in.
    if isinstance(obj, (BaseMatcherNode, MatchIfTrue, InverseOf)):
        # We can use the overridden __invert__ in this case. Pyre doesn't think
        # we can though, and casting doesn't fix the issue.
        # pyre-ignore All three types above have overridden __invert__.
        inverse = ~obj
    else:
        # We must wrap in a InverseOf.
        inverse = InverseOf(obj)
    return cast(_OtherNodeT, inverse)


def _sequence_matches(  # noqa: C901
    nodes: Sequence[Union[MaybeSentinel, libcst.CSTNode]],
    matchers: Sequence[
        Union[
            BaseMatcherNode,
            _BaseWildcardNode,
            MatchIfTrue[Callable[..., bool]],
            DoNotCareSentinel,
        ]
    ],
) -> bool:
    if not nodes and not matchers:
        # Base case, empty lists are alwatys matches
        return True
    if not nodes and matchers:
        # Base case, we have one or more matcher that wasn't matched
        return all(
            (isinstance(m, AtLeastN) and m.n == 0) or isinstance(m, AtMostN)
            for m in matchers
        )
    if nodes and not matchers:
        # Base case, we have nodes left that don't match any matcher
        return False

    # Recursive case, nodes and matchers LHS matches
    node = nodes[0]
    matcher = matchers[0]
    if isinstance(matcher, DoNotCareSentinel):
        # We don't care about the value for this node.
        return _sequence_matches(nodes[1:], matchers[1:])
    elif isinstance(matcher, _BaseWildcardNode):
        if isinstance(matcher, AtMostN):
            if matcher.n > 0:
                # First, assume that this does match a node (greedy).
                # Consume one node since it matched this matcher.
                if _attribute_matches(nodes[0], matcher.matcher) and _sequence_matches(
                    nodes[1:],
                    [AtMostN(matcher.matcher, n=matcher.n - 1), *matchers[1:]],
                ):
                    return True
            # Finally, assume that this does not match the current node.
            # Consume the matcher but not the node.
            if _sequence_matches(nodes, matchers[1:]):
                return True
        elif isinstance(matcher, AtLeastN):
            if matcher.n > 0:
                # Only match if we can consume one of the matches, since we still
                # need to match N nodes.
                if _attribute_matches(nodes[0], matcher.matcher) and _sequence_matches(
                    nodes[1:],
                    [AtLeastN(matcher.matcher, n=matcher.n - 1), *matchers[1:]],
                ):
                    return True
            else:
                # First, assume that this does match a node (greedy).
                # Consume one node since it matched this matcher.
                if _attribute_matches(nodes[0], matcher.matcher) and _sequence_matches(
                    nodes[1:], matchers
                ):
                    return True
                # Now, assume that this does not match the current node.
                # Consume the matcher but not the node.
                if _sequence_matches(nodes, matchers[1:]):
                    return True
        else:
            # There are no other types of wildcard consumers, but we're making
            # pyre happy with that fact.
            raise Exception(f"Logic error unrecognized wildcard {type(matcher)}!")
    elif _matches(node, matcher):
        # These values match directly
        return _sequence_matches(nodes[1:], matchers[1:])

    # Failed recursive case, no match
    return False


_AttributeValueT = Optional[Union[MaybeSentinel, libcst.CSTNode, str, bool]]
_AttributeMatcherT = Optional[Union[BaseMatcherNode, DoNotCareSentinel, str, bool]]


def _attribute_matches(  # noqa: C901
    node: Union[_AttributeValueT, Sequence[_AttributeValueT]],
    matcher: Union[_AttributeMatcherT, Sequence[_AttributeMatcherT]],
) -> bool:
    if isinstance(matcher, DoNotCareSentinel):
        # We don't care what this is, so don't penalize a non-match.
        return True
    if isinstance(matcher, InverseOf):
        # Return the opposite evaluation
        return not _attribute_matches(node, matcher.matcher)

    if isinstance(matcher, MatchIfTrue):
        # We should only return if the matcher function is true.
        return matcher.func(node)

    if matcher is None:
        # Should exactly be None
        return node is None

    if isinstance(matcher, str):
        # Should exactly match matcher text
        return node == matcher

    if isinstance(matcher, bool):
        # Should exactly match matcher bool
        return node is matcher

    if isinstance(node, collections.abc.Sequence):
        # Given we've generated the types for matchers based on LibCST, we know that
        # this is true unless the node is badly constructed and types were ignored.
        node = cast(
            Sequence[Union[MaybeSentinel, RemovalSentinel, libcst.CSTNode]], node
        )

        if isinstance(matcher, OneOf):
            # We should compare against each of the sequences in the OneOf
            for m in matcher.options:
                if isinstance(m, collections.abc.Sequence):
                    # Should match the sequence of requested nodes
                    if _sequence_matches(node, m):
                        return True
                elif isinstance(m, MatchIfTrue):
                    return matcher.func(node)
        elif isinstance(matcher, AllOf):
            # We should compare against each of the sequences in the AllOf
            for m in matcher.options:
                if isinstance(m, collections.abc.Sequence):
                    # Should match the sequence of requested nodes
                    if not _sequence_matches(node, m):
                        return False
                elif isinstance(m, MatchIfTrue):
                    return matcher.func(node)
                else:
                    # The value in the AllOf wasn't a sequence, it can't match.
                    return False
            # We passed the checks above for each node, so we passed.
            return True
        elif isinstance(matcher, collections.abc.Sequence):
            # We should assume that this matcher is a sequence to compare. Given
            # the way we generate match classes, this should be true unless the
            # match is badly constructed and types were ignored.
            return _sequence_matches(
                node,
                cast(
                    Sequence[
                        Union[
                            BaseMatcherNode,
                            _BaseWildcardNode,
                            MatchIfTrue[Callable[..., bool]],
                            DoNotCareSentinel,
                        ]
                    ],
                    matcher,
                ),
            )

        # We exhausted our possibilities, there's no match
        return False

    # Base case, should match node via matcher. We know the type of node is
    # correct here because we generate matchers directly off of LibCST nodes,
    # so the only way it is wrong is if the node was badly constructed and
    # types were ignored.
    return _matches(
        cast(Union[MaybeSentinel, RemovalSentinel, libcst.CSTNode], node),
        cast(Union[BaseMatcherNode, MatchIfTrue], matcher),
    )


def _node_matches(
    node: libcst.CSTNode,
    matcher: Union[
        BaseMatcherNode,
        MatchIfTrue[Callable[..., bool]],
        InverseOf[Union[BaseMatcherNode, MatchIfTrue[Callable[..., bool]]]],
    ],
) -> bool:
    # If this is a InverseOf, then invert the result.
    if isinstance(matcher, InverseOf):
        return not _node_matches(node, matcher.matcher)

    # Now, check if this is a lambda matcher.
    if isinstance(matcher, MatchIfTrue):
        return matcher.func(node)

    # Now, check that the node and matcher classes are the same.
    if node.__class__.__name__ != matcher.__class__.__name__:
        return False

    # Now, check that the children match for each attribute.
    for field in fields(matcher):
        if field.name == "_metadata":
            # We don't care about this field, its a dataclasses implementation detail.
            continue

        desired = getattr(matcher, field.name)
        actual = getattr(node, field.name)
        if not _attribute_matches(actual, desired):
            return False

    # We didn't find a non-match in the above loop, so it matches!
    return True


def _matches(
    node: Union[MaybeSentinel, libcst.CSTNode],
    matcher: Union[
        BaseMatcherNode,
        MatchIfTrue[Callable[..., bool]],
        InverseOf[Union[BaseMatcherNode, MatchIfTrue[Callable[..., bool]]]],
    ],
) -> bool:
    if isinstance(node, MaybeSentinel):
        # We can't possibly match on a maybe sentinel, so it only matches if
        # the matcher we have is a InverseOf.
        return isinstance(matcher, InverseOf)

    # Now, evaluate the matcher node itself.
    if isinstance(matcher, OneOf):
        return any(_node_matches(node, matcher) for matcher in matcher.options)
    elif isinstance(matcher, AllOf):
        return all(_node_matches(node, matcher) for matcher in matcher.options)
    else:
        return _node_matches(node, matcher)


def matches(
    node: Union[MaybeSentinel, RemovalSentinel, libcst.CSTNode],
    matcher: BaseMatcherNode,
) -> bool:
    """
    Given an arbitrary node from a LibCST tree, and an arbitrary matcher, returns
    ``True`` if the node matches the shape defined in the matcher. Note that the node
    can also be a :class:`~libcst.RemovalSentinel` or a :class:`~libcst.MaybeSentinel`
    in order to use matches directly on transform results and node attributes. In these
    cases, :func:`matches` will always return ``False``.

    The matcher can be any concrete match class that subclasses from
    :class:BaseMatcherNode, or a :class:`OneOf`/:class:`AllOf` combination node. It
    cannot be a :class:`MatchIfTrue` matcher since this is redundant. It cannot be a
    :class:`AtLeastN` or :class:`AtMostN` matcher because these types are wildcards
    which can only be used inside sequences.
    """
    if isinstance(node, RemovalSentinel):
        # We can't possibly match on a removal sentinel, so it doesn't match.
        return False
    if isinstance(matcher, (AtLeastN, AtMostN, MatchIfTrue)):
        # We can't match this, since these matchers are forbidden at top level.
        # These are not subclasses of BaseMatcherNode, but in the case that the
        # user is not using type checking, this should still behave correctly.
        return False

    return _matches(node, matcher)
