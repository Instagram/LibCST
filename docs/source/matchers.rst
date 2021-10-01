.. _libcst-matchers:

========
Matchers
========

Matchers are provided as a way of asking whether a particular LibCST node and its
children match a particular shape. It is possible to write a visitor that
tracks attributes using ``visit_<Node>`` methods. It is also possible to implement
manual instance checking and traversal of a node's children. However, both are
cumbersome to write and hard to understand. Matchers offer a more concise way of
defining what attributes on a node matter when matching against predefined patterns.

To accomplish this, a matcher has been created which corresponds to each LibCST
node documented in :ref:`libcst-nodes`. Matchers default each of their attributes
to the special sentinel matcher :func:`~libcst.matchers.DoNotCare`. When constructing
a matcher, you can initialize the node with only the values of attributes that
you are concerned with, leaving the rest of the attributes set to
:func:`~libcst.matchers.DoNotCare` in order to skip comparing against them.

------------
Matcher APIs
------------

Functions
^^^^^^^^^

Matchers can be used either by calling :func:`~libcst.matchers.matches` or
:func:`~libcst.matchers.findall` directly, or by using various decorators to
selectively control when LibCST calls visitor functions.

.. autofunction:: libcst.matchers.matches
.. autofunction:: libcst.matchers.findall
.. autofunction:: libcst.matchers.extract
.. autofunction:: libcst.matchers.extractall
.. autofunction:: libcst.matchers.replace

.. _libcst-matcher-decorators:

Decorators
^^^^^^^^^^

The following decorators can be placed onto a method in a visitor or transformer
in order to convert it into a visitor which is called when the provided matcher is
true.

.. autofunction:: libcst.matchers.visit
.. autofunction:: libcst.matchers.leave

The following decorators can be placed onto any existing ``visit_<Node>`` or
``leave_<Node>`` visitor, as well as any visitor created using either
:func:`~libcst.matchers.visit` or :func:`~libcst.matchers.leave`. They control
whether the visitor itself gets called or skipped by LibCST when traversing a tree.
Note that when a visitor function is skipped, its children will still be visited
based on the rules set forth in :ref:`libcst-visitors`. Namely, if you have a separate
``visit_<Node>`` visitor that returns ``False`` for a particular node, we will not
traverse to its children.

.. autofunction:: libcst.matchers.call_if_inside
.. autofunction:: libcst.matchers.call_if_not_inside

When using matcher decorators, your visitors must subclass from
:class:`~libcst.matchers.MatcherDecoratableVisitor` instead of :class:`libcst.CSTVisitor`,
and from :class:`~libcst.matchers.MatcherDecoratableTransformer` instead of
:class:`libcst.CSTTransformer`. This is so that visitors and transformers not making
use of matcher decorators do not pay the extra cost of their implementation. Note that
if you do not subclass from :class:`~libcst.matchers.MatcherDecoratableVisitor` or
:class:`~libcst.matchers.MatcherDecoratableTransformer`, you can still use the
:func:`~libcst.matchers.matches` function.

Both of these classes are strict subclasses of their corresponding LibCST base class,
so they can be used anywhere that expects a LibCST base class. See :ref:`libcst-visitors`
for more information.

.. autoclass:: libcst.matchers.MatcherDecoratableVisitor
.. autoclass:: libcst.matchers.MatcherDecoratableTransformer

Traversal Order
^^^^^^^^^^^^^^^

Visit and leave functions created using :func:`~libcst.matchers.visit` or
:func:`~libcst.matchers.leave` follow the traversal order rules laid out in
LibCST's visitor :ref:`libcst-visitor-traversal` with one additional rule. Any
visit function created using the :func:`~libcst.matchers.visit` decorator will be
called **before** a ``visit_<Node>`` function if it is defined for your visitor.
The order in which various visit functions which are created with
:func:`~libcst.matchers.visit` are called is indeterminate, but all such functions
will be called before calling the ``visit_<Node>`` method. Similarly, any leave
function created using the :func:`~libcst.matchers.leave` decorator will be called
**after** a ``leave_<Node>`` function if it is defined for your visitor. The order
in which various leave functions which are created with
:func:`~libcst.matchers.leave` are called is indeterminate, but all such functions
will be called after calling the ``visit_<Node>`` function if it is defined for
your visitor.

This has a few implications. The first is that if you return ``False`` from a
``visit_<Node>`` method, we are guaranteed to call your decorated visit functions
as well. Second, when modifying a node in both ``leave_<Node>`` and a visitor
created with :func:`~libcst.matchers.leave`, the ``original_node`` will be unchanged
for both and the ``updated_node`` available to the decorated leave method will be
the node that is returned by the ``leave_<Node>`` method. Chaining modifications
across multiple leave functions is supported, but must be done with care.

-------------
Matcher Types
-------------

Concrete Matchers
^^^^^^^^^^^^^^^^^

For each node found in :ref:`libcst-nodes`, a corresponding concrete matcher
has been generated. Each matcher has attributes identical to its LibCST node
counterpart. For example, :class:`libcst.Expr` includes the ``value`` and ``semicolon``
attributes, and therefore :class:`libcst.matchers.Expr` similarly includes the same
attributes. Just as :class:`libcst.Expr`'s ``value`` is typed as taking a
:class:`libcst.BaseExpression`, :class:`libcst.matchers.Expr`'s ``value`` is typed
as taking a :class:`libcst.matchers.BaseExpression`. For every node that exists in
LibCST, both concrete and abstract, a corresponding matcher has been defined.

There are a few special cases to the rules laid out above. For starters, matchers
don't support evaluating :class:`~libcst.MaybeSentinel`. There is no way to specify
that you wish to match against a :class:`~libcst.MaybeSentinel` except with the
:func:`~libcst.matchers.DoNotCare` matcher. This tends not to be an issue in
practice because :class:`~libcst.MaybeSentinel` is only found on syntax nodes.

While there are base classes such as :class:`libcst.matchers.BaseExpression`, you
cannot match directly on them. They are provided for typing purposes only in order
to exactly match the types on LibCST node attributes. If you need to match on
all concrete subclasses of a base class, we recommend using the special matcher
:class:`~libcst.matchers.OneOf`.

.. autoclass:: libcst.matchers.BaseMatcherNode

Special Matchers
^^^^^^^^^^^^^^^^

Special matchers are matchers that don't have a corresponding LibCST node. Concrete
matchers only match against their corresponding LibCST node, limiting their use
under certain circumstances. Special matchers fill in the gap by allowing
higher-level logic constructs such as inversion. You can use any special matcher
in place of a concrete matcher when specifying matcher attributes. Additionally,
you can also use the :class:`~libcst.matchers.AllOf` and
:class:`~libcst.matchers.OneOf` special matchers in place of a concrete matcher
when calling :func:`~libcst.matchers.matches` or using decorators.

.. autoclass:: libcst.matchers.OneOf
.. autoclass:: libcst.matchers.AllOf
.. autoclass:: libcst.matchers.TypeOf
.. autofunction:: libcst.matchers.DoesNotMatch
.. autoclass:: libcst.matchers.MatchIfTrue
.. autofunction:: libcst.matchers.MatchRegex
.. autoclass:: libcst.matchers.MatchMetadata
.. autoclass:: libcst.matchers.MatchMetadataIfTrue
.. autofunction:: libcst.matchers.SaveMatchedNode
.. autofunction:: libcst.matchers.DoNotCare

Sequence Wildcard Matchers
^^^^^^^^^^^^^^^^^^^^^^^^^^

Sequence wildcard matchers are matchers that only get used when constructing a
sequence to match against. Not all LibCST nodes have attributes which are sequences,
but for those that do, sequence wildcard matchers offer a great degree of
flexibility. Unlike all other matcher types, these allow you to match against
more than one LibCST node, much like wildcards in regular expressions do.

LibCST does not implicitly match on partial sequences for you. So, when matching
against a sequence you will need to provide a complete pattern. This often means
using helpers such as :func:`~libcst.matchers.ZeroOrMore` as the first and last
element of your sequence. Think of it as the difference between Python's
`re.match <https://docs.python.org/3/library/re.html#re.match>`_ and
`re.fullmatch <https://docs.python.org/3/library/re.html#re.fullmatch>`_ functions.
LibCST matchers behave like the latter so that it is possible to specify sequences
which must start with, end with or be exactly equal to some pattern.

.. autoclass:: libcst.matchers.AtLeastN
.. autofunction:: libcst.matchers.ZeroOrMore
.. autoclass:: libcst.matchers.AtMostN
.. autofunction:: libcst.matchers.ZeroOrOne
