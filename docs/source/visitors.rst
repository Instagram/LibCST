Visitors
========

.. autoclass:: libcst.CSTVisitor
.. autoclass:: libcst.CSTTransformer

Visit and Leave Helper Functions
--------------------------------

While it is possible to subclass from :class:`~libcst.CSTVisitor` or :class:`~libcst.CSTTransformer`
and override the ``on_visit``/``on_leave`` functions directly, it is not recommended. The default
implementation for both visitors will look up a ``visit_<Type[CSTNode]>`` and ``leave_<Type[CSTNode]>``
function on the visitor subclass and call it directly. If such a function exists for the node in
question, the visitor base class will call the relevant function, respecting the above outlined
semantics. If the function does not exist, the visitor base class will assume that you do not care
about that node and visit its children for you without requiring a default implementation.

As a convenience, you can return ``None`` instead of a boolean value from your ``visit_<Type[CSTNode]>``
functions. Returning a ``None`` value is treated as a request for default behavior, which causes the
visitor to traverse children. It is equivalent to returning ``True``, but requires no explicit return.

For example, the below visitor will visit every function definition, traversing to its children only
if the function name doesn't include the word "foo". Notice that we don't need to provide our own
``on_visit`` or ``on_leave`` function, nor do we need to provide visit and leave functions for the
rest of the nodes which we do not care about. This will have the effect of visiting all strings not
inside of functions that have "foo" in the name. Note that we take advantage of default behavior when
we decline to return a value in ``visit_SimpleString``.

.. code-block:: python

    class FooingAround(libcst.CSTVisitor):
        def visit_FunctionDef(self, node: libcst.FunctionDef) -> bool:
            return "foo" not in node.name.value

        def visit_SimpleString(self, node: libcst.SimpleString) -> None:
            print(node.value)

An example Python REPL using the above visitor is as follows::

    >>> import libcst
    >>> demo = libcst.parse_module("'abc'\n'123'\ndef foo():\n    'not printed'")
    >>> _ = demo.visit(FooingAround())
    'abc'
    '123'
