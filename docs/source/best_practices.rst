==============
Best Practices
==============

While there are plenty of ways to interact with LibCST, we recommend some patterns
over others. Various best practices are laid out here along with their justifications.

Avoid ``isinstance`` when traversing
------------------------------------

Excessive use of ``isinstance`` implies that you should rewrite your check as a
matcher or unroll it into a set of visitor methods. Often, you should make use of
:func:`~libcst.ensure_type` to make your type checker aware of a node's type.

Often it is far easier to use :ref:`libcst-matchers` over explicit instance checks
in a transform. Matching against some pattern and then extracting a value from a
node's child is often easier and far more readable. Unfortunately this clashes with
various type-checkers which do not understand that :func:`~libcst.matchers.matches`
guarantees a particular set of children. Instead of instance checks, you should use
:func:`~libcst.ensure_type` which can be inlined and nested.

For example, if you have written the following::

    def get_identifier_name(node: cst.CSTNode) -> Optional[str]:
        if m.matches(node, m.Name()):
            assert isinstance(node, cst.Name)
            return node.value
        return None

You could instead write something like::

    def get_identifier_name(node: cst.CSTNode) -> Optional[str]:
        return (
            cst.ensure_type(node, cst.Name).value
            if m.matches(node, m.Name())
            else None
        )

If you find yourself attempting to manually traverse a tree using ``isinstance``,
you can often rewrite your code using visitor methods instead. Nested instance checks
can often be unrolled into visitors methods along with matcher decorators. This may
entail adding additional state to your visitor, but the resulting code is far more
likely to work after changes to LibCST itself. For example, if you have written the
following::

    class CountBazFoobarArgs(cst.CSTVisitor):
        """
        Given a set of function names, count how many arguments to those function
        calls are the identifiers "baz" or "foobar".
        """

        def __init__(self, functions: Set[str]) -> None:
            super().__init__()
            self.functions: Set[str] = functions
            self.arg_count: int = 0

        def visit_Call(self, node: cst.Call) -> None:
            # See if the call itself is one of our functions we care about
            if isinstance(node.func, cst.Name) and node.func.value in self.functions:
                # Loop through each argument
                for arg in node.args:
                    # See if the argument is an identifier matching what we want to count
                    if isinstance(arg.value, cst.Name) and arg.value.value in {"baz", "foobar"}:
                        self.arg_count += 1
                        
You could instead write something like::

    class CountBazFoobarArgs(m.MatcherDecoratableVisitor):
        """
        Given a set of function names, count how many arguments to those function
        calls are the identifiers "baz" or "foobar".
        """

        def __init__(self, functions: Set[str]) -> None:
            super().__init__()
            self.functions: Set[str] = functions
            self.arg_count: int = 0
            self.call_stack: List[str] = []

        def visit_Call(self, node: cst.Call) -> None:
            # Store all calls in a stack
            if m.matches(node.func, m.Name()):
                self.call_stack.append(cst.ensure_type(node.func, cst.Name).value)

        def leave_Call(self, original_node: cst.Call) -> None:
            # Pop the latest call off the stack
            if m.matches(node.func, m.Name()):
                self.call_stack.pop()

        @m.visit(m.Arg(m.Name("baz") | m.Name("foobar")))
        def _count_args(self, node: cst.Arg) -> None:
            # See if the most shallow call is one we're interested in, so we can
            # count the args we care about only in calls we care about.
            if self.call_stack[-1] in self.functions:
                self.arg_count += 1

While there is more code than the previous example, it is arguably easier to understand
and maintain each part of the code. It is also immune to any future changes to LibCST
which change's the tree shape. Note that LibCST is traversing the tree completely
in both cases, so while the first appears to be faster, it is actually doing the same
amount of work as the second.

Prefer ``updated_node`` when modifying trees
--------------------------------------------

When you are using :class:`~libcst.CSTTransformer` to modify a LibCST tree, only return
modifications to ``updated_node``. The ``original_node`` parameter on any
``leave_<Node>`` method is provided for book-keeping and is guaranteed to be
equal via ``==`` and ``is`` checks to the ``node`` parameter in the corresponding
``visit_<Node>`` method. Remember that LibCST trees are immutable, so the only
way to make a modification is to return a new tree. Hence, by the time we get to
calling ``leave_<Node>`` methods, we have an updated tree whose children have been
modified. Therefore, you should only return ``original_node`` when you want to
explicitly discard changes performed on the node's children.

Say you wanted to rename all function calls which were calling global functions.
So, you might write the following::

    class FunctionRenamer(cst.CSTTransformer):
        def leave_Call(self, original_node: cst.Call, updated_node: cst.Call) -> cst.Call:
            if m.matches(original_node.func, m.Name()):
                return original_node.with_changes(
                    func=cst.Name(
                        "renamed_" + cst.ensure_type(original_node.func, cst.Name).value
                    )
                )
            return original_node

Consider writing instead::

    class FunctionRenamer(cst.CSTTransformer):
        def leave_Call(self, original_node: cst.Call, updated_node: cst.Call) -> cst.Call:
            if m.matches(updated_node.func, m.Name()):
                return updated_node.with_changes(
                    func=cst.Name(
                        "renamed_" + cst.ensure_type(updated_node.func, cst.Name).value
                    )
                )
            return updated_node

The version that returns modifications to ``original_node`` has a subtle bug. Consider
the following code snippet::

    some_func(1, 2, other_func(3))

Running the recommended transform will return us a new code snippet that looks like this::

    renamed_some_func(1, 2, renamed_other_func(3))

However, running the version which modifies ``original_node`` will instead return::

    renamed_some_func(1, 2, other_func(3))

That's because the ``updated_node`` tree contains the modification to ``other_func``.
By returning modifications to ``original_node`` instead of ``updated_node``, we accidentally
discarded all the work done deeper in the tree.

.. _libcst-config_best_practice:

Provide a ``config`` when generating code from templates
--------------------------------------------------------

When generating complex trees it is often far easier to pass a string to
:func:`~libcst.parse_statement` or :func:`~libcst.parse_expression` than it is to
manually construct the tree. When using these functions to generate code, you should
always use the ``config`` parameter in order to generate code that matches the
defaults of the module you are modifying. The :class:`~libcst.Module` class even has
a helper attribute :attr:`~libcst.Module.config_for_parsing` to make it easy to use. This
ensures that line endings and indentation are consistent with the defaults in the
module you are adding the code to.

For example, to add a print statement to the end of a module::

    module = cst.parse_module(some_code_string)
    new_module = module.with_changes(
        body=(
            *module.body,
            cst.parse_statement(
                "print('Hello, world!')",
                config=module.config_for_parsing,
            ),
        ),
    )
    new_code_string = new_module.code

Leaving out the ``config`` parameter means that LibCST will assume some defaults
and could result in added code which is formatted differently than the rest of the
module it was added to. In the above example, because we used the config from the
already-parsed example, the print statement will be added with line endings matching
the rest of the module. If we neglect the ``config`` parameter, we might accidentally
insert a windows line ending into a unix file or vice versa, depending on what system
we ran the code under.
