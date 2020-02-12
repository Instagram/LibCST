=======
Helpers
=======

Helpers are higher level functions built for reducing recurring code boilerplate.
We add helpers as method of ``CSTNode`` or ``libcst.helpers`` package based on those principles:

  - ``CSTNode`` method: simple, read-only and only require data of the direct children of a CSTNode.
  - ``libcst.helpers``: node transforms or require recursively traversing the syntax tree.

Construction Helpers
--------------------

Functions that assist in creating a new LibCST tree.

.. autofunction:: libcst.helpers.parse_template_module
.. autofunction:: libcst.helpers.parse_template_expression
.. autofunction:: libcst.helpers.parse_template_statement

Transformation Helpers
----------------------

Functions that assist in transforming an existing LibCST node.

.. autofunction:: libcst.helpers.insert_header_comments

Traversing Helpers
------------------

Functions that assist in traversing an existing LibCST tree.

.. autofunction:: libcst.helpers.get_full_name_for_node
.. autofunction:: libcst.helpers.get_full_name_for_node_or_raise
.. autofunction:: libcst.helpers.ensure_type
