=======
Helpers
=======

Helpers are higher level functions built for reducing recurring code boilerplate.
We add helpers as method of ``CSTNode`` or ``libcst.helpers`` package based on those principles:

- ``CSTNode`` method: simple, read-only and only require data of the direct children of a CSTNode.
- ``libcst.helpers``: node transforms or require recursively traversing the syntax tree.

libcst.helpers
--------------

.. autofunction:: libcst.helpers.insert_header_comments
.. autofunction:: libcst.helpers.get_full_name_for_node
.. autofunction:: libcst.helpers.ensure_type
