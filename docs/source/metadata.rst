.. _libcst-metadata:

Metadata
========

LibCST ships with a metadata interface that defines a standardized way to
associate nodes in a CST with arbitrary metadata while maintaining the immutability
of the tree. The metadata interface is designed to be declarative and type safe.
Here's a quick example of using the metadata interface to get line and column
numbers of nodes through the :class:`~libcst.SyntacticPositionProvider`:

.. _libcst-metadata-position-example:
.. code-block:: python

    class NamePrinter(cst.CSTVisitor):
        METADATA_DEPENDENCIES = (cst.SyntacticPositionProvider,)

        def visit_Name(self, node: cst.Name) -> None:
            pos = self.get_metadata(cst.SyntacticPositionProvider, node).start
            print(f"{node.value} found at line {pos.line}, column {pos.column}")

    wrapper = cst.MetadataWrapper(cst.parse_module("x = 1"))
    result = wrapper.visit(NamePrinter())  # should print "x found at line 1, column 0"

More examples of using the metadata interface can be found on the
:doc:`Metadata Tutorial <metadata_tutorial>`.

Accessing Metadata
------------------

To work with metadata you need to wrap a module with a :class:`~libcst.MetadataWrapper`.
The wrapper provides a :func:`~libcst.MetadataWrapper.resolve` function and a
:func:`~libcst.MetadataWrapper.resolve_many` function to generate metadata.

.. autoclass:: libcst.MetadataWrapper

If you're working with visitors, which extend :class:`~libcst.MetadataDependent`, 
metadata dependencies will be automatically computed when visited by a 
:class:`~libcst.MetadataWrapper` and are accessible through
:func:`~libcst.MetadataDependent.get_metadata`

.. autoclass:: libcst.MetadataDependent

Providing Metadata
------------------

Metadata is generated through provider classes that can be declared as a dependency
by a subclass of :class:`~libcst.MetadataDependent`. These providers are then
resolved automatically using methods provided by :class:`~libcst.MetadataWrapper`.
In most cases, you should extend :class:`~libcst.BatchableMetadataProvider` when
writing a provider, unless you have a particular reason to not to use a
batchable visitor. Only extend from :class:`~libcst.BaseMetadataProvider` if
your provider does not use the visitor pattern for computing metadata for a tree.

.. autoclass:: libcst.BaseMetadataProvider
.. autoclass:: libcst.BatchableMetadataProvider
.. autoclass:: libcst.VisitorMetadataProvider

.. _libcst-metadata-position:

Position Metadata
-----------------

Position (line and column numbers) metadata are accessible through the metadata
interface by declaring the one of the following providers as a dependency. For
most cases, :class:`~libcst.SyntacticPositionProvider` is what you probably want.
Accessing position metadata through the :class:`~libcst.MetadataDepedent`
interface will return a :class:`~libcst.CodeRange` object. See
:ref:`the above example<libcst-metadata-position-example>`.

.. autoclass:: libcst.BasicPositionProvider
.. autoclass:: libcst.SyntacticPositionProvider

.. autoclass:: libcst.CodeRange
.. autoclass:: libcst.CodePosition
