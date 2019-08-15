.. _libcst-metadata:

Metadata
========

LibCST ships with a metadata interface that defines a standardized way to
associate nodes in a CST with arbitrary metadata while maintaining the immutability
of the tree. The metadata interface is designed to be declarative and type safe.
Here's a quick example of using the metadata interface to get line and column
numbers of nodes:

.. code-block:: python

    class NamePrinter(cst.CSTVisitor):
        METADATA_DEPENDENCIES = (cst.SyntacticPositionProvider,)

        def visit_Name(self, node: cst.Name) -> None:
            pos = self.get_metadata(cst.SyntacticPositionProvider, node).start
            print(f"{node.value} found at line {pos.line}, column {pos.column}")

    wrapper = cst.MetadataWrapper(cst.parse_module("x = 1"))
    result = wrapper.visit(NamePrinter())  # should print "x found at line 1, column 0"

Accessing Metadata
------------------

Metadata providers are declared as dependencies by a :class:`~libcst.MetadataDependent`
or any of its subclasses and will be computed automatically whenever visited
by a :class:`~libcst.MetadataWrapper`. Alternatively, you can use 
:func:`~libcst.MetadataWrapper.resolve` or :func:`~libcst.MetadataWrapper.resolve_many`
in the wrapper to generate and access metadata directly.

.. autoclass:: libcst.MetadataDependent
.. autoclass:: libcst.MetadataWrapper

Providing Metadata
------------------

Metadata is generated through provider classes that can be declared as a dependency
by a subclass of :class:`~libcst.MetadataDependent`. These providers are then
resolved automatically using methods provided by :class:`~libcst.MetadataWrapper`.

.. autoclass:: libcst.BaseMetadataProvider
.. autoclass:: libcst.VisitorMetadataProvider
.. autoclass:: libcst.BatchableMetadataProvider

Position Metadata
-----------------

Position (line and column numbers) metadata are accessible through the metadata
interface by declaring the one of the following providers as a dependency. For
most cases, :class:`~libcst.SyntacticPositionProvider` is what you probably want.
Accessing position metadata through the :class:`~libcst.MetadataDepedent`
interface will return a :class:`~libcst.CodeRange` object.

.. autoclass:: libcst.BasicPositionProvider
.. autoclass:: libcst.SyntacticPositionProvider

.. autoclass:: libcst.CodeRange
.. autoclass:: libcst.CodePosition
