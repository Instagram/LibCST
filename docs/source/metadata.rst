.. _libcst-metadata:

Metadata
========

LibCST ships with a metadata interface that defines a standardized way to
associate nodes in a CST with arbitrary metadata. The metadata interface is
designed to be declarative and type safe.

Accessing Metadata
------------------

Metadata providers are declared as dependencies by a :class:`~libcst.MetadataDependent`
or any of its subclasses and will be computed automatically whenever one of the
resolve methods is used. Alternatively, you can use one of the visit methods in
the wrapper when working with visitors.

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
most cases, :class:`~libcst.SytacticPositionProvider` is what you probably want.

.. autoclass:: libcst.BasicPositionProvider
.. autoclass:: libcst.SyntacticPositionProvider

Accessing position metadata through the :class:`~libcst.MetadataDepedent`
interface will return a :class:`~libcst.CodeRange` object.

.. autoclass:: libcst.CodeRange
    :members:
.. autoclass:: libcst.CodePosition
    :members:

Usage
-----

Here's an example of a visitor that prints out the starting position of every
:class:`~libcst.Name` node.

.. code-block:: python

    import libcst as cst

    class NamePrinter(cst.CSTVisitor):
        METADATA_DEPENDENCIES = (cst.SyntacticPositionProvider,)

        def visit_Name(self, node: cst.Name) -> None:
            pos = self.get_metadata(cst.SyntacticPositionProvider, node).start
            print(f"{node.value} found at line {pos.line}, column {pos.column}")

    module = cst.parse_module("x = 1")
    wrapper = cst.MetadataWrapper(module)
    wrapper.visit(NamePrinter())  # should print "x found at line 1, column 0"
