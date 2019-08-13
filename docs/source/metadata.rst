.. _libcst-metadata:

Metadata
========

LibCST ships with a metadata interface that defines a standardized way to
associate nodes in a CST with arbitrary metadata. The metadata interface is
designed to be declarative and type safe.

Providing Metadata
------------------

Metadata is generated through provider classes that can be declared as a dependency
by a subclass of :class:`~libcst.MetadataDependent`. These providers are then
resolved automatically using methods provided by :class:`~libcst.MetadataWrapper`.

.. autoclass:: libcst.BaseMetadataProvider
.. autoclass:: libcst.VisitorMetadataProvider
.. autoclass:: libcst.BatchableMetadataProvider

Accessing Metadata
------------------

.. autoclass:: libcst.MetadataDependent

.. autoclass:: libcst.MetadataWrapper

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
