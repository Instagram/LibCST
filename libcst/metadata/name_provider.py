# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import re
from pathlib import Path
from typing import Collection, List, Mapping, Optional, Pattern, Union

import libcst as cst
from libcst._metadata_dependent import MetadataDependent
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.scope_provider import (
    QualifiedName,
    QualifiedNameSource,
    ScopeProvider,
)


class QualifiedNameProvider(BatchableMetadataProvider[Collection[QualifiedName]]):
    """
    Compute possible qualified names of a variable CSTNode
    (extends `PEP-3155 <https://www.python.org/dev/peps/pep-3155/>`_).
    It uses the
    :func:`~libcst.metadata.Scope.get_qualified_names_for` underlying to get qualified names.
    Multiple qualified names may be returned, such as when we have conditional imports or an
    import shadows another. E.g., the provider finds ``a.b``, ``d.e`` and
    ``f.g`` as possible qualified names of ``c``::

        >>> wrapper = MetadataWrapper(
        >>>     cst.parse_module(dedent(
        >>>     '''
        >>>         if something:
        >>>             from a import b as c
        >>>         elif otherthing:
        >>>             from d import e as c
        >>>         else:
        >>>             from f import g as c
        >>>         c()
        >>>     '''
        >>>     ))
        >>> )
        >>> call = wrapper.module.body[1].body[0].value
        >>> wrapper.resolve(QualifiedNameProvider)[call],
        {
            QualifiedName(name="a.b", source=QualifiedNameSource.IMPORT),
            QualifiedName(name="d.e", source=QualifiedNameSource.IMPORT),
            QualifiedName(name="f.g", source=QualifiedNameSource.IMPORT),
        }

    For qualified name of a variable in a function or a comprehension, please refer
    :func:`~libcst.metadata.Scope.get_qualified_names_for` for more detail.
    """

    METADATA_DEPENDENCIES = (ScopeProvider,)

    def visit_Module(self, node: cst.Module) -> Optional[bool]:
        visitor = QualifiedNameVisitor(self)
        node.visit(visitor)

    @staticmethod
    def has_name(
        visitor: MetadataDependent, node: cst.CSTNode, name: Union[str, QualifiedName]
    ) -> bool:
        """Check if any of qualified name has the str name or :class:`~libcst.metadata.QualifiedName` name."""
        qualified_names = visitor.get_metadata(QualifiedNameProvider, node, set())
        if isinstance(name, str):
            return any(qn.name == name for qn in qualified_names)
        else:
            return any(qn == name for qn in qualified_names)


class QualifiedNameVisitor(cst.CSTVisitor):
    def __init__(self, provider: "QualifiedNameProvider") -> None:
        self.provider: QualifiedNameProvider = provider

    def on_visit(self, node: cst.CSTNode) -> bool:
        scope = self.provider.get_metadata(ScopeProvider, node, None)
        if scope:
            self.provider.set_metadata(node, scope.get_qualified_names_for(node))
        else:
            self.provider.set_metadata(node, set())
        super().on_visit(node)
        return True
