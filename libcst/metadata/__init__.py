# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from libcst._position import CodePosition, CodeRange
from libcst.metadata.base_provider import (
    BaseMetadataProvider,
    BatchableMetadataProvider,
    ProviderT,
    VisitorMetadataProvider,
)
from libcst.metadata.expression_context_provider import (
    ExpressionContext,
    ExpressionContextProvider,
)
from libcst.metadata.parent_node_provider import ParentNodeProvider
from libcst.metadata.position_provider import (
    BasicPositionProvider,
    PositionProvider,
    SyntacticPositionProvider,
    WhitespaceInclusivePositionProvider,
)
from libcst.metadata.reentrant_codegen import (
    CodegenPartial,
    ExperimentalReentrantCodegenProvider,
)
from libcst.metadata.scope_provider import (
    Access,
    Accesses,
    Assignment,
    Assignments,
    BaseAssignment,
    BuiltinAssignment,
    ClassScope,
    ComprehensionScope,
    FunctionScope,
    GlobalScope,
    QualifiedName,
    QualifiedNameProvider,
    QualifiedNameSource,
    Scope,
    ScopeProvider,
)
from libcst.metadata.wrapper import MetadataWrapper


__all__ = [
    "CodePosition",
    "CodeRange",
    "WhitespaceInclusivePositionProvider",
    "PositionProvider",
    "BasicPositionProvider",  # deprecated name for backwards compatibility
    "SyntacticPositionProvider",  # deprecated name for backwards compatibility
    "BaseMetadataProvider",
    "ExpressionContext",
    "ExpressionContextProvider",
    "BaseAssignment",
    "Assignment",
    "BuiltinAssignment",
    "Access",
    "Scope",
    "GlobalScope",
    "FunctionScope",
    "ClassScope",
    "ComprehensionScope",
    "ScopeProvider",
    "ParentNodeProvider",
    "QualifiedName",
    "QualifiedNameSource",
    "MetadataWrapper",
    "BatchableMetadataProvider",
    "VisitorMetadataProvider",
    "QualifiedNameProvider",
    "ProviderT",
    "Assignments",
    "Accesses",
    # Experimental APIs:
    "ExperimentalReentrantCodegenProvider",
    "CodegenPartial",
]
