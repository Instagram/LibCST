# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import abc
import builtins
from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import Generic, List, MutableMapping, Optional, Tuple, TypeVar

import libcst as cst
from libcst.metadata.base_provider import BatchableMetadataProvider
from libcst.metadata.expression_context_provider import (
    ExpressionContext, ExpressionContextProvider)


@dataclass(frozen=True)
class Access:
    name: str
    node: cst.CSTNode
    scope: "Scope"


_ASTType = TypeVar("_ASTType", bound=cst.CSTNode)


@dataclass(frozen=True)
class Assignment(Generic[Type[cst.CSTNode]]):
    name: str
    node: cst.CSTNode
    scope: "Scope"
    __accesses: List[Access] = field(default_factory=list)

    def record_access(self, access: Access) -> None:
        self.__accesses.append(access)

    @property
    def accesses(self) -> Tuple[Access, ...]:
        # we don't want to publicly expose the mutable version of this
        return tuple(self.__accesses)


class Scope(abc.ABC):
    parent: "Scope"
    globals: "GlobalScope"
    _assignments = MutableMapping[str, List[Assignment[cst.CSTNode]]]

    def __init__(self, parent: "Scope") -> None:
        super().__init__()
        self.parent = parent

    def record_assignment(self, name: str, node: ast.AST) -> None:
        pass


class GlobalScope(Scope):
    pass


class ScopeProvider(BatchableMetadataProvider[Optional[Scope]]):
    METADATA_DEPENDENCIES = (ExpressionContextProvider,)

    @contextmanager
    def _new_scope(self, kind: ):

    def visit_Name(self, node: cst.Name) -> Optional[bool]:
        if (
            self.get_metadata(ExpressionContextProvider, node)
            == ExpressionContext.STORE
        ):
            self.scope.record_assignment()
