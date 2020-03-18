# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from typing import Dict, List, NamedTuple, Sequence, Union

import libcst as cst
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareTransformer


class InsertStatementsVisitorContext(NamedTuple):
    """
    Context for the InsertStatementsVisitor about which statements
    have been requested to insert before/after the current one.
    """

    # Mapping from a statement to list of statements to insert before it
    stmts_before: Dict[cst.BaseStatement, List[cst.BaseStatement]]

    # Mapping from a statement to list of statements to insert after it
    stmts_after: Dict[cst.BaseStatement, List[cst.BaseStatement]]

    # List of statements to insert before the current one
    stmts_before_acc: List[cst.BaseStatement]

    # List of statements to insert after the current one
    stmts_after_acc: List[cst.BaseStatement]


class InsertStatementsVisitor(ContextAwareTransformer):
    """
    Allows :class:`~libcst.visitor.CSTTransformer` to insert multiple statements before and after the currently-visited statement.
    """

    CONTEXT_KEY = "InsertStatementsVisitor"

    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)
        self.context.scratch[
            InsertStatementsVisitor.CONTEXT_KEY
        ] = InsertStatementsVisitorContext({}, {}, [], [])

    def _context(self) -> InsertStatementsVisitorContext:
        return self.context.scratch[InsertStatementsVisitor.CONTEXT_KEY]

    def insert_statements_before_current(self, stmts: List[cst.BaseStatement]) -> None:
        self._context().stmts_before_acc.extend(stmts)

    def insert_statements_after_current(self, stmts: List[cst.BaseStatement]) -> None:
        self._context().stmts_after_acc.extend(stmts)

    def _build_body(self, body: Sequence[cst.BaseStatement]) -> List[cst.BaseStatement]:
        stmts_before = self._context().stmts_before
        stmts_after = self._context().stmts_after
        new_stmts = []
        for stmt in body:
            new_stmts.extend(stmts_before.get(stmt, []))
            new_stmts.append(stmt)
            new_stmts.extend(stmts_after.get(stmt, []))
        return new_stmts

    def leave_IndentedBlock(
        self, original_node: cst.IndentedBlock, updated_node: cst.IndentedBlock
    ) -> cst.BaseSuite:
        final_node = super().leave_IndentedBlock(original_node, updated_node)
        if isinstance(final_node, cst.IndentedBlock):
            return final_node.with_changes(body=self._build_body(final_node.body))
        return final_node

    def leave_Module(
        self, original_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        updated_node = super().leave_Module(original_node, updated_node)
        return updated_node.with_changes(body=self._build_body(updated_node.body))

    def leave_SimpleStatementLine(
        self,
        original_node: cst.SimpleStatementLine,
        updated_node: cst.SimpleStatementLine,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_SimpleStatementLine(original_node, updated_node)
        context = self._context()
        if isinstance(final_node, cst.BaseStatement):
            context.stmts_before[final_node] = context.stmts_before_acc.copy()
            context.stmts_after[final_node] = context.stmts_after_acc.copy()
        context.stmts_before_acc.clear()
        context.stmts_after_acc.clear()
        return final_node
