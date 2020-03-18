# pyre-strict
from typing import TYPE_CHECKING, List, Tuple, Type, Optional, Union, Dict, Sequence
import itertools

import libcst as cst
from libcst.codemod._visitor import ContextAwareTransformer
from libcst.codemod._context import CodemodContext
from libcst._nodes.base import CSTNode
from libcst._types import CSTNodeT
from libcst._removal_sentinel import RemovalSentinel


class InsertStatementsVisitor(ContextAwareTransformer):
    stmts_before: Dict[cst.BaseStatement, List[cst.BaseStatement]]
    stmts_after: Dict[cst.BaseStatement, List[cst.BaseStatement]]
    stmts_before_acc: List[cst.BaseStatement]
    stmts_after_acc: List[cst.BaseStatement]

    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)
        self.stmts_before = {}
        self.stmts_after = {}
        self.stmts_before_acc = []
        self.stmts_after_acc = []

    def insert_statements_before_current(self, stmts: List[cst.BaseStatement]) -> None:
        self.stmts_before_acc.extend(stmts)

    def insert_statements_after_current(self, stmts: List[cst.BaseStatement]) -> None:
        self.stmts_after_acc.extend(stmts)

    def _build_body(self, body: Sequence[cst.BaseStatement]) -> List[cst.BaseStatement]:
        new_stmts = []
        for stmt in body:
            new_stmts.extend(self.stmts_before.get(stmt, []))
            new_stmts.append(stmt)
            new_stmts.extend(self.stmts_after.get(stmt, []))
        return new_stmts

    def leave_IndentedBlock(self, original_node: cst.IndentedBlock,
                            updated_node: cst.IndentedBlock) -> cst.BaseSuite:
        final_node = super().leave_IndentedBlock(original_node, updated_node)
        if isinstance(final_node, cst.IndentedBlock):
            return final_node.with_changes(body=self._build_body(final_node.body))
        return final_node

    def leave_Module(self, original_node: cst.Module,
                     updated_node: cst.Module) -> cst.Module:
        updated_node = super().leave_Module(original_node, updated_node)
        return updated_node.with_changes(body=self._build_body(updated_node.body))

    def leave_SimpleStatementLine(self, original_node: cst.SimpleStatementLine,
                                  updated_node: cst.SimpleStatementLine
                                  ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_SimpleStatementLine(original_node, updated_node)
        if isinstance(final_node, cst.BaseStatement):
            self.stmts_before[final_node] = self.stmts_before_acc
            self.stmts_after[final_node] = self.stmts_after_acc
        self.stmts_before_acc = []
        self.stmts_after_acc = []
        return final_node
