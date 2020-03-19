# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from typing import Dict, List, NamedTuple, Optional, Sequence, Union

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

    # Stack of BaseStatements being visited
    stmt_visit_stack: List[cst.BaseStatement]


class InsertStatementsVisitor(ContextAwareTransformer):
    """
    Allows transformers to insert multiple statements before and after the currently-visited statement.

    This class is a mixin for :class:`~libcst.codemod.ContextAwareTransformer`. Subclasses gain the methods :meth:`~libcst.codemod.visitors.InsertStatementsVisitor.insert_statements_before_current` and :meth:`~libcst.codemod.visitors.InsertStatementsVisitor.insert_statements_after_current`. For example, you can create a pass that inserts print statements before each use of a variable::

        from libcst.metadata.visitors import InsertStatementsVisitor
        from libcst.metadata import ExpressionContextProvider, ExpressionContext
        class InsertPrintVisitor(InsertStatementsVisitor):
            METADATA_DEPENDENCIES = (ExpressionContextProvider,)

            def __init__(self, context: CodemodContext, name: str) -> None:
                super().__init__(context)
                self.name = name

            def visit_Name(self, node: cst.Name) -> None:
                if (
                    node.value == self.name
                    and self.get_metadata(ExpressionContextProvider, node)
                    == ExpressionContext.LOAD
                ):
                    self.insert_statements_before_current(
                        [cst.parse_statement(f"print({self.name})")]
                    )

    After initializing this visitor with ``name = "y"``, it will take this code::

          y = 1
          x = y

    And transform it into this code::

          y = 1
          print(y)
          x = y
    """

    CONTEXT_KEY = "InsertStatementsVisitor"

    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)
        self.context.scratch[
            InsertStatementsVisitor.CONTEXT_KEY] = InsertStatementsVisitorContext({}, {},
                                                                                  [])

    def _context(self) -> InsertStatementsVisitorContext:
        return self.context.scratch[InsertStatementsVisitor.CONTEXT_KEY]

    def insert_statements_before_current(self, stmts: List[cst.BaseStatement]) -> None:
        """
        Inserts a list of statements before the currently visited statement.

        While traversing the AST, the InsertStatementVisitor collects a stack of visited statements. For example, in the snippet::

            if y:
              x = 1

        When visiting ``y`` in ``visit_Name``, the current statement is the ``if``. When visiting ``1`` in ``visit_Integer``, the current statement is the ``x = 1`` assignment. Calling ``insert_statments_before_current`` will add a list of statements to be inserted before ``current``, which is handled in :meth:`~libcst.codemod.visitors.InsertStatementsVisitor.leave_Module` and :meth:`~libcst.codemod.visitors.InsertStatementsVisitor.leave_IndentedBlock`. This means you **must** call the corresponding ``super()`` method if you override these in a child class.
        """

        ctx = self._context()
        assert (
            len(ctx.stmt_visit_stack) > 0
        ), "Attempted to call insert_statments_before_current before visiting a statement"
        cur_stmt = ctx.stmt_visit_stack[-1]
        if cur_stmt not in ctx.stmts_before:
            ctx.stmts_before[cur_stmt] = []
        ctx.stmts_before[cur_stmt].extend(stmts)

    def insert_statements_after_current(self, stmts: List[cst.BaseStatement]) -> None:
        """
        Inserts a list of statements after the currently visited statement.

        See :meth:`libcst.codemod.visitors.InsertStatementVisitor.insert_statements_before_current` for details.
        """

        ctx = self._context()
        assert (
            len(ctx.stmt_visit_stack) > 0
        ), "Attempted to call insert_statments_after_current before visiting a statement"
        cur_stmt = ctx.stmt_visit_stack[-1]
        if cur_stmt not in ctx.stmts_after:
            ctx.stmts_after[cur_stmt] = []
        ctx.stmts_after[cur_stmt].extend(stmts)

    def _build_body(self, body: Sequence[cst.BaseStatement]) -> List[cst.BaseStatement]:
        ctx = self._context()
        new_stmts = []
        for stmt in body:
            new_stmts.extend(ctx.stmts_before.get(stmt, []))
            new_stmts.append(stmt)
            new_stmts.extend(ctx.stmts_after.get(stmt, []))
        return new_stmts

    def leave_IndentedBlock(self, original_node: cst.IndentedBlock,
                            updated_node: cst.IndentedBlock) -> cst.BaseSuite:
        final_node = super().leave_IndentedBlock(original_node, updated_node)
        if isinstance(final_node, cst.IndentedBlock):
            return final_node.with_changes(body=self._build_body(final_node.body))
        return final_node

    def leave_Module(self, original_node: cst.Module,
                     updated_node: cst.Module) -> cst.Module:
        final_node = super().leave_Module(original_node, updated_node)
        return final_node.with_changes(body=self._build_body(updated_node.body))

    def _visit_stmt(self, node: cst.BaseStatement) -> None:
        ctx = self._context()
        ctx.stmt_visit_stack.append(node)

    def _leave_stmt(
            self,
            original_node: cst.BaseStatement,
            final_node: Union[cst.BaseStatement, cst.RemovalSentinel],
    ) -> None:
        ctx = self._context()
        ctx.stmt_visit_stack.pop()

        if isinstance(final_node, cst.BaseStatement):
            ctx.stmts_before[final_node] = ctx.stmts_before.get(original_node, [])
            ctx.stmts_after[final_node] = ctx.stmts_after.get(original_node, [])

    def visit_SimpleStatementLine(self, node: cst.SimpleStatementLine) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_SimpleStatementLine(node)

    def leave_SimpleStatementLine(
            self,
            original_node: cst.SimpleStatementLine,
            updated_node: cst.SimpleStatementLine,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_SimpleStatementLine(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_If(self, node: cst.If) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_If(node)

    def leave_If(
            self,
            original_node: cst.If,
            updated_node: cst.If,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_If(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_Try(self, node: cst.Try) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_Try(node)

    def leave_Try(
            self,
            original_node: cst.Try,
            updated_node: cst.Try,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_Try(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_FunctionDef(self, node: cst.FunctionDef) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_FunctionDef(node)

    def leave_FunctionDef(
            self,
            original_node: cst.FunctionDef,
            updated_node: cst.FunctionDef,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_FunctionDef(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_ClassDef(self, node: cst.ClassDef) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_ClassDef(node)

    def leave_ClassDef(
            self,
            original_node: cst.ClassDef,
            updated_node: cst.ClassDef,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_ClassDef(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_With(self, node: cst.With) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_With(node)

    def leave_With(
            self,
            original_node: cst.With,
            updated_node: cst.With,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_With(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_For(self, node: cst.For) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_For(node)

    def leave_For(
            self,
            original_node: cst.For,
            updated_node: cst.For,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_For(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node

    def visit_While(self, node: cst.While) -> Optional[bool]:
        self._visit_stmt(node)
        return super().visit_While(node)

    def leave_While(
            self,
            original_node: cst.While,
            updated_node: cst.While,
    ) -> Union[cst.BaseStatement, cst.RemovalSentinel]:
        final_node = super().leave_While(original_node, updated_node)
        self._leave_stmt(original_node, final_node)
        return final_node
