# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
import libcst as cst
from libcst.codemod import CodemodContext, CodemodTest
from libcst.codemod.visitors import InsertStatementsVisitor
from libcst.metadata import ExpressionContext, ExpressionContextProvider


class InsertAssignAroundIntegerVisitor(InsertStatementsVisitor):
    def leave_Integer(
        self, original_node: cst.Integer, updated_node: cst.Integer
    ) -> cst.Integer:
        self.insert_statements_before_current([cst.parse_statement("y = 1")])
        self.insert_statements_after_current([cst.parse_statement("z = 1")])
        return updated_node

    def leave_While(
        self, original_node: cst.While, updated_node: cst.While
    ) -> cst.RemovalSentinel:
        super().leave_While(original_node, updated_node)
        return cst.RemovalSentinel.REMOVE

    def leave_Try(
        self, original_node: cst.Try, updated_node: cst.Try
    ) -> cst.RemovalSentinel:
        super().leave_Try(original_node, updated_node)
        return cst.RemovalSentinel.REMOVE

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        super().visit_FunctionDef(node)
        return False


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


class TestInsertStatementsVisitor(CodemodTest):
    def insert_statements(self, visitor: InsertStatementsVisitor, code: str) -> str:
        input_tree = cst.parse_module(CodemodTest.make_fixture_data(code))
        return cst.MetadataWrapper(input_tree).visit(visitor).code

    def test_noop(self) -> None:
        """
        Should do nothing.
        """

        before = """
            x = "a"
        """

        expected_after = """
            x = "a"
        """

        actual_after = self.insert_statements(
            InsertAssignAroundIntegerVisitor(CodemodContext()), before
        )
        self.assertCodeEqual(expected_after, actual_after)

    def test_insert(self) -> None:
        """
        Should add assignments before and after the x = ...
        """

        before = """
            x = 1
            if True:
                x = 1
        """

        expected_after = """
            y = 1
            x = 1
            z = 1
            if True:
                y = 1
                x = 1
                z = 1
        """

        actual_after = self.insert_statements(
            InsertAssignAroundIntegerVisitor(CodemodContext()), before
        )
        self.assertCodeEqual(expected_after, actual_after)

    def test_compound_statement(self) -> None:
        """
        Test that inserts before/after a compound statement (If) should not
        get inserted into an indented block.
        """

        before = """
            if x == 1:
              pass
        """

        expected_after = """
            y = 1
            if x == 1:
              pass
            z = 1
        """

        actual_after = self.insert_statements(
            InsertAssignAroundIntegerVisitor(CodemodContext()), before
        )
        self.assertCodeEqual(expected_after, actual_after)

    def test_nested_remove(self) -> None:
        """
        Test that insertions placed *into *a deleted block *do* get deleted.
        """

        before = """
            pass
            try:
              x = 1
            except:
              pass
        """

        expected_after = """
            pass
        """

        actual_after = self.insert_statements(
            InsertAssignAroundIntegerVisitor(CodemodContext()), before
        )
        self.assertCodeEqual(expected_after, actual_after)

    def test_insert_around_remove(self) -> None:
        """
        Test that insertions placed *around* a deleted block do *not* get deleted.
        """

        before = """
            while 1:
                pass
        """

        expected_after = """
            y = 1
            z = 1
        """

        actual_after = self.insert_statements(
            InsertAssignAroundIntegerVisitor(CodemodContext()), before
        )
        self.assertCodeEqual(expected_after, actual_after)

    def test_no_visit(self) -> None:

        before = """
            pass
            def test(): pass
            pass
        """

        expected_after = """
            pass
            def test(): pass
            pass
        """

        actual_after = self.insert_statements(
            InsertAssignAroundIntegerVisitor(CodemodContext()), before
        )
        self.assertCodeEqual(expected_after, actual_after)

    def test_print(self) -> None:
        """
        Test print visitor used in documentation
        """

        before = """
          y = 1
          x = y
        """

        expected_after = """
          y = 1
          print(y)
          x = y
        """

        actual_after = self.insert_statements(
            InsertPrintVisitor(CodemodContext(), "y"), before
        )
        self.assertCodeEqual(expected_after, actual_after)
