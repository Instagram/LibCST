# pyre-strict
import libcst as cst
from libcst.codemod import CodemodContext, CodemodTest
from libcst.codemod.visitors import InsertStatementsVisitor
from libcst.testing.utils import UnitTest
from typing import Type


class InsertAssignAroundAssignVisitor(InsertStatementsVisitor):
    def leave_Integer(self, original_node: cst.Integer,
                      updated_node: cst.Integer) -> cst.Integer:
        self.insert_statements_before_current([cst.parse_statement("y = 1")])
        self.insert_statements_after_current([cst.parse_statement("z = 1")])
        return updated_node


class TestInsertStatementsVisitor(CodemodTest):
    def insert_statements(self, Visitor: Type[InsertStatementsVisitor], code: str) -> str:
        transform_instance = Visitor(CodemodContext())
        input_tree = cst.parse_module(CodemodTest.make_fixture_data(code))
        return input_tree.visit(transform_instance).code

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

        actual_after = self.insert_statements(InsertAssignAroundAssignVisitor, before)
        self.assertCodeEqual(expected_after, actual_after)

    def test_insert_assign(self) -> None:
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

        actual_after = self.insert_statements(InsertAssignAroundAssignVisitor, before)
        self.assertCodeEqual(expected_after, actual_after)
