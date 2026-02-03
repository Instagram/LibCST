# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from typing import Union

import libcst as cst
from libcst.codemod import CodemodTest, VisitorBasedCodemodCommand


class TestRemoveUnusedImportHelper(CodemodTest):
    """Tests for the remove_unused_import helper method in CodemodCommand."""

    def test_remove_unused_import_simple(self) -> None:
        """
        Test that remove_unused_import helper method works correctly.
        """

        class RemoveBarImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule removal
                self.remove_unused_import("bar")

        before = """
            import bar
            import baz

            def foo() -> None:
                pass
        """
        after = """
            import baz

            def foo() -> None:
                pass
        """

        self.TRANSFORM = RemoveBarImport
        self.assertCodemod(before, after)

    def test_remove_unused_import_from_simple(self) -> None:
        """
        Test that remove_unused_import helper method works correctly with from imports.
        """

        class RemoveBarFromImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule removal
                self.remove_unused_import("a.b.c", "bar")

        before = """
            from a.b.c import bar, baz

            def foo() -> None:
                baz()
        """
        after = """
            from a.b.c import baz

            def foo() -> None:
                baz()
        """

        self.TRANSFORM = RemoveBarFromImport
        self.assertCodemod(before, after)

    def test_remove_unused_import_with_alias(self) -> None:
        """
        Test that remove_unused_import helper method works correctly with aliased imports.
        """

        class RemoveBarAsQuxImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule removal
                self.remove_unused_import("a.b.c", "bar", "qux")

        before = """
            from a.b.c import bar as qux, baz

            def foo() -> None:
                baz()
        """
        after = """
            from a.b.c import baz

            def foo() -> None:
                baz()
        """

        self.TRANSFORM = RemoveBarAsQuxImport
        self.assertCodemod(before, after)


class TestRemoveUnusedImportByNodeHelper(CodemodTest):
    """Tests for the remove_unused_import_by_node helper method in CodemodCommand."""

    def test_remove_unused_import_by_node_simple(self) -> None:
        """
        Test that remove_unused_import_by_node helper method works correctly.
        """

        class RemoveBarCallAndImport(VisitorBasedCodemodCommand):
            METADATA_DEPENDENCIES = (
                cst.metadata.QualifiedNameProvider,
                cst.metadata.ScopeProvider,
            )

            def leave_SimpleStatementLine(
                self,
                original_node: cst.SimpleStatementLine,
                updated_node: cst.SimpleStatementLine,
            ) -> Union[cst.RemovalSentinel, cst.SimpleStatementLine]:
                # Remove any statement that calls bar()
                if cst.matchers.matches(
                    updated_node,
                    cst.matchers.SimpleStatementLine(
                        body=[cst.matchers.Expr(cst.matchers.Call())]
                    ),
                ):
                    call = cst.ensure_type(updated_node.body[0], cst.Expr).value
                    if cst.matchers.matches(
                        call, cst.matchers.Call(func=cst.matchers.Name("bar"))
                    ):
                        # Use the helper method to remove imports referenced by this node
                        self.remove_unused_import_by_node(original_node)
                        return cst.RemoveFromParent()
                return updated_node

        before = """
            from foo import bar, baz

            def fun() -> None:
                bar()
                baz()
        """
        after = """
            from foo import baz

            def fun() -> None:
                baz()
        """

        self.TRANSFORM = RemoveBarCallAndImport
        self.assertCodemod(before, after)


class TestAddNeededImportHelper(CodemodTest):
    """Tests for the add_needed_import helper method in CodemodCommand."""

    def test_add_needed_import_simple(self) -> None:
        """
        Test that add_needed_import helper method works correctly.
        """

        class AddBarImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule import addition
                self.add_needed_import("bar")

        before = """
            def foo() -> None:
                pass
        """
        after = """
            import bar

            def foo() -> None:
                pass
        """

        self.TRANSFORM = AddBarImport
        self.assertCodemod(before, after)

    def test_add_needed_import_from_simple(self) -> None:
        """
        Test that add_needed_import helper method works correctly with from imports.
        """

        class AddBarFromImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule import addition
                self.add_needed_import("a.b.c", "bar")

        before = """
            def foo() -> None:
                pass
        """
        after = """
            from a.b.c import bar

            def foo() -> None:
                pass
        """

        self.TRANSFORM = AddBarFromImport
        self.assertCodemod(before, after)

    def test_add_needed_import_with_alias(self) -> None:
        """
        Test that add_needed_import helper method works correctly with aliased imports.
        """

        class AddBarAsQuxImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule import addition
                self.add_needed_import("a.b.c", "bar", "qux")

        before = """
            def foo() -> None:
                pass
        """
        after = """
            from a.b.c import bar as qux

            def foo() -> None:
                pass
        """

        self.TRANSFORM = AddBarAsQuxImport
        self.assertCodemod(before, after)

    def test_add_needed_import_relative(self) -> None:
        """
        Test that add_needed_import helper method works correctly with relative imports.
        """

        class AddRelativeImport(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Use the helper method to schedule relative import addition
                self.add_needed_import("c", "bar", relative=2)

        before = """
            def foo() -> None:
                pass
        """
        after = """
            from ..c import bar

            def foo() -> None:
                pass
        """

        self.TRANSFORM = AddRelativeImport
        self.assertCodemod(before, after)


class TestCombinedHelpers(CodemodTest):
    """Tests for combining add_needed_import and remove_unused_import helper methods."""

    def test_add_and_remove_imports(self) -> None:
        """
        Test that both helper methods work correctly when used together.
        """

        class ReplaceBarWithBaz(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Add new import and remove old one
                self.add_needed_import("new_module", "baz")
                self.remove_unused_import("old_module", "bar")

        before = """
            from other_module import qux
            from old_module import bar

            def foo() -> None:
                pass
        """
        after = """
            from other_module import qux
            from new_module import baz

            def foo() -> None:
                pass
        """

        self.TRANSFORM = ReplaceBarWithBaz
        self.assertCodemod(before, after)

    def test_add_and_remove_same_import(self) -> None:
        """
        Test that both helper methods work correctly when used together.
        """

        class AddAndRemoveBar(VisitorBasedCodemodCommand):
            def visit_Module(self, node: cst.Module) -> None:
                # Add new import and remove old one
                self.add_needed_import("hello_module", "bar")
                self.remove_unused_import("hello_module", "bar")

        self.TRANSFORM = AddAndRemoveBar

        before = """
            from other_module import baz

            def foo() -> None:
                pass
        """
        # Should remain unchanged
        self.assertCodemod(before, before)

        before = """
            from other_module import baz
            from hello_module import bar

            def foo() -> None:
                bar.func()
        """
        self.assertCodemod(before, before)

        before = """
            from other_module import baz
            from hello_module import bar

            def foo() -> None:
                pass
        """

        after = """
            from other_module import baz

            def foo() -> None:
                pass
        """
        self.assertCodemod(before, after)
