# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict

from libcst.codemod import CodemodTest
from libcst.codemod.commands.rename import RenameCommand


class TestRenameCommand(CodemodTest):

    ONCALL_SHORTNAME = "instagram_server_framework"

    TRANSFORM = RenameCommand

    def test_rename_name(self) -> None:

        before = """
            from foo import bar

            def test() -> None:
                bar(5)
        """
        after = """
            from baz import qux

            def test() -> None:
                qux(5)
        """

        self.assertCodemod(
            before,
            after,
            orig_module="foo",
            orig_object="bar",
            new_module="baz",
            new_object="qux",
        )

    def test_rename_name_asname(self) -> None:

        before = """
            from foo import bar as bla

            def test() -> None:
                bla(5)
        """
        after = """
            from baz import qux as bla

            def test() -> None:
                bla(5)
        """

        self.assertCodemod(
            before,
            after,
            orig_module="foo",
            orig_object="bar",
            new_module="baz",
            new_object="qux",
        )

    def test_rename_attr(self) -> None:

        before = """
            import a.b

            def test() -> None:
                a.b.c(5)
        """
        after = """
            import d.e

            def test() -> None:
                d.e.f(5)
        """

        self.assertCodemod(
            before,
            after,
            orig_module="a.b",
            orig_object="c",
            new_module="d.e",
            new_object="f",
        )

    def test_rename_attr_asname(self) -> None:

        before = """
            import foo as bar

            def test() -> None:
                bar.qux(5)
        """
        after = """
            import baz as bar

            def test() -> None:
                bar.quux(5)
        """

        self.assertCodemod(
            before,
            after,
            orig_module="foo",
            orig_object="qux",
            new_module="baz",
            new_object="quux",
        )

    def test_rename_module(self) -> None:
        before = """
            from a import b

            class Foo(b):
                pass
        """
        after = """
            from c import b

            class Foo(b):
                pass
        """

        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="b",
            new_module="c",
            new_object="b",
        )

    def test_rename_local_variable(self) -> None:
        before = """
            x = 5
            y = 5 + x
        """
        after = """
            z = 5
            y = 5 + z
        """

        self.assertCodemod(
            before, after, orig_object="x", new_object="z",
        )

    def test_rename_local_object_attr(self) -> None:
        before = """
            x = Foo()
            x.bar = 5
        """
        after = """
            x = Foo()
            x.baz = 5
        """
        self.assertCodemod(
            before, after, orig_object="x.bar", new_object="x.baz",
        )

    def test_module_does_not_change(self) -> None:
        before = """
            from a import b

            class Foo(b):
                pass
        """
        after = """
            from a import c

            class Foo(c):
                pass
        """
        self.assertCodemod(
            before, after, orig_module="a", orig_object="b", new_object="c"
        )

    def test_other_imports_untouched(self) -> None:
        before = """
            from a import b, c, d, e

            class Foo(b):
                pass
        """
        after = """
            from a import c, d, e
            from f import b

            class Foo(b):
                pass
        """
        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="b",
            new_module="f",
            new_object="b",
        )
