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
            import baz

            def test() -> None:
                baz.quux(5)
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
            from A import B

            class Foo(B.some_class):
                pass
        """
        after = """
            from C import B

            class Foo(B.some_class):
                pass
        """

        self.assertCodemod(
            before,
            after,
            orig_module="A",
            orig_object="B",
            new_module="C",
            new_object="B",
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
            import a, b, c

            class Foo(a.z):
                bar: b.bar
                baz: c.baz
        """
        after = """
            import b, c
            import d

            class Foo(d.z):
                bar: b.bar
                baz: c.baz
        """
        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="z",
            new_module="d",
            new_object="z",
        )

    def test_other_import_froms_untouched(self) -> None:
        before = """
            from a import b, c, d

            class Foo(b):
                bar: c.bar
                baz: d.baz
        """
        after = """
            from a import c, d
            from f import b

            class Foo(b):
                bar: c.bar
                baz: d.baz
        """
        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="b",
            new_module="f",
            new_object="b",
        )

    def test_no_removal_of_import_in_use(self) -> None:
        before = """
            import a

            class Foo(a.b):
                pass
            class Foo2(a.c):
                pass
        """
        after = """
            import a
            import z

            class Foo(z.b):
                pass
            class Foo2(a.c):
                pass
        """
        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="b",
            new_module="z",
            new_object="b",
        )

    def test_no_removal_of_import_from_in_use(self) -> None:
        before = """
            from a import m

            class Foo(m.CstNode):
                bar: m.Attribute
        """
        after = """
            from a import m
            import blah

            class Foo(blah.CstNode):
                bar: m.Attribute
        """
        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="m.CstNode",
            new_module="blah",
            new_object="CstNode",
        )

    def test_other_unused_imports_untouched(self) -> None:
        before = """
            import a
            import b

            class Foo(a.obj):
                pass
        """
        after = """
            import b
            import c

            class Foo(c.obj):
                pass
        """
        self.assertCodemod(
            before,
            after,
            orig_module="a",
            orig_object="obj",
            new_module="c",
            new_object="obj",
        )
