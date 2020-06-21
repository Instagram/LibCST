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

        self.assertCodemod(before, after, old_name="foo.bar", new_name="baz.qux")

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
            before, after, old_name="foo.bar", new_name="baz.qux",
        )

    def test_rename_attr(self) -> None:

        before = """
            import a.b

            def test() -> None:
                a.b.c(5)
        """
        after = """
            import a.b
            import d.e

            def test() -> None:
                d.e.f(5)
        """

        self.assertCodemod(
            before, after, old_name="a.b.c", new_name="d.e.f",
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
            before, after, old_name="foo.qux", new_name="baz.quux",
        )

    def test_rename_module_import(self) -> None:
        before = """
            import a.b

            class Foo(a.b.C):
                pass
        """
        after = """
            import c.b

            class Foo(c.b.C):
                pass
        """

        self.assertCodemod(
            before, after, old_name="a.b", new_name="c.b",
        )

    def test_rename_module_import_from(self) -> None:
        before = """
            from a import b

            class Foo(b.C):
                pass
        """
        after = """
            from c import b

            class Foo(b.C):
                pass
        """

        self.assertCodemod(
            before, after, old_name="a.b", new_name="c.b",
        )

    def test_rename_class(self) -> None:
        before = """
            from a.b import some_class

            class Foo(some_class):
                pass
        """
        after = """
            from c.b import some_class

            class Foo(some_class):
                pass
        """
        self.assertCodemod(
            before, after, old_name="a.b.some_class", new_name="c.b.some_class",
        )

    def test_rename_object_same_module(self) -> None:
        before = """
            from a.b import Class_1, Class_2

            class Foo(Class_1):
                pass
        """
        after = """
            from a.b import Class_3, Class_2

            class Foo(Class_3):
                pass
        """
        self.assertCodemod(
            before, after, old_name="a.b.Class_1", new_name="a.b.Class_3",
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
            before, after, old_name="x", new_name="z",
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
        self.assertCodemod(before, after, old_name="a.b", new_name="a.c")

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
            before, after, old_name="a.z", new_name="d.z",
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
            before, after, old_name="a.b", new_name="f.b",
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
            before, after, old_name="a.b", new_name="z.b",
        )

    def test_no_removal_of_import_from_in_use(self) -> None:
        before = """
            from a import b

            class Foo(b.some_class):
                bar: b.some_other_class
        """
        after = """
            from a import b
            import blah

            class Foo(blah.some_class):
                bar: b.some_other_class
        """
        self.assertCodemod(
            before, after, old_name="a.b.some_class", new_name="blah.some_class",
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
            before, after, old_name="a.obj", new_name="c.obj",
        )

    def test_complex_module_rename(self) -> None:
        before = """
            from a.b.c import d

            class Foo(d.e.f):
                pass
        """
        after = """
            import g.h.i

            class Foo(g.h.i.j):
                pass
        """
        self.assertCodemod(before, after, old_name="a.b.c.d.e.f", new_name="g.h.i.j")

    def test_names_with_repeated_substrings(self) -> None:
        before = """
            from aa import aaaa

            class Foo(aaaa.Bar):
                pass
        """
        after = """
            from b import c

            class Foo(c.Bar):
                pass
        """
        self.assertCodemod(
            before, after, old_name="aa.aaaa", new_name="b.c",
        )

    def test_import_star_attr(self) -> None:
        before = """
            from foo import *

            def baz():
                foo.bar(5)
        """
        after = """
            from foo import *
            import qux

            def baz():
                qux.bar(5)
        """
        self.assertCodemod(
            before, after, old_name="foo.bar", new_name="qux.bar",
        )

    def test_import_star_obj(self) -> None:
        before = """
            from foo import *

            class Bar(foo.Bar):
                pass
        """
        after = """
            from foo import *
            import qux

            class Bar(qux.Bar):
                pass
        """
        self.assertCodemod(
            before, after, old_name="foo.Bar", new_name="qux.Bar",
        )

    def test_repeated_name(self) -> None:
        before = """
            from foo import foo

            def bar():
                foo(5)
        """
        after = """
            from qux import qux

            def bar():
                qux(5)
        """
        self.assertCodemod(
            before, after, old_name="foo.foo", new_name="qux.qux",
        )

    def test_no_codemod(self) -> None:
        before = """
            from foo import bar

            def baz():
                bar(5)
        """
        self.assertCodemod(
            before, before, old_name="bar", new_name="qux",
        )
