# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from libcst.codemod import CodemodContext, CodemodTest
from libcst.codemod.visitors import AddImportsVisitor


class TestAddImportsCodemod(CodemodTest):

    TRANSFORM = AddImportsVisitor

    def test_noop(self) -> None:
        """
        Should do nothing.
        """

        before = """
            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [])

    def test_add_module_simple(self) -> None:
        """
        Should add module as an import.
        """

        before = """
            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            import a.b.c

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", None, None)])

    def test_dont_add_module_simple(self) -> None:
        """
        Should not add module as an import since it exists
        """

        before = """
            import a.b.c

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            import a.b.c

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", None, None)])

    def test_add_module_alias_simple(self) -> None:
        """
        Should add module with alias as an import.
        """

        before = """
            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            import a.b.c as d

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", None, "d")])

    def test_dont_add_module_alias_simple(self) -> None:
        """
        Should not add module with alias as an import since it exists
        """

        before = """
            import a.b.c as d

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            import a.b.c as d

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", None, "d")])

    def test_add_module_complex(self) -> None:
        """
        Should add some modules as an import.
        """

        before = """
            import argparse
            import sys

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            import argparse
            import sys
            import a.b.c
            import defg.hi
            import jkl as h
            import i.j as k

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before,
            after,
            [
                ("a.b.c", None, None),
                ("defg.hi", None, None),
                ("argparse", None, None),
                ("jkl", None, "h"),
                ("i.j", None, "k"),
            ],
        )

    def test_add_object_simple(self) -> None:
        """
        Should add object as an import.
        """

        before = """
            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", None)])

    def test_add_object_alias_simple(self) -> None:
        """
        Should add object with alias as an import.
        """

        before = """
            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D as E

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", "E")])

    def test_add_future(self) -> None:
        """
        Should add future import before any other imports.
        """

        before = """
            import unittest
            import abc

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from __future__ import dummy_feature
            import unittest
            import abc

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("__future__", "dummy_feature", None)])

    def test_dont_add_object_simple(self) -> None:
        """
        Should not add object as an import since it exists.
        """

        before = """
            from a.b.c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", None)])

    def test_dont_add_object_alias_simple(self) -> None:
        """
        Should not add object as an import since it exists.
        """

        before = """
            from a.b.c import D as E

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D as E

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", "E")])

    def test_add_object_modify_simple(self) -> None:
        """
        Should modify existing import to add new object
        """

        before = """
            from a.b.c import E, F

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D, E, F

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", None)])

    def test_add_object_alias_modify_simple(self) -> None:
        """
        Should modify existing import with alias to add new object
        """

        before = """
            from a.b.c import E, F

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D as _, E, F

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", "_")])

    def test_add_object_modify_complex(self) -> None:
        """
        Should modify existing import to add new object
        """

        before = """
            from a.b.c import E, F, G as H
            from d.e.f import Foo, Bar

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D, E, F, G as H
            from d.e.f import Baz as Qux, Foo, Bar
            from g.h.i import V as W, X, Y, Z

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before,
            after,
            [
                ("a.b.c", "D", None),
                ("a.b.c", "F", None),
                ("a.b.c", "G", "H"),
                ("d.e.f", "Foo", None),
                ("g.h.i", "Z", None),
                ("g.h.i", "X", None),
                ("d.e.f", "Bar", None),
                ("d.e.f", "Baz", "Qux"),
                ("g.h.i", "Y", None),
                ("g.h.i", "V", "W"),
                ("a.b.c", "F", None),
            ],
        )

    def test_add_and_modify_complex(self) -> None:
        """
        Should correctly add both module and object imports
        """

        before = """
            import argparse
            import sys
            from a.b.c import E, F
            from d.e.f import Foo, Bar
            import bar as baz

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            import argparse
            import sys
            from a.b.c import D, E, F
            from d.e.f import Foo, Bar
            import bar as baz
            import foo
            import qux as quux
            from g.h.i import X, Y, Z

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before,
            after,
            [
                ("a.b.c", "D", None),
                ("a.b.c", "F", None),
                ("d.e.f", "Foo", None),
                ("sys", None, None),
                ("g.h.i", "Z", None),
                ("g.h.i", "X", None),
                ("d.e.f", "Bar", None),
                ("g.h.i", "Y", None),
                ("foo", None, None),
                ("a.b.c", "F", None),
                ("bar", None, "baz"),
                ("qux", None, "quux"),
            ],
        )

    def test_add_import_preserve_doctring_simple(self) -> None:
        """
        Should preserve any doctring if adding to the beginning.
        """

        before = """
            # This is some docstring

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            # This is some docstring

            from a.b.c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(before, after, [("a.b.c", "D", None)])

    def test_add_import_preserve_doctring_multiples(self) -> None:
        """
        Should preserve any doctring if adding to the beginning.
        """

        before = """
            # This is some docstring

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            # This is some docstring

            import argparse
            from a.b.c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before, after, [("a.b.c", "D", None), ("argparse", None, None)]
        )

    def test_strict_module_no_imports(self) -> None:
        """
        First added import in strict module should go after __strict__ flag.
        """
        before = """
            __strict__ = True

            class Foo:
                pass
        """
        after = """
            __strict__ = True
            import argparse

            class Foo:
                pass
        """

        self.assertCodemod(before, after, [("argparse", None, None)])

    def test_strict_module_with_imports(self) -> None:
        """
        First added import in strict module should go after __strict__ flag.
        """
        before = """
            __strict__ = True

            import unittest

            class Foo:
                pass
        """
        after = """
            __strict__ = True

            import unittest
            import argparse

            class Foo:
                pass
        """

        self.assertCodemod(before, after, [("argparse", None, None)])

    def test_dont_add_relative_object_simple(self) -> None:
        """
        Should not add object as an import since it exists.
        """

        before = """
            from .c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from .c import D

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before,
            after,
            [("a.b.c", "D", None)],
            context_override=CodemodContext(full_module_name="a.b.foobar"),
        )

    def test_add_object_relative_modify_simple(self) -> None:
        """
        Should modify existing import to add new object
        """

        before = """
            from .c import E, F

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from .c import D, E, F

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before,
            after,
            [("a.b.c", "D", None)],
            context_override=CodemodContext(full_module_name="a.b.foobar"),
        )

    def test_import_order(self) -> None:
        """
        The imports should be in alphabetic order of added imports, added import alias, original imports.
        """
        before = """
            from a import b, e, h
        """
        after = """
            from a import c, f, d as x, g as y, b, e, h
        """

        self.assertCodemod(
            before,
            after,
            [("a", "f", None), ("a", "g", "y"), ("a", "c", None), ("a", "d", "x")],
            context_override=CodemodContext(full_module_name="a.b.foobar"),
        )
