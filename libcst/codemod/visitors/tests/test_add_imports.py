# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from libcst.codemod import CodemodTest
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

        self.assertCodemod(before, after, [("a.b.c", None)])

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

        self.assertCodemod(before, after, [("a.b.c", None)])

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

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """

        self.assertCodemod(
            before, after, [("a.b.c", None), ("defg.hi", None), ("argparse", None)]
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

        self.assertCodemod(before, after, [("a.b.c", "D")])

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

        self.assertCodemod(before, after, [("__future__", "dummy_feature")])

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

        self.assertCodemod(before, after, [("a.b.c", "D")])

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

        self.assertCodemod(before, after, [("a.b.c", "D")])

    def test_add_object_modify_complex(self) -> None:
        """
        Should modify existing import to add new object
        """

        before = """
            from a.b.c import E, F
            from d.e.f import Foo, Bar

            def foo() -> None:
                pass

            def bar() -> int:
                return 5
        """
        after = """
            from a.b.c import D, E, F
            from d.e.f import Foo, Bar
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
                ("a.b.c", "D"),
                ("a.b.c", "F"),
                ("d.e.f", "Foo"),
                ("g.h.i", "Z"),
                ("g.h.i", "X"),
                ("d.e.f", "Bar"),
                ("g.h.i", "Y"),
                ("a.b.c", "F"),
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
            import foo
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
                ("a.b.c", "D"),
                ("a.b.c", "F"),
                ("d.e.f", "Foo"),
                ("sys", None),
                ("g.h.i", "Z"),
                ("g.h.i", "X"),
                ("d.e.f", "Bar"),
                ("g.h.i", "Y"),
                ("foo", None),
                ("a.b.c", "F"),
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

        self.assertCodemod(before, after, [("a.b.c", "D")])

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

        self.assertCodemod(before, after, [("a.b.c", "D"), ("argparse", None)])

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

        self.assertCodemod(before, after, [("argparse", None)])

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

        self.assertCodemod(before, after, [("argparse", None)])
