# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys

from libcst.codemod import CodemodTest
from libcst.codemod.commands.convert_type_comments import ConvertTypeComments


class TestConvertTypeComments(CodemodTest):

    maxDiff = 1500
    TRANSFORM = ConvertTypeComments

    def assertCodemod39Plus(self, before: str, after: str) -> None:
        """
        Assert that the codemod works on Python 3.9+, and that we raise
        a NotImplementedError on other Python versions.
        """
        if (sys.version_info.major, sys.version_info.minor) < (3, 9):
            with self.assertRaises(NotImplementedError):
                super().assertCodemod(before, after)
        else:
            super().assertCodemod(before, after)

    # Tests converting assignment type comments -----------------

    def test_convert_assignments(self) -> None:
        before = """
            y = 5  # type: int
            z = ('this', 7)  # type: typing.Tuple[str, int]
        """
        after = """
            y: int = 5
            z: "typing.Tuple[str, int]" = ('this', 7)
        """
        self.assertCodemod39Plus(before, after)

    def test_convert_assignments_in_context(self) -> None:
        """
        Also verify that our matching works regardless of spacing
        """
        before = """
            def foo():
                z = ('this', 7) # type: typing.Tuple[str, int]

            class C:
                attr0 = 10# type: int
                def __init__(self):
                    self.attr1 = True  # type: bool
        """
        after = """
            def foo():
                z: "typing.Tuple[str, int]" = ('this', 7)

            class C:
                attr0: int = 10
                def __init__(self):
                    self.attr1: bool = True
        """
        self.assertCodemod39Plus(before, after)

    def test_multiple_elements_in_assign_lhs(self) -> None:
        before = """
            x, y = [], []        # type: List[int], List[str]
            z, w = [], []        # type: (List[int], List[str])

            a, b, *c = range(5)  # type: float, float, List[float]

            d, (e1, e2) = foo()  # type: float, (int, str)
        """
        after = """
            x: "List[int]"
            y: "List[str]"
            x, y = [], []
            z: "List[int]"
            w: "List[str]"
            z, w = [], []

            a: float
            b: float
            c: "List[float]"
            a, b, *c = range(5)

            d: float
            e1: int
            e2: str
            d, (e1, e2) = foo()
        """
        self.assertCodemod39Plus(before, after)

    def test_multiple_assignments(self) -> None:
        before = """
            x = y = z = 15 # type: int

            a, b = c, d = 'this', 'that' # type: (str, str)
        """
        after = """
            x: int
            y: int
            z: int
            x = y = z = 15

            a: str
            b: str
            c: str
            d: str
            a, b = c, d = 'this', 'that'
        """
        self.assertCodemod39Plus(before, after)

    def test_semicolons_with_assignment(self) -> None:
        """
        When we convert an Assign to an AnnAssign, preserve
        semicolons. But if we have to add separate type declarations,
        expand them.
        """
        before = """
            foo(); x = 12  # type: int

            bar(); y, z = baz() # type: int, str
        """
        after = """
            foo(); x: int = 12

            bar()
            y: int
            z: str
            y, z = baz()
        """
        self.assertCodemod39Plus(before, after)

    def test_converting_for_statements(self) -> None:
        before = """
        # simple binding
        for x in foo():  # type: int
            pass

        # nested binding
        for (a, (b, c)) in bar(): # type: int, (str, float)
            pass
        """
        after = """
        # simple binding
        x: int
        for x in foo():
            pass

        # nested binding
        a: int
        b: str
        c: float
        for (a, (b, c)) in bar():
            pass
        """
        self.assertCodemod39Plus(before, after)

    def test_converting_with_statements(self) -> None:
        before = """
        # simple binding
        with open('file') as f:  # type: File
            pass

        # simple binding, with extra items
        with foo(), open('file') as f, bar():  # type: File
            pass

        # nested binding
        with bar() as (a, (b, c)): # type: int, (str, float)
            pass
        """
        after = """
        # simple binding
        f: "File"
        with open('file') as f:
            pass

        # simple binding, with extra items
        f: "File"
        with foo(), open('file') as f, bar():
            pass

        # nested binding
        a: int
        b: str
        c: float
        with bar() as (a, (b, c)):
            pass
        """
        self.assertCodemod39Plus(before, after)

    def test_no_change_when_type_comment_unused(self) -> None:
        before = """
            # type-ignores are not type comments
            x = 10  # type: ignore

            # a commented type comment (per PEP 484) is not a type comment
            z = 15  # # type: int

            # a type comment in an illegal location won't be used
            print("hello")  # type: None

            # These examples are not PEP 484 compliant, and result in arity errors
            a, b = 1, 2  # type: Tuple[int, int]
            w = foo()  # type: float, str

            # Multiple assigns with mismatched LHS arities always result in arity
            # errors, and we only codemod if each target is error-free
            v = v0, v1 = (3, 5)  # type: int, int

            # Ignore for statements with arity mismatches
            for x in []: # type: int, int
                pass

            # Ignore with statements with arity mismatches
            with open('file') as (f0, f1): # type: File
                pass

            # Ignore with statements that have multiple item bindings
            with open('file') as f0, open('file') as f1: # type: File
                pass
        """
        after = before
        self.assertCodemod39Plus(before, after)
