# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys

from libcst.codemod import CodemodTest
from libcst.codemod.commands.convert_type_comments import ConvertTypeComments


class TestConvertTypeComments(CodemodTest):

    TRANSFORM = ConvertTypeComments

    def assertCodemod38Plus(self, before: str, after: str) -> None:
        """
        Assert that the codemod works on Python 3.8+, and that we raise
        a NotImplementedError on other python versions.
        """
        if (sys.version_info.major, sys.version_info.minor) < (3, 8):
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
        self.assertCodemod38Plus(before, after)

    def test_convert_assignments_in_context(self) -> None:
        """
        Also verify that our matching works regardless of spacing
        """
        before = """
            bar(); baz = 12  # type: int

            def foo():
                z = ('this', 7) # type: typing.Tuple[str, int]

            class C:
                attr0 = 10# type: int
                def __init__(self):
                    self.attr1 = True  # type: bool
        """
        after = """
            bar(); baz: int = 12

            def foo():
                z: "typing.Tuple[str, int]" = ('this', 7)

            class C:
                attr0: int = 10
                def __init__(self):
                    self.attr1: bool = True
        """
        self.assertCodemod38Plus(before, after)

    def test_no_change_when_type_comment_unused(self) -> None:
        before = """
            # type-ignores are not type comments
            x = 10  # type: ignore

            # a commented type comment (per PEP 484) is not a type comment
            z = 15  # # type: int

            # a type comment in an illegal location won't be used
            print("hello")  # type: None
        """
        after = before
        self.assertCodemod38Plus(before, after)
