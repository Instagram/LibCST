# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from libcst.codemod import CodemodTest
from libcst.codemod.commands.convert_type_comments import ConvertTypeComments


class TestConvertTypeComments(CodemodTest):

    TRANSFORM = ConvertTypeComments

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
        self.assertCodemod(before, after)

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
        self.assertCodemod(before, after)

    def test_strip_useless_type_comments(self) -> None:
        """
        Verify that we remove type comments that are in a place
        not permitted by PEP 484. This is debateable behavior but
        is intended, because according to PEP 484 such comments are
        illegal.
        """
        before = """
           print("hello")  # type: None
        """
        after = """
           print("hello")
        """
        self.assertCodemod(before, after)

    def test_non_type_comments_on_assignments(self) -> None:
        before = """
            # type-ignores are not type comments
            x = 10  # type: ignore
            # a commented type comment (per PEP 484) is not a type comment
            z = 15  # # type: int
        """
        after = before
        self.assertCodemod(before, after)

    def test_no_change_to_non_single_statement_comments(self) -> None:
        before = """
            def f(
                x,  # type: int
            ):
                # type (...) -> int
                return x
        """
        after = before
        self.assertCodemod(before, after)
