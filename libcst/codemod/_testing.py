# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from textwrap import dedent
from typing import Optional, Sequence, Type

from libcst import PartialParserConfig, parse_module
from libcst.codemod._codemod import Codemod
from libcst.codemod._context import CodemodContext
from libcst.codemod._runner import SkipFile
from libcst.testing.utils import UnitTest


# pyre-fixme[13]: This should be an ABC but there are metaclass conflicts due to
# the way we implement the data_provider decorator, so pyre complains about the
# uninitialized TRANSFORM below.
class _CodemodTest:
    """
    Mixin that can be added to a unit test framework in order to provide
    convenience features. This is provided as an internal-only feature so
    that CodemodTest can be used with other frameworks. This is necessary
    since we set a metaclass on our UnitTest implementation.
    """

    TRANSFORM: Type[Codemod]

    @staticmethod
    def make_fixture_data(data: str) -> str:
        lines = dedent(data).split("\n")

        def filter_line(line: str) -> str:
            if len(line.strip()) == 0:
                return ""
            return line

        # Get rid of lines that are space only
        lines = [filter_line(line) for line in lines]

        # Get rid of leading and trailing newlines (because of """ style strings)
        while lines and lines[0] == "":
            lines = lines[1:]
        while lines and lines[-1] == "":
            lines = lines[:-1]

        code = "\n".join(lines)
        if not code.endswith("\n"):
            return code + "\n"
        else:
            return code

    def assertCodeEqual(self, expected: str, actual: str) -> None:
        """
        Given a before and after code string, makes sure they equal. This ensures
        that both the expected and actual are sanitized, so its safe to use this
        on two strings that may have come from a triple-quoted string.
        """

        # pyre-ignore This mixin needs to be used with a UnitTest subclass.
        self.assertEqual(
            CodemodTest.make_fixture_data(expected),
            CodemodTest.make_fixture_data(actual),
        )

    def assertCodemod(
        self,
        before: str,
        after: str,
        *args: object,
        context_override: Optional[CodemodContext] = None,
        python_version: Optional[str] = None,
        expected_warnings: Optional[Sequence[str]] = None,
        expected_skip: bool = False,
        **kwargs: object,
    ) -> None:
        """
        Given a before and after string, and optionally any args/kwargs that
        should be passed to the codemod visitor constructor, validate that
        the codemod executes as expected.
        """

        context = context_override if context_override is not None else CodemodContext()
        transform_instance = self.TRANSFORM(context, *args, **kwargs)
        input_tree = parse_module(
            CodemodTest.make_fixture_data(before),
            config=(
                PartialParserConfig(python_version=python_version)
                if python_version is not None
                else PartialParserConfig()
            ),
        )
        try:
            output_tree = transform_instance.transform_module(input_tree)
        except SkipFile:
            if not expected_skip:
                raise
            output_tree = input_tree
        else:
            if expected_skip:
                # pyre-ignore This mixin needs to be used with a UnitTest subclass.
                self.fail("Expected SkipFile but was not raised")
        # pyre-ignore This mixin needs to be used with a UnitTest subclass.
        self.assertEqual(CodemodTest.make_fixture_data(after), output_tree.code)
        if expected_warnings is not None:
            # pyre-ignore This mixin needs to be used with a UnitTest subclass.
            self.assertSequenceEqual(expected_warnings, context.warnings)


class CodemodTest(_CodemodTest, UnitTest):
    """
    Base test class for a transform test. Provides facilities for auto-instantiating
    and executing a transform, given the args/kwargs that should be passed to it.
    Set the TRANSFORM class attribute to the class you wish to test and call
    assertCodemod to verify it transforms various code chunks correctly.
    """
