# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import os.path

import libcst.codegen.gen_matcher_classes as matcher_codegen
import libcst.codegen.gen_type_mapping as type_codegen
import libcst.codegen.gen_visitor_functions as visitor_codegen
from libcst.codegen.generate import clean_generated_code, format_file
from libcst.testing.utils import UnitTest


class TestCodegenClean(UnitTest):
    def assert_code_matches(
        self,
        old_code: str,
        new_code: str,
        module_name: str,
    ) -> None:
        self.assertTrue(
            old_code == new_code,
            f"{module_name} needs new codegen, see "
            + "`python -m libcst.codegen.generate --help` "
            + "for instructions, or run `python -m libcst.codegen.generate all`",
        )

    def test_codegen_clean_visitor_functions(self) -> None:
        """
        Verifies that codegen of visitor functions would not result in a
        changed file. If this test fails, please run 'python -m libcst.codegen.generate all'
        to generate new files.
        """
        new_code = clean_generated_code("\n".join(visitor_codegen.generated_code))
        new_file = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "visitor_codegen.deleteme.py"
        )
        with open(new_file, "w") as fp:
            fp.write(new_code)
        try:
            format_file(new_file)
        except Exception:
            # We failed to format, but this is probably due to invalid code that
            # black doesn't like. This test will still fail and report to run codegen.
            pass
        with open(new_file, "r") as fp:
            new_code = fp.read()
        os.remove(new_file)
        with open(
            os.path.join(
                os.path.dirname(os.path.abspath(__file__)), "../../_typed_visitor.py"
            ),
            "r",
        ) as fp:
            old_code = fp.read()

        # Now that we've done simple codegen, verify that it matches.
        self.assert_code_matches(old_code, new_code, "libcst._typed_visitor")

    def test_codegen_clean_matcher_classes(self) -> None:
        """
        Verifies that codegen of matcher classes would not result in a
        changed file. If this test fails, please run 'python -m libcst.codegen.generate all'
        to generate new files.
        """
        new_code = clean_generated_code("\n".join(matcher_codegen.generated_code))
        new_file = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "matcher_codegen.deleteme.py"
        )
        with open(new_file, "w") as fp:
            fp.write(new_code)
        try:
            format_file(new_file)
        except Exception:
            # We failed to format, but this is probably due to invalid code that
            # black doesn't like. This test will still fail and report to run codegen.
            pass
        with open(new_file, "r") as fp:
            new_code = fp.read()
        os.remove(new_file)
        with open(
            os.path.join(
                os.path.dirname(os.path.abspath(__file__)), "../../matchers/__init__.py"
            ),
            "r",
        ) as fp:
            old_code = fp.read()

        # Now that we've done simple codegen, verify that it matches.
        self.assert_code_matches(old_code, new_code, "libcst.matchers.__init__")

    def test_codegen_clean_return_types(self) -> None:
        """
        Verifies that codegen of return types would not result in a
        changed file. If this test fails, please run 'python -m libcst.codegen.generate all'
        to generate new files.
        """
        new_code = clean_generated_code("\n".join(type_codegen.generated_code))
        new_file = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "type_codegen.deleteme.py"
        )
        with open(new_file, "w") as fp:
            fp.write(new_code)
        try:
            format_file(new_file)
        except Exception:
            # We failed to format, but this is probably due to invalid code that
            # black doesn't like. This test will still fail and report to run codegen.
            pass
        with open(new_file, "r") as fp:
            new_code = fp.read()
        os.remove(new_file)
        with open(
            os.path.join(
                os.path.dirname(os.path.abspath(__file__)),
                "../../matchers/_return_types.py",
            ),
            "r",
        ) as fp:
            old_code = fp.read()

        # Now that we've done simple codegen, verify that it matches.
        self.assert_code_matches(old_code, new_code, "libcst.matchers._return_types")
