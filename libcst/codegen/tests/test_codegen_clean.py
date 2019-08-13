# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import os
import os.path
import subprocess

import libcst.codegen.gen_visitor_functions as visitor_codegen
from libcst.testing.utils import UnitTest


class TestCodegenClean(UnitTest):
    def test_codegen_clean_visitor_functions(self) -> None:
        """
        Verifies that codegen of visitor functions would not result in a
        changed file. If this test fails, please run 'tox -e codegen' to
        generate new files.
        """
        new_code = "\n".join(visitor_codegen.generated_code)
        new_file = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "visitor_codegen.py.deleteme"
        )
        with open(new_file, "w") as fp:
            fp.write(new_code)
        with open(os.devnull, "w") as devnull:
            subprocess.check_call(
                ["isort", "-y", "-q", new_file], stdout=devnull, stderr=devnull
            )
            subprocess.check_call(["black", new_file], stdout=devnull, stderr=devnull)
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
        self.assertTrue(
            old_code == new_code, "libcst._typed_visitor needs to new codegen!"
        )
