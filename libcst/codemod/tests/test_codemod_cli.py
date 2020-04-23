# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict


import subprocess
import sys

from libcst.testing.utils import UnitTest


class TestCodemodCLI(UnitTest):
    def test_codemod_formatter_error_input(self) -> None:
        rlt = subprocess.run(
            [
                "python",
                "-m",
                "libcst.tool",
                "codemod",
                "remove_unused_imports.RemoveUnusedImportsCommand",
                "libcst/codemod/tests/codemod_formatter_error_input.py.txt",
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        version = sys.version_info
        if version[0] == 3 and version[1] == 6:
            self.assertIn(
                "ParserSyntaxError: Syntax Error @ 14:11.", rlt.stderr.decode("utf-8"),
            )
        else:
            self.assertIn(
                "error: cannot format -: Cannot parse: 13:10:     async with AsyncExitStack() as stack:",
                rlt.stderr.decode("utf-8"),
            )
