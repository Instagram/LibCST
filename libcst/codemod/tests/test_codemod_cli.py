# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#


import subprocess
from pathlib import Path

from libcst._parser.entrypoints import is_native
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
                # `ArgumentParser.parse_known_args()`'s behavior dictates that options
                # need to go after instead of before the codemod command identifier.
                "--python-version",
                "3.6",
                str(Path(__file__).parent / "codemod_formatter_error_input.py.txt"),
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        if not is_native():
            self.assertIn(
                "ParserSyntaxError: Syntax Error @ 14:11.",
                rlt.stderr.decode("utf-8"),
            )
        else:
            self.assertIn(
                "error: cannot format -: Cannot parse: 13:10:     async with AsyncExitStack() as stack:",
                rlt.stderr.decode("utf-8"),
            )
