# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#


import subprocess
import sys
import tempfile
from pathlib import Path

from libcst._parser.entrypoints import is_native
from libcst.testing.utils import UnitTest
from libcst.codemod import CodemodTest

class TestCodemodCLI(UnitTest):
    def test_codemod_formatter_error_input(self) -> None:
        rlt = subprocess.run(
            [
                "python",
                "-m",
                "libcst.tool",
                "codemod",
                "remove_unused_imports.RemoveUnusedImportsCommand",
                str(Path(__file__).parent / "codemod_formatter_error_input.py.txt"),
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        version = sys.version_info
        if version[0] == 3 and version[1] == 6 and not is_native():
            self.assertIn(
                "ParserSyntaxError: Syntax Error @ 14:11.",
                rlt.stderr.decode("utf-8"),
            )
        else:
            self.assertIn(
                "error: cannot format -: Cannot parse: 13:10:     async with AsyncExitStack() as stack:",
                rlt.stderr.decode("utf-8"),
            )

    def test_warning_messages_several_files(self) -> None:
        mod = """
        def baz() -> str:
            return "{}: {}".format(*baz)

        def foobar() -> str:
            return "{x}: {y}".format(**baz)
        """
        with tempfile.TemporaryDirectory() as root:
            p = Path(root)
            (p / "mod1.py").write_text(CodemodTest.make_fixture_data(mod))
            (p / "mod2.py").write_text(CodemodTest.make_fixture_data(mod))
            (p / "mod3.py").write_text(CodemodTest.make_fixture_data(mod))
            rlt = subprocess.run(
                [
                    "python",
                    "-m",
                    "libcst.tool",
                    "codemod",
                    "convert_format_to_fstring.ConvertFormatStringCommand",
                    p,
                ],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            # Each module will generate 2 warnings, so we should get 6 warnings in total
            self.assertIn(
                "- 6 warnings were generated.",
                rlt.stderr.decode("utf-8"),
            )
