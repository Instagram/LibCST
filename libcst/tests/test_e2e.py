import contextlib
import os
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Generator
from unittest import TestCase

from libcst import BaseExpression, Call, Name, matchers as m
from libcst.codemod import (
    CodemodContext,
    VisitorBasedCodemodCommand,
    gather_files,
    parallel_exec_transform_with_prettyprint,
)
from libcst.codemod.visitors import AddImportsVisitor


class PrintToPPrintCommand(VisitorBasedCodemodCommand):
    def leave_Call(self, original_node: Call, updated_node: Call) -> BaseExpression:
        if m.matches(updated_node, m.Call(func=m.Name("print"))):
            AddImportsVisitor.add_needed_import(
                self.context,
                "pprint",
                "pprint",
            )
            return updated_node.with_changes(func=Name("pprint"))
        return super().leave_Call(original_node, updated_node)


@contextlib.contextmanager
def temp_workspace() -> Generator[Path, None, None]:
    cwd = os.getcwd()
    with TemporaryDirectory() as temp_dir:
        try:
            ws = Path(temp_dir).resolve()
            os.chdir(ws)
            yield ws
        finally:
            os.chdir(cwd)


class ToolE2ETest(TestCase):
    def test_leaky_codemod(self) -> None:
        with temp_workspace() as tmp:
            # File to trigger codemod
            example: Path = tmp / "example.py"
            example.write_text("""print("Hello")""")
            # File that should not be modified
            other = tmp / "other.py"
            other.touch()

            # Run command
            command_instance = PrintToPPrintCommand(CodemodContext())
            files = gather_files(".")
            result = parallel_exec_transform_with_prettyprint(
                command_instance,
                files,
                format_code=False,
                hide_progress=True,
            )

            # Check results
            self.assertEqual(2, result.successes)
            self.assertEqual(0, result.skips)
            self.assertEqual(0, result.failures)
            # Expect example.py to be modified
            self.assertIn(
                "from pprint import pprint",
                example.read_text(),
                "import missing in example.py",
            )
            # Expect other.py to NOT be modified
            self.assertNotIn(
                "from pprint import pprint",
                other.read_text(),
                "import found in other.py",
            )
