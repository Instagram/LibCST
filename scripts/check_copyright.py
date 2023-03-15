# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
import sys
from pathlib import Path
from subprocess import run
from typing import Iterable, List, Pattern

# Use the copyright header from this file as the benchmark for all files
EXPECTED_HEADER: str = "\n".join(
    line for line in Path(__file__).read_text().splitlines()[:4]
)

EXCEPTION_PATTERNS: List[Pattern[str]] = [
    re.compile(pattern)
    for pattern in (
        r"^native/libcst/tests/fixtures/",
        r"^libcst/_add_slots\.py$",
        r"^libcst/tests/test_(e2e|fuzz)\.py$",
        r"^libcst/_parser/base_parser\.py$",
        r"^libcst/_parser/parso/utils\.py$",
        r"^libcst/_parser/parso/pgen2/(generator|grammar_parser)\.py$",
        r"^libcst/_parser/parso/python/(py_token|tokenize)\.py$",
        r"^libcst/_parser/parso/tests/test_(fstring|tokenize|utils)\.py$",
    )
]


def tracked_files() -> Iterable[Path]:
    proc = run(
        ["git", "ls-tree", "-r", "--name-only", "HEAD"],
        check=True,
        capture_output=True,
        encoding="utf-8",
    )
    yield from (
        path
        for line in proc.stdout.splitlines()
        if not any(pattern.search(line) for pattern in EXCEPTION_PATTERNS)
        if (path := Path(line)) and path.is_file() and path.suffix in (".py", ".sh")
    )


def main() -> None:
    error = False
    for path in tracked_files():
        content = path.read_text("utf-8")
        if EXPECTED_HEADER not in content:
            print(f"Missing or incomplete copyright in {path}")
            error = True
    sys.exit(1 if error else 0)


if __name__ == "__main__":
    main()
