# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import subprocess
from pathlib import Path
from typing import Tuple

from mypy_extensions import TypedDict


def _run_command(command: str) -> Tuple[str, str]:
    process = subprocess.Popen(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True
    )
    stdout, stderr = process.communicate()
    return stdout.decode(), stderr.decode()


class Position(TypedDict):
    line: int
    column: int


class Location(TypedDict):
    path: str
    start: Position
    stop: Position


class InferredType(TypedDict):
    location: Location
    annotation: str


def _sort_by_position(data: InferredType) -> Tuple[int, int, int, int]:
    start = data["location"]["start"]
    stop = data["location"]["stop"]
    return (start["line"], start["column"], stop["line"], stop["column"])


if __name__ == "__main__":
    """Run this script directly to generate pyre data for test suite (tests/pyre/*.py)
    """
    print("start pyre server")
    stdout: str
    stderr: str
    stdout, stderr = _run_command("pyre")
    print(stdout)
    print(stderr)

    p: Path = Path(__file__).parent / "pyre"
    for path in (p).glob("*.py"):
        cmd = f'''pyre query "types(path='{path}')"'''
        print(cmd)
        stdout, stderr = _run_command(cmd)
        data = json.loads(stdout)
        data = data["response"][0]
        del data["path"]
        data["types"].sort(key=_sort_by_position)
        output_path = path.with_suffix(".json")
        print(f"write output to {output_path}")
        output_path.write_text(json.dumps(data, indent=2))
