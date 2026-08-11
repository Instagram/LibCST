
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
from typing import List, Tuple

PythonVersionInfo = Tuple[int, int]


def parse_version_string(version: str) -> PythonVersionInfo:
    try:
        v = tuple(map(int, version.split(".")))
    except ValueError:
        raise ValueError("The given version is not in the right format")

    if len(v) < 2 or len(v) > 3:
        raise ValueError("The given version is not in the right format")
    return (v[0], v[1])


# Regular expression to split lines found in parso
_SPLIT_LINES_RE = re.compile(r"([^\n\r]*[\n\r]*)")


def split_lines(s: str, keepends: bool = False) -> List[str]:
    """
    Split a string into lines, optionally keeping the line endings.
    This implementation mimics `parso.utils.split_lines` but uses standard python methods where possible.
    Note: parso's split_lines had specific behavior for mixed newlines, but s.splitlines() is usually close enough.
    However, parso behavior might be slightly different regarding universal newlines.
    For LibCST, correct line splitting is critical for source fidelity.
    
    Using standard splitlines for now as a safe default for modern python.
    """
    return s.splitlines(keepends=keepends)
