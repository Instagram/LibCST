# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Regenerate test fixtures, eg. after upgrading Pyre
"""

import json
import os
from pathlib import Path
from subprocess import run

from libcst.metadata import TypeInferenceProvider


def main() -> None:
    CWD = Path.cwd()
    repo_root = Path(__file__).parent.parent
    test_root = repo_root / "libcst" / "tests" / "pyre"

    try:
        os.chdir(test_root)
        run(["pyre", "-n", "start", "--no-watchman"], check=True)

        for file_path in test_root.glob("*.py"):
            json_path = file_path.with_suffix(".json")
            print(f"generating {file_path} -> {json_path}")

            path_str = file_path.as_posix()
            cache = TypeInferenceProvider.gen_cache(test_root, [path_str], timeout=None)
            result = cache[path_str]
            json_path.write_text(json.dumps(result, sort_keys=True, indent=2))

    finally:
        run(["pyre", "-n", "stop"], check=True)
        os.chdir(CWD)


if __name__ == "__main__":
    main()
