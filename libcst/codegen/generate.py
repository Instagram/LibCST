# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Usage:
#
# python -m libcst.codegen.generate --help
# python -m libcst.codegen.generate visitors

# pyre-strict
import argparse
import os
import os.path
import shutil
import subprocess
import sys
from typing import List


def format_file(fname: str) -> None:
    with open(os.devnull, "w") as devnull:
        subprocess.check_call(
            ["isort", "-y", "-q", fname], stdout=devnull, stderr=devnull
        )
        subprocess.check_call(["black", fname], stdout=devnull, stderr=devnull)


def codegen_visitors() -> None:
    # First, back up the original file, since we have a nasty bootstrap problem.
    # We're in a situation where we want to import libcst in order to get the
    # valid nodes for visitors, but doing so means that we depend on ourselves.
    # So, this attempts to keep the repo in a working state for as many operations
    # as possible.
    base = os.path.abspath(
        os.path.join(os.path.dirname(os.path.abspath(__file__)), "../")
    )
    visitors_file = os.path.join(base, "_typed_visitor.py")
    shutil.copyfile(visitors_file, f"{visitors_file}.bak")

    try:
        # Now that we backed up the file, lets codegen a new version.
        # We import now, because this script does work on import.
        import libcst.codegen.gen_visitor_functions as visitor_codegen

        new_code = "\n".join(visitor_codegen.generated_code)
        with open(visitors_file, "w") as fp:
            fp.write(new_code)
            fp.close()

        # Now, see if the file we generated causes any import errors
        # by attempting to run codegen again in a new process.
        with open(os.devnull, "w") as devnull:
            subprocess.check_call(
                ["python3", "-m", "libcst.codegen.gen_visitor_functions"],
                cwd=base,
                stdout=devnull,
            )

        # If it worked, lets format the file
        format_file(visitors_file)

        # Since we were successful with importing, we can remove the backup.
        os.remove(f"{visitors_file}.bak")

        # Inform the user
        print(f"Successfully generated a new {visitors_file} file.")
    except Exception:
        # On failure, we put the original file back, and keep the failed version
        # for developers to look at.
        print(
            f"Failed to generated a new {visitors_file} file, failure "
            + f"is saved in {visitors_file}.failed_generate.",
            file=sys.stderr,
        )
        os.rename(visitors_file, f"{visitors_file}.failed_generate")
        os.rename(f"{visitors_file}.bak", visitors_file)

        # Reraise so we can debug
        raise


def main(cli_args: List[str]) -> int:
    # Parse out arguments, run codegen
    parser = argparse.ArgumentParser(description="Generate code for libcst.")
    parser.add_argument(
        "system",
        metavar="SYSTEM",
        help='System to generate code for. Valid values include: "visitors"',
        type=str,
    )
    args = parser.parse_args(cli_args)
    if args.system == "visitors":
        codegen_visitors()
        return 0
    else:
        print(f'Invalid system "{args.system}".')
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
