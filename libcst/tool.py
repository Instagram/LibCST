# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Usage:
#
# python -m libcst.tool --help
# python -m libcst.tool print python_file.py

import argparse
import sys
from typing import List

from libcst.parser import parse_module


def print_tree(args: argparse.Namespace) -> int:
    infile = args.infile

    # Grab input file
    if infile == "-":
        code = sys.stdin.read()
    else:
        with open(infile, "rb") as fp:
            code = fp.read()

    tree = parse_module(code)
    print(tree)
    return 0


def main(cli_args: List[str]) -> int:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(title="commands", description="valid commands")

    print_parser = subparsers.add_parser(
        "print", help="Print LibCST tree for a python file."
    )
    print_parser.set_defaults(func=print_tree)
    print_parser.add_argument(
        "infile",
        metavar="INFILE",
        help='File to print tree for. Use "-" for stdin',
        type=str,
    )
    args = parser.parse_args(cli_args)
    if "func" in args:
        return args.func(args)
    else:
        print("Please specify a command!\n", file=sys.stderr)
        parser.print_help(sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
