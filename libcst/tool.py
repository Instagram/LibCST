# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Usage:
#
# python -m libcst.tool --help
# python -m libcst.tool print python_file.py

# pyre-strict
import argparse
import dataclasses
import sys
from typing import List, Sequence

from libcst import CSTNode, IndentedBlock, Module, parse_module
from libcst._nodes._deep_equals import deep_equals


_DEFAULT_INDENT: str = "  "


def _node_repr_recursive(  # noqa: C901
    node: object,
    *,
    indent: str = _DEFAULT_INDENT,
    show_defaults: bool = False,
    show_syntax: bool = False,
    show_whitespace: bool = False,
) -> List[str]:
    if isinstance(node, CSTNode):
        # This is a CSTNode, we must pretty-print it.
        tokens: List[str] = [node.__class__.__name__]
        fields: Sequence["dataclasses.Field[object]"] = dataclasses.fields(node)

        # Hide all fields prefixed with "_"
        fields = [f for f in fields if f.name[0] != "_"]

        # Filter whitespace nodes if needed
        if not show_whitespace:

            def _is_whitespace(field: "dataclasses.Field[object]") -> bool:
                if "whitespace" in field.name:
                    return True
                if "leading_lines" in field.name:
                    return True
                if "lines_after_decorators" in field.name:
                    return True
                if isinstance(node, (IndentedBlock, Module)) and field.name in [
                    "header",
                    "footer",
                ]:
                    return True
                if isinstance(node, IndentedBlock) and field.name == "indent":
                    return True
                return False

            fields = [f for f in fields if not _is_whitespace(f)]
        # Filter values which aren't changed from their defaults
        if not show_defaults:
            fields = [
                f for f in fields if not deep_equals(getattr(node, f.name), f.default)
            ]
        # Filter out values which aren't interesting if needed
        if not show_syntax:

            def _is_syntax(field: "dataclasses.Field[object]") -> bool:
                if isinstance(node, Module) and field.name in [
                    "encoding",
                    "default_indent",
                    "default_newline",
                    "has_trailing_newline",
                ]:
                    return True
                type_str = repr(field.type)
                if (
                    "Sentinel" in type_str
                    and field.name not in ["star_arg", "star"]
                    and "whitespace" not in field.name
                ):
                    # This is a value that can optionally be specified, so its
                    # definitely syntax.
                    return True

                for name in ["Semicolon", "Colon", "Comma", "Dot", "AssignEqual"]:
                    # These are all nodes that exist for separation syntax
                    if name in type_str:
                        return True

                return False

            fields = [f for f in fields if not _is_syntax(f)]

        if len(fields) == 0:
            tokens.append("()")
        else:
            tokens.append("(\n")

            for field in fields:
                child_tokens: List[str] = [field.name, "="]
                value = getattr(node, field.name)

                if isinstance(value, (str, bytes)) or not isinstance(value, Sequence):
                    # Render out the node contents
                    child_tokens.extend(
                        _node_repr_recursive(
                            value,
                            show_whitespace=show_whitespace,
                            show_defaults=show_defaults,
                            show_syntax=show_syntax,
                        )
                    )
                elif isinstance(value, Sequence):
                    # Render out a list of individual nodes
                    if len(value) > 0:
                        child_tokens.append("[\n")
                        list_tokens: List[str] = []

                        last_value = len(value) - 1
                        for j, v in enumerate(value):
                            list_tokens.extend(
                                _node_repr_recursive(
                                    v,
                                    show_whitespace=show_whitespace,
                                    show_defaults=show_defaults,
                                    show_syntax=show_syntax,
                                )
                            )
                            if j != last_value:
                                list_tokens.append(",\n")
                            else:
                                list_tokens.append(",")

                        split_by_line = "".join(list_tokens).split("\n")
                        child_tokens.append(
                            "\n".join(f"{indent}{t}" for t in split_by_line)
                        )

                        child_tokens.append("\n]")
                    else:
                        child_tokens.append("[]")
                else:
                    raise Exception("Logic error!")

                # Handle indentation and trailing comma.
                split_by_line = "".join(child_tokens).split("\n")
                tokens.append("\n".join(f"{indent}{t}" for t in split_by_line))
                tokens.append(",\n")

            tokens.append(")")

        return tokens
    else:
        # This is a python value, just return the repr
        return [repr(node)]


def dump(
    node: CSTNode,
    *,
    indent: str = _DEFAULT_INDENT,
    show_defaults: bool = False,
    show_syntax: bool = False,
    show_whitespace: bool = False,
) -> str:
    """
    Returns a string representation of the node that contains minimal differences
    from the default contruction of the node while also hiding whitespace and
    syntax fields.

    Setting ``show_default`` to ``True`` will add fields regardless if their
    value is different from the default value.

    Setting ``show_whitespace`` will add whitespace fields and setting
    ``show_syntax`` will add syntax fields while respecting the value of
    ``show_default``.

    When all keyword args are set to true, the output of this function is
    indentical to the __repr__ method of the node.
    """
    return "".join(
        _node_repr_recursive(
            node,
            indent=indent,
            show_defaults=show_defaults,
            show_syntax=show_syntax,
            show_whitespace=show_whitespace,
        )
    )


def _print_tree_impl(args: argparse.Namespace) -> int:
    infile = args.infile

    # Grab input file
    if infile == "-":
        code = sys.stdin.read()
    else:
        with open(infile, "rb") as fp:
            code = fp.read()

    tree = parse_module(code)
    print(
        dump(
            tree,
            show_defaults=args.show_defaults,
            show_syntax=args.show_syntax,
            show_whitespace=args.show_whitespace,
        )
    )
    return 0


def main(cli_args: List[str]) -> int:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(title="commands", description="valid commands")

    print_parser = subparsers.add_parser(
        "print", help="Print LibCST tree for a python file."
    )
    print_parser.set_defaults(func=_print_tree_impl)
    print_parser.add_argument(
        "infile",
        metavar="INFILE",
        help='File to print tree for. Use "-" for stdin',
        type=str,
    )
    print_parser.add_argument(
        "--show-whitespace",
        action="store_true",
        help="Show whitespace nodes in printed tree",
    )
    print_parser.add_argument(
        "--show-defaults",
        action="store_true",
        help="Show values that are unchanged from the default",
    )
    print_parser.add_argument(
        "--show-syntax",
        action="store_true",
        help="Show values that exist only for syntax, like commas or semicolons",
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
