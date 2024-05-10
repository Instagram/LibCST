# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import argparse
import os
import pathlib
import sys
from collections.abc import Sequence

from libcst import CSTNode, parse_module, PartialParserConfig
from libcst.helpers import filter_node_fields

_syntax_style = ', color="#777777", fillcolor="#eeeeee"'
_value_style = ', color="#3e99ed", fillcolor="#b8d9f8", shape="box"'

node_style: dict[str, str] = {
    "__default__": "",

    "EmptyLine": _syntax_style,
    "IndentedBlock": _syntax_style,
    "SimpleStatementLine": _syntax_style,
    "SimpleWhitespace": _syntax_style,
    "TrailingWhitespace": _syntax_style,
    "Newline": _syntax_style,
    "Comma": _syntax_style,
    "LeftParen": _syntax_style,
    "RightParen": _syntax_style,
    "LeftSquareBracket": _syntax_style,
    "RightSquareBracket": _syntax_style,
    "LeftCurlyBrace": _syntax_style,
    "RightCurlyBrace": _syntax_style,
    "BaseSmallStatement": _syntax_style,
    "BaseCompoundStatement": _syntax_style,
    "SimpleStatementSuite": _syntax_style,
    "Colon": _syntax_style,
    "Dot": _syntax_style,
    "Semicolon": _syntax_style,
    "ParenthesizedWhitespace": _syntax_style,
    "BaseParenthesizableWhitespace": _syntax_style,

    "Name": _value_style,
    "Integer": _value_style,
    "Float": _value_style,
    "Imaginary": _value_style,
    "SimpleString": _value_style,
    "FormattedStringText": _value_style,
}
"""Graphviz style for specific CST nodes"""


def _create_node_graphviz(node: CSTNode) -> str:
    node_name = node.__class__.__qualname__

    if node_name in node_style:
        style = node_style[node_name]
    else:
        style = node_style["__default__"]

    if hasattr(node, "value") and isinstance(node.value, str) and node.value:
        line_break = r"\n"
        quote = '"'
        escaped_quote = r'\"'
        value = f"{line_break}<{node.value.replace(quote, escaped_quote)}>"
    else:
        value = ""

    return f'{id(node)} [label="{node_name}{value}"{style}]'


def _node_repr_recursive(
    node: object,
    *,
    show_defaults: bool,
    show_syntax: bool,
    show_whitespace: bool,
) -> list[str]:
    """repr a CST node, graphviz-style.
    If a node has child nodes, repr them as well."""
    if not isinstance(node, CSTNode):
        return []

    fields = filter_node_fields(
        node,
        show_defaults=show_defaults,
        show_syntax=show_syntax,
        show_whitespace=show_whitespace,
    )

    graphviz_lines: list[str] = [_create_node_graphviz(node)]

    for field in fields:
        value = getattr(node, field.name)
        if isinstance(value, CSTNode):
            graphviz_lines.append(f'{id(node)} -> {id(value)} [label="{field.name}"]')
            graphviz_lines.extend(
                _node_repr_recursive(
                    value,
                    show_defaults=show_defaults,
                    show_syntax=show_syntax,
                    show_whitespace=show_whitespace,
                )
            )
            continue

        if isinstance(value, Sequence):
            for index, child in enumerate(value):
                if isinstance(child, CSTNode):
                    graphviz_lines.append(
                        rf'{id(node)} -> {id(child)} [label="{field.name}[{index}]"]'
                    )
                    graphviz_lines.extend(
                        _node_repr_recursive(
                            child,
                            show_defaults=show_defaults,
                            show_syntax=show_syntax,
                            show_whitespace=show_whitespace,
                        )
                    )

    return graphviz_lines


def graphviz_repr(
    node: object,
    *,
    show_defaults: bool = False,
    show_syntax: bool = False,
    show_whitespace: bool = False,
) -> str:
    """
    Returns a string representation (in graphviz .dot style) of a CST node.
    If the node has child nodes, they will be represented as well.

    Setting ``show_defaults`` to ``True`` will add fields regardless if their
    value is different from the default value.

    Setting ``show_whitespace`` will add whitespace fields and setting
    ``show_syntax`` will add syntax fields while respecting the value of
    ``show_defaults``.

    When all keyword args are set to true, the output of this function is
    indentical to the __repr__ method of the node.
    """

    return "\n".join(
        [
            r"""digraph {
layout=dot;
rankdir=TB;
splines=line;
ranksep=0.5;
nodesep=1.0;
dpi=300;
bgcolor=transparent;
node [
    style=filled,
    color="#fb8d3f",
    fontcolor="#4b4f54",
    fillcolor="#fdd2b3",
    fontname="Source Code Pro Semibold",
    penwidth="2",
    group=main,
];
edge [
    color="#999999",
    fontcolor="#4b4f54",
    fontname="Source Code Pro Semibold",
    fontsize=12,
    penwidth=2,
];"""
        ]
        + _node_repr_recursive(
            node,
            show_defaults=show_defaults,
            show_syntax=show_syntax,
            show_whitespace=show_whitespace,
        )
        + ["}"]
    )


def print_tree_graphviz(proc_name: str, command_args: list[str]) -> int:
    parser = argparse.ArgumentParser(
        description="Build the Graphviz representation of a LibCST graph",
        prog=f"{proc_name} print-graphviz",
        fromfile_prefix_chars="@",
    )
    parser.add_argument(
        "infile",
        metavar="INFILE",
        help='File to print tree for. Use "-" for stdin',
        type=str,
    )
    parser.add_argument(
        "outfile",
        help="File where to save the graphviz representation of the tree (.dot file)"
             'Use "-" for stdout',
        type=str,
    )
    parser.add_argument(
        "--show-whitespace",
        action="store_true",
        help="Show whitespace nodes in graphviz graph",
    )
    parser.add_argument(
        "--show-defaults",
        action="store_true",
        help="Show values that are unchanged from the default",
    )
    parser.add_argument(
        "--show-syntax",
        action="store_true",
        help="Show values that exist only for syntax, like commas or semicolons",
    )
    parser.add_argument(
        "-p",
        "--python-version",
        metavar="VERSION",
        help=(
            "Override the version string used for parsing Python source files. "
            "Defaults to the version of python used to run this tool."
        ),
        type=str,
        default=None,
    )
    args = parser.parse_args(command_args)
    infile = args.infile

    # Grab input file
    if infile == "-":
        code = sys.stdin.read()
    else:
        with open(infile, "rb") as fp:
            code = fp.read()
    # Parse tree
    tree = parse_module(
        code,
        config=(
            PartialParserConfig(python_version=args.python_version)
            if args.python_version is not None
            else PartialParserConfig()
        ),
    )
    # Write output file
    output_file = args.outfile
    if output_file == "-":
        print(
            graphviz_repr(
                tree,
                show_defaults=args.show_defaults,
                show_syntax=args.show_syntax,
                show_whitespace=args.show_whitespace,
            )
        )
    else:
        out_path_dir = pathlib.Path(output_file).parent.resolve()
        out_path_dir.mkdir(parents=True, exist_ok=True)
        with open(output_file, "w", encoding='utf-8') as out_file:
            out_file.write(
                graphviz_repr(
                    tree,
                    show_defaults=args.show_defaults,
                    show_syntax=args.show_syntax,
                    show_whitespace=args.show_whitespace,
                )
            )

    return 0


if __name__ == '__main__':
    print_tree_graphviz(os.environ.get("LIBCST_TOOL_COMMAND_NAME", "libcst.tool"), sys.argv[1:])
