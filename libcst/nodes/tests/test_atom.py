# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst.nodes as cst
from libcst.nodes._internal import CodeRange
from libcst.nodes.tests.base import CSTNodeTest
from libcst.parser import parse_expression
from libcst.testing.utils import data_provider


class AtomTest(CSTNodeTest):
    @data_provider(
        (
            # Simple identifier
            (cst.Name("test"), "test"),
            # Parenthesized identifier
            (
                cst.Name("test", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                "(test)",
                CodeRange.create((1, 1), (1, 5)),
            ),
            # Decimal integers
            (cst.Number(cst.Integer("12345")), "12345"),
            (cst.Number(cst.Integer("0000")), "0000"),
            (cst.Number(cst.Integer("1_234_567")), "1_234_567"),
            (cst.Number(cst.Integer("0_000")), "0_000"),
            # Binary integers
            (cst.Number(cst.Integer("0b0000")), "0b0000"),
            (cst.Number(cst.Integer("0B1011_0100")), "0B1011_0100"),
            # Octal integers
            (cst.Number(cst.Integer("0o12345")), "0o12345"),
            (cst.Number(cst.Integer("0O12_345")), "0O12_345"),
            # Hex numbers
            (cst.Number(cst.Integer("0x123abc")), "0x123abc"),
            (cst.Number(cst.Integer("0X12_3ABC")), "0X12_3ABC"),
            # Parenthesized integers
            (
                cst.Number(
                    cst.Integer(
                        "123", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    )
                ),
                "(123)",
                # TODO: fix numbers
                # CodeRange.create((1, 1), (1, 4)),
            ),
            # Non-exponent floats
            (cst.Number(cst.Float("12345.")), "12345."),
            (cst.Number(cst.Float("00.00")), "00.00"),
            (cst.Number(cst.Float("12.21")), "12.21"),
            (cst.Number(cst.Float(".321")), ".321"),
            (cst.Number(cst.Float("1_234_567.")), "1_234_567."),
            (cst.Number(cst.Float("0.000_000")), "0.000_000"),
            # Exponent floats
            (cst.Number(cst.Float("12345.e10")), "12345.e10"),
            (cst.Number(cst.Float("00.00e10")), "00.00e10"),
            (cst.Number(cst.Float("12.21e10")), "12.21e10"),
            (cst.Number(cst.Float(".321e10")), ".321e10"),
            (cst.Number(cst.Float("1_234_567.e10")), "1_234_567.e10"),
            (cst.Number(cst.Float("0.000_000e10")), "0.000_000e10"),
            (cst.Number(cst.Float("1e+10")), "1e+10"),
            (cst.Number(cst.Float("1e-10")), "1e-10"),
            # Parenthesized floats
            (
                cst.Number(
                    cst.Float(
                        "123.4", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    )
                ),
                "(123.4)",
                # TODO: fix numbers
                # CodeRange.create((1, 1), (1, 5)),
            ),
            # Imaginary numbers
            (cst.Number(cst.Imaginary("12345j")), "12345j"),
            (cst.Number(cst.Imaginary("1_234_567J")), "1_234_567J"),
            (cst.Number(cst.Imaginary("12345.e10j")), "12345.e10j"),
            (cst.Number(cst.Imaginary(".321J")), ".321J"),
            # Parenthesized imaginary
            (
                cst.Number(
                    cst.Imaginary(
                        "123.4j", lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    )
                ),
                "(123.4j)",
                # TODO: fix numbers
                # CodeRange.create((1, 1), (1, 6)),
            ),
            # Simple elipses
            (cst.Ellipses(), "..."),
            # Parenthesized elipses
            (
                cst.Ellipses(lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)),
                "(...)",
                CodeRange.create((1, 1), (1, 4)),
            ),
            # Simple strings
            (cst.SimpleString('""'), '""'),
            (cst.SimpleString("''"), "''"),
            (cst.SimpleString('"test"'), '"test"'),
            (cst.SimpleString('b"test"'), 'b"test"'),
            (cst.SimpleString('r"test"'), 'r"test"'),
            (cst.SimpleString('"""test"""'), '"""test"""'),
            # Validate parens
            (
                cst.SimpleString(
                    '"test"', lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                ),
                '("test")',
            ),
            (
                cst.SimpleString(
                    'rb"test"', lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                ),
                '(rb"test")',
                CodeRange.create((1, 1), (1, 9)),
            ),
            # Empty formatted strings
            (cst.FormattedString(start='f"', parts=(), end='"'), 'f""'),
            (cst.FormattedString(start="f'", parts=(), end="'"), "f''"),
            (cst.FormattedString(start='f"""', parts=(), end='"""'), 'f""""""'),
            (cst.FormattedString(start="f'''", parts=(), end="'''"), "f''''''"),
            # Non-empty formatted strings
            (cst.FormattedString(parts=(cst.FormattedStringText("foo"),)), 'f"foo"'),
            (
                cst.FormattedString(
                    parts=(cst.FormattedStringExpression(cst.Name("foo")),)
                ),
                'f"{foo}"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringText("foo "),
                        cst.FormattedStringExpression(cst.Name("bar")),
                        cst.FormattedStringText(" baz"),
                    )
                ),
                'f"foo {bar} baz"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringText("foo "),
                        cst.FormattedStringExpression(cst.Call(cst.Name("bar"))),
                        cst.FormattedStringText(" baz"),
                    )
                ),
                'f"foo {bar()} baz"',
            ),
            # Formatted strings with conversions and format specifiers
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringExpression(cst.Name("foo"), conversion="s"),
                    )
                ),
                'f"{foo!s}"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringExpression(cst.Name("foo"), format_spec=()),
                    )
                ),
                'f"{foo:}"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringExpression(
                            cst.Name("today"),
                            format_spec=(cst.FormattedStringText("%B %d, %Y"),),
                        ),
                    )
                ),
                'f"{today:%B %d, %Y}"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringExpression(
                            cst.Name("foo"),
                            format_spec=(
                                cst.FormattedStringExpression(cst.Name("bar")),
                            ),
                        ),
                    )
                ),
                'f"{foo:{bar}}"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringExpression(
                            cst.Name("foo"),
                            format_spec=(
                                cst.FormattedStringExpression(cst.Name("bar")),
                                cst.FormattedStringText("."),
                                cst.FormattedStringExpression(cst.Name("baz")),
                            ),
                        ),
                    )
                ),
                'f"{foo:{bar}.{baz}}"',
            ),
            (
                cst.FormattedString(
                    parts=(
                        cst.FormattedStringExpression(
                            cst.Name("foo"),
                            conversion="s",
                            format_spec=(
                                cst.FormattedStringExpression(cst.Name("bar")),
                            ),
                        ),
                    )
                ),
                'f"{foo!s:{bar}}"',
            ),
            # Validate parens
            (
                cst.FormattedString(
                    start='f"',
                    parts=(),
                    end='"',
                    lpar=(cst.LeftParen(),),
                    rpar=(cst.RightParen(),),
                ),
                '(f"")',
                CodeRange.create((1, 1), (1, 4)),
            ),
            # Concatenated strings
            (
                cst.ConcatenatedString(
                    cst.SimpleString('"ab"'), cst.SimpleString('"c"')
                ),
                '"ab""c"',
            ),
            # Concatenated parenthesized strings
            (
                cst.ConcatenatedString(
                    lpar=(cst.LeftParen(),),
                    left=cst.SimpleString('"ab"'),
                    right=cst.SimpleString('"c"'),
                    rpar=(cst.RightParen(),),
                ),
                '("ab""c")',
            ),
            # Validate spacing
            (
                cst.ConcatenatedString(
                    lpar=(cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),),
                    left=cst.SimpleString('"ab"'),
                    whitespace_between=cst.SimpleWhitespace(" "),
                    right=cst.SimpleString('"c"'),
                    rpar=(cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),),
                ),
                '( "ab" "c" )',
                CodeRange.create((1, 2), (1, 10)),
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        # We don't have sentinel nodes for atoms, so we know that 100% of atoms
        # can be parsed identically to their creation.
        self.validate_node(node, code, parse_expression, expected_position=position)

    @data_provider(
        (
            (
                cst.FormattedStringExpression(
                    cst.Name("today"),
                    format_spec=(cst.FormattedStringText("%B %d, %Y"),),
                ),
                "{today:%B %d, %Y}",
                CodeRange.create((1, 0), (1, 17)),
            ),
        )
    )
    def test_valid_no_parse(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        # Test some nodes that aren't valid source code by themselves
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            # Expression wrapping parenthesis rules
            (
                lambda: cst.Name("foo", lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Name("foo", rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.Ellipses(lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Ellipses(rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.Integer("5", lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Integer("5", rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.Float("5.5", lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Float("5.5", rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.Imaginary("5j", lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Imaginary("5j", rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.Number(cst.Integer("5"), lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.Number(cst.Integer("5"), rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.SimpleString("'foo'", lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                lambda: cst.SimpleString("'foo'", rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                # pyre-fixme[6]: Expected `Sequence[BaseFormattedStringContent]` for
                #  1st param but got `str`.
                lambda: cst.FormattedString("f''", lpar=(cst.LeftParen(),)),
                "left paren without right paren",
            ),
            (
                # pyre-fixme[6]: Expected `Sequence[BaseFormattedStringContent]` for
                #  1st param but got `str`.
                lambda: cst.FormattedString("f''", rpar=(cst.RightParen(),)),
                "right paren without left paren",
            ),
            (
                lambda: cst.ConcatenatedString(
                    cst.SimpleString("'foo'"),
                    cst.SimpleString("'foo'"),
                    lpar=(cst.LeftParen(),),
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.ConcatenatedString(
                    cst.SimpleString("'foo'"),
                    cst.SimpleString("'foo'"),
                    rpar=(cst.RightParen(),),
                ),
                "right paren without left paren",
            ),
            # Node-specific rules
            (lambda: cst.Name(""), "empty name identifier"),
            (lambda: cst.Name(r"\/"), "not a valid identifier"),
            (lambda: cst.Integer(""), "not a valid integer"),
            (lambda: cst.Integer("012345"), "not a valid integer"),
            (lambda: cst.Integer("012345"), "not a valid integer"),
            (lambda: cst.Integer("_12345"), "not a valid integer"),
            (lambda: cst.Integer("0b2"), "not a valid integer"),
            (lambda: cst.Integer("0o8"), "not a valid integer"),
            (lambda: cst.Integer("0xg"), "not a valid integer"),
            (lambda: cst.Integer("123.45"), "not a valid integer"),
            (lambda: cst.Integer("12345j"), "not a valid integer"),
            (lambda: cst.Float("12.3.45"), "not a valid float"),
            (lambda: cst.Float("12"), "not a valid float"),
            (lambda: cst.Float("12.3j"), "not a valid float"),
            (lambda: cst.Imaginary("_12345j"), "not a valid imaginary"),
            (lambda: cst.Imaginary("0b0j"), "not a valid imaginary"),
            (lambda: cst.Imaginary("0o0j"), "not a valid imaginary"),
            (lambda: cst.Imaginary("0x0j"), "not a valid imaginary"),
            (lambda: cst.SimpleString('wee""'), "Invalid string prefix"),
            (lambda: cst.SimpleString(""), "must have enclosing quotes"),
            (lambda: cst.SimpleString("'"), "must have enclosing quotes"),
            (lambda: cst.SimpleString('"'), "must have enclosing quotes"),
            (lambda: cst.SimpleString("\"'"), "must have matching enclosing quotes"),
            (lambda: cst.SimpleString("'bla"), "must have matching enclosing quotes"),
            (lambda: cst.SimpleString("f''"), "Invalid string prefix"),
            (
                lambda: cst.SimpleString("'''bla''"),
                "must have matching enclosing quotes",
            ),
            (
                lambda: cst.SimpleString("'''bla\"\"\""),
                "must have matching enclosing quotes",
            ),
            (
                lambda: cst.FormattedString(start="'", parts=(), end="'"),
                "Invalid f-string prefix",
            ),
            (
                lambda: cst.FormattedString(start="f'", parts=(), end='"'),
                "must have matching enclosing quotes",
            ),
            (
                lambda: cst.FormattedString(start="f'''", parts=(), end="''"),
                "must have matching enclosing quotes",
            ),
            (
                lambda: cst.ConcatenatedString(
                    cst.SimpleString(
                        '"ab"', lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                    cst.SimpleString('"c"'),
                ),
                "Cannot concatenate parenthesized",
            ),
            (
                lambda: cst.ConcatenatedString(
                    cst.SimpleString('"ab"'),
                    cst.SimpleString(
                        '"c"', lpar=(cst.LeftParen(),), rpar=(cst.RightParen(),)
                    ),
                ),
                "Cannot concatenate parenthesized",
            ),
            (
                lambda: cst.ConcatenatedString(
                    cst.SimpleString('"ab"'), cst.SimpleString('b"c"')
                ),
                "Cannot concatenate string and bytes",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
