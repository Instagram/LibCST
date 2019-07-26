# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst import parse_statement
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest, DummyIndentedBlock
from libcst.testing.utils import data_provider


class TryTest(CSTNodeTest):
    @data_provider(
        (
            # Simple try/except block
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            whitespace_after_except=cst.SimpleWhitespace(""),
                        ),
                    ),
                ),
                "try: pass\nexcept: pass\n",
                parse_statement,
            ),
            # Try/except with a class
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            type=cst.Name("Exception"),
                        ),
                    ),
                ),
                "try: pass\nexcept Exception: pass\n",
                parse_statement,
            ),
            # Try/except with a named class
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            type=cst.Name("Exception"),
                            name=cst.AsName(cst.Name("exc")),
                        ),
                    ),
                ),
                "try: pass\nexcept Exception as exc: pass\n",
                parse_statement,
            ),
            # Try/except with multiple clauses
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            type=cst.Name("TypeError"),
                            name=cst.AsName(cst.Name("e")),
                        ),
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            type=cst.Name("KeyError"),
                            name=cst.AsName(cst.Name("e")),
                        ),
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            whitespace_after_except=cst.SimpleWhitespace(""),
                        ),
                    ),
                ),
                "try: pass\n"
                + "except TypeError as e: pass\n"
                + "except KeyError as e: pass\n"
                + "except: pass\n",
                parse_statement,
            ),
            # Simple try/finally block
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    finalbody=cst.Finally(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "try: pass\nfinally: pass\n",
                parse_statement,
            ),
            # Simple try/except/finally block
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            whitespace_after_except=cst.SimpleWhitespace(""),
                        ),
                    ),
                    finalbody=cst.Finally(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "try: pass\nexcept: pass\nfinally: pass\n",
                parse_statement,
            ),
            # Simple try/except/else block
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            whitespace_after_except=cst.SimpleWhitespace(""),
                        ),
                    ),
                    orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "try: pass\nexcept: pass\nelse: pass\n",
                parse_statement,
            ),
            # Simple try/except/else block/finally
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            whitespace_after_except=cst.SimpleWhitespace(""),
                        ),
                    ),
                    orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                    finalbody=cst.Finally(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "try: pass\nexcept: pass\nelse: pass\nfinally: pass\n",
                parse_statement,
            ),
            # Verify whitespace in various locations
            (
                cst.Try(
                    leading_lines=(cst.EmptyLine(comment=cst.Comment("# 1")),),
                    body=cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            leading_lines=(cst.EmptyLine(comment=cst.Comment("# 2")),),
                            type=cst.Name("TypeError"),
                            name=cst.AsName(
                                cst.Name("e"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                            whitespace_after_except=cst.SimpleWhitespace("  "),
                            whitespace_before_colon=cst.SimpleWhitespace(" "),
                            body=cst.SimpleStatementSuite((cst.Pass(),)),
                        ),
                    ),
                    orelse=cst.Else(
                        leading_lines=(cst.EmptyLine(comment=cst.Comment("# 3")),),
                        body=cst.SimpleStatementSuite((cst.Pass(),)),
                        whitespace_before_colon=cst.SimpleWhitespace(" "),
                    ),
                    finalbody=cst.Finally(
                        leading_lines=(cst.EmptyLine(comment=cst.Comment("# 4")),),
                        body=cst.SimpleStatementSuite((cst.Pass(),)),
                        whitespace_before_colon=cst.SimpleWhitespace(" "),
                    ),
                    whitespace_before_colon=cst.SimpleWhitespace(" "),
                ),
                "# 1\ntry : pass\n# 2\nexcept  TypeError  as  e : pass\n# 3\nelse : pass\n# 4\nfinally : pass\n",
                parse_statement,
            ),
            # Please don't write code like this
            (
                cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    handlers=(
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            type=cst.Name("TypeError"),
                            name=cst.AsName(cst.Name("e")),
                        ),
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            type=cst.Name("KeyError"),
                            name=cst.AsName(cst.Name("e")),
                        ),
                        cst.ExceptHandler(
                            cst.SimpleStatementSuite((cst.Pass(),)),
                            whitespace_after_except=cst.SimpleWhitespace(""),
                        ),
                    ),
                    orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                    finalbody=cst.Finally(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "try: pass\n"
                + "except TypeError as e: pass\n"
                + "except KeyError as e: pass\n"
                + "except: pass\n"
                + "else: pass\n"
                + "finally: pass\n",
                parse_statement,
            ),
            # Verify indentation
            (
                DummyIndentedBlock(
                    "    ",
                    cst.Try(
                        cst.SimpleStatementSuite((cst.Pass(),)),
                        handlers=(
                            cst.ExceptHandler(
                                cst.SimpleStatementSuite((cst.Pass(),)),
                                type=cst.Name("TypeError"),
                                name=cst.AsName(cst.Name("e")),
                            ),
                            cst.ExceptHandler(
                                cst.SimpleStatementSuite((cst.Pass(),)),
                                type=cst.Name("KeyError"),
                                name=cst.AsName(cst.Name("e")),
                            ),
                            cst.ExceptHandler(
                                cst.SimpleStatementSuite((cst.Pass(),)),
                                whitespace_after_except=cst.SimpleWhitespace(""),
                            ),
                        ),
                        orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                        finalbody=cst.Finally(cst.SimpleStatementSuite((cst.Pass(),))),
                    ),
                ),
                "    try: pass\n"
                + "    except TypeError as e: pass\n"
                + "    except KeyError as e: pass\n"
                + "    except: pass\n"
                + "    else: pass\n"
                + "    finally: pass\n",
                None,
            ),
            # Verify indentation in bodies
            (
                DummyIndentedBlock(
                    "    ",
                    cst.Try(
                        cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),)),
                        handlers=(
                            cst.ExceptHandler(
                                cst.IndentedBlock(
                                    (cst.SimpleStatementLine((cst.Pass(),)),)
                                ),
                                whitespace_after_except=cst.SimpleWhitespace(""),
                            ),
                        ),
                        orelse=cst.Else(
                            cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),))
                        ),
                        finalbody=cst.Finally(
                            cst.IndentedBlock((cst.SimpleStatementLine((cst.Pass(),)),))
                        ),
                    ),
                ),
                "    try:\n"
                + "        pass\n"
                + "    except:\n"
                + "        pass\n"
                + "    else:\n"
                + "        pass\n"
                + "    finally:\n"
                + "        pass\n",
                None,
            ),
        )
    )
    def test_valid(
        self,
        node: cst.CSTNode,
        code: str,
        parser: Optional[Callable[[str], cst.CSTNode]],
        position: Optional[CodeRange] = None,
    ) -> None:
        self.validate_node(node, code, parser, expected_position=position)

    @data_provider(
        (
            (lambda: cst.AsName(cst.Name("")), "empty name identifier"),
            (
                lambda: cst.AsName(
                    cst.Name("bla"), whitespace_after_as=cst.SimpleWhitespace("")
                ),
                "between 'as'",
            ),
            (
                lambda: cst.AsName(
                    cst.Name("bla"), whitespace_before_as=cst.SimpleWhitespace("")
                ),
                "before 'as'",
            ),
            (
                lambda: cst.ExceptHandler(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    name=cst.AsName(cst.Name("bla")),
                ),
                "name for an empty type",
            ),
            (
                lambda: cst.ExceptHandler(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    type=cst.Name("TypeError"),
                    whitespace_after_except=cst.SimpleWhitespace(""),
                ),
                "at least one space after except",
            ),
            (
                lambda: cst.Try(cst.SimpleStatementSuite((cst.Pass(),))),
                "at least one ExceptHandler or Finally",
            ),
            (
                lambda: cst.Try(
                    cst.SimpleStatementSuite((cst.Pass(),)),
                    orelse=cst.Else(cst.SimpleStatementSuite((cst.Pass(),))),
                    finalbody=cst.Finally(cst.SimpleStatementSuite((cst.Pass(),))),
                ),
                "at least one ExceptHandler in order to have an Else",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)
