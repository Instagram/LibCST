# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Callable, Optional

import libcst as cst
from libcst._helpers import ensure_type
from libcst._nodes._internal import CodeRange
from libcst._nodes.tests.base import CSTNodeTest
from libcst.parser import parse_statement
from libcst.testing.utils import data_provider


class ImportCreateTest(CSTNodeTest):
    @data_provider(
        (
            # Simple import statement
            (cst.Import(names=(cst.ImportAlias(cst.Name("foo")),)), "import foo"),
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar"))
                        ),
                    )
                ),
                "import foo.bar",
            ),
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar"))
                        ),
                    )
                ),
                "import foo.bar",
            ),
            # Comma-separated list of imports
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar"))
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("baz"))
                        ),
                    )
                ),
                "import foo.bar, foo.baz",
            ),
            # Import with an alias
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            asname=cst.AsName(cst.Name("baz")),
                        ),
                    )
                ),
                "import foo.bar as baz",
            ),
            # Import with an alias, comma separated
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            asname=cst.AsName(cst.Name("baz")),
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("baz")),
                            asname=cst.AsName(cst.Name("bar")),
                        ),
                    )
                ),
                "import foo.bar as baz, foo.baz as bar",
            ),
            # Combine for fun and profit
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            asname=cst.AsName(cst.Name("baz")),
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("insta"), cst.Name("gram"))
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("baz"))
                        ),
                        cst.ImportAlias(
                            cst.Name("unittest"), asname=cst.AsName(cst.Name("ut"))
                        ),
                    )
                ),
                "import foo.bar as baz, insta.gram, foo.baz, unittest as ut",
            ),
            # Verify whitespace works everywhere.
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(
                                cst.Name("foo"),
                                cst.Name("bar"),
                                dot=cst.Dot(
                                    whitespace_before=cst.SimpleWhitespace(" "),
                                    whitespace_after=cst.SimpleWhitespace(" "),
                                ),
                            ),
                            asname=cst.AsName(
                                cst.Name("baz"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.ImportAlias(
                            cst.Name("unittest"),
                            asname=cst.AsName(
                                cst.Name("ut"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    whitespace_after_import=cst.SimpleWhitespace("  "),
                ),
                "import  foo . bar  as  baz ,  unittest  as  ut",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            (lambda: cst.Import(names=()), "at least one ImportAlias"),
            (
                lambda: cst.Import(names=(cst.ImportAlias(cst.Name("")),)),
                "empty name identifier",
            ),
            (
                lambda: cst.Import(
                    names=(
                        cst.ImportAlias(cst.Attribute(cst.Name(""), cst.Name("bla"))),
                    )
                ),
                "empty name identifier",
            ),
            (
                lambda: cst.Import(
                    names=(
                        cst.ImportAlias(cst.Attribute(cst.Name("bla"), cst.Name(""))),
                    )
                ),
                "empty name identifier",
            ),
            (
                lambda: cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            comma=cst.Comma(),
                        ),
                    )
                ),
                "trailing comma",
            ),
            (
                lambda: cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar"))
                        ),
                    ),
                    whitespace_after_import=cst.SimpleWhitespace(""),
                ),
                "at least one space",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class ImportParseTest(CSTNodeTest):
    @data_provider(
        (
            # Simple import statement
            (cst.Import(names=(cst.ImportAlias(cst.Name("foo")),)), "import foo"),
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar"))
                        ),
                    )
                ),
                "import foo.bar",
            ),
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar"))
                        ),
                    )
                ),
                "import foo.bar",
            ),
            # Comma-separated list of imports
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("baz"))
                        ),
                    )
                ),
                "import foo.bar, foo.baz",
            ),
            # Import with an alias
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            asname=cst.AsName(cst.Name("baz")),
                        ),
                    )
                ),
                "import foo.bar as baz",
            ),
            # Import with an alias, comma separated
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            asname=cst.AsName(cst.Name("baz")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("baz")),
                            asname=cst.AsName(cst.Name("bar")),
                        ),
                    )
                ),
                "import foo.bar as baz, foo.baz as bar",
            ),
            # Combine for fun and profit
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                            asname=cst.AsName(cst.Name("baz")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("insta"), cst.Name("gram")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(
                            cst.Attribute(cst.Name("foo"), cst.Name("baz")),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(
                            cst.Name("unittest"), asname=cst.AsName(cst.Name("ut"))
                        ),
                    )
                ),
                "import foo.bar as baz, insta.gram, foo.baz, unittest as ut",
            ),
            # Verify whitespace works everywhere.
            (
                cst.Import(
                    names=(
                        cst.ImportAlias(
                            cst.Attribute(
                                cst.Name("foo"),
                                cst.Name("bar"),
                                dot=cst.Dot(
                                    whitespace_before=cst.SimpleWhitespace(" "),
                                    whitespace_after=cst.SimpleWhitespace(" "),
                                ),
                            ),
                            asname=cst.AsName(
                                cst.Name("baz"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.ImportAlias(
                            cst.Name("unittest"),
                            asname=cst.AsName(
                                cst.Name("ut"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    whitespace_after_import=cst.SimpleWhitespace("  "),
                ),
                "import  foo . bar  as  baz ,  unittest  as  ut",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(
            node,
            code,
            lambda code: ensure_type(
                parse_statement(code), cst.SimpleStatementLine
            ).body[0],
            expected_position=position,
        )


class ImportFromCreateTest(CSTNodeTest):
    @data_provider(
        (
            # Simple from import statement
            (
                cst.ImportFrom(
                    module=cst.Name("foo"), names=(cst.ImportAlias(cst.Name("bar")),)
                ),
                "from foo import bar",
            ),
            # From import statement with alias
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"), asname=cst.AsName(cst.Name("baz"))
                        ),
                    ),
                ),
                "from foo import bar as baz",
            ),
            # Multiple imports
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(
                        cst.ImportAlias(cst.Name("bar")),
                        cst.ImportAlias(cst.Name("baz")),
                    ),
                ),
                "from foo import bar, baz",
            ),
            # Trailing comma
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(
                        cst.ImportAlias(cst.Name("bar"), comma=cst.Comma()),
                        cst.ImportAlias(cst.Name("baz"), comma=cst.Comma()),
                    ),
                ),
                "from foo import bar,baz,",
            ),
            # Star import statement
            (
                cst.ImportFrom(module=cst.Name("foo"), names=cst.ImportStar()),
                "from foo import *",
            ),
            # Simple relative import statement
            (
                cst.ImportFrom(
                    relative=(cst.Dot(),),
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                ),
                "from .foo import bar",
            ),
            (
                cst.ImportFrom(
                    relative=(cst.Dot(), cst.Dot()),
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                ),
                "from ..foo import bar",
            ),
            # Relative only import
            (
                cst.ImportFrom(
                    relative=(cst.Dot(), cst.Dot()),
                    module=None,
                    names=(cst.ImportAlias(cst.Name("bar")),),
                ),
                "from .. import bar",
            ),
            # Parenthesis
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    lpar=cst.LeftParen(),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"), asname=cst.AsName(cst.Name("baz"))
                        ),
                    ),
                    rpar=cst.RightParen(),
                ),
                "from foo import (bar as baz)",
            ),
            # Verify whitespace works everywhere.
            (
                cst.ImportFrom(
                    relative=(
                        cst.Dot(
                            whitespace_before=cst.SimpleWhitespace(" "),
                            whitespace_after=cst.SimpleWhitespace(" "),
                        ),
                        cst.Dot(
                            whitespace_before=cst.SimpleWhitespace(" "),
                            whitespace_after=cst.SimpleWhitespace(" "),
                        ),
                    ),
                    module=cst.Name("foo"),
                    lpar=cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"),
                            asname=cst.AsName(
                                cst.Name("baz"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.ImportAlias(
                            cst.Name("unittest"),
                            asname=cst.AsName(
                                cst.Name("ut"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    rpar=cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),
                    whitespace_after_from=cst.SimpleWhitespace("  "),
                    whitespace_before_import=cst.SimpleWhitespace("  "),
                    whitespace_after_import=cst.SimpleWhitespace("  "),
                ),
                "from   .  . foo  import  ( bar  as  baz ,  unittest  as  ut )",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(node, code, expected_position=position)

    @data_provider(
        (
            (
                lambda: cst.ImportFrom(
                    module=None, names=(cst.ImportAlias(cst.Name("bar")),)
                ),
                "Must have a module specified",
            ),
            (
                lambda: cst.ImportFrom(module=cst.Name("foo"), names=()),
                "at least one ImportAlias",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                    lpar=cst.LeftParen(),
                ),
                "left paren without right paren",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                    rpar=cst.RightParen(),
                ),
                "right paren without left paren",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"), names=cst.ImportStar(), lpar=cst.LeftParen()
                ),
                "cannot have parens",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=cst.ImportStar(),
                    rpar=cst.RightParen(),
                ),
                "cannot have parens",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                    whitespace_after_from=cst.SimpleWhitespace(""),
                ),
                "one space after from",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                    whitespace_before_import=cst.SimpleWhitespace(""),
                ),
                "one space before import",
            ),
            (
                lambda: cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                    whitespace_after_import=cst.SimpleWhitespace(""),
                ),
                "one space after import",
            ),
        )
    )
    def test_invalid(
        self, get_node: Callable[[], cst.CSTNode], expected_re: str
    ) -> None:
        self.assert_invalid(get_node, expected_re)


class ImportFromParseTest(CSTNodeTest):
    @data_provider(
        (
            # Simple from import statement
            (
                cst.ImportFrom(
                    module=cst.Name("foo"), names=(cst.ImportAlias(cst.Name("bar")),)
                ),
                "from foo import bar",
            ),
            # From import statement with alias
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"), asname=cst.AsName(cst.Name("baz"))
                        ),
                    ),
                ),
                "from foo import bar as baz",
            ),
            # Multiple imports
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(cst.Name("baz")),
                    ),
                ),
                "from foo import bar, baz",
            ),
            # Trailing comma
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"),
                            comma=cst.Comma(whitespace_after=cst.SimpleWhitespace(" ")),
                        ),
                        cst.ImportAlias(cst.Name("baz"), comma=cst.Comma()),
                    ),
                ),
                "from foo import bar, baz,",
            ),
            # Star import statement
            (
                cst.ImportFrom(module=cst.Name("foo"), names=cst.ImportStar()),
                "from foo import *",
            ),
            # Simple relative import statement
            (
                cst.ImportFrom(
                    relative=(cst.Dot(),),
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                ),
                "from .foo import bar",
            ),
            (
                cst.ImportFrom(
                    relative=(cst.Dot(), cst.Dot()),
                    module=cst.Name("foo"),
                    names=(cst.ImportAlias(cst.Name("bar")),),
                ),
                "from ..foo import bar",
            ),
            # Relative only import
            (
                cst.ImportFrom(
                    relative=(cst.Dot(), cst.Dot()),
                    module=None,
                    names=(cst.ImportAlias(cst.Name("bar")),),
                ),
                "from .. import bar",
            ),
            # Parenthesis
            (
                cst.ImportFrom(
                    module=cst.Name("foo"),
                    lpar=cst.LeftParen(),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"), asname=cst.AsName(cst.Name("baz"))
                        ),
                    ),
                    rpar=cst.RightParen(),
                ),
                "from foo import (bar as baz)",
            ),
            # Verify whitespace works everywhere.
            (
                cst.ImportFrom(
                    relative=(
                        cst.Dot(
                            whitespace_before=cst.SimpleWhitespace(""),
                            whitespace_after=cst.SimpleWhitespace("  "),
                        ),
                        cst.Dot(
                            whitespace_before=cst.SimpleWhitespace(""),
                            whitespace_after=cst.SimpleWhitespace(" "),
                        ),
                    ),
                    module=cst.Name("foo"),
                    lpar=cst.LeftParen(whitespace_after=cst.SimpleWhitespace(" ")),
                    names=(
                        cst.ImportAlias(
                            cst.Name("bar"),
                            asname=cst.AsName(
                                cst.Name("baz"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                            comma=cst.Comma(
                                whitespace_before=cst.SimpleWhitespace(" "),
                                whitespace_after=cst.SimpleWhitespace("  "),
                            ),
                        ),
                        cst.ImportAlias(
                            cst.Name("unittest"),
                            asname=cst.AsName(
                                cst.Name("ut"),
                                whitespace_before_as=cst.SimpleWhitespace("  "),
                                whitespace_after_as=cst.SimpleWhitespace("  "),
                            ),
                        ),
                    ),
                    rpar=cst.RightParen(whitespace_before=cst.SimpleWhitespace(" ")),
                    whitespace_after_from=cst.SimpleWhitespace("   "),
                    whitespace_before_import=cst.SimpleWhitespace("  "),
                    whitespace_after_import=cst.SimpleWhitespace("  "),
                ),
                "from   .  . foo  import  ( bar  as  baz ,  unittest  as  ut )",
            ),
        )
    )
    def test_valid(
        self, node: cst.CSTNode, code: str, position: Optional[CodeRange] = None
    ) -> None:
        self.validate_node(
            node,
            code,
            lambda code: ensure_type(
                parse_statement(code), cst.SimpleStatementLine
            ).body[0],
            expected_position=position,
        )
