# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
"""
Parser entrypoints define the way users of our API are allowed to interact with the
parser. A parser entrypoint should take the source code and some configuration
information
"""

from typing import TypeVar, Union

from libcst._nodes._base import CSTNode
from libcst._nodes._expression import BaseExpression
from libcst._nodes._module import Module
from libcst._nodes._statement import BaseCompoundStatement, SimpleStatementLine
from libcst._parser._detect_config import detect_config
from libcst._parser._grammar import get_grammar, validate_grammar
from libcst._parser._python_parser import PythonCSTParser
from libcst._parser._types.config import PartialParserConfig


_CSTNodeT = TypeVar("_CSTNodeT", bound=CSTNode)
_DEFAULT_PARTIAL_PARSER_CONFIG: PartialParserConfig = PartialParserConfig()


def _parse(
    entrypoint: str,
    source: Union[str, bytes],
    config: PartialParserConfig,
    *,
    detect_trailing_newline: bool,
) -> CSTNode:
    detection_result = detect_config(
        source, partial=config, detect_trailing_newline=detect_trailing_newline
    )
    validate_grammar()
    grammar = get_grammar()

    parser = PythonCSTParser(
        tokens=detection_result.tokens,
        config=detection_result.config,
        pgen_grammar=grammar,
        start_nonterminal=entrypoint,
    )
    # The parser has an Any return type, we can at least refine it to CSTNode here.
    result = parser.parse()
    assert isinstance(result, CSTNode)
    return result


def parse_module(
    source: Union[str, bytes],  # the only entrypoint that accepts bytes
    config: PartialParserConfig = _DEFAULT_PARTIAL_PARSER_CONFIG,
) -> Module:
    result = _parse("file_input", source, config, detect_trailing_newline=True)
    assert isinstance(result, Module)
    return result


def parse_statement(
    source: str, config: PartialParserConfig = _DEFAULT_PARTIAL_PARSER_CONFIG
) -> Union[SimpleStatementLine, BaseCompoundStatement]:
    """
    Accepts a statement followed by a trailing newline. If a trailing newline is not
    provided, one will be added.

    Leading comments and trailing comments (on the same line) are accepted, but
    whitespace (or anything else) after the statement's trailing newline is not valid
    (there's nowhere to store it).
    """
    # use detect_trailing_newline to insert a newline
    result = _parse("stmt_input", source, config, detect_trailing_newline=True)
    assert isinstance(result, (SimpleStatementLine, BaseCompoundStatement))
    return result


def parse_expression(
    source: str, config: PartialParserConfig = _DEFAULT_PARTIAL_PARSER_CONFIG
) -> BaseExpression:
    result = _parse("expression_input", source, config, detect_trailing_newline=False)
    assert isinstance(result, BaseExpression)
    return result
