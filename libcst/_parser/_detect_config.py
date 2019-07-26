# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import itertools
from dataclasses import dataclass
from io import BytesIO
from tokenize import detect_encoding as py_tokenize_detect_encoding
from typing import Iterable, Iterator, Union

from parso.python.token import PythonTokenTypes, TokenType
from parso.utils import split_lines

from libcst._nodes._whitespace import NEWLINE_RE
from libcst._parser._types.config import AutoConfig, ParserConfig, PartialParserConfig
from libcst._parser._types.token import Token
from libcst._parser._wrapped_tokenize import tokenize_lines


_INDENT: TokenType = PythonTokenTypes.INDENT
_FALLBACK_DEFAULT_NEWLINE = "\n"
_FALLBACK_DEFAULT_INDENT = "    "


@dataclass(frozen=True)
class ConfigDetectionResult:
    # The config is a set of constant values used by the parser.
    config: ParserConfig
    # The tokens iterator is mutated by the parser.
    tokens: Iterator[Token]


def _detect_encoding(source: Union[str, bytes]) -> str:
    """
    Detects the encoding from the presence of a UTF-8 BOM or an encoding cookie as
    specified in PEP 263.

    If given a string (instead of bytes) the encoding is assumed to be utf-8.
    """

    if isinstance(source, str):
        return "utf-8"
    return py_tokenize_detect_encoding(BytesIO(source).readline)[0]


def _detect_default_newline(source_str: str) -> str:
    """
    Finds the first newline, and uses that value as the default newline.
    """
    # Don't use `NEWLINE_RE` for this, because it might match multiple newlines as a
    # single newline.
    match = NEWLINE_RE.search(source_str)
    return match.group(0) if match is not None else _FALLBACK_DEFAULT_NEWLINE


def _detect_indent(tokens: Iterable[Token]) -> str:
    """
    Finds the first INDENT token, and uses that as the value of the default indent.
    """
    try:
        first_indent = next(t for t in tokens if t.type is _INDENT)
    except StopIteration:
        return _FALLBACK_DEFAULT_INDENT
    first_indent_str = first_indent.relative_indent
    assert first_indent_str is not None, "INDENT tokens must contain a relative_indent"
    return first_indent_str


def detect_config(
    source: Union[str, bytes],
    *,
    partial: PartialParserConfig,
    detect_trailing_newline: bool,
) -> ConfigDetectionResult:
    """
    Computes a ParserConfig given the current source code to be parsed and a partial
    config.
    """

    python_version = partial.parsed_python_version

    partial_encoding = partial.encoding
    encoding = (
        _detect_encoding(source)
        if isinstance(partial_encoding, AutoConfig)
        else partial_encoding
    )

    source_str = source if isinstance(source, str) else source.decode(encoding)

    partial_default_newline = partial.default_newline
    default_newline = (
        _detect_default_newline(source_str)
        if isinstance(partial_default_newline, AutoConfig)
        else partial_default_newline
    )

    # HACK: The grammar requires a trailing newline, but python doesn't actually require
    # a trailing newline. Add one onto the end to make the parser happy. We'll strip it
    # out again during cst.Module's codegen.
    #
    # I think parso relies on error recovery support to handle this, which we don't
    # have. lib2to3 doesn't handle this case at all AFAICT.
    has_trailing_newline = detect_trailing_newline and bool(
        len(source_str) != 0 and NEWLINE_RE.match(source_str[-1])
    )
    if detect_trailing_newline and not has_trailing_newline:
        source_str += default_newline

    lines = split_lines(source_str, keepends=True)

    tokens = tokenize_lines(lines, python_version)

    partial_default_indent = partial.default_indent
    if isinstance(partial_default_indent, AutoConfig):
        # We need to clone `tokens` before passing it to `_detect_indent`, because
        # `_detect_indent` consumes some tokens, mutating `tokens`.
        #
        # Implementation detail: CPython's `itertools.tee` uses weakrefs to reduce the
        # size of its FIFO, so this doesn't retain items (leak memory) for `tokens_dup`
        # once `token_dup` is freed at the end of this method (subject to
        # GC/refcounting).
        tokens, tokens_dup = itertools.tee(tokens)
        default_indent = _detect_indent(tokens_dup)
    else:
        default_indent = partial_default_indent

    return ConfigDetectionResult(
        config=ParserConfig(
            lines=lines,
            encoding=encoding,
            default_indent=default_indent,
            default_newline=default_newline,
            has_trailing_newline=has_trailing_newline,
        ),
        tokens=tokens,
    )
