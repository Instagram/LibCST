
import itertools
import re
from dataclasses import dataclass
from io import BytesIO
from tokenize import detect_encoding as py_tokenize_detect_encoding
from typing import FrozenSet, Iterable, Iterator, Pattern, Set, Tuple, Union

from libcst._parser.types.config import AutoConfig, ParserConfig, PartialParserConfig


def _detect_encoding(source: Union[str, bytes]) -> str:
    """
    Detects the encoding from the presence of a UTF-8 BOM or an encoding cookie as
    specified in PEP 263.

    If given a string (instead of bytes) the encoding is assumed to be utf-8.
    """

    if isinstance(source, str):
        return "utf-8"
    return py_tokenize_detect_encoding(BytesIO(source).readline)[0]


def convert_to_utf8(
    source: Union[str, bytes], *, partial: PartialParserConfig
) -> Tuple[str, str]:
    """
    Returns an (original encoding, converted source) tuple.
    """
    partial_encoding = partial.encoding
    encoding = (
        _detect_encoding(source)
        if isinstance(partial_encoding, AutoConfig)
        else partial_encoding
    )

    source_str = source if isinstance(source, str) else source.decode(encoding)
    return (encoding, source_str)

