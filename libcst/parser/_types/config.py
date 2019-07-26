# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import abc
import codecs
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Pattern, Sequence, Union

from parso.utils import PythonVersionInfo, parse_version_string

from libcst._add_slots import add_slots
from libcst._nodes._whitespace import NEWLINE_RE


_INDENT_RE: Pattern[str] = re.compile(r"[ \t]+")


class BaseWhitespaceParserConfig(abc.ABC):
    """
    Represents the subset of ParserConfig that the whitespace parser requires. This
    makes calling the whitespace parser in tests with a mocked configuration easier.
    """

    # pyre-fixme[13]: Uninitialized attribute
    lines: Sequence[str]
    # pyre-fixme[13]: Uninitialized attribute
    default_newline: str


@add_slots  # We'll access these properties frequently, so use slots
@dataclass(frozen=True)
class ParserConfig(BaseWhitespaceParserConfig):
    """
    An internal configuration object that the python parser passes around. These values
    are global to the parsed code and should not change during the lifetime of the
    parser object.
    """

    lines: Sequence[str]
    encoding: str
    default_indent: str
    default_newline: str
    has_trailing_newline: bool


class AutoConfig(Enum):
    """
    A sentinel value used in PartialParserConfig
    """

    token: int = 0


@dataclass(frozen=True)
class PartialParserConfig:
    """
    An optional object that can be supplied to the parser entrypoints (e.g.
    `parse_module`) to configure the parser.

    Unspecified fields will be inferred from the input source code or from the execution
    environment (the current Python version).
    """

    # `python_version` only configures the tokenization/lexer right now. The grammar
    # isn't currently versioned.
    python_version: Union[str, AutoConfig] = AutoConfig.token
    # parsed_python_version is derived from python_version in __post_init__
    parsed_python_version: PythonVersionInfo = field(init=False)
    encoding: Union[str, AutoConfig] = AutoConfig.token
    default_indent: Union[str, AutoConfig] = AutoConfig.token
    default_newline: Union[str, AutoConfig] = AutoConfig.token

    def __post_init__(self) -> None:
        raw_python_version = self.python_version
        # `parse_version_string` will raise a ValueError if the version is invalid.
        #
        # We use object.__setattr__ because the dataclass is frozen. See:
        # https://docs.python.org/3/library/dataclasses.html#frozen-instances
        # This should be safe behavior inside of `__post_init__`.
        object.__setattr__(
            self,
            "parsed_python_version",
            parse_version_string(
                None  # parso will derive the version from `sys.version_info`
                if isinstance(raw_python_version, AutoConfig)
                else raw_python_version
            ),
        )

        encoding = self.encoding
        if not isinstance(encoding, AutoConfig):
            try:
                codecs.lookup(encoding)
            except LookupError:
                raise ValueError(f"{repr(encoding)} is not a supported encoding")

        newline = self.default_newline
        if (
            not isinstance(newline, AutoConfig)
            and NEWLINE_RE.fullmatch(newline) is None
        ):
            raise ValueError(
                f"Got an invalid value for default_newline: {repr(newline)}"
            )

        indent = self.default_indent
        if not isinstance(indent, AutoConfig) and _INDENT_RE.fullmatch(indent) is None:
            raise ValueError(f"Got an invalid value for default_indent: {repr(indent)}")
