# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import abc
import codecs
import re
from dataclasses import dataclass, field, fields
from enum import Enum
from typing import FrozenSet, List, Pattern, Sequence, Union

from libcst._add_slots import add_slots
from libcst._nodes.whitespace import NEWLINE_RE
from libcst._parser.parso.utils import PythonVersionInfo, parse_version_string


_INDENT_RE: Pattern[str] = re.compile(r"[ \t]+")


class BaseWhitespaceParserConfig(abc.ABC):
    """
    Represents the subset of ParserConfig that the whitespace parser requires. This
    makes calling the whitespace parser in tests with a mocked configuration easier.
    """

    lines: Sequence[str]
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
    version: PythonVersionInfo
    future_imports: FrozenSet[str]


class AutoConfig(Enum):
    """
    A sentinel value used in PartialParserConfig
    """

    token: int = 0

    def __repr__(self) -> str:
        return str(self)


KNOWN_PYTHON_VERSION_STRINGS = ["3.0", "3.1", "3.3", "3.5", "3.6", "3.7", "3.8"]


@add_slots
@dataclass(frozen=True)
class PartialParserConfig:
    r"""
    An optional object that can be supplied to the parser entrypoints (e.g.
    :func:`parse_module`) to configure the parser.

    Unspecified fields will be inferred from the input source code or from the execution
    environment.

    >>> import libcst as cst
    >>> tree = cst.parse_module("abc")
    >>> tree.bytes
    b'abc'
    >>> # override the default utf-8 encoding
    ... tree = cst.parse_module("abc", cst.PartialParserConfig(encoding="utf-32"))
    >>> tree.bytes
    b'\xff\xfe\x00\x00a\x00\x00\x00b\x00\x00\x00c\x00\x00\x00'
    """

    #: The version of Python that the input source code is expected to be syntactically
    #: compatible with. This may be different from the Python interpreter being used to
    #: run LibCST. For example, you can parse code as 3.7 with a CPython 3.6
    #: interpreter.
    #:
    #: Currently, only Python 3.0, 3.1, 3.3, 3.5, 3.6, 3.7 and 3.8 syntax is supported.
    python_version: Union[str, AutoConfig] = AutoConfig.token

    #: A named tuple with the ``major`` and ``minor`` Python version numbers. This is
    #: derived from :attr:`python_version` and should not be supplied to the
    #: :class:`PartialParserConfig` constructor.
    parsed_python_version: PythonVersionInfo = field(init=False)

    #: The file's encoding format. When parsing a ``bytes`` object, this value may be
    #: inferred from the contents of the parsed source code. When parsing a ``str``,
    #: this value defaults to ``"utf-8"``.
    encoding: Union[str, AutoConfig] = AutoConfig.token

    #: Detected ``__future__`` import names
    future_imports: Union[FrozenSet[str], AutoConfig] = AutoConfig.token

    #: The indentation of the file, expressed as a series of tabs and/or spaces. This
    #: value is inferred from the contents of the parsed source code by default.
    default_indent: Union[str, AutoConfig] = AutoConfig.token

    #: The newline of the file, expressed as ``\n``, ``\r\n``, or ``\r``. This value is
    #: inferred from the contents of the parsed source code by default.
    default_newline: Union[str, AutoConfig] = AutoConfig.token

    def __post_init__(self) -> None:
        raw_python_version = self.python_version
        # `parse_version_string` will raise a ValueError if the version is invalid.
        #
        # We use object.__setattr__ because the dataclass is frozen. See:
        # https://docs.python.org/3/library/dataclasses.html#frozen-instances
        # This should be safe behavior inside of `__post_init__`.
        parsed_python_version = parse_version_string(
            None if isinstance(raw_python_version, AutoConfig) else raw_python_version
        )

        # Once we add support for more versions of Python, we can change this to detect
        # the supported version range.
        if not any(
            parsed_python_version == parse_version_string(v)
            for v in KNOWN_PYTHON_VERSION_STRINGS
        ):
            comma_versions = ", ".join(KNOWN_PYTHON_VERSION_STRINGS)
            raise ValueError(
                "LibCST can only parse code using one of the following versions of "
                + f"Python's grammar: {comma_versions}. More versions may be "
                + "supported by future releases."
            )

        object.__setattr__(self, "parsed_python_version", parsed_python_version)

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

    def __repr__(self) -> str:
        init_keys: List[str] = []

        for f in fields(self):
            # We don't display the parsed_python_version attribute because it contains
            # the same value as python_version, only parsed.
            if f.name == "parsed_python_version":
                continue
            value = getattr(self, f.name)
            if not isinstance(value, AutoConfig):
                init_keys.append(f"{f.name}={value!r}")

        return f"{self.__class__.__name__}({', '.join(init_keys)})"
