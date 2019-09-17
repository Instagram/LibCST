# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Callable, Sequence

from libcst._parser.types.config import ParserConfig
from libcst._parser.types.token import Token


NonterminalConversion = Callable[[ParserConfig, Sequence[Any]], Any]
TerminalConversion = Callable[[ParserConfig, Token], Any]
