# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Optional
import libcst

def parse_module(source: str, encoding: Optional[str]) -> libcst.Module: ...
def parse_expression(source: str) -> libcst.BaseExpression: ...
def parse_statement(source: str) -> libcst.BaseStatement: ...
