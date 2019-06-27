# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Tuple

from libcst.nodes._internal import CodegenState
from libcst.testing.utils import UnitTest


def position(state: CodegenState) -> Tuple[int, int]:
    return state.line, state.column


class InternalTest(UnitTest):
    def test_codegen_initial_position(self) -> None:
        state = CodegenState(" " * 4, "\n")
        self.assertEqual(position(state), (1, 0))

    def test_codegen_add_token(self) -> None:
        state = CodegenState(" " * 4, "\n")
        state.add_token("1234")
        self.assertEqual(position(state), (1, 4))

    def test_codegen_add_tokens(self) -> None:
        state = CodegenState(" " * 4, "\n")
        state.add_token("1234\n1234")
        self.assertEqual(position(state), (2, 4))

    def test_codegen_add_newline(self) -> None:
        state = CodegenState(" " * 4, "\n")
        state.add_token("\n")
        self.assertEqual(position(state), (2, 0))

    def test_codegen_add_indent_tokens(self) -> None:
        state = CodegenState(" " * 4, "\n")
        state.increase_indent(state.default_indent)
        state.add_indent_tokens()
        self.assertEqual(position(state), (1, 4))

    def test_codegen_decrease_indent(self) -> None:
        state = CodegenState(" " * 4, "\n")
        state.increase_indent(state.default_indent)
        state.increase_indent(state.default_indent)
        state.increase_indent(state.default_indent)
        state.decrease_indent()
        state.add_indent_tokens()
        self.assertEqual(position(state), (1, 8))
