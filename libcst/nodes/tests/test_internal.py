# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Tuple

import libcst.nodes as cst
from libcst.nodes._internal import CodegenState, CodePosition
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

    def test_context_manager(self) -> None:
        # create a dummy node
        node = cst.Pass()

        # simulate codegen behavior for the dummy node
        # generates the code " pass "
        state = CodegenState(" " * 4, "\n")
        start = (state.line, state.column)
        state.add_token(" ")
        with state.record_semantic_position(node):
            state.add_token("pass")
        state.add_token(" ")
        end = (state.line, state.column)
        state.update_position(node, CodePosition(start, end))

        # check syntactic whitespace is correctly recorded (includes whitespace)
        self.assertEqual(state.positions[node], CodePosition((1, 0), (1, 6)))
        # check semantic whitespace is correctly recorded (ignoring whitespace)
        self.assertEqual(state.semantic_positions[node], CodePosition((1, 1), (1, 5)))
