# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Tuple

import libcst as cst
from libcst.metadata.position_provider import (
    BasicPositionProvider,
    SyntacticPositionProvider,
)
from libcst.nodes._internal import (
    CodegenState,
    CodePosition,
    CodeRange,
    SyntacticCodegenState,
)
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

    def test_position(self) -> None:
        # create a dummy node
        node = cst.Pass()

        # simulate codegen behavior for the dummy node
        # generates the code " pass "
        state = CodegenState(" " * 4, "\n")
        start = CodePosition(state.line, state.column)
        state.add_token(" ")
        with state.record_syntactic_position(node):
            state.add_token("pass")
        state.add_token(" ")
        end = CodePosition(state.line, state.column)
        state.record_position(node, CodeRange(start, end))

        # check syntactic whitespace is correctly recorded
        self.assertEqual(
            node._metadata[BasicPositionProvider], CodeRange.create((1, 0), (1, 6))
        )

    def test_semantic_position(self) -> None:
        # create a dummy node
        node = cst.Pass()

        # simulate codegen behavior for the dummy node
        # generates the code " pass "
        state = SyntacticCodegenState(" " * 4, "\n")
        start = CodePosition(state.line, state.column)
        state.add_token(" ")
        with state.record_syntactic_position(node):
            state.add_token("pass")
        state.add_token(" ")
        end = CodePosition(state.line, state.column)
        state.record_position(node, CodeRange(start, end))

        # check semantic whitespace is correctly recorded (ignoring whitespace)
        self.assertEqual(
            node._metadata[SyntacticPositionProvider], CodeRange.create((1, 1), (1, 5))
        )
