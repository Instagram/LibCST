# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Tuple

import libcst as cst
from libcst._nodes.internal import (
    CodegenState,
    CodePosition,
    CodeRange,
    PositionProvidingCodegenState,
    WhitespaceInclusivePositionProvidingCodegenState,
)
from libcst.metadata.position_provider import (
    PositionProvider,
    WhitespaceInclusivePositionProvider,
)
from libcst.testing.utils import UnitTest


def position(state: CodegenState) -> Tuple[int, int]:
    return state.line, state.column


class InternalTest(UnitTest):
    def test_codegen_initial_position(self) -> None:
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
        self.assertEqual(position(state), (1, 0))

    def test_codegen_add_token(self) -> None:
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
        state.add_token("1234")
        self.assertEqual(position(state), (1, 4))

    def test_codegen_add_tokens(self) -> None:
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
        state.add_token("1234\n1234")
        self.assertEqual(position(state), (2, 4))

    def test_codegen_add_newline(self) -> None:
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
        state.add_token("\n")
        self.assertEqual(position(state), (2, 0))

    def test_codegen_add_indent_tokens(self) -> None:
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
        state.increase_indent(state.default_indent)
        state.add_indent_tokens()
        self.assertEqual(position(state), (1, 4))

    def test_codegen_decrease_indent(self) -> None:
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
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
        state = WhitespaceInclusivePositionProvidingCodegenState(
            " " * 4, "\n", WhitespaceInclusivePositionProvider()
        )
        start = CodePosition(state.line, state.column)
        state.add_token(" ")
        with state.record_syntactic_position(node):
            state.add_token("pass")
        state.add_token(" ")
        end = CodePosition(state.line, state.column)
        state.record_position(node, CodeRange(start, end))

        # check whitespace is correctly recorded
        self.assertEqual(state.provider._computed[node], CodeRange((1, 0), (1, 6)))

    def test_syntactic_position(self) -> None:
        # create a dummy node
        node = cst.Pass()

        # simulate codegen behavior for the dummy node
        # generates the code " pass "
        state = PositionProvidingCodegenState(" " * 4, "\n", PositionProvider())
        start = CodePosition(state.line, state.column)
        state.add_token(" ")
        with state.record_syntactic_position(node):
            state.add_token("pass")
        state.add_token(" ")
        end = CodePosition(state.line, state.column)
        state.record_position(node, CodeRange(start, end))

        # check syntactic position ignores whitespace
        self.assertEqual(state.provider._computed[node], CodeRange((1, 1), (1, 5)))
