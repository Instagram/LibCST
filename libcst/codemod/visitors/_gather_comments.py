# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
from typing import Dict, Union

import libcst as cst
import libcst.matchers as m
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareVisitor
from libcst.metadata import PositionProvider


class GatherCommentsVisitor(ContextAwareVisitor):
    METADATA_DEPENDENCIES = (PositionProvider,)

    def __init__(self, context: CodemodContext, comment_regex: str) -> None:
        super().__init__(context)

        self.comments: Dict[int, cst.Comment] = {}

        self._comment_matcher = re.compile(comment_regex)

    @m.visit(m.EmptyLine(comment=m.DoesNotMatch(None)))
    @m.visit(m.TrailingWhitespace(comment=m.DoesNotMatch(None)))
    def visit_comment(self, node: Union[cst.EmptyLine, cst.TrailingWhitespace]) -> None:
        assert node.comment is not None  # hello, type checker
        if not self._comment_matcher.match(node.comment.value):
            return
        line = self.get_metadata(PositionProvider, node.comment).start.line
        if isinstance(node, cst.EmptyLine):
            # Standalone comments refer to the next line
            line += 1
        self.comments[line] = node.comment
