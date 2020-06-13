# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from itertools import islice
from typing import List

import libcst


def insert_header_comments(node: libcst.Module, comments: List[str]) -> libcst.Module:
    """
    Insert comments after last non-empty line in header. Use this to insert one or more
    comments after any copyright preamble in a :class:`~libcst.Module`. Each comment in
    the list of ``comments`` must start with a ``#`` and will be placed on its own line
    in the appropriate location.
    """
    # Split the lines up into a contiguous comment-containing section and
    # the empty whitespace section that follows
    last_comment_index = -1
    for i, line in enumerate(node.header):
        if line.comment is not None:
            last_comment_index = i

    comment_lines = islice(node.header, last_comment_index + 1)
    empty_lines = islice(node.header, last_comment_index + 1, None)
    inserted_lines = [
        libcst.EmptyLine(comment=libcst.Comment(value=comment)) for comment in comments
    ]
    return node.with_changes(header=(*comment_lines, *inserted_lines, *empty_lines))
