# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
import libcst
from libcst.helpers import insert_header_comments
from libcst.testing.utils import UnitTest


class ModuleTest(UnitTest):
    def test_insert_header_comments(self) -> None:
        inserted_comments = ["# INSERT ME", "# AND ME"]
        comment_lines = ["# First comment", "# Another one", "# comment 3"]
        empty_lines = [" ", ""]
        non_header_line = ["SOME_VARIABLE = 0"]
        original_code = "\n".join(comment_lines + empty_lines + non_header_line)
        expected_code = "\n".join(
            comment_lines + inserted_comments + empty_lines + non_header_line
        )
        node = libcst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # No comment case
        original_code = "\n".join(empty_lines + non_header_line)
        expected_code = "\n".join(inserted_comments + empty_lines + non_header_line)
        node = libcst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # No empty lines case
        original_code = "\n".join(comment_lines + non_header_line)
        expected_code = "\n".join(comment_lines + inserted_comments + non_header_line)
        node = libcst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # Empty line between comments
        comment_lines.insert(1, " ")
        original_code = "\n".join(comment_lines + empty_lines + non_header_line)
        expected_code = "\n".join(
            comment_lines + inserted_comments + empty_lines + non_header_line
        )
        node = libcst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # No header case
        original_code = "\n".join(non_header_line)
        expected_code = "\n".join(inserted_comments + non_header_line)
        node = libcst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )
