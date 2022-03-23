# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from typing import Optional

import libcst as cst
from libcst.helpers.common import ensure_type
from libcst.helpers.module import (
    calculate_module_and_package,
    get_absolute_module_for_import,
    get_absolute_module_for_import_or_raise,
    insert_header_comments,
    ModuleNameAndPackage,
)
from libcst.testing.utils import data_provider, UnitTest


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
        node = cst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # No comment case
        original_code = "\n".join(empty_lines + non_header_line)
        expected_code = "\n".join(inserted_comments + empty_lines + non_header_line)
        node = cst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # No empty lines case
        original_code = "\n".join(comment_lines + non_header_line)
        expected_code = "\n".join(comment_lines + inserted_comments + non_header_line)
        node = cst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # Empty line between comments
        comment_lines.insert(1, " ")
        original_code = "\n".join(comment_lines + empty_lines + non_header_line)
        expected_code = "\n".join(
            comment_lines + inserted_comments + empty_lines + non_header_line
        )
        node = cst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

        # No header case
        original_code = "\n".join(non_header_line)
        expected_code = "\n".join(inserted_comments + non_header_line)
        node = cst.parse_module(original_code)
        self.assertEqual(
            insert_header_comments(node, inserted_comments).code, expected_code
        )

    @data_provider(
        (
            # Simple imports that are already absolute.
            (None, "from a.b import c", "a.b"),
            ("x.y.z", "from a.b import c", "a.b"),
            # Relative import that can't be resolved due to missing module.
            (None, "from ..w import c", None),
            # Relative import that goes past the module level.
            ("x", "from ...y import z", None),
            ("x.y.z", "from .....w import c", None),
            ("x.y.z", "from ... import c", None),
            # Correct resolution of absolute from relative modules.
            ("x.y.z", "from . import c", "x.y"),
            ("x.y.z", "from .. import c", "x"),
            ("x.y.z", "from .w import c", "x.y.w"),
            ("x.y.z", "from ..w import c", "x.w"),
            ("x.y.z", "from ...w import c", "w"),
        )
    )
    def test_get_absolute_module(
        self,
        module: Optional[str],
        importfrom: str,
        output: Optional[str],
    ) -> None:
        node = ensure_type(cst.parse_statement(importfrom), cst.SimpleStatementLine)
        assert len(node.body) == 1, "Unexpected number of statements!"
        import_node = ensure_type(node.body[0], cst.ImportFrom)

        self.assertEqual(get_absolute_module_for_import(module, import_node), output)
        if output is None:
            with self.assertRaises(Exception):
                get_absolute_module_for_import_or_raise(module, import_node)
        else:
            self.assertEqual(
                get_absolute_module_for_import_or_raise(module, import_node), output
            )

    @data_provider(
        (
            # Nodes without an asname
            (cst.ImportAlias(name=cst.Name("foo")), "foo", None),
            (
                cst.ImportAlias(name=cst.Attribute(cst.Name("foo"), cst.Name("bar"))),
                "foo.bar",
                None,
            ),
            # Nodes with an asname
            (
                cst.ImportAlias(
                    name=cst.Name("foo"), asname=cst.AsName(name=cst.Name("baz"))
                ),
                "foo",
                "baz",
            ),
            (
                cst.ImportAlias(
                    name=cst.Attribute(cst.Name("foo"), cst.Name("bar")),
                    asname=cst.AsName(name=cst.Name("baz")),
                ),
                "foo.bar",
                "baz",
            ),
        )
    )
    def test_importalias_helpers(
        self, alias_node: cst.ImportAlias, full_name: str, alias: Optional[str]
    ) -> None:
        self.assertEqual(alias_node.evaluated_name, full_name)
        self.assertEqual(alias_node.evaluated_alias, alias)

    @data_provider(
        (
            # Providing no root should give back no module.
            (None, "/some/dummy/file.py", None),
            # Providing a file outside the root should give back no module.
            ("/home/username/root", "/some/dummy/file.py", None),
            ("/home/username/root/", "/some/dummy/file.py", None),
            ("/home/username/root", "/home/username/file.py", None),
            # Various files inside the root should give back valid modules.
            (
                "/home/username/root",
                "/home/username/root/file.py",
                ModuleNameAndPackage("file", ""),
            ),
            (
                "/home/username/root/",
                "/home/username/root/file.py",
                ModuleNameAndPackage("file", ""),
            ),
            (
                "/home/username/root/",
                "/home/username/root/some/dir/file.py",
                ModuleNameAndPackage("some.dir.file", "some.dir"),
            ),
            # Various special files inside the root should give back valid modules.
            (
                "/home/username/root/",
                "/home/username/root/some/dir/__init__.py",
                ModuleNameAndPackage("some.dir", "some.dir"),
            ),
            (
                "/home/username/root/",
                "/home/username/root/some/dir/__main__.py",
                ModuleNameAndPackage("some.dir", "some.dir"),
            ),
            # some windows tests
            (
                "c:/Program Files/",
                "d:/Program Files/some/dir/file.py",
                None,
            ),
            (
                "c:/Program Files/other/",
                "c:/Program Files/some/dir/file.py",
                None,
            ),
            (
                "c:/Program Files/",
                "c:/Program Files/some/dir/file.py",
                ModuleNameAndPackage("some.dir.file", "some.dir"),
            ),
            (
                "c:/Program Files/",
                "c:/Program Files/some/dir/__main__.py",
                ModuleNameAndPackage("some.dir", "some.dir"),
            ),
        ),
    )
    def test_calculate_module_and_package(
        self,
        repo_root: Optional[str],
        filename: str,
        module_and_package: Optional[ModuleNameAndPackage],
    ) -> None:
        self.assertEqual(
            calculate_module_and_package(repo_root, filename), module_and_package
        )
