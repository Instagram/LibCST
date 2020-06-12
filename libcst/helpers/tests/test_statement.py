# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from typing import Optional

import libcst as cst
from libcst.helpers import (
    ensure_type,
    get_absolute_module_for_import,
    get_absolute_module_for_import_or_raise,
)
from libcst.testing.utils import UnitTest, data_provider


class StatementTest(UnitTest):
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
        self, module: Optional[str], importfrom: str, output: Optional[str],
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
