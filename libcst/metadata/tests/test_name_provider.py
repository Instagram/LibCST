# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from textwrap import dedent
from typing import Set

import libcst as cst
from libcst.metadata.name_provider import (
    FullyQualifiedNameProvider,
    QualifiedNameProvider,
)
from libcst.metadata.scope_provider import QualifiedName, QualifiedNameSource
from libcst.testing.utils import UnitTest


def get_qualified_names(module_str: str) -> Set[QualifiedName]:
    wrapper = cst.MetadataWrapper(cst.parse_module(dedent(module_str)))
    qnames = wrapper.resolve(QualifiedNameProvider)
    return set().union(*qnames.values())


def get_fully_qualified_names(file_path: str, module_str: str) -> Set[QualifiedName]:
    wrapper = cst.MetadataWrapper(
        cst.parse_module(dedent(module_str)),
        cache={
            FullyQualifiedNameProvider: FullyQualifiedNameProvider.gen_cache(
                Path(""), [file_path], None
            ).get(file_path)
        },
    )
    return set().union(*wrapper.resolve(FullyQualifiedNameProvider).values())


class QualifiedNameProviderTest(UnitTest):
    def test_imports(self) -> None:
        qnames = get_qualified_names(
            """
            from a.b import c as d
            d
            """
        )
        self.assertEqual({"a.b.c"}, {qname.name for qname in qnames})
        for qname in qnames:
            self.assertEqual(qname.source, QualifiedNameSource.IMPORT, msg=f"{qname}")

    def test_builtins(self) -> None:
        qnames = get_qualified_names(
            """
            int(None)
            """
        )
        self.assertEqual(
            {"builtins.int", "builtins.None"}, {qname.name for qname in qnames}
        )
        for qname in qnames:
            self.assertEqual(qname.source, QualifiedNameSource.BUILTIN, msg=f"{qname}")

    def test_locals(self) -> None:
        qnames = get_qualified_names(
            """
            class X:
                a: "X"
            """
        )
        self.assertEqual({"X", "X.a"}, {qname.name for qname in qnames})
        for qname in qnames:
            self.assertEqual(qname.source, QualifiedNameSource.LOCAL, msg=f"{qname}")
