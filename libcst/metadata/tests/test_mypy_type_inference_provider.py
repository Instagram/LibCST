# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import libcst as cst
from libcst import MetadataWrapper
from libcst.metadata.mypy_type_inference_provider import MypyTypeInferenceProvider
from libcst.testing.utils import data_provider, UnitTest
from libcst.tests.test_pyre_integration import TEST_SUITE_PATH


def _test_simple_class_helper(test: UnitTest, wrapper: MetadataWrapper) -> None:
    mypy_nodes = wrapper.resolve(MypyTypeInferenceProvider)
    m = wrapper.module
    assign = cst.ensure_type(
        cst.ensure_type(
            cst.ensure_type(
                cst.ensure_type(m.body[1].body, cst.IndentedBlock).body[0],
                cst.FunctionDef,
            ).body.body[0],
            cst.SimpleStatementLine,
        ).body[0],
        cst.AnnAssign,
    )
    self_number_attr = cst.ensure_type(assign.target, cst.Attribute)
    test.assertEqual(str(mypy_nodes[self_number_attr]), "builtins.int")

    # self
    test.assertEqual(
        str(mypy_nodes[self_number_attr.value]), "libcst.tests.pyre.simple_class.Item"
    )
    collector_assign = cst.ensure_type(
        cst.ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Assign
    )
    collector = collector_assign.targets[0].target
    test.assertEqual(
        str(mypy_nodes[collector]), "libcst.tests.pyre.simple_class.ItemCollector"
    )
    items_assign = cst.ensure_type(
        cst.ensure_type(m.body[4], cst.SimpleStatementLine).body[0], cst.AnnAssign
    )
    items = items_assign.target
    test.assertEqual(
        str(mypy_nodes[items]), "typing.Sequence[libcst.tests.pyre.simple_class.Item]"
    )


class MypyTypeInferenceProviderTest(UnitTest):
    @data_provider(
        ((TEST_SUITE_PATH / "simple_class.py", TEST_SUITE_PATH / "simple_class.json"),)
    )
    def test_simple_class_types(self, source_path: Path, data_path: Path) -> None:
        file = str(source_path)
        repo_root = Path(__file__).parents[3]
        cache = MypyTypeInferenceProvider.gen_cache(repo_root, [file])
        wrapper = MetadataWrapper(
            cst.parse_module(source_path.read_text()),
            cache={MypyTypeInferenceProvider: cache[file]},
        )
        _test_simple_class_helper(self, wrapper)
