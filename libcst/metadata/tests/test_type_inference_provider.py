# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
import os
import subprocess
import sys
from pathlib import Path
from unittest import skipIf

import libcst as cst
from libcst import MetadataWrapper
from libcst.metadata.type_inference_provider import PyreData, TypeInferenceProvider
from libcst.testing.utils import UnitTest, data_provider
from libcst.tests.test_pyre_integration import TEST_SUITE_PATH


def _test_simple_class_helper(test: UnitTest, wrapper: MetadataWrapper) -> None:
    types = wrapper.resolve(TypeInferenceProvider)
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
    test.assertEqual(types[self_number_attr], "int")

    value = assign.value
    if value:
        test.assertEqual(types[value], "int")

    # self
    test.assertEqual(types[self_number_attr.value], "simple_class.Item")
    collector_assign = cst.ensure_type(
        cst.ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Assign
    )
    collector = collector_assign.targets[0].target
    test.assertEqual(types[collector], "simple_class.ItemCollector")
    items_assign = cst.ensure_type(
        cst.ensure_type(m.body[4], cst.SimpleStatementLine).body[0], cst.AnnAssign
    )
    items = items_assign.target
    test.assertEqual(types[items], "typing.Sequence[simple_class.Item]")


@skipIf(
    sys.version_info < (3, 7), "TypeInferenceProvider doesn't support 3.6 and below"
)
@skipIf(sys.platform == "win32", "TypeInferenceProvider doesn't support windows")
class TypeInferenceProviderTest(UnitTest):
    @classmethod
    def setUpClass(cls):
        os.chdir(TEST_SUITE_PATH)
        try:
            subprocess.run(["pyre", "-n", "start", "--no-watchman"])
        except subprocess.TimeoutExpired as exc:
            raise exc

    @classmethod
    def tearDownClass(cls):
        try:
            subprocess.run(["pyre", "-n", "stop"], cwd=TEST_SUITE_PATH)
        except subprocess.TimeoutExpired as exc:
            raise exc

    @data_provider(
        ((TEST_SUITE_PATH / "simple_class.py", TEST_SUITE_PATH / "simple_class.json"),)
    )
    def test_gen_cache(self, source_path: Path, data_path: Path) -> None:
        cache = TypeInferenceProvider.gen_cache(
            root_path=source_path.parent, paths=[source_path.name], timeout=None
        )
        data: PyreData = json.loads(data_path.read_text())
        self.assertEqual(cache[source_path.name], data)

    @data_provider(
        ((TEST_SUITE_PATH / "simple_class.py", TEST_SUITE_PATH / "simple_class.json"),)
    )
    def test_simple_class_types(self, source_path: Path, data_path: Path) -> None:
        data: PyreData = json.loads(data_path.read_text())
        wrapper = MetadataWrapper(
            cst.parse_module(source_path.read_text()),
            # pyre-fixme[6]: Expected `Mapping[Type[BaseMetadataProvider[object]],
            #  Any]` for 2nd param but got `Dict[Type[TypeInferenceProvider],
            #  Sequence[InferredType]]`.
            cache={TypeInferenceProvider: data},
        )
        _test_simple_class_helper(self, wrapper)

    def test_with_empty_cache(self) -> None:
        tip = TypeInferenceProvider({})
        self.assertEqual(tip.lookup, {})

        tip = TypeInferenceProvider(PyreData())
        self.assertEqual(tip.lookup, {})
