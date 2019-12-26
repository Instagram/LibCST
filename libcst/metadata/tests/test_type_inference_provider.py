# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
from pathlib import Path

import libcst as cst
from libcst import MetadataWrapper
from libcst.metadata.type_inference_provider import TypeInferenceProvider
from libcst.testing.utils import UnitTest, data_provider
from libcst.tests.test_pyre_integration import TEST_SUITE_PATH, PyreData


class TypeInferenceProviderTest(UnitTest):
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
            cache={TypeInferenceProvider: data["types"]},
        )
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
            cst.Assign,
        )
        self_number_attr = cst.ensure_type(assign.targets[0].target, cst.Attribute,)
        # TODO: uncomment when typing issue is fixed
        # self.assertEqual(types[self_number_attr], "int")

        self.assertEqual(types[assign.value], "int")

        # self
        self.assertEqual(
            types[self_number_attr.value], "libcst.tests.pyre.simple_class.Item"
        )
        collector_assign = cst.ensure_type(
            cst.ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Assign
        )
        collector = collector_assign.targets[0].target
        self.assertEqual(
            types[collector], "libcst.tests.pyre.simple_class.ItemCollector"
        )
        items_assign = cst.ensure_type(
            cst.ensure_type(m.body[4], cst.SimpleStatementLine).body[0], cst.Assign
        )
        items = items_assign.targets[0].target
        self.assertEqual(
            types[items], "typing.Sequence[libcst.tests.pyre.simple_class.Item]"
        )
