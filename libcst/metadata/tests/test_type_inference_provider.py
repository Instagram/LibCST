from textwrap import dedent

import libcst as cst
from libcst import MetadataWrapper
from libcst.metadata.type_inference_provider import TypeInferenceProvider
from libcst.testing.utils import UnitTest


class TypeInferenceProviderTest(UnitTest):
    def test_basic_class_types(self) -> None:
        wrapper = MetadataWrapper(
            cst.parse_module(
                dedent(
                    """\
                    from typing import Sequence


                    class Item:
                        def __init__(self, n: int):
                            self.number = n


                    class ItemCollector:
                        def get_items(self, n: int) -> Sequence[Item]:
                            return [Item() for i in range(n)]


                    collector = ItemCollector()
                    items = collector.get_items()
                    for item in items:
                        item.number
                    """
                )
            ),
            # pyre-fixme[6]: Expected `Mapping[Type[BaseMetadataProvider[object]],
            #  object]` for 2nd param but got `Dict[Type[TypeInferenceProvider],
            #  List[Dict[str, Union[Dict[str, Dict[str, int]], str]]]]`.
            cache={
                TypeInferenceProvider: [
                    {
                        "location": {
                            "start": {"line": 6, "column": 8},
                            "stop": {"line": 6, "column": 19},
                        },
                        "annotation": "int",
                    },
                    {
                        "location": {
                            "start": {"line": 6, "column": 8},
                            "stop": {"line": 6, "column": 12},
                        },
                        "annotation": "libcst.metadata.example_type_infer.Item",
                    },
                    {
                        "location": {
                            "start": {"line": 15, "column": 8},
                            "stop": {"line": 15, "column": 17},
                        },
                        "annotation": "libcst.metadata.example_type_infer.ItemCollector",
                    },
                    {
                        "location": {
                            "start": {"line": 10, "column": 27},
                            "stop": {"line": 10, "column": 30},
                        },
                        "annotation": "typing.Type[int]",
                    },
                    {
                        "location": {
                            "start": {"line": 10, "column": 35},
                            "stop": {"line": 10, "column": 49},
                        },
                        "annotation": "typing.Type[typing.Sequence[libcst.metadata.example_type_infer.Item]]",
                    },
                    {
                        "location": {
                            "start": {"line": 11, "column": 15},
                            "stop": {"line": 11, "column": 41},
                        },
                        "annotation": "typing.List[libcst.metadata.example_type_infer.Item]",
                    },
                    {
                        "location": {
                            "start": {"line": 11, "column": 38},
                            "stop": {"line": 11, "column": 39},
                        },
                        "annotation": "int",
                    },
                    {
                        "location": {
                            "start": {"line": 10, "column": 44},
                            "stop": {"line": 10, "column": 48},
                        },
                        "annotation": "typing.Type[libcst.metadata.example_type_infer.Item]",
                    },
                    {
                        "location": {
                            "start": {"line": 10, "column": 18},
                            "stop": {"line": 10, "column": 22},
                        },
                        "annotation": "libcst.metadata.example_type_infer.ItemCollector",
                    },
                    {
                        "location": {
                            "start": {"line": 15, "column": 0},
                            "stop": {"line": 15, "column": 5},
                        },
                        "annotation": "typing.Sequence[libcst.metadata.example_type_infer.Item]",
                    },
                    {
                        "location": {
                            "start": {"line": 11, "column": 16},
                            "stop": {"line": 11, "column": 20},
                        },
                        "annotation": "typing.Type[libcst.metadata.example_type_infer.Item]",
                    },
                    {
                        "location": {
                            "start": {"line": 14, "column": 12},
                            "stop": {"line": 14, "column": 27},
                        },
                        "annotation": "libcst.metadata.example_type_infer.ItemCollector",
                    },
                    {
                        "location": {
                            "start": {"line": 14, "column": 12},
                            "stop": {"line": 14, "column": 25},
                        },
                        "annotation": "typing.Type[libcst.metadata.example_type_infer.ItemCollector]",
                    },
                    {
                        "location": {
                            "start": {"line": 5, "column": 23},
                            "stop": {"line": 5, "column": 24},
                        },
                        "annotation": "int",
                    },
                    {
                        "location": {
                            "start": {"line": 5, "column": 26},
                            "stop": {"line": 5, "column": 29},
                        },
                        "annotation": "typing.Type[int]",
                    },
                    {
                        "location": {
                            "start": {"line": 11, "column": 16},
                            "stop": {"line": 11, "column": 22},
                        },
                        "annotation": "libcst.metadata.example_type_infer.Item",
                    },
                    {
                        "location": {
                            "start": {"line": 15, "column": 8},
                            "stop": {"line": 15, "column": 29},
                        },
                        "annotation": "typing.Sequence[libcst.metadata.example_type_infer.Item]",
                    },
                    {
                        "location": {
                            "start": {"line": 11, "column": 32},
                            "stop": {"line": 11, "column": 37},
                        },
                        "annotation": "typing.Type[range]",
                    },
                    {
                        "location": {
                            "start": {"line": 10, "column": 35},
                            "stop": {"line": 10, "column": 43},
                        },
                        "annotation": "typing.Callable(typing.GenericMeta.__getitem__)[[typing.Type[Variable[typing._T_co](covariant)]], typing.Type[typing.Sequence[Variable[typing._T_co](covariant)]]]",
                    },
                    {
                        "location": {
                            "start": {"line": 5, "column": 17},
                            "stop": {"line": 5, "column": 21},
                        },
                        "annotation": "libcst.metadata.example_type_infer.Item",
                    },
                    {
                        "location": {
                            "start": {"line": 16, "column": 4},
                            "stop": {"line": 16, "column": 8},
                        },
                        "annotation": "libcst.metadata.example_type_infer.Item",
                    },
                    {
                        "location": {
                            "start": {"line": 11, "column": 32},
                            "stop": {"line": 11, "column": 40},
                        },
                        "annotation": "range",
                    },
                    {
                        "location": {
                            "start": {"line": 15, "column": 8},
                            "stop": {"line": 15, "column": 27},
                        },
                        "annotation": "typing.Callable(libcst.metadata.example_type_infer.ItemCollector.get_items)[[Named(n, int)], typing.Sequence[libcst.metadata.example_type_infer.Item]]",
                    },
                    {
                        "location": {
                            "start": {"line": 10, "column": 24},
                            "stop": {"line": 10, "column": 25},
                        },
                        "annotation": "int",
                    },
                    {
                        "location": {
                            "start": {"line": 6, "column": 22},
                            "stop": {"line": 6, "column": 23},
                        },
                        "annotation": "int",
                    },
                    {
                        "location": {
                            "start": {"line": 16, "column": 12},
                            "stop": {"line": 16, "column": 17},
                        },
                        "annotation": "typing.Sequence[libcst.metadata.example_type_infer.Item]",
                    },
                    {
                        "location": {
                            "start": {"line": 17, "column": 4},
                            "stop": {"line": 17, "column": 8},
                        },
                        "annotation": "libcst.metadata.example_type_infer.Item",
                    },
                    {
                        "location": {
                            "start": {"line": 14, "column": 0},
                            "stop": {"line": 14, "column": 9},
                        },
                        "annotation": "libcst.metadata.example_type_infer.ItemCollector",
                    },
                ]
            },
        )
        types = wrapper.resolve(TypeInferenceProvider)
        m = wrapper.module
        self_number_attr = cst.ensure_type(
            cst.ensure_type(
                cst.ensure_type(
                    cst.ensure_type(
                        cst.ensure_type(m.body[1].body, cst.IndentedBlock).body[0],
                        cst.FunctionDef,
                    ).body.body[0],
                    cst.SimpleStatementLine,
                ).body[0],
                cst.Assign,
            )
            .targets[0]
            .target,
            cst.Attribute,
        )
        self.assertEqual(types[self_number_attr], "int")
        self.assertEqual(
            types[self_number_attr.value], "libcst.metadata.example_type_infer.Item"
        )
        collector_assign = cst.ensure_type(
            cst.ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Assign
        )
        collector = collector_assign.targets[0].target
        self.assertEqual(
            types[collector], "libcst.metadata.example_type_infer.ItemCollector"
        )
        items_assign = cst.ensure_type(
            cst.ensure_type(m.body[4], cst.SimpleStatementLine).body[0], cst.Assign
        )
        items = items_assign.targets[0].target
        self.assertEqual(
            types[items], "typing.Sequence[libcst.metadata.example_type_infer.Item]"
        )
