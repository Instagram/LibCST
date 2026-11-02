# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from libcst import parse_module
from libcst.codemod import CodemodContext, CodemodTest
from libcst.codemod.visitors import GatherExportsVisitor
from libcst.testing.utils import UnitTest


class TestGatherExportsVisitor(UnitTest):
    def gather_exports(self, code: str) -> GatherExportsVisitor:
        transform_instance = GatherExportsVisitor(CodemodContext())
        input_tree = parse_module(CodemodTest.make_fixture_data(code))
        input_tree.visit(transform_instance)
        return transform_instance

    def test_gather_noop(self) -> None:
        code = """
            from foo import bar

            from typing import List

            bar(["foo", "bar"])

            list_of_str = ["foo", "bar", "baz"]

            set_of_str = {"foo", "bar", "baz"}

            tuple_of_str = ("foo", "bar", "baz")

            another: List[str] = ["foobar", "foobarbaz"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, set())

    def test_gather_exports_simple(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = ["bar", "baz"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_simple2(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = ["bar"]
            __all__ += ["baz"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_simple_set(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = {"bar", "baz"}
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_simple_tuple(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = ("bar", "baz")
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_simple_annotated(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            from typing import List

            __all__: List[str] = ["bar", "baz"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_append_and_extend(self) -> None:
        cases = [
            ('__all__ = []\n__all__.append("added")', {"added"}),
            ('__all__.append("joined" "name")', {"joinedname"}),
            ('__all__.extend(["first", "second"])', {"first", "second"}),
            ('__all__.extend(("joined" "name", b"bytes"))', {"joinedname"}),
            ('__all__.append("a")\n__all__.append("a")', {"a"}),
            ('__all__.extend(["a", "a"])', {"a"}),
            (
                '__all__ = ["a"]\n__all__ += ["b"]\n__all__.append("c")\n__all__.extend(("d",))',
                {"a", "b", "c", "d"},
            ),
        ]
        for code, expected in cases:
            with self.subTest(code=code):
                gatherer = self.gather_exports(code)
                self.assertEqual(gatherer.explicit_exported_objects, expected)

    def test_gather_exports_ignores_dynamic_or_invalid_method_calls(self) -> None:
        code = """
            __all__ = []
            other.append("other")
            __all__().append("call_result")
            __all__[0].append("item")
            module.__all__.append("attribute")
            __all__.append(b"bytes")
            __all__.append(name)
            __all__.append(value="keyword")
            __all__.append("one", "two")
            __all__.append(*["starred"])
            __all__.extend(module.__all__)
            __all__.extend([name, ["nested"]])
        """
        self.assertEqual(self.gather_exports(code).explicit_exported_objects, set())

    def test_gather_exports_ignores_deferred_or_conditional_mutations(self) -> None:
        for code in [
            '__all__ = ["public"]\ndef later():\n    __all__.remove("public")',
            '__all__ = ["public"]\ndef later():\n    __all__.append("private")',
            '__all__ = ["public"]\nif condition:\n    __all__.extend(["private"])',
            '__all__ = ["public"]\nif condition:\n    __all__.remove("public")',
            '__all__ = ["public"]\nclass Other:\n    __all__ = []\n    __all__.remove("public")',
        ]:
            with self.subTest(code=code):
                self.assertEqual(
                    self.gather_exports(code).explicit_exported_objects, {"public"}
                )

    def test_gather_exports_ignore_invalid_1(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = [bar, baz]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, set())

    def test_gather_exports_ignore_invalid_2(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = ["bar", "baz", ["biz"]]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_ignore_valid_1(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = ["bar", "b""a""z"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_ignore_valid_2(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__, _ = ["bar", "baz"], ["biz"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})

    def test_gather_exports_ignore_valid_3(self) -> None:
        code = """
            from foo import bar
            from biz import baz

            __all__ = exported = ["bar", "baz"]
        """

        gatherer = self.gather_exports(code)
        self.assertEqual(gatherer.explicit_exported_objects, {"bar", "baz"})
