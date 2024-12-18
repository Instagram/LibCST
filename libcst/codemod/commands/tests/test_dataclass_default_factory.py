# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
# pyre-strict

from libcst.codemod import CodemodTest
from libcst.codemod.commands.dataclass_default_factory import (
    DataclassDefaultFactoryCodemod,
)


class TestDataclassCommand(CodemodTest):
    TRANSFORM = DataclassDefaultFactoryCodemod

    def test_simple_immutable(self) -> None:
        before = """
        from dataclasses import dataclass
        @dataclass
        class Foo:
            x: int = 1
            y: bool = False
            z: str = "foo"
        """
        self.assertCodemod(before, before)

    def test_simple_mutable(self) -> None:
        before = """
        from dataclasses import dataclass
        @dataclass
        class Foo:
            x: list[int] = []
            y: foo = bar()
        """
        after = """
        from dataclasses import field, dataclass
        @dataclass
        class Foo:
            x: list[int] = field(default_factory = lambda: [])
            y: foo = field(default_factory = lambda: bar())
        """
        self.assertCodemod(before, after)

    def test_idempotent(self) -> None:
        before = """
        from dataclasses import dataclass, field
        @dataclass
        class Foo:
            x: list[int] = field(default_factory=lambda: [])
            y: list[int] = field(repr=False)
        """
        self.assertCodemod(before, before)

    def test_field_with_default(self) -> None:
        before = """
        from dataclasses import dataclass, field
        @dataclass
        class Foo:
            x: list[int] = field(default=[])
            y: bool = field(default=True)
        """
        after = """
        from dataclasses import dataclass, field
        @dataclass
        class Foo:
            x: list[int] = field(default_factory = lambda: [])
            y: bool = field(default=True)
        """
        self.assertCodemod(before, after)
