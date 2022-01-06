# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass, replace
from typing import Optional

from libcst.helpers import get_absolute_module


@dataclass(frozen=True)
class Import:
    """Representation of individual import items for codemods."""

    module_name: str
    obj: Optional[str] = None
    alias: Optional[str] = None
    relative: int = 0

    def __post_init__(self):
        if self.module_name.startswith("."):
            mod = self.module_name.lstrip(".")
            rel = self.relative + len(self.module_name) - len(mod)
            object.__setattr__(self, "module_name", mod)
            object.__setattr__(self, "relative", rel)

    @property
    def module(self) -> str:
        return "." * self.relative + self.module_name

    def resolve_relative(self, base_module: Optional[str]) -> "Import":
        """Return an Import with an absolute module name if possible."""
        if base_module is None:
            return self
        m = get_absolute_module(base_module, self.module_name, self.relative)
        return self if m is None else replace(self, module_name=m, relative=0)
