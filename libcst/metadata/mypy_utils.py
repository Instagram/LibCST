# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass, field
from typing import Dict, Optional, Union

import mypy.build
import mypy.main
import mypy.modulefinder
import mypy.nodes
import mypy.options
import mypy.patterns
import mypy.traverser
import mypy.types
import mypy.typetraverser

from libcst._add_slots import add_slots
from libcst._position import CodePosition, CodeRange


@add_slots
@dataclass(frozen=True)
class MypyTypeInferenceProviderCache:
    module_name: str
    mypy_file: mypy.nodes.MypyFile


@add_slots
@dataclass(frozen=True)
class MypyType:
    is_type_constructor: bool
    mypy_type: Union[mypy.types.Type, mypy.nodes.TypeInfo]
    fullname: str = field(init=False)

    def __post_init__(self) -> None:
        if isinstance(self.mypy_type, mypy.types.Type):
            fullname = str(self.mypy_type)
        else:
            fullname = self.mypy_type.fullname
        if self.is_type_constructor:
            fullname = f"typing.Type[{fullname}]"
        object.__setattr__(self, "fullname", fullname)

    def __str__(self) -> str:
        return self.fullname


class CodeRangeToMypyNodesBinder(
    mypy.traverser.TraverserVisitor, mypy.typetraverser.TypeTraverserVisitor
):
    def __init__(self, module_name: str) -> None:
        super().__init__()
        self.locations: Dict[CodeRange, MypyType] = {}
        self.in_type_alias_expr = False
        self.module_name = module_name

    # Helpers

    @staticmethod
    def get_code_range(o: mypy.nodes.Context) -> CodeRange:
        return CodeRange(
            start=CodePosition(o.line, o.column),
            end=CodePosition(o.end_line, o.end_column),
        )

    @staticmethod
    def check_bounds(o: mypy.nodes.Context) -> bool:
        return (
            (o.line is not None)
            and (o.line >= 1)
            and (o.column is not None)
            and (o.column >= 0)
            and (o.end_line is not None)
            and (o.end_line >= 1)
            and (o.end_column is not None)
            and (o.end_column >= 0)
        )

    def record_type_location_using_code_range(
        self,
        code_range: CodeRange,
        t: Optional[Union[mypy.types.Type, mypy.nodes.TypeInfo]],
        is_type_constructor: bool,
    ) -> None:
        if t is not None:
            self.locations[code_range] = MypyType(
                is_type_constructor=is_type_constructor, mypy_type=t
            )

    def record_type_location(
        self,
        o: mypy.nodes.Context,
        t: Optional[Union[mypy.types.Type, mypy.nodes.TypeInfo]],
        is_type_constructor: bool,
    ) -> None:
        if self.check_bounds(o):
            self.record_type_location_using_code_range(
                code_range=self.get_code_range(o),
                t=t,
                is_type_constructor=is_type_constructor,
            )

    def record_location_by_name_expr(
        self, code_range: CodeRange, o: mypy.nodes.NameExpr, is_type_constructor: bool
    ) -> None:
        if isinstance(o.node, mypy.nodes.Var):
            self.record_type_location_using_code_range(
                code_range=code_range, t=o.node.type, is_type_constructor=False
            )
        elif isinstance(o.node, mypy.nodes.TypeInfo):
            self.record_type_location_using_code_range(
                code_range=code_range, t=o.node, is_type_constructor=is_type_constructor
            )

    # Actual visitors

    def visit_var(self, o: mypy.nodes.Var) -> None:
        super().visit_var(o)
        self.record_type_location(o=o, t=o.type, is_type_constructor=False)

    def visit_name_expr(self, o: mypy.nodes.NameExpr) -> None:
        super().visit_name_expr(o)
        # Implementation in base classes is omitted, record it if it is variable or class
        self.record_location_by_name_expr(
            self.get_code_range(o), o, is_type_constructor=True
        )

    def visit_member_expr(self, o: mypy.nodes.MemberExpr) -> None:
        super().visit_member_expr(o)
        # Implementation in base classes is omitted, record it
        # o.def_var should not be None after mypy run, checking here just to be sure
        if o.def_var is not None:
            self.record_type_location(o=o, t=o.def_var.type, is_type_constructor=False)

    def visit_call_expr(self, o: mypy.nodes.CallExpr) -> None:
        super().visit_call_expr(o)
        if isinstance(o.callee, mypy.nodes.NameExpr):
            self.record_location_by_name_expr(
                code_range=self.get_code_range(o), o=o.callee, is_type_constructor=False
            )

    def visit_instance(self, o: mypy.types.Instance) -> None:
        super().visit_instance(o)
        self.record_type_location(o=o, t=o, is_type_constructor=False)
