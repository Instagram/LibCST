# pyre-strict
from ast import literal_eval
from typing import Union

import libcst
import libcst.matchers as m
from libcst import parse_expression
from libcst.codemod import VisitorBasedCodemodCommand
from libcst.codemod.visitors import AddImportsVisitor


class StripStringsCommand(VisitorBasedCodemodCommand):

    DESCRIPTION: str = "Converts string type annotations to 3.7-compatible forward references."

    @m.call_if_inside(m.Annotation())
    @m.call_if_not_inside(m.Subscript(m.Name("Literal")))
    def leave_SimpleString(
        self, original_node: libcst.SimpleString, updated_node: libcst.SimpleString
    ) -> Union[libcst.SimpleString, libcst.BaseExpression]:
        AddImportsVisitor.add_needed_import(self.context, "__future__", "annotations")
        return parse_expression(
            literal_eval(updated_node.value), config=self.module.config_for_parsing
        )
