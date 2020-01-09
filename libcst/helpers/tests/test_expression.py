# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from typing import Optional, Union

import libcst as cst
from libcst.helpers.expression import get_full_name_for_node
from libcst.testing.utils import UnitTest, data_provider


class ExpressionTest(UnitTest):
    @data_provider(
        (
            ("a string", "a string"),
            (cst.Name("a_name"), "a_name"),
            (cst.parse_expression("a.b.c"), "a.b.c"),
            (cst.parse_expression("a.b()"), "a.b"),
            (cst.parse_expression("a.b.c[i]"), "a.b.c"),
            (cst.parse_statement("def fun():  pass"), "fun"),
            (cst.parse_statement("class cls:  pass"), "cls"),
            (cst.parse_statement("(a.b()).c()"), None),  # not a supported Node type
        )
    )
    def test_get_full_name_for_expression(
        self, input: Union[str, cst.CSTNode], output: Optional[str],
    ) -> None:
        self.assertEqual(get_full_name_for_node(input), output)
