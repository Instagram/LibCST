# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Any

import libcst as cst
from libcst import CodeRange
from libcst._nodes.tests.base import CSTNodeTest
from libcst.testing.utils import data_provider


class DictTest(CSTNodeTest):
    @data_provider(
        [
            # zero-element dict
            {
                "node": cst.Dict([]),
                "code": "{}",
                "expected_position": CodeRange.create((1, 0), (1, 2)),
            },
            # one-element dict, sentinel comma value
            {
                "node": cst.Dict([cst.DictElement(cst.Name("k"), cst.Name("v"))]),
                "code": "{k: v}",
                "expected_position": CodeRange.create((1, 0), (1, 6)),
            },
            {
                "node": cst.Dict([cst.StarredDictElement(cst.Name("expanded"))]),
                "code": "{**expanded}",
                "expected_position": CodeRange.create((1, 0), (1, 12)),
            },
            # two-element dict, sentinel comma value
            {
                "node": cst.Dict(
                    [
                        cst.DictElement(cst.Name("k1"), cst.Name("v1")),
                        cst.DictElement(cst.Name("k2"), cst.Name("v2")),
                    ]
                ),
                "code": "{k1: v1, k2: v2}",
                "expected_position": CodeRange.create((1, 0), (1, 16)),
            },
            # custom whitespace between brackets
            {
                "node": cst.Dict(
                    [cst.DictElement(cst.Name("k"), cst.Name("v"))],
                    lbrace=cst.LeftCurlyBrace(
                        whitespace_after=cst.SimpleWhitespace("\t")
                    ),
                    rbrace=cst.RightCurlyBrace(
                        whitespace_before=cst.SimpleWhitespace("\t\t")
                    ),
                ),
                "code": "{\tk: v\t\t}",
                "expected_position": CodeRange.create((1, 0), (1, 9)),
            },
            # with parenthesis
            {
                "node": cst.Dict(
                    [cst.DictElement(cst.Name("k"), cst.Name("v"))],
                    lpar=[cst.LeftParen()],
                    rpar=[cst.RightParen()],
                ),
                "code": "({k: v})",
                "expected_position": CodeRange.create((1, 1), (1, 7)),
            },
            # starred element
            {
                "node": cst.Dict(
                    [
                        cst.StarredDictElement(cst.Name("one")),
                        cst.StarredDictElement(cst.Name("two")),
                    ]
                ),
                "code": "{**one, **two}",
                "expected_position": CodeRange.create((1, 0), (1, 14)),
            },
            # custom comma on DictElement
            {
                "node": cst.Dict(
                    [cst.DictElement(cst.Name("k"), cst.Name("v"), comma=cst.Comma())]
                ),
                "code": "{k: v,}",
                "expected_position": CodeRange.create((1, 0), (1, 7)),
            },
            # custom comma on StarredDictElement
            {
                "node": cst.Dict(
                    [cst.StarredDictElement(cst.Name("expanded"), comma=cst.Comma())]
                ),
                "code": "{**expanded,}",
                "expected_position": CodeRange.create((1, 0), (1, 13)),
            },
            # custom whitespace on DictElement
            {
                "node": cst.Dict(
                    [
                        cst.DictElement(
                            cst.Name("k"),
                            cst.Name("v"),
                            whitespace_before_colon=cst.SimpleWhitespace("\t"),
                            whitespace_after_colon=cst.SimpleWhitespace("\t\t"),
                        )
                    ]
                ),
                "code": "{k\t:\t\tv}",
                "expected_position": CodeRange.create((1, 0), (1, 8)),
            },
            # custom parenthesis on StarredDictElement
            {
                "node": cst.Dict(
                    [
                        cst.StarredDictElement(
                            cst.Name("abc"),
                            lpar=[cst.LeftParen()],
                            rpar=[cst.RightParen()],
                            comma=cst.Comma(),
                        )
                    ]
                ),
                "code": "{(**abc),}",
                "expected_position": CodeRange.create((1, 0), (1, 10)),
            },
            # custom whitespace on StarredDictElement
            {
                "node": cst.Dict(
                    [
                        cst.DictElement(
                            cst.Name("k"), cst.Name("v"), comma=cst.Comma()
                        ),
                        cst.StarredDictElement(
                            cst.Name("expanded"),
                            whitespace_before_value=cst.SimpleWhitespace("  "),
                            lpar=[cst.LeftParen()],
                            rpar=[cst.RightParen()],
                        ),
                    ]
                ),
                "code": "{k: v,(**  expanded)}",
                "expected_position": CodeRange.create((1, 0), (1, 21)),
            },
            # missing spaces around dict is always okay
            {
                "node": cst.GeneratorExp(
                    cst.Name("a"),
                    cst.CompFor(
                        cst.Name("b"),
                        cst.Dict([cst.DictElement(cst.Name("k"), cst.Name("v"))]),
                        ifs=[
                            cst.CompIf(
                                cst.Name("c"),
                                whitespace_before=cst.SimpleWhitespace(""),
                            )
                        ],
                        whitespace_after_in=cst.SimpleWhitespace(""),
                    ),
                ),
                "code": "(a for b in{k: v}if c)",
            },
        ]
    )
    def test_valid(self, **kwargs: Any) -> None:
        self.validate_node(**kwargs)

    @data_provider(
        [
            # unbalanced Dict
            {
                "get_node": lambda: cst.Dict([], lpar=[cst.LeftParen()]),
                "expected_re": "left paren without right paren",
            },
            # unbalanced StarredDictElement
            {
                "get_node": lambda: cst.Dict(
                    [
                        cst.StarredDictElement(
                            cst.Name("unbalanced"), lpar=[cst.LeftParen()]
                        )
                    ]
                ),
                "expected_re": "left paren without right paren",
            },
        ]
    )
    def test_invalid(self, **kwargs: Any) -> None:
        self.assert_invalid(**kwargs)
