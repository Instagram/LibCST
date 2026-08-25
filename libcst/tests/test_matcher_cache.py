#!/usr/bin/env python
# Copyright (c) Meta Platforms, Inc. and affiliates.

import unittest

import libcst as cst
from libcst import matchers as m
from libcst.matchers._matcher_base import _get_matcher_fields


class NameVisitorMixin:
    def visit_Name(self, node: cst.Name) -> None:
        pass


class MixedVisitor(NameVisitorMixin, cst.BatchableCSTVisitor):
    def visit_Call(self, node: cst.Call) -> None:
        pass


class MatcherCacheTest(unittest.TestCase):
    def test_matcher_fields_are_cached_by_type(self) -> None:
        name_fields = _get_matcher_fields(type(m.Name("first")))

        self.assertIs(name_fields, _get_matcher_fields(type(m.Name("second"))))
        self.assertTrue(m.matches(cst.Name("value"), m.Name("value")))

    def test_visitor_discovery_includes_mixin_methods(self) -> None:
        visitor = MixedVisitor()

        self.assertEqual(
            set(visitor.get_visitors()),
            {"visit_Call", "visit_Name"},
        )
