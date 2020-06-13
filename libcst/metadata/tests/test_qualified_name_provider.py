# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from textwrap import dedent
from typing import Collection, Mapping, Optional, Tuple

import libcst as cst
from libcst import ensure_type
from libcst.metadata import (
    MetadataWrapper,
    QualifiedName,
    QualifiedNameProvider,
    QualifiedNameSource,
)
from libcst.testing.utils import UnitTest


def get_qualified_name_metadata_provider(
    module_str: str,
) -> Tuple[cst.Module, Mapping[cst.CSTNode, Collection[QualifiedName]]]:
    wrapper = MetadataWrapper(cst.parse_module(dedent(module_str)))
    return wrapper.module, wrapper.resolve(QualifiedNameProvider)


class ScopeProviderTest(UnitTest):
    def test_simple_qualified_names(self) -> None:
        m, names = get_qualified_name_metadata_provider(
            """
            from a.b import c
            class Cls:
                def f(self) -> "c":
                    c()
                    d = {}
                    d['key'] = 0
            def g():
                pass
            g()
            """
        )
        cls = ensure_type(m.body[1], cst.ClassDef)
        f = ensure_type(cls.body.body[0], cst.FunctionDef)
        self.assertEqual(
            names[ensure_type(f.returns, cst.Annotation).annotation], set()
        )

        c_call = ensure_type(
            ensure_type(f.body.body[0], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertEqual(
            names[c_call], {QualifiedName("a.b.c", QualifiedNameSource.IMPORT)}
        )
        self.assertEqual(
            names[c_call], {QualifiedName("a.b.c", QualifiedNameSource.IMPORT)}
        )

        g_call = ensure_type(
            ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertEqual(names[g_call], {QualifiedName("g", QualifiedNameSource.LOCAL)})
        d_name = (
            ensure_type(
                ensure_type(f.body.body[1], cst.SimpleStatementLine).body[0], cst.Assign
            )
            .targets[0]
            .target
        )
        self.assertEqual(
            names[d_name],
            {QualifiedName("Cls.f.<locals>.d", QualifiedNameSource.LOCAL)},
        )
        d_subscript = (
            ensure_type(
                ensure_type(f.body.body[2], cst.SimpleStatementLine).body[0], cst.Assign
            )
            .targets[0]
            .target
        )
        self.assertEqual(
            names[d_subscript],
            {QualifiedName("Cls.f.<locals>.d", QualifiedNameSource.LOCAL)},
        )

    def test_nested_qualified_names(self) -> None:
        m, names = get_qualified_name_metadata_provider(
            """
            class A:
                def f1(self):
                    def f2():
                        pass
                    f2()

                def f3(self):
                    class B():
                        ...
                    B()
            def f4():
                def f5():
                    class C:
                        pass
                    C()
                f5()
            """
        )

        cls_a = ensure_type(m.body[0], cst.ClassDef)
        self.assertEqual(names[cls_a], {QualifiedName("A", QualifiedNameSource.LOCAL)})
        func_f1 = ensure_type(cls_a.body.body[0], cst.FunctionDef)
        self.assertEqual(
            names[func_f1], {QualifiedName("A.f1", QualifiedNameSource.LOCAL)}
        )
        func_f2_call = ensure_type(
            ensure_type(func_f1.body.body[1], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertEqual(
            names[func_f2_call],
            {QualifiedName("A.f1.<locals>.f2", QualifiedNameSource.LOCAL)},
        )
        func_f3 = ensure_type(cls_a.body.body[1], cst.FunctionDef)
        self.assertEqual(
            names[func_f3], {QualifiedName("A.f3", QualifiedNameSource.LOCAL)}
        )
        call_b = ensure_type(
            ensure_type(func_f3.body.body[1], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertEqual(
            names[call_b], {QualifiedName("A.f3.<locals>.B", QualifiedNameSource.LOCAL)}
        )
        func_f4 = ensure_type(m.body[1], cst.FunctionDef)
        self.assertEqual(
            names[func_f4], {QualifiedName("f4", QualifiedNameSource.LOCAL)}
        )
        func_f5 = ensure_type(func_f4.body.body[0], cst.FunctionDef)
        self.assertEqual(
            names[func_f5], {QualifiedName("f4.<locals>.f5", QualifiedNameSource.LOCAL)}
        )
        cls_c = func_f5.body.body[0]
        self.assertEqual(
            names[cls_c],
            {QualifiedName("f4.<locals>.f5.<locals>.C", QualifiedNameSource.LOCAL)},
        )

    def test_multiple_assignments(self) -> None:
        m, names = get_qualified_name_metadata_provider(
            """
            if 1:
                from a import b as c
            elif 2:
                from d import e as c
            c()
            """
        )
        call = ensure_type(
            ensure_type(m.body[1], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertEqual(
            names[call],
            {
                QualifiedName(name="a.b", source=QualifiedNameSource.IMPORT),
                QualifiedName(name="d.e", source=QualifiedNameSource.IMPORT),
            },
        )

    def test_comprehension(self) -> None:
        m, names = get_qualified_name_metadata_provider(
            """
            class C:
                def fn(self) -> None:
                    [[k for k in i] for i in [j for j in range(10)]]
                    # Note:
                    # The qualified name of i is straightforward to be "C.fn.<locals>.<comprehension>.i".
                    # ListComp j is evaluated outside of the ListComp i.
                    # so j has qualified name "C.fn.<locals>.<comprehension>.j".
                    # ListComp k is evaluated inside ListComp i.
                    # so k has qualified name "C.fn.<locals>.<comprehension>.<comprehension>.k".
            """
        )
        cls_def = ensure_type(m.body[0], cst.ClassDef)
        fn_def = ensure_type(cls_def.body.body[0], cst.FunctionDef)
        outer_comp = ensure_type(
            ensure_type(
                ensure_type(fn_def.body.body[0], cst.SimpleStatementLine).body[0],
                cst.Expr,
            ).value,
            cst.ListComp,
        )
        i = outer_comp.for_in.target
        self.assertEqual(
            names[i],
            {
                QualifiedName(
                    name="C.fn.<locals>.<comprehension>.i",
                    source=QualifiedNameSource.LOCAL,
                )
            },
        )
        inner_comp_j = ensure_type(outer_comp.for_in.iter, cst.ListComp)
        j = inner_comp_j.for_in.target
        self.assertEqual(
            names[j],
            {
                QualifiedName(
                    name="C.fn.<locals>.<comprehension>.j",
                    source=QualifiedNameSource.LOCAL,
                )
            },
        )
        inner_comp_k = ensure_type(outer_comp.elt, cst.ListComp)
        k = inner_comp_k.for_in.target
        self.assertEqual(
            names[k],
            {
                QualifiedName(
                    name="C.fn.<locals>.<comprehension>.<comprehension>.k",
                    source=QualifiedNameSource.LOCAL,
                )
            },
        )

    def test_has_name_helper(self) -> None:
        class TestVisitor(cst.CSTVisitor):
            METADATA_DEPENDENCIES = (QualifiedNameProvider,)

            def __init__(self, test: UnitTest) -> None:
                self.test = test

            def visit_Call(self, node: cst.Call) -> Optional[bool]:
                self.test.assertTrue(
                    QualifiedNameProvider.has_name(self, node, "a.b.c")
                )
                self.test.assertFalse(QualifiedNameProvider.has_name(self, node, "a.b"))
                self.test.assertTrue(
                    QualifiedNameProvider.has_name(
                        self, node, QualifiedName("a.b.c", QualifiedNameSource.IMPORT)
                    )
                )
                self.test.assertFalse(
                    QualifiedNameProvider.has_name(
                        self, node, QualifiedName("a.b.c", QualifiedNameSource.LOCAL)
                    )
                )

        MetadataWrapper(cst.parse_module("import a;a.b.c()")).visit(TestVisitor(self))

    def test_name_in_attribute(self) -> None:
        m, names = get_qualified_name_metadata_provider(
            """
            obj = object()
            obj.eval
            """
        )
        attr = ensure_type(
            ensure_type(
                ensure_type(m.body[1], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Attribute,
        )
        self.assertEqual(
            names[attr],
            {QualifiedName(name="obj.eval", source=QualifiedNameSource.LOCAL)},
        )
        eval = attr.attr
        self.assertEqual(names[eval], set())

    def test_repeated_values_in_qualified_name(self) -> None:
        m, names = get_qualified_name_metadata_provider(
            """
            import a
            class Foo:
                bar: a.aa.aaa
            """
        )
        foo = ensure_type(m.body[1], cst.ClassDef)
        bar = ensure_type(
            ensure_type(
                ensure_type(foo.body, cst.IndentedBlock).body[0],
                cst.SimpleStatementLine,
            ).body[0],
            cst.AnnAssign,
        )

        annotation = ensure_type(bar.annotation, cst.Annotation)
        attribute = ensure_type(annotation.annotation, cst.Attribute)

        self.assertEqual(
            names[attribute], {QualifiedName("a.aa.aaa", QualifiedNameSource.IMPORT)}
        )
