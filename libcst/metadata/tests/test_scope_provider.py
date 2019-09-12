# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from textwrap import dedent
from typing import Mapping, Tuple, cast

import libcst as cst
from libcst import ensure_type
from libcst.metadata.scope_provider import (
    Assignment,
    ClassScope,
    ComprehensionScope,
    FunctionScope,
    GlobalScope,
    Scope,
    ScopeProvider,
)
from libcst.metadata.wrapper import MetadataWrapper
from libcst.testing.utils import UnitTest, data_provider


class DependentVisitor(cst.CSTVisitor):
    METADATA_DEPENDENCIES = (ScopeProvider,)


def get_scope_metadata_provider(
    module_str: str
) -> Tuple[cst.Module, Mapping[cst.CSTNode, Scope]]:
    wrapper = MetadataWrapper(cst.parse_module(dedent(module_str)))
    return (
        wrapper.module,
        cast(
            Mapping[cst.CSTNode, Scope], wrapper.resolve(ScopeProvider)
        ),  # we're sure every node has an associated scope
    )


class ScopeProviderTest(UnitTest):
    def test_not_in_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            pass
            """
        )
        global_scope = scopes[m]
        self.assertEqual(global_scope["not_in_scope"], ())

    def test_accesses(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            foo = 'toplevel'
            fn1(foo)
            fn2(foo)
            def fn_def():
                foo = 'shadow'
                fn3(foo)
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        global_foo_assignments = scope_of_module["foo"]
        self.assertEqual(len(global_foo_assignments), 1)
        foo_assignment = global_foo_assignments[0]
        self.assertEqual(len(foo_assignment.accesses), 2)
        fn1_call_arg = ensure_type(
            ensure_type(
                ensure_type(m.body[1], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Call,
        ).args[0]
        self.assertEqual(foo_assignment.accesses[0].node, fn1_call_arg.value)
        fn2_call_arg = ensure_type(
            ensure_type(
                ensure_type(m.body[2], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Call,
        ).args[0]
        self.assertEqual(foo_assignment.accesses[1].node, fn2_call_arg.value)
        func_body = ensure_type(m.body[3], cst.FunctionDef).body
        func_foo_statement = func_body.body[0]
        scope_of_func_statement = scopes[func_foo_statement]
        self.assertIsInstance(scope_of_func_statement, FunctionScope)
        func_foo_assignments = scope_of_func_statement["foo"]
        self.assertEqual(len(func_foo_assignments), 1)
        foo_assignment = func_foo_assignments[0]
        self.assertEqual(len(foo_assignment.accesses), 1)
        fn3_call_arg = ensure_type(
            ensure_type(
                ensure_type(func_body.body[1], cst.SimpleStatementLine).body[0],
                cst.Expr,
            ).value,
            cst.Call,
        ).args[0]
        self.assertEqual(foo_assignment.accesses[0].node, fn3_call_arg.value)

        wrapper = MetadataWrapper(cst.parse_module("from a import b\n"))
        wrapper.visit(DependentVisitor())

        wrapper = MetadataWrapper(cst.parse_module("def a():\n    from b import c\n\n"))
        wrapper.visit(DependentVisitor())

    @data_provider((("any",), ("True",), ("Exception",), ("__name__",)))
    def test_builtins(self, builtin: str) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            def fn():
                pass
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertEqual(len(scope_of_module[builtin]), 1)
        self.assertEqual(len(scope_of_module["something_not_a_builtin"]), 0)

        func_body = ensure_type(m.body[0], cst.FunctionDef).body
        func_pass_statement = func_body.body[0]
        scope_of_func_statement = scopes[func_pass_statement]
        self.assertIsInstance(scope_of_func_statement, FunctionScope)
        self.assertEqual(len(scope_of_func_statement[builtin]), 1)
        self.assertEqual(len(scope_of_func_statement["something_not_a_builtin"]), 0)

    def test_import(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            import foo.bar
            import fizz.buzz as fizzbuzz
            import a.b.c
            import d.e.f as g
            """
        )
        scope_of_module = scopes[m]
        for idx, in_scope in enumerate(["foo", "fizzbuzz", "a", "g"]):
            self.assertEqual(
                len(scope_of_module[in_scope]), 1, f"{in_scope} should be in scope."
            )

            assignment = cast(Assignment, scope_of_module[in_scope][0])
            self.assertEqual(
                assignment.name,
                in_scope,
                f"Assignment name {assignment.name} should equal to {in_scope}.",
            )
            import_node = ensure_type(m.body[idx], cst.SimpleStatementLine).body[0]
            self.assertEqual(
                assignment.node,
                import_node,
                f"The node of Assignment {assignment.node} should equal to {import_node}",
            )

    def test_imoprt_from(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            from foo.bar import a, b as b_renamed
            from . import c
            from .foo import d
            """
        )
        scope_of_module = scopes[m]
        for idx, in_scope in [(0, "a"), (0, "b_renamed"), (1, "c"), (2, "d")]:
            self.assertEqual(
                len(scope_of_module[in_scope]), 1, f"{in_scope} should be in scope."
            )
            import_assignment = cast(Assignment, scope_of_module[in_scope][0])
            self.assertEqual(
                import_assignment.name,
                in_scope,
                f"The name of Assignment {import_assignment.name} should equal to {in_scope}.",
            )
            import_node = ensure_type(m.body[idx], cst.SimpleStatementLine).body[0]
            self.assertEqual(
                import_assignment.node,
                import_node,
                f"The node of Assignment {import_assignment.node} should equal to {import_node}",
            )

        for not_in_scope in ["foo", "bar", "foo.bar", "b"]:
            self.assertEqual(
                len(scope_of_module[not_in_scope]),
                0,
                f"{not_in_scope} should not be in scope.",
            )

    def test_function_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            global_var = None
            def foo(arg, **kwargs):
                local_var = 5
            """
        )
        scope_of_module = scopes[m]
        func_body = ensure_type(m.body[1], cst.FunctionDef).body
        func_body_statement = func_body.body[0]
        scope_of_func = scopes[func_body_statement]
        self.assertIsInstance(scope_of_func, FunctionScope)
        self.assertTrue("global_var" in scope_of_module)
        self.assertTrue("global_var" in scope_of_func)
        self.assertTrue("arg" not in scope_of_module)
        self.assertTrue("arg" in scope_of_func)
        self.assertTrue("kwargs" not in scope_of_module)
        self.assertTrue("kwargs" in scope_of_func)
        self.assertTrue("local_var" not in scope_of_module)
        self.assertTrue("local_var" in scope_of_func)

    def test_class_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            global_var = None
            @cls_attr
            class Cls(cls_attr, kwarg=cls_attr):
                cls_attr = 5
                def f():
                    pass
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        cls_assignments = scope_of_module["Cls"]
        self.assertEqual(len(cls_assignments), 1)
        cls_assignment = cast(Assignment, cls_assignments[0])
        cls_def = ensure_type(m.body[1], cst.ClassDef)
        self.assertEqual(cls_assignment.node, cls_def)
        cls_body = cls_def.body
        cls_body_statement = cls_body.body[0]
        scope_of_class = scopes[cls_body_statement]
        self.assertIsInstance(scope_of_class, ClassScope)
        func_body = ensure_type(cls_body.body[1], cst.FunctionDef).body
        func_body_statement = func_body.body[0]
        scope_of_func = scopes[func_body_statement]
        self.assertIsInstance(scope_of_func, FunctionScope)
        self.assertTrue("global_var" in scope_of_module)
        self.assertTrue("global_var" in scope_of_class)
        self.assertTrue("global_var" in scope_of_func)
        self.assertTrue("Cls" in scope_of_module)
        self.assertTrue("Cls" in scope_of_class)
        self.assertTrue("Cls" in scope_of_func)
        self.assertTrue("cls_attr" not in scope_of_module)
        self.assertTrue("cls_attr" in scope_of_class)
        self.assertTrue("cls_attr" not in scope_of_func)

    def test_comprehension_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            iterator = None
            condition = None
            [elt for target in iterator if condition]
            {elt for target in iterator if condition}
            {elt: target for target in iterator if condition}
            (elt for target in iterator if condition)
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)

        list_comp = ensure_type(
            ensure_type(
                ensure_type(m.body[2], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.ListComp,
        )
        scope_of_list_comp = scopes[list_comp.elt]
        self.assertIsInstance(scope_of_list_comp, ComprehensionScope)

        set_comp = ensure_type(
            ensure_type(
                ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.SetComp,
        )
        scope_of_set_comp = scopes[set_comp.elt]
        self.assertIsInstance(scope_of_set_comp, ComprehensionScope)

        dict_comp = ensure_type(
            ensure_type(
                ensure_type(m.body[4], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.DictComp,
        )
        scope_of_dict_comp = scopes[dict_comp.key]
        self.assertIsInstance(scope_of_dict_comp, ComprehensionScope)

        generator_expr = ensure_type(
            ensure_type(
                ensure_type(m.body[5], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.GeneratorExp,
        )
        scope_of_generator_expr = scopes[generator_expr.elt]
        self.assertIsInstance(scope_of_generator_expr, ComprehensionScope)

        self.assertTrue("iterator" in scope_of_module)
        self.assertTrue("iterator" in scope_of_list_comp)
        self.assertTrue("iterator" in scope_of_set_comp)
        self.assertTrue("iterator" in scope_of_dict_comp)
        self.assertTrue("iterator" in scope_of_generator_expr)

        self.assertTrue("condition" in scope_of_module)
        self.assertTrue("condition" in scope_of_list_comp)
        self.assertTrue("condition" in scope_of_set_comp)
        self.assertTrue("condition" in scope_of_dict_comp)
        self.assertTrue("condition" in scope_of_generator_expr)

        self.assertTrue("elt" not in scope_of_module)
        self.assertTrue("elt" not in scope_of_list_comp)
        self.assertTrue("elt" not in scope_of_set_comp)
        self.assertTrue("elt" not in scope_of_dict_comp)
        self.assertTrue("elt" not in scope_of_generator_expr)

        self.assertTrue("target" not in scope_of_module)
        self.assertTrue("target" in scope_of_list_comp)
        self.assertTrue("target" in scope_of_set_comp)
        self.assertTrue("target" in scope_of_dict_comp)
        self.assertTrue("target" in scope_of_generator_expr)

    def test_nested_comprehension_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            [y for x in iterator for y in x]
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)

        list_comp = ensure_type(
            ensure_type(
                ensure_type(m.body[0], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.ListComp,
        )
        scope_of_list_comp = scopes[list_comp.elt]
        self.assertIsInstance(scope_of_list_comp, ComprehensionScope)

        self.assertIs(scopes[list_comp], scope_of_module)
        self.assertIs(scopes[list_comp.elt], scope_of_list_comp)

        self.assertIs(scopes[list_comp.for_in], scope_of_module)
        self.assertIs(scopes[list_comp.for_in.iter], scope_of_module)
        self.assertIs(scopes[list_comp.for_in.target], scope_of_list_comp)

        inner_for_in = ensure_type(list_comp.for_in.inner_for_in, cst.CompFor)
        self.assertIs(scopes[inner_for_in], scope_of_list_comp)
        self.assertIs(scopes[inner_for_in.iter], scope_of_list_comp)
        self.assertIs(scopes[inner_for_in.target], scope_of_list_comp)

    def test_global_scope_overwrites(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            class Cls:
                def f():
                    global var
                    var = ...
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertTrue("var" in scope_of_module)

        cls = ensure_type(m.body[0], cst.ClassDef)
        scope_of_cls = scopes[cls.body.body[0]]
        self.assertIsInstance(scope_of_cls, ClassScope)
        self.assertTrue("var" in scope_of_cls)

        f = ensure_type(cls.body.body[0], cst.FunctionDef)
        scope_of_f = scopes[f.body.body[0]]
        self.assertIsInstance(scope_of_f, FunctionScope)
        self.assertTrue("var" in scope_of_f)
        self.assertEqual(scope_of_f["var"], scope_of_module["var"])

    def test_nonlocal_scope_overwrites(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            def outer_f():
                var = ...
                class Cls:
                    var = ...
                    def inner_f():
                        nonlocal var
                        var = ...
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertTrue("var" not in scope_of_module)

        outer_f = ensure_type(m.body[0], cst.FunctionDef)
        outer_f_body_var_assign = ensure_type(
            ensure_type(outer_f.body.body[0], cst.SimpleStatementLine).body[0],
            cst.Assign,
        )
        scope_of_outer_f = scopes[outer_f_body_var_assign]
        self.assertIsInstance(scope_of_outer_f, FunctionScope)
        self.assertTrue("var" in scope_of_outer_f)
        self.assertEqual(len(scope_of_outer_f["var"]), 2)

        cls = ensure_type(outer_f.body.body[1], cst.ClassDef)
        scope_of_cls = scopes[cls.body.body[0]]
        self.assertIsInstance(scope_of_cls, ClassScope)
        self.assertTrue("var" in scope_of_cls)

        inner_f = ensure_type(cls.body.body[1], cst.FunctionDef)
        inner_f_body_var_assign = ensure_type(
            ensure_type(inner_f.body.body[1], cst.SimpleStatementLine).body[0],
            cst.Assign,
        )
        scope_of_inner_f = scopes[inner_f_body_var_assign]
        self.assertIsInstance(scope_of_inner_f, FunctionScope)
        self.assertTrue("var" in scope_of_inner_f)
        self.assertEqual(len(scope_of_inner_f["var"]), 2)
        self.assertEqual(
            {
                cast(Assignment, assignment).node
                for assignment in scope_of_outer_f["var"]
            },
            {
                outer_f_body_var_assign.targets[0].target,
                inner_f_body_var_assign.targets[0].target,
            },
        )

    def test_local_scope_shadowing_with_functions(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            def f():
                def f():
                    f = ...
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertTrue("f" in scope_of_module)

        outer_f = ensure_type(m.body[0], cst.FunctionDef)
        scope_of_outer_f = scopes[outer_f.body.body[0]]
        self.assertIsInstance(scope_of_outer_f, FunctionScope)
        self.assertTrue("f" in scope_of_outer_f)
        out_f_assignment = scope_of_module["f"][0]
        self.assertEqual(cast(Assignment, out_f_assignment).node, outer_f)

        inner_f = ensure_type(outer_f.body.body[0], cst.FunctionDef)
        scope_of_inner_f = scopes[inner_f.body.body[0]]
        self.assertIsInstance(scope_of_inner_f, FunctionScope)
        self.assertTrue("f" in scope_of_inner_f)
        inner_f_assignment = scope_of_outer_f["f"][0]
        self.assertEqual(cast(Assignment, inner_f_assignment).node, inner_f)

    def test_func_param_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            @decorator
            def f(x: T=1, *vararg, y: T=2, z, **kwarg) -> RET:
                pass
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertTrue("f" in scope_of_module)

        f = ensure_type(m.body[0], cst.FunctionDef)
        scope_of_f = scopes[f.body.body[0]]
        self.assertIsInstance(scope_of_f, FunctionScope)

        decorator = f.decorators[0]
        x = f.params.default_params[0]
        xT = ensure_type(x.annotation, cst.Annotation)
        one = ensure_type(x.default, cst.BaseExpression)
        vararg = ensure_type(f.params.star_arg, cst.Param)
        y = f.params.kwonly_params[0]
        yT = ensure_type(y.annotation, cst.Annotation)
        two = ensure_type(y.default, cst.BaseExpression)
        z = f.params.kwonly_params[1]
        kwarg = ensure_type(f.params.star_kwarg, cst.Param)
        ret = ensure_type(f.returns, cst.Annotation).annotation

        self.assertEqual(scopes[decorator], scope_of_module)
        self.assertEqual(scopes[x], scope_of_f)
        self.assertEqual(scopes[xT], scope_of_module)
        self.assertEqual(scopes[one], scope_of_module)
        self.assertEqual(scopes[vararg], scope_of_f)
        self.assertEqual(scopes[y], scope_of_f)
        self.assertEqual(scopes[yT], scope_of_module)
        self.assertEqual(scopes[z], scope_of_f)
        self.assertEqual(scopes[two], scope_of_module)
        self.assertEqual(scopes[kwarg], scope_of_f)
        self.assertEqual(scopes[ret], scope_of_module)

        self.assertTrue("x" not in scope_of_module)
        self.assertTrue("x" in scope_of_f)
        self.assertTrue("vararg" not in scope_of_module)
        self.assertTrue("vararg" in scope_of_f)
        self.assertTrue("y" not in scope_of_module)
        self.assertTrue("y" in scope_of_f)
        self.assertTrue("z" not in scope_of_module)
        self.assertTrue("z" in scope_of_f)
        self.assertTrue("kwarg" not in scope_of_module)
        self.assertTrue("kwarg" in scope_of_f)

        self.assertEqual(cast(Assignment, scope_of_f["x"][0]).node, x)
        self.assertEqual(cast(Assignment, scope_of_f["vararg"][0]).node, vararg)
        self.assertEqual(cast(Assignment, scope_of_f["y"][0]).node, y)
        self.assertEqual(cast(Assignment, scope_of_f["z"][0]).node, z)
        self.assertEqual(cast(Assignment, scope_of_f["kwarg"][0]).node, kwarg)

    def test_lambda_param_scope(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            lambda x=1, *vararg, y=2, z, **kwarg:x
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)

        f = ensure_type(
            ensure_type(
                ensure_type(m.body[0], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Lambda,
        )
        scope_of_f = scopes[f.body]
        self.assertIsInstance(scope_of_f, FunctionScope)

        x = f.params.default_params[0]
        one = ensure_type(x.default, cst.BaseExpression)
        vararg = ensure_type(f.params.star_arg, cst.Param)
        y = f.params.kwonly_params[0]
        two = ensure_type(y.default, cst.BaseExpression)
        z = f.params.kwonly_params[1]
        kwarg = ensure_type(f.params.star_kwarg, cst.Param)

        self.assertEqual(scopes[x], scope_of_f)
        self.assertEqual(scopes[one], scope_of_module)
        self.assertEqual(scopes[vararg], scope_of_f)
        self.assertEqual(scopes[y], scope_of_f)
        self.assertEqual(scopes[z], scope_of_f)
        self.assertEqual(scopes[two], scope_of_module)
        self.assertEqual(scopes[kwarg], scope_of_f)

        self.assertTrue("x" not in scope_of_module)
        self.assertTrue("x" in scope_of_f)
        self.assertTrue("vararg" not in scope_of_module)
        self.assertTrue("vararg" in scope_of_f)
        self.assertTrue("y" not in scope_of_module)
        self.assertTrue("y" in scope_of_f)
        self.assertTrue("z" not in scope_of_module)
        self.assertTrue("z" in scope_of_f)
        self.assertTrue("kwarg" not in scope_of_module)
        self.assertTrue("kwarg" in scope_of_f)

        self.assertEqual(cast(Assignment, scope_of_f["x"][0]).node, x)
        self.assertEqual(cast(Assignment, scope_of_f["vararg"][0]).node, vararg)
        self.assertEqual(cast(Assignment, scope_of_f["y"][0]).node, y)
        self.assertEqual(cast(Assignment, scope_of_f["z"][0]).node, z)
        self.assertEqual(cast(Assignment, scope_of_f["kwarg"][0]).node, kwarg)

    def test_except_handler(self) -> None:
        """
        The ``except as`` is a special case. The asname is only available in the excep body
        block and it'll be removed when existing the block.
        See https://docs.python.org/3.4/reference/compound_stmts.html#except
        We don't create a new block for except body because we don't handle del in our Scope Analysis.
        """
        m, scopes = get_scope_metadata_provider(
            """
            try:
                ...
            except Exception as ex:
                ...
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertTrue("ex" in scope_of_module)
        self.assertEqual(
            cast(Assignment, scope_of_module["ex"][0]).node,
            ensure_type(
                ensure_type(m.body[0], cst.Try).handlers[0].name, cst.AsName
            ).name,
        )

    def test_with_asname(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            with open(file_name) as f:
                ...
            """
        )
        scope_of_module = scopes[m]
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertTrue("f" in scope_of_module)
        self.assertEqual(
            cast(Assignment, scope_of_module["f"][0]).node,
            ensure_type(
                ensure_type(m.body[0], cst.With).items[0].asname, cst.AsName
            ).name,
        )
