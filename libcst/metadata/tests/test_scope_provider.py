# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from textwrap import dedent
from typing import Mapping, Tuple, cast

import libcst as cst
from libcst import ensure_type
from libcst.metadata import MetadataWrapper
from libcst.metadata.scope_provider import (
    Assignment,
    ClassScope,
    ComprehensionScope,
    FunctionScope,
    GlobalScope,
    LocalScope,
    QualifiedName,
    QualifiedNameSource,
    Scope,
    ScopeProvider,
)
from libcst.testing.utils import UnitTest, data_provider


class DependentVisitor(cst.CSTVisitor):
    METADATA_DEPENDENCIES = (ScopeProvider,)


def get_scope_metadata_provider(
    module_str: str,
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
        self.assertEqual(global_scope["not_in_scope"], set())

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
        global_foo_assignments = list(scope_of_module["foo"])
        self.assertEqual(len(global_foo_assignments), 1)
        foo_assignment = global_foo_assignments[0]
        self.assertEqual(len(foo_assignment.references), 2)
        fn1_call_arg = ensure_type(
            ensure_type(
                ensure_type(m.body[1], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Call,
        ).args[0]

        fn2_call_arg = ensure_type(
            ensure_type(
                ensure_type(m.body[2], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Call,
        ).args[0]
        self.assertEqual(
            {access.node for access in foo_assignment.references},
            {fn1_call_arg.value, fn2_call_arg.value},
        )
        func_body = ensure_type(m.body[3], cst.FunctionDef).body
        func_foo_statement = func_body.body[0]
        scope_of_func_statement = scopes[func_foo_statement]
        self.assertIsInstance(scope_of_func_statement, FunctionScope)
        func_foo_assignments = scope_of_func_statement["foo"]
        self.assertEqual(len(func_foo_assignments), 1)
        foo_assignment = list(func_foo_assignments)[0]
        self.assertEqual(len(foo_assignment.references), 1)
        fn3_call_arg = ensure_type(
            ensure_type(
                ensure_type(func_body.body[1], cst.SimpleStatementLine).body[0],
                cst.Expr,
            ).value,
            cst.Call,
        ).args[0]
        self.assertEqual(
            {access.node for access in foo_assignment.references}, {fn3_call_arg.value}
        )

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
        for idx, in_scopes in enumerate(
            [["foo", "foo.bar"], ["fizzbuzz"], ["a", "a.b", "a.b.c"], ["g"],]
        ):
            for in_scope in in_scopes:
                self.assertEqual(
                    len(scope_of_module[in_scope]), 1, f"{in_scope} should be in scope."
                )

                assignment = cast(Assignment, list(scope_of_module[in_scope])[0])
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

    def test_dotted_import_access(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
            import a.b.c, x.y
            a.b.c(x.z)
            """
        )
        scope_of_module = scopes[m]
        first_statement = ensure_type(m.body[1], cst.SimpleStatementLine)
        call = ensure_type(
            ensure_type(first_statement.body[0], cst.Expr).value, cst.Call
        )
        self.assertTrue("a.b.c" in scope_of_module)
        self.assertTrue("a" in scope_of_module)
        self.assertEqual(scope_of_module.accesses["a"], set())

        a_b_c_assignment = cast(Assignment, list(scope_of_module["a.b.c"])[0])
        a_b_c_access = list(a_b_c_assignment.references)[0]
        self.assertEqual(scope_of_module.accesses["a.b.c"], {a_b_c_access})
        self.assertEqual(a_b_c_access.node, call.func)

        x_assignment = cast(Assignment, list(scope_of_module["x"])[0])
        x_access = list(x_assignment.references)[0]
        self.assertEqual(scope_of_module.accesses["x"], {x_access})
        self.assertEqual(
            x_access.node, ensure_type(call.args[0].value, cst.Attribute).value
        )

        self.assertTrue("x.y" in scope_of_module)
        self.assertEqual(list(scope_of_module["x.y"])[0].references, set())
        self.assertEqual(scope_of_module.accesses["x.y"], set())

    def test_import_from(self) -> None:
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
            import_assignment = cast(Assignment, list(scope_of_module[in_scope])[0])
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
        cls_assignments = list(scope_of_module["Cls"])
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
        out_f_assignment = list(scope_of_module["f"])[0]
        self.assertEqual(cast(Assignment, out_f_assignment).node, outer_f)

        inner_f = ensure_type(outer_f.body.body[0], cst.FunctionDef)
        scope_of_inner_f = scopes[inner_f.body.body[0]]
        self.assertIsInstance(scope_of_inner_f, FunctionScope)
        self.assertTrue("f" in scope_of_inner_f)
        inner_f_assignment = list(scope_of_outer_f["f"])[0]
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
        x = f.params.params[0]
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

        self.assertEqual(cast(Assignment, list(scope_of_f["x"])[0]).node, x)
        self.assertEqual(cast(Assignment, list(scope_of_f["vararg"])[0]).node, vararg)
        self.assertEqual(cast(Assignment, list(scope_of_f["y"])[0]).node, y)
        self.assertEqual(cast(Assignment, list(scope_of_f["z"])[0]).node, z)
        self.assertEqual(cast(Assignment, list(scope_of_f["kwarg"])[0]).node, kwarg)

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

        x = f.params.params[0]
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

        self.assertEqual(cast(Assignment, list(scope_of_f["x"])[0]).node, x)
        self.assertEqual(cast(Assignment, list(scope_of_f["vararg"])[0]).node, vararg)
        self.assertEqual(cast(Assignment, list(scope_of_f["y"])[0]).node, y)
        self.assertEqual(cast(Assignment, list(scope_of_f["z"])[0]).node, z)
        self.assertEqual(cast(Assignment, list(scope_of_f["kwarg"])[0]).node, kwarg)

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
            cast(Assignment, list(scope_of_module["ex"])[0]).node,
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
            cast(Assignment, list(scope_of_module["f"])[0]).node,
            ensure_type(
                ensure_type(m.body[0], cst.With).items[0].asname, cst.AsName
            ).name,
        )

    def test_get_qualified_names_for(self) -> None:
        m, scopes = get_scope_metadata_provider(
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
        scope_of_module = scopes[m]
        self.assertEqual(
            scope_of_module.get_qualified_names_for(
                ensure_type(f.returns, cst.Annotation).annotation
            ),
            set(),
            "Get qualified name given a SimpleString type annotation is not supported",
        )

        c_call = ensure_type(
            ensure_type(f.body.body[0], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        scope_of_f = scopes[c_call]
        self.assertIsInstance(scope_of_f, FunctionScope)
        self.assertEqual(
            scope_of_f.get_qualified_names_for(c_call),
            {QualifiedName("a.b.c", QualifiedNameSource.IMPORT)},
        )
        self.assertEqual(
            scope_of_f.get_qualified_names_for(c_call),
            {QualifiedName("a.b.c", QualifiedNameSource.IMPORT)},
        )

        g_call = ensure_type(
            ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertIsInstance(scope_of_module, GlobalScope)
        self.assertEqual(
            scope_of_module.get_qualified_names_for(g_call),
            {QualifiedName("g", QualifiedNameSource.LOCAL)},
        )
        d_name = (
            ensure_type(
                ensure_type(f.body.body[1], cst.SimpleStatementLine).body[0], cst.Assign
            )
            .targets[0]
            .target
        )
        self.assertEqual(
            scope_of_f.get_qualified_names_for(d_name),
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
            scope_of_f.get_qualified_names_for(d_subscript),
            {QualifiedName("Cls.f.<locals>.d", QualifiedNameSource.LOCAL)},
        )

        for builtin in ["map", "int", "dict"]:
            self.assertEqual(
                scope_of_f.get_qualified_names_for(cst.Name(value=builtin)),
                {QualifiedName(f"builtins.{builtin}", QualifiedNameSource.BUILTIN)},
                f"Test builtin: {builtin}.",
            )

        self.assertEqual(
            scope_of_module.get_qualified_names_for(cst.Name(value="d")),
            set(),
            "Test variable d in global scope.",
        )

    def test_get_qualified_names_for_nested_cases(self) -> None:
        m, scopes = get_scope_metadata_provider(
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
        func_f1 = ensure_type(cls_a.body.body[0], cst.FunctionDef)
        scope_of_cls_a = scopes[func_f1]
        self.assertIsInstance(scope_of_cls_a, ClassScope)
        self.assertEqual(
            scope_of_cls_a.get_qualified_names_for(func_f1),
            {QualifiedName("A.f1", QualifiedNameSource.LOCAL)},
        )
        func_f2_call = ensure_type(
            ensure_type(func_f1.body.body[1], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        scope_of_f1 = scopes[func_f2_call]
        self.assertIsInstance(scope_of_f1, FunctionScope)
        self.assertEqual(
            scope_of_f1.get_qualified_names_for(func_f2_call),
            {QualifiedName("A.f1.<locals>.f2", QualifiedNameSource.LOCAL)},
        )
        func_f3 = ensure_type(cls_a.body.body[1], cst.FunctionDef)
        call_b = ensure_type(
            ensure_type(func_f3.body.body[1], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        scope_of_f3 = scopes[call_b]
        self.assertIsInstance(scope_of_f3, FunctionScope)
        self.assertEqual(
            scope_of_f3.get_qualified_names_for(call_b),
            {QualifiedName("A.f3.<locals>.B", QualifiedNameSource.LOCAL)},
        )
        func_f4 = ensure_type(m.body[1], cst.FunctionDef)
        func_f5 = ensure_type(func_f4.body.body[0], cst.FunctionDef)
        scope_of_f4 = scopes[func_f5]
        self.assertIsInstance(scope_of_f4, FunctionScope)
        self.assertEqual(
            scope_of_f4.get_qualified_names_for(func_f5),
            {QualifiedName("f4.<locals>.f5", QualifiedNameSource.LOCAL)},
        )
        cls_c = func_f5.body.body[0]
        scope_of_f5 = scopes[cls_c]
        self.assertIsInstance(scope_of_f5, FunctionScope)
        self.assertEqual(
            scope_of_f5.get_qualified_names_for(cls_c),
            {QualifiedName("f4.<locals>.f5.<locals>.C", QualifiedNameSource.LOCAL)},
        )

    def test_get_qualified_names_for_dotted_imports(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
                import a.b.c
                a(a.b.d)
            """
        )
        call = ensure_type(
            ensure_type(
                ensure_type(m.body[1], cst.SimpleStatementLine).body[0], cst.Expr
            ).value,
            cst.Call,
        )
        module_scope = scopes[m]
        self.assertEqual(
            module_scope.get_qualified_names_for(call.func),
            {QualifiedName("a", QualifiedNameSource.IMPORT)},
        )
        self.assertEqual(
            module_scope.get_qualified_names_for(call.args[0].value),
            {QualifiedName("a.b.d", QualifiedNameSource.IMPORT)},
        )

        import_stmt = ensure_type(
            ensure_type(m.body[0], cst.SimpleStatementLine).body[0], cst.Import
        )
        a_b_c = ensure_type(import_stmt.names[0].name, cst.Attribute)
        a_b = ensure_type(a_b_c.value, cst.Attribute)
        a = a_b.value
        self.assertEqual(
            module_scope.get_qualified_names_for(a_b_c),
            {QualifiedName("a.b.c", QualifiedNameSource.IMPORT)},
        )
        self.assertEqual(
            module_scope.get_qualified_names_for(a_b),
            {QualifiedName("a.b", QualifiedNameSource.IMPORT)},
        )
        self.assertEqual(
            module_scope.get_qualified_names_for(a),
            {QualifiedName("a", QualifiedNameSource.IMPORT)},
        )

    def test_multiple_assignments(self) -> None:
        m, scopes = get_scope_metadata_provider(
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
        scope = scopes[call]
        self.assertIsInstance(scope, GlobalScope)
        self.assertEqual(
            scope.get_qualified_names_for(call),
            {
                QualifiedName(name="a.b", source=QualifiedNameSource.IMPORT),
                QualifiedName(name="d.e", source=QualifiedNameSource.IMPORT),
            },
        )
        self.assertEqual(
            scope.get_qualified_names_for("c"),
            {
                QualifiedName(name="a.b", source=QualifiedNameSource.IMPORT),
                QualifiedName(name="d.e", source=QualifiedNameSource.IMPORT),
            },
        )

    def test_assignments_and_accesses(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
                a = 1
                def f():
                    a = 2
                    a, b
                    def g():
                        b = a
                a
            """
        )
        a_outer_assign = (
            ensure_type(
                ensure_type(m.body[0], cst.SimpleStatementLine).body[0], cst.Assign
            )
            .targets[0]
            .target
        )
        a_outer_access = ensure_type(
            ensure_type(m.body[2], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        scope_of_module = scopes[a_outer_assign]
        a_outer_assignments = scope_of_module.assignments[a_outer_access]
        self.assertEqual(len(a_outer_assignments), 1)
        a_outer_assignment = list(a_outer_assignments)[0]
        self.assertEqual(cast(Assignment, a_outer_assignment).node, a_outer_assign)
        self.assertEqual(
            {i.node for i in a_outer_assignment.references}, {a_outer_access}
        )

        a_outer_assesses = scope_of_module.accesses[a_outer_assign]
        self.assertEqual(len(a_outer_assesses), 1)
        self.assertEqual(list(a_outer_assesses)[0].node, a_outer_access)

        self.assertEqual(
            {cast(Assignment, i).node for i in list(a_outer_assesses)[0].referents},
            {a_outer_assign},
        )

        self.assertTrue(a_outer_assign in scope_of_module.accesses)
        self.assertTrue(a_outer_assign in scope_of_module.assignments)
        self.assertTrue(a_outer_access in scope_of_module.accesses)
        self.assertTrue(a_outer_access in scope_of_module.assignments)

        f = ensure_type(m.body[1], cst.FunctionDef)
        a_inner_assign = (
            ensure_type(
                ensure_type(
                    ensure_type(f.body, cst.IndentedBlock).body[0],
                    cst.SimpleStatementLine,
                ).body[0],
                cst.Assign,
            )
            .targets[0]
            .target
        )
        scope_of_f = scopes[a_inner_assign]
        a_inner_assignments = scope_of_f.assignments["a"]
        self.assertEqual(len(a_inner_assignments), 1)
        self.assertEqual(
            cast(Assignment, list(a_inner_assignments)[0]).node, a_inner_assign
        )
        tup = ensure_type(
            ensure_type(
                ensure_type(
                    ensure_type(f.body, cst.IndentedBlock).body[1],
                    cst.SimpleStatementLine,
                ).body[0],
                cst.Expr,
            ).value,
            cst.Tuple,
        )
        a_inner_access = tup.elements[0].value
        b_inner_access = tup.elements[1].value
        all_inner_accesses = [i for i in scope_of_f.accesses]
        self.assertEqual(len(all_inner_accesses), 2)
        self.assertEqual(
            {i.node for i in all_inner_accesses}, {a_inner_access, b_inner_access}
        )

        g = ensure_type(ensure_type(f.body, cst.IndentedBlock).body[2], cst.FunctionDef)
        inner_most_assign = ensure_type(
            ensure_type(g.body.body[0], cst.SimpleStatementLine).body[0], cst.Assign
        )
        b_inner_most_assign = inner_most_assign.targets[0].target
        a_inner_most_access = inner_most_assign.value
        scope_of_g = scopes[b_inner_most_assign]
        self.assertEqual({i.node for i in scope_of_g.accesses}, {a_inner_most_access})
        self.assertEqual(
            {cast(Assignment, i).node for i in scope_of_g.assignments},
            {b_inner_most_assign},
        )

        self.assertEqual(len(set(scopes.values())), 3)

    def test_node_of_scopes(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
                def f1():
                    target()

                class C:
                    attr = target()
            """
        )
        f1 = ensure_type(m.body[0], cst.FunctionDef)
        target_call = ensure_type(
            ensure_type(f1.body.body[0], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        f1_scope = scopes[target_call]
        self.assertIsInstance(f1_scope, FunctionScope)
        self.assertEqual(cast(FunctionScope, f1_scope).node, f1)
        c = ensure_type(m.body[1], cst.ClassDef)
        target_call_2 = ensure_type(
            ensure_type(c.body.body[0], cst.SimpleStatementLine).body[0], cst.Assign
        ).value
        c_scope = scopes[target_call_2]
        self.assertIsInstance(c_scope, ClassScope)
        self.assertEqual(cast(ClassScope, c_scope).node, c)

    def test_with_statement(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
                import unittest.mock

                with unittest.mock.patch("something") as obj:
                    obj.f1()

                unittest.mock
            """
        )
        import_ = ensure_type(m.body[0], cst.SimpleStatementLine).body[0]
        assignments = scopes[import_]["unittest"]
        self.assertEqual(len(assignments), 1)
        self.assertEqual(cast(Assignment, list(assignments)[0]).node, import_)
        with_ = ensure_type(m.body[1], cst.With)
        fn_call = with_.items[0].item
        self.assertEqual(
            scopes[fn_call].get_qualified_names_for(fn_call),
            {
                QualifiedName(
                    name="unittest.mock.patch", source=QualifiedNameSource.IMPORT
                )
            },
        )
        mock = ensure_type(
            ensure_type(m.body[2], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        self.assertEqual(
            scopes[fn_call].get_qualified_names_for(mock),
            {QualifiedName(name="unittest.mock", source=QualifiedNameSource.IMPORT)},
        )

    def test_del_context_names(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
                import a
                dic = {}
                del dic
                del dic["key"]
                del a.b
            """
        )
        dic = ensure_type(
            ensure_type(
                ensure_type(m.body[1], cst.SimpleStatementLine).body[0], cst.Assign
            ).targets[0],
            cst.AssignTarget,
        ).target
        del_dic = ensure_type(
            ensure_type(m.body[2], cst.SimpleStatementLine).body[0], cst.Del
        )
        scope = scopes[del_dic]
        assignments = list(scope["dic"])
        self.assertEqual(len(assignments), 1)
        dic_assign = assignments[0]
        self.assertIsInstance(dic_assign, Assignment)
        self.assertEqual(cast(Assignment, dic_assign).node, dic)
        self.assertEqual(len(dic_assign.references), 2)
        del_dic_subscript = ensure_type(
            ensure_type(
                ensure_type(m.body[3], cst.SimpleStatementLine).body[0], cst.Del
            ).target,
            cst.Subscript,
        )
        self.assertSetEqual(
            {i.node for i in dic_assign.references},
            {del_dic.target, del_dic_subscript.value},
        )
        assignments = list(scope["a"])
        self.assertEqual(len(assignments), 1)
        a_assign = assignments[0]
        self.assertIsInstance(a_assign, Assignment)
        import_a = ensure_type(m.body[0], cst.SimpleStatementLine).body[0]
        self.assertEqual(cast(Assignment, a_assign).node, import_a)
        self.assertEqual(len(a_assign.references), 1)
        del_a_b = ensure_type(
            ensure_type(m.body[4], cst.SimpleStatementLine).body[0], cst.Del
        )
        self.assertEqual(
            {i.node for i in a_assign.references},
            {ensure_type(del_a_b.target, cst.Attribute).value},
        )
        self.assertEqual(scope["b"], set())

    def test_keyword_arg_in_call(self) -> None:
        m, scopes = get_scope_metadata_provider("call(arg=val)")
        call = ensure_type(
            ensure_type(m.body[0], cst.SimpleStatementLine).body[0], cst.Expr
        ).value
        scope = scopes[call]
        self.assertIsInstance(scope, GlobalScope)
        self.assertEqual(len(scope["arg"]), 0)  # no assignment should exist

    def test_global_contains_is_read_only(self) -> None:
        gscope = GlobalScope()
        before_assignments = list(gscope._assignments.items())
        before_accesses = list(gscope._accesses.items())
        self.assertFalse("doesnt_exist" in gscope)
        self.assertEqual(list(gscope._accesses.items()), before_accesses)
        self.assertEqual(list(gscope._assignments.items()), before_assignments)

    def test_contains_is_read_only(self) -> None:
        for s in [LocalScope, FunctionScope, ClassScope, ComprehensionScope]:
            with self.subTest(scope=s):
                gscope = GlobalScope()
                scope = s(parent=gscope, node=cst.Name("lol"))
                before_assignments = list(scope._assignments.items())
                before_accesses = list(scope._accesses.items())
                before_overwrites = list(scope._scope_overwrites.items())
                before_parent_assignments = list(scope.parent._assignments.items())
                before_parent_accesses = list(scope.parent._accesses.items())

                self.assertFalse("doesnt_exist" in scope)
                self.assertEqual(list(scope._accesses.items()), before_accesses)
                self.assertEqual(list(scope._assignments.items()), before_assignments)
                self.assertEqual(
                    list(scope._scope_overwrites.items()), before_overwrites
                )
                self.assertEqual(
                    list(scope.parent._assignments.items()), before_parent_assignments
                )
                self.assertEqual(
                    list(scope.parent._accesses.items()), before_parent_accesses
                )

    def test_attribute_of_function_call(self) -> None:
        get_scope_metadata_provider("foo().bar")

    def test_attribute_of_subscript_called(self) -> None:
        m, scopes = get_scope_metadata_provider("foo[0].bar.baz()")
        scope = scopes[m]
        self.assertIn("foo", scope.accesses)

    def test_self(self) -> None:
        with open(__file__) as f:
            get_scope_metadata_provider(f.read())

    def test_get_qualified_names_for_is_read_only(self) -> None:
        m, scopes = get_scope_metadata_provider(
            """
                import a
                import b
            """
        )
        a = m.body[0]
        scope = scopes[a]
        assignments_len_before = len(scope._assignments)
        accesses_len_before = len(scope._accesses)
        scope.get_qualified_names_for("doesnt_exist")
        self.assertEqual(len(scope._assignments), assignments_len_before)
        self.assertEqual(len(scope._accesses), accesses_len_before)
