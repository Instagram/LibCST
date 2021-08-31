# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#

import sys
import textwrap
import unittest
from typing import Dict, Type

from libcst import parse_module
from libcst.codemod import Codemod, CodemodContext, CodemodTest
from libcst.codemod.visitors._apply_type_annotations import ApplyTypeAnnotationsVisitor
from libcst.testing.utils import data_provider


class TestApplyAnnotationsVisitor(CodemodTest):
    TRANSFORM: Type[Codemod] = ApplyTypeAnnotationsVisitor

    def run_simple_test_case(
        self,
        stub: str,
        before: str,
        after: str,
    ) -> None:
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, parse_module(textwrap.dedent(stub.rstrip()))
        )
        self.assertCodemod(before, after, context_override=context)

    def run_test_case_with_flags(
        self,
        stub: str,
        before: str,
        after: str,
        **kwargs: Dict[str, bool],
    ) -> None:
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, parse_module(textwrap.dedent(stub.rstrip()))
        )
        # Test setting the flag on the codemod instance.
        self.assertCodemod(before, after, context_override=context, **kwargs)

        # Test setting the flag when storing the stub in the context.
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context,
            parse_module(textwrap.dedent(stub.rstrip())),
            **kwargs,
        )
        self.assertCodemod(before, after, context_override=context)

    @data_provider(
        {
            "supported_cases": (
                """
                from __future__ import annotations
                from foo import Foo
                from baz import Baz
                """,
                """
                from foo import Bar
                import bar
                """,
                """
                from __future__ import annotations
                from foo import Foo, Bar
                import bar
                from baz import Baz
                """,
            ),
            "unsupported_cases": (
                """
                from Foo import foo as bar
                import foo
                from .. import baz
                from boo import *
                """,
                """
                """,
                # This is a bug, it would be better to just ignor aliased
                # imports than to add them incorrectly.
                """
                from Foo import bar
                """,
            ),
        }
    )
    def test_merge_module_imports(self, stub: str, before: str, after: str) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "simple": (
                """
                bar: int = ...
                """,
                """
                bar = foo()
                """,
                """
                bar: int = foo()
                """,
            ),
            "simple_with_existing": (
                """
                bar: int = ...
                """,
                """
                bar: str = foo()
                """,
                """
                bar: str = foo()
                """,
            ),
            "with_separate_declaration": (
                """
                x: int = ...
                y: int = ...
                z: int = ...
                """,
                """
                x = y = z = 1
                """,
                """
                x: int
                y: int
                z: int

                x = y = z = 1
                """,
            ),
            "needs_added_import": (
                """
                FOO: a.b.Example = ...
                """,
                """
                FOO = bar()
                """,
                """
                from a.b import Example

                FOO: Example = bar()
                """,
            ),
            "with_generic": (
                """
                FOO: Union[a.b.Example, int] = ...
                """,
                """
                FOO = bar()
                """,
                """
                from a.b import Example

                FOO: Union[Example, int] = bar()
                """,
            ),
        }
    )
    def test_annotate_globals(self, stub: str, before: str, after: str) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "basic_return": (
                """
                def foo() -> int: ...
                """,
                """
                def foo():
                    return 1
                """,
                """
                def foo() -> int:
                    return 1
                """,
            ),
            "return_with_existing_param": (
                """
                def foo(x: int) -> str: ...
                """,
                """
                def foo(x: str):
                    pass
                """,
                """
                def foo(x: str) -> str:
                    pass
                """,
            ),
            "param_with_existng_return": (
                """
                def foo(x: int) -> int: ...
                """,
                """
                def foo(x) -> int:
                    return x
                """,
                """
                def foo(x: int) -> int:
                    return x
                """,
            ),
            "return_and_params_general": (
                """
                def foo(
                    b: str, c: int = ..., *, d: str = ..., e: int, f: int = ...
                ) -> int: ...
                """,
                """
                def foo(
                    b, c=5, *, d="a", e, f=10
                ) -> int:
                    return 1
                """,
                """
                def foo(
                    b: str, c: int=5, *, d: str="a", e: int, f: int=10
                ) -> int:
                    return 1
                """,
            ),
            "with_import__basic": (
                """
                def foo() -> bar.Baz: ...
                """,
                """
                def foo():
                    return returns_baz()
                """,
                """
                from bar import Baz

                def foo() -> Baz:
                    return returns_baz()
                """,
            ),
            "with_import__unneeded_explicit": (
                """
                import bar

                def foo() -> bar.Baz: ...
                """,
                """
                def foo():
                    return returns_baz()
                """,
                """
                from bar import Baz

                def foo() -> Baz:
                    return returns_baz()
                """,
            ),
            # Keep the existing `import A` instead of using `from A import B`.
            "with_import__preexisting": (
                """
                def foo() -> bar.Baz: ...
                """,
                """
                import bar

                def foo():
                    return returns_baz()
                """,
                """
                import bar

                def foo() -> bar.Baz:
                    return returns_baz()
                """,
            ),
            "with_nested_import": (
                """
                def foo(x: django.http.response.HttpResponse) -> str:
                    pass
                """,
                """
                def foo(x) -> str:
                    pass
                """,
                """
                from django.http.response import HttpResponse

                def foo(x: HttpResponse) -> str:
                    pass
                """,
            ),
            "no_override_existing": (
                """
                def foo(x: int = 1) -> List[str]: ...
                """,
                """
                from typing import Iterable, Any

                def foo(x = 1) -> Iterable[Any]:
                    return ['']
                """,
                """
                from typing import Iterable, Any

                def foo(x: int = 1) -> Iterable[Any]:
                    return ['']
                """,
            ),
            "with_typing_import__basic": (
                """
                from typing import List

                def foo() -> List[int]: ...
                """,
                """
                def foo():
                    return [1]
                """,
                """
                from typing import List

                def foo() -> List[int]:
                    return [1]
                """,
            ),
            "with_typing_import__add_to_preexisting_line": (
                """
                from typing import List

                def foo() -> List[int]: ...
                """,
                """
                from typing import Union

                def foo():
                    return [1]
                """,
                """
                from typing import List, Union

                def foo() -> List[int]:
                    return [1]
                """,
            ),
            "add_imports_for_nested_types": (
                """
                def foo(x: int) -> Optional[a.b.Example]: ...
                """,
                """
                def foo(x: int):
                    pass
                """,
                """
                from a.b import Example

                def foo(x: int) -> Optional[Example]:
                    pass
                """,
            ),
            "UNSUPPORTED_add_imports_for_generics": (
                """
                def foo(x: int) -> typing.Optional[Example]: ...
                """,
                """
                def foo(x: int):
                    pass
                """,
                """
                def foo(x: int) -> typing.Optional[Example]:
                    pass
                """,
            ),
            "add_imports_for_doubly_nested_types": (
                """
                def foo(x: int) -> List[Union[a.b.Example, str]]: ...
                """,
                """
                def foo(x: int):
                    return [barfoo(), ""]
                """,
                """
                from a.b import Example

                def foo(x: int) -> List[Union[Example, str]]:
                    return [barfoo(), ""]
                """,
            ),
            "deeply_nested_example_with_multiline_annotation": (
                """
                def foo(x: int)-> Union[
                    Coroutine[Any, Any, django.http.response.HttpResponse], str
                ]:
                    ...
                """,
                """
                def foo(x: int):
                    pass
                """,
                """
                from django.http.response import HttpResponse

                def foo(x: int) -> Union[
                    Coroutine[Any, Any, HttpResponse], str
                ]:
                    pass
                """,
            ),
            "do_not_add_imports_inside_of_Type": (
                """
                from typing import Type

                def foo() -> Type[foo.A]: ...
                """,
                """
                def foo():
                    class A:
                        x = 1
                    return A

                """,
                """
                from typing import Type

                def foo() -> Type[foo.A]:
                    class A:
                        x = 1
                    return A
                """,
            ),
            # The following two tests verify that we can annotate functions
            # with async and decorator information, regardless of whether this
            # is part of the stub file.
            "async_with_decorators__full_stub": (
                """
                @second_decorator
                @first_decorator(5)
                async def async_with_decorators(r: Request, b: bool) -> django.http.response.HttpResponse: ...
                """,
                """
                @second_decorator
                @first_decorator(5)
                async def async_with_decorators(r, b):
                    return respond(r, b)
                """,
                """
                from django.http.response import HttpResponse

                @second_decorator
                @first_decorator(5)
                async def async_with_decorators(r: Request, b: bool) -> HttpResponse:
                    return respond(r, b)
                """,
            ),
            "async_with_decorators__bare_stub": (
                """
                def async_with_decorators(r: Request, b: bool) -> django.http.response.HttpResponse: ...
                """,
                """
                @second_decorator
                @first_decorator(5)
                async def async_with_decorators(r, b):
                    return respond(r, b)
                """,
                """
                from django.http.response import HttpResponse

                @second_decorator
                @first_decorator(5)
                async def async_with_decorators(r: Request, b: bool) -> HttpResponse:
                    return respond(r, b)
                """,
            ),
            # test cases named with the REQUIRES_PREEXISTING prefix are verifying
            # that certain special cases work if the stub and the existing code
            # happen to align well, but none of these cases are guaranteed to work
            # in general - for example duplicate type names will generally result in
            # incorrect codemod.
            "REQURIES_PREEXISTING_new_import_okay_if_existing_aliased": (
                """
                def foo() -> b.b.A: ...
                """,
                """
                from c import A as B, bar

                def foo():
                    return bar()
                """,
                """
                from c import A as B, bar
                from b.b import A

                def foo() -> A:
                    return bar()
                """,
            ),
            "REQUIRES_PREEXISTING_fully_qualified_with_alias": (
                """
                def foo() -> db.Connection: ...
                """,
                """
                import my.cool.db as db
                def foo():
                  return db.Connection()
                """,
                """
                import my.cool.db as db
                def foo() -> db.Connection:
                  return db.Connection()
                """,
            ),
            "REQURIRES_PREEXISTING_fully_qualified_typing": (
                """
                def foo() -> typing.Sequence[int]: ...
                """,
                """
                import typing
                def foo():
                  return []
                """,
                """
                import typing
                def foo() -> typing.Sequence[int]:
                  return []
                """,
            ),
        }
    )
    def test_annotate_simple_functions(
        self, stub: str, before: str, after: str
    ) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "respect_default_values_1": (
                """
                class B:
                    def foo(self, x: int = a.b.A.__add__(1), y=None) -> int: ...
                """,
                """
                class B:
                    def foo(self, x = A + 1, y = None) -> int:
                        return x

                """,
                """
                class B:
                    def foo(self, x: int = A + 1, y = None) -> int:
                        return x
                """,
            ),
            "respect_default_values_2": (
                """
                from typing import Optional

                class A:
                    def foo(self, atticus, b: Optional[int] = None, c: bool = False): ...
                """,
                """
                class A:
                    def foo(self, atticus, b = None, c = False): ...
                """,
                """
                from typing import Optional

                class A:
                    def foo(self, atticus, b: Optional[int] = None, c: bool = False): ...
                """,
            ),
        }
    )
    def test_annotate_classes(self, stub: str, before: str, after: str) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "method_and_function_of_same_name": (
                """
                def foo() -> int: ...

                class A:
                    def foo() -> str: ...
                """,
                """
                def foo():
                    return 1
                class A:
                    def foo():
                        return ''
                """,
                """
                def foo() -> int:
                    return 1
                class A:
                    def foo() -> str:
                        return ''
                """,
            ),
            "global_and_attribute_of_same_name": (
                """
                bar: int = ...
                class A:
                    bar: str = ...
                """,
                """
                bar = foo()
                class A:
                    bar = foobar()
                """,
                """
                bar: int = foo()
                class A:
                    bar: str = foobar()
                """,
            ),
            "add_global_annotation_simple_case": (
                """
                a: Dict[str, int] = ...
                """,
                """
                def foo() -> int:
                    return 1
                a = {}
                a['x'] = foo()
                """,
                """
                def foo() -> int:
                    return 1
                a: Dict[str, int] = {}
                a['x'] = foo()
                """,
            ),
            "add_global_annotation_with_Type__no_added_import": (
                """
                from typing import Dict

                example: Dict[str, Type[foo.Example]] = ...
                """,
                """
                from typing import Type

                def foo() -> Type[foo.Example]:
                    class Example:
                        pass
                    return Example

                example = { "test": foo() }
                """,
                """
                from typing import Dict, Type

                def foo() -> Type[foo.Example]:
                    class Example:
                        pass
                    return Example

                example: Dict[str, Type[foo.Example]] = { "test": foo() }
                """,
            ),
            "tuple_assign__add_new_top_level_declarations": (
                """
                a: int = ...
                b: str = ...
                """,
                """
                def foo() -> Tuple[int, str]:
                    return (1, "")

                a, b = foo()
                """,
                """
                a: int
                b: str

                def foo() -> Tuple[int, str]:
                    return (1, "")

                a, b = foo()
                """,
            ),
            "list_assign__add_new_top_level_declarations": (
                """
                a: int = ...
                b: str = ...
                """,
                """
                def foo() -> Tuple[int, str]:
                    return (1, "")

                [a, b] = foo()
                """,
                """
                a: int
                b: str

                def foo() -> Tuple[int, str]:
                    return (1, "")

                [a, b] = foo()
                """,
            ),
            "tuples_with_subscripts__add_new_toplevel_declaration": (
                """
                a: int = ...
                """,
                """
                from typing import Tuple

                def foo() -> Tuple[str, int]:
                    return "", 1

                b['z'], a = foo()
                """,
                """
                from typing import Tuple
                a: int

                def foo() -> Tuple[str, int]:
                    return "", 1

                b['z'], a = foo()
                """,
            ),
            "handle_quoted_annotations": (
                """
                bar: "a.b.Example"

                def f(x: "typing.Union[int, str]") -> "typing.Union[int, str]": ...

                class A:
                    def f(self: "A") -> "A": ...
                """,
                """
                bar = Example()

                def f(x):
                    return x

                class A:
                    def f(self):
                        return self
                """,
                """
                bar: "a.b.Example" = Example()

                def f(x: "typing.Union[int, str]") -> "typing.Union[int, str]":
                    return x

                class A:
                    def f(self: "A") -> "A":
                        return self
                """,
            ),
        }
    )
    def test_annotate_mixed(self, stub: str, before: str, after: str) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "insert_new_TypedDict_class_not_in_source_file": (
                """
                from mypy_extensions import TypedDict

                class MovieTypedDict(TypedDict):
                    name: str
                    year: int
                """,
                """
                def foo() -> None:
                    pass
                """,
                """
                from mypy_extensions import TypedDict

                class MovieTypedDict(TypedDict):
                    name: str
                    year: int

                def foo() -> None:
                    pass
                """,
            ),
            "insert_only_TypedDict_class_not_already_in_source": (
                """
                from mypy_extensions import TypedDict

                class MovieTypedDict(TypedDict):
                    name: str
                    year: int

                class ExistingMovieTypedDict(TypedDict):
                    name: str
                    year: int
                """,
                """
                from mypy_extensions import TypedDict

                class ExistingMovieTypedDict(TypedDict):
                    name: str
                    year: int

                def foo() -> None:
                    pass
                """,
                """
                from mypy_extensions import TypedDict

                class MovieTypedDict(TypedDict):
                    name: str
                    year: int

                class ExistingMovieTypedDict(TypedDict):
                    name: str
                    year: int

                def foo() -> None:
                    pass
                """,
            ),
        }
    )
    def test_adding_typed_dicts(self, stub: str, before: str, after: str) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "required_positional_only_args": (
                """
                def foo(
                    a: int, /, b: str, c: int = ..., *, d: str = ..., e: int, f: int = ...
                ) -> int: ...
                """,
                """
                def foo(
                    a, /, b, c=5, *, d="a", e, f=10
                ) -> int:
                    return 1
                """,
                """
                def foo(
                    a: int, /, b: str, c: int=5, *, d: str="a", e: int, f: int=10
                ) -> int:
                    return 1
                """,
            ),
            "positional_only_arg_with_default_value": (
                """
                def foo(
                    a: int, b: int = ..., /, c: int = ..., *, d: str = ..., e: int, f: int = ...
                ) -> int: ...
                """,
                """
                def foo(
                    a, b = 5, /, c = 10, *, d = "a", e, f = 20
                ) -> int:
                    return 1
                """,
                """
                def foo(
                    a: int, b: int = 5, /, c: int = 10, *, d: str = "a", e: int, f: int = 20
                ) -> int:
                    return 1
                """,
            ),
        }
    )
    @unittest.skipIf(sys.version_info < (3, 8), "Unsupported Python version")
    def test_annotate_functions_py38(self, stub: str, before: str, after: str) -> None:
        self.run_simple_test_case(stub=stub, before=before, after=after)

    @data_provider(
        {
            "fully_annotated_with_different_stub": (
                """
                def f(a: bool, b: bool) -> str: ...
                """,
                """
                def f(a: int, b: str) -> bool:
                    return 'hello'
                """,
                """
                def f(a: bool, b: bool) -> str:
                    return 'hello'
                """,
            ),
        }
    )
    def test_annotate_functions_with_existing_annotations(
        self, stub: str, before: str, after: str
    ) -> None:
        self.run_test_case_with_flags(
            stub=stub,
            before=before,
            after=after,
            overwrite_existing_annotations=True,
        )

    @data_provider(
        {
            "fully_annotated_with_untyped_stub": (
                """
                def f(a, b): ...
                """,
                """
                def f(a: bool, b: bool) -> str:
                    return "hello"
                """,
                """
                def f(a: bool, b: bool) -> str:
                    return "hello"
                """,
            ),
            "params_annotated_with_return_from_stub": (
                """
                def f(a, b) -> str: ...
                """,
                """
                def f(a: bool, b: bool):
                    return "hello"
                """,
                """
                def f(a: bool, b: bool) -> str:
                    return "hello"
                """,
            ),
            "partially_annotated_params_with_partial_stub": (
                """
                def f(a, b: int): ...
                """,
                """
                def f(a: bool, b) -> str:
                    return "hello"
                """,
                """
                def f(a: bool, b: int) -> str:
                    return "hello"
                """,
            ),
        }
    )
    def test_annotate_using_incomplete_stubs(
        self, stub: str, before: str, after: str
    ) -> None:
        """
        Ensure that when the stubs are missing annotations where the existing
        code has them, we won't remove the existing annotations even when
        `overwrite_existing_annotations` is set to `True`.
        """
        self.run_test_case_with_flags(
            stub=stub,
            before=before,
            after=after,
            overwrite_existing_annotations=True,
        )
