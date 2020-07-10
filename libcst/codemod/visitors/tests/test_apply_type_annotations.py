# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#

import sys
import textwrap
import unittest
from typing import Type

from libcst import parse_module
from libcst.codemod import Codemod, CodemodContext, CodemodTest
from libcst.codemod.visitors._apply_type_annotations import ApplyTypeAnnotationsVisitor
from libcst.testing.utils import data_provider


class TestApplyAnnotationsVisitor(CodemodTest):
    TRANSFORM: Type[Codemod] = ApplyTypeAnnotationsVisitor

    @data_provider(
        (
            (
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
            (
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
            (
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
            (
                """
                import bar

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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            # Don't add annotations if one is already present
            (
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
            (
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
            (
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
            (
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
            # Test that tuples with subscripts are handled correctly
            # and top level annotations are added in the correct place
            (
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
            # Don't override existing default parameter values
            (
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
            (
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
            (
                """
                async def a(r: Request, z=None) -> django.http.response.HttpResponse: ...
                async def b(r: Request, z=None) -> django.http.response.HttpResponse: ...
                async def c(r: Request, z=None) -> django.http.response.HttpResponse: ...
                """,
                """
                async def a(r: Request, z=None): ...
                async def b(r: Request, z=None): ...
                async def c(r: Request, z=None): ...
                """,
                """
                from django.http.response import HttpResponse

                async def a(r: Request, z=None) -> HttpResponse: ...
                async def b(r: Request, z=None) -> HttpResponse: ...
                async def c(r: Request, z=None) -> HttpResponse: ...
                """,
            ),
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            (
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
            # Insert a TypedDict class that is not in the source file.
            (
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
            # Insert only the TypedDict class that is not in the source file.
            (
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
            # Sanity check that we don't fail when the stub has relative imports.
            # We don't do anything with those imports, though.
            (
                """
                from .. import hello
                def foo() -> typing.Sequence[int]: ...
                """,
                """
                def foo():
                  return []
                """,
                """
                def foo() -> typing.Sequence[int]:
                  return []
                """,
            ),
            (
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
        )
    )
    def test_annotate_functions(self, stub: str, before: str, after: str) -> None:
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, parse_module(textwrap.dedent(stub.rstrip()))
        )
        self.assertCodemod(before, after, context_override=context)

    @data_provider(
        (
            (
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
            (
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
        )
    )
    @unittest.skipIf(sys.version_info < (3, 8), "Unsupported Python version")
    def test_annotate_functions_py38(self, stub: str, before: str, after: str) -> None:
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, parse_module(textwrap.dedent(stub.rstrip()))
        )
        self.assertCodemod(before, after, context_override=context)

    @data_provider(
        (
            (
                """
                def fully_annotated_with_different_stub(a: bool, b: bool) -> str: ...
                """,
                """
                def fully_annotated_with_different_stub(a: int, b: str) -> bool:
                    return 'hello'
                """,
                """
                def fully_annotated_with_different_stub(a: bool, b: bool) -> str:
                    return 'hello'
                """,
            ),
        )
    )
    def test_annotate_functions_with_existing_annotations(
        self, stub: str, before: str, after: str
    ) -> None:
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, parse_module(textwrap.dedent(stub.rstrip()))
        )
        # Test setting the overwrite flag on the codemod instance.
        self.assertCodemod(
            before, after, context_override=context, overwrite_existing_annotations=True
        )

        # Test setting the flag when storing the stub in the context.
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context,
            parse_module(textwrap.dedent(stub.rstrip())),
            overwrite_existing_annotations=True,
        )
        self.assertCodemod(before, after, context_override=context)
