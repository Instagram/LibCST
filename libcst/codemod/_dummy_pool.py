# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from concurrent.futures import Future, Executor
from types import TracebackType
from typing import Callable, Optional, Type, TypeVar, ParamSpec

Return = TypeVar("Return")
ParamSpec = ParamSpec("ParamSpec")


class DummyExecutor(Executor):
    """
    Synchronous dummy `concurrent.futures.Executor` analogue.
    """

    def __init__(self, max_workers: Optional[int] = None) -> None:
        pass

    def submit(
        self,
        fn: Callable[ParamSpec, Return],
        *args: ParamSpec.args,
        **kwargs: ParamSpec.kwargs,
    ) -> Future[Return]:
        future: Future[Return] = Future()
        try:
            result = fn(*args, **kwargs)
            future.set_result(result)
        except Exception as exc:
            future.set_exception(exc)
        return future

    def __enter__(self) -> "DummyExecutor":
        return self

    def __exit__(
        self,
        exc_type: Optional[Type[Exception]],
        exc: Optional[Exception],
        tb: Optional[TracebackType],
    ) -> None:
        pass
