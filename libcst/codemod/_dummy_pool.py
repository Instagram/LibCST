# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from types import TracebackType
from typing import Callable, Generator, Iterable, Optional, Type, TypeVar

RetT = TypeVar("RetT")
ArgT = TypeVar("ArgT")


class DummyPool:
    """
    Synchronous dummy `multiprocessing.Pool` analogue.
    """

    def __init__(self, processes: Optional[int] = None) -> None:
        pass

    def imap_unordered(
        self,
        func: Callable[[ArgT], RetT],
        iterable: Iterable[ArgT],
        chunksize: Optional[int] = None,
    ) -> Generator[RetT, None, None]:
        for args in iterable:
            yield func(args)

    def __enter__(self) -> "DummyPool":
        return self

    def __exit__(
        self,
        exc_type: Optional[Type[Exception]],
        exc: Optional[Exception],
        tb: Optional[TracebackType],
    ) -> None:
        pass
