import os
from contextlib import contextmanager
from pathlib import Path
from typing import Generator

from libcst._types import StrPath


@contextmanager
def chdir(path: StrPath) -> Generator[Path, None, None]:
    """
    Temporarily chdir to the given path, and then return to the previous path.
    """
    try:
        path = Path(path).resolve()
        cwd = os.getcwd()
        os.chdir(path)
        yield path
    finally:
        os.chdir(cwd)
