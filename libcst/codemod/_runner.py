"""
Provides everything needed to run a CodemodCommand.
"""

import traceback
from dataclasses import dataclass
from enum import Enum
from typing import Sequence, Union

from libcst import parse_module
from libcst.codemod._codemod import Codemod


@dataclass(frozen=True)
class TransformSuccess:
    """
    Stores all the information we might need to display to the user upon success, as
    well as the transformed file contents.

    This datastructure is pickleable so that it can be used as a return value with
    the multiprocessing module.
    """

    warning_messages: Sequence[str]
    code: str


@dataclass(frozen=True)
class TransformFailure:
    """
    Stores all the information we might need to display to the user upon a failure.

    This datastructure is pickleable so that it can be used as a return value with
    the multiprocessing module.
    """

    warning_messages: Sequence[str]
    error: Exception
    traceback_str: str


@dataclass(frozen=True)
class TransformExit:
    """
    If the script is interrupted (e.g. KeyboardInterrupt) we don't have to
    print anything special.
    """

    warning_messages: Sequence[str] = ()


class SkipReason(Enum):
    GENERATED = "generated"
    BLACKLISTED = "blacklisted"
    OTHER = "other"


@dataclass(frozen=True)
class TransformSkip:
    """
    This file was skipped.

    This could be because it's a generated file, or due to filename
    blacklist, or because the transform raised SkipFile.
    """

    skip_reason: SkipReason
    skip_description: str
    warning_messages: Sequence[str] = ()


class SkipFile(Exception):
    """Raise this exception to skip codemodding the current file.

    The exception message should be the reason for skipping.
    """


TransformResult = Union[
    TransformSuccess, TransformFailure, TransformExit, TransformSkip
]


def transform_module(transformer: Codemod, code: str) -> TransformResult:
    """
    Given a module in a string and a Codemod to transform the module with,
    execute the codemod on the code and return a TransformResult. This will
    never raise an exception, instead will return a TransformFailure.
    """
    try:
        input_tree = parse_module(code)
        output_tree = transformer.transform_module(input_tree)
        return TransformSuccess(
            code=output_tree.code, warning_messages=transformer.context.warnings
        )
    except KeyboardInterrupt:
        return TransformExit()
    except SkipFile as ex:
        return TransformSkip(
            skip_description=str(ex),
            skip_reason=SkipReason.OTHER,
            warning_messages=transformer.context.warnings,
        )
    except Exception as ex:
        return TransformFailure(
            error=ex,
            traceback_str=traceback.format_exc(),
            warning_messages=transformer.context.warnings,
        )
