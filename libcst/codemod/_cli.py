# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
"""
Provides helpers for CLI interaction.
"""

import difflib
import os.path
import re
import subprocess
import sys
import traceback
from dataclasses import dataclass, replace
from multiprocessing import Process, Queue, cpu_count
from pathlib import Path
from queue import Empty
from typing import AnyStr, List, Optional, Sequence, cast

from libcst import parse_module
from libcst.codemod._codemod import Codemod
from libcst.codemod._runner import (
    SkipFile,
    SkipReason,
    TransformExit,
    TransformFailure,
    TransformResult,
    TransformSkip,
    TransformSuccess,
    transform_module,
)


_DEFAULT_GENERATED_CODE_MARKER: str = f"@gen{''}erated"


def invoke_formatter(formatter_args: Sequence[str], code: AnyStr) -> AnyStr:
    """
    Given a code string, run an external formatter on the code and return new
    formatted code.
    """

    # Make sure there is something to run
    if len(formatter_args) == 0:
        raise Exception("No formatter configured but code formatting requested.")

    # Invoke the formatter, giving it the code as stdin and assuming the formatted
    # code comes from stdout.
    work_with_bytes = isinstance(code, bytes)
    return cast(
        AnyStr,
        subprocess.check_output(
            formatter_args,
            env={},
            input=code,
            stderr=subprocess.DEVNULL,
            universal_newlines=not work_with_bytes,
            encoding=None if work_with_bytes else "utf-8",
        ),
    )


def print_execution_result(result: TransformResult) -> None:
    for warning in result.warning_messages:
        print(f"WARNING: {warning}", file=sys.stderr)

    if isinstance(result, TransformFailure):
        print(result.traceback_str, file=sys.stderr)


def gather_files(
    files_or_dirs: Sequence[str], *, include_stubs: bool = False
) -> List[str]:
    """
    Given a list of files or directories (can be intermingled), return a list of
    all python files that exist at those locations.
    """
    ret: List[str] = []
    for fd in files_or_dirs:
        if os.path.isfile(fd):
            ret.append(fd)
        elif os.path.isdir(fd):
            ret.extend(
                str(p)
                for p in Path(fd).rglob("*.py*")
                if str(p).endswith("py") or (include_stubs and str(p).endswith("pyi"))
            )
    return sorted(ret)


def diff_code(
    oldcode: str, newcode: str, context: int, *, filename: Optional[str] = None
) -> str:
    if oldcode == newcode:
        return ""

    if filename:
        difflines = difflib.unified_diff(
            oldcode.split("\n"),
            newcode.split("\n"),
            fromfile=filename,
            tofile=filename,
            lineterm="",
            n=context,
        )
    else:
        difflines = difflib.unified_diff(
            oldcode.split("\n"), newcode.split("\n"), lineterm="", n=context
        )
    return "\n".join(difflines)


def exec_transform_with_prettyprint(
    transform: Codemod,
    code: str,
    *,
    include_generated: bool = False,
    generated_code_marker: str = _DEFAULT_GENERATED_CODE_MARKER,
    format_code: bool = False,
    formatter_args: Sequence[str] = (),
) -> Optional[str]:
    """
    Given an instantiated transform, and a code string, transform that code string
    by executing the transform, and then print any generated warnings to the screen.
    """

    if not include_generated and generated_code_marker in code:
        print(
            "WARNING: Code is generated and we are set to ignore generated code, "
            + "skipping!",
            file=sys.stderr,
        )
        return code

    result = transform_module(transform, code)
    code: Optional[str] = None if isinstance(
        result, (TransformFailure, TransformExit, TransformSkip)
    ) else result.code

    if code is not None and format_code:
        try:
            code = invoke_formatter(formatter_args, code)
        except Exception as ex:
            # Failed to format code, treat as a failure and make sure that
            # we print the exception for debugging.
            code = None
            result = TransformFailure(
                error=ex,
                traceback_str=traceback.format_exc(),
                warning_messages=result.warning_messages,
            )

    # Finally, print the output, regardless of what happened
    print_execution_result(result)
    return code


@dataclass(frozen=True)
class ParallelExecResult:
    # File we have results for
    filename: str
    # Whether we actually changed the code for the file or not
    changed: bool
    # The actual result
    transform_result: TransformResult


def _parallel_exec_process_stub(  # noqa: C901
    result_queue: "Queue[ParallelExecResult]",
    transformer: Codemod,
    filename: str,
    unified_diff: Optional[int],
    include_generated: bool,
    generated_code_marker: str,
    format_code: bool,
    formatter_args: Sequence[str],
    blacklist_patterns: Sequence[str],
) -> None:
    for pattern in blacklist_patterns:
        if re.fullmatch(pattern, filename):
            result_queue.put(
                ParallelExecResult(
                    filename=filename,
                    changed=False,
                    transform_result=TransformSkip(
                        skip_reason=SkipReason.BLACKLISTED,
                        skip_description=f"Blacklisted by pattern {pattern}.",
                    ),
                )
            )
            return

    try:
        with open(filename, "rb") as fp:
            oldcode = fp.read()

        # Skip generated files
        if not include_generated and generated_code_marker.encode("utf-8") in oldcode:
            result_queue.put(
                ParallelExecResult(
                    filename=filename,
                    changed=False,
                    transform_result=TransformSkip(
                        skip_reason=SkipReason.GENERATED,
                        skip_description="Generated file.",
                    ),
                )
            )
            return

        # Somewhat gross hack to provide the filename in the transform's context.
        # We do this after the fork so that a context that was initialized with
        # some defaults before calling parallel_exec_transform_with_prettyprint
        # will be updated per-file.
        transformer.context = replace(transformer.context, filename=filename)

        # Run the transform, bail if we failed or if we aren't formatting code
        try:
            input_tree = parse_module(oldcode)
            output_tree = transformer.transform_module(input_tree)
            newcode = output_tree.bytes
            encoding = output_tree.encoding
        except KeyboardInterrupt:
            result_queue.put(
                ParallelExecResult(
                    filename=filename, changed=False, transform_result=TransformExit()
                )
            )
            return
        except SkipFile as ex:
            result_queue.put(
                ParallelExecResult(
                    filename=filename,
                    changed=False,
                    transform_result=TransformSkip(
                        skip_reason=SkipReason.OTHER,
                        skip_description=str(ex),
                        warning_messages=transformer.context.warnings,
                    ),
                )
            )
            return
        except Exception as ex:
            result_queue.put(
                ParallelExecResult(
                    filename=filename,
                    changed=False,
                    transform_result=TransformFailure(
                        error=ex,
                        traceback_str=traceback.format_exc(),
                        warning_messages=transformer.context.warnings,
                    ),
                )
            )
            return

        # Call formatter if needed, but only if we actually changed something in this
        # file
        if format_code and newcode != oldcode:
            try:
                newcode = invoke_formatter(formatter_args, newcode)
            except KeyboardInterrupt:
                result_queue.put(
                    ParallelExecResult(
                        filename=filename,
                        changed=False,
                        transform_result=TransformExit(),
                    )
                )
                return
            except Exception as ex:
                result_queue.put(
                    ParallelExecResult(
                        filename=filename,
                        changed=False,
                        transform_result=TransformFailure(
                            error=ex,
                            traceback_str=traceback.format_exc(),
                            warning_messages=transformer.context.warnings,
                        ),
                    )
                )
                return

        # Format as unified diff if needed, otherwise save it back
        changed = oldcode != newcode
        if unified_diff:
            newcode = diff_code(
                oldcode.decode(encoding),
                newcode.decode(encoding),
                unified_diff,
                filename=filename,
            )
        else:
            # Write back if we changed
            if changed:
                with open(filename, "wb") as fp:
                    fp.write(newcode)
            # Not strictly necessary, but saves space in pickle since we won't use it
            newcode = ""

        # Inform success
        result_queue.put(
            ParallelExecResult(
                filename=filename,
                changed=changed,
                transform_result=TransformSuccess(
                    warning_messages=transformer.context.warnings, code=newcode
                ),
            )
        )
    except KeyboardInterrupt:
        result_queue.put(
            ParallelExecResult(
                filename=filename, changed=False, transform_result=TransformExit()
            )
        )
    except Exception as ex:
        result_queue.put(
            ParallelExecResult(
                filename=filename,
                changed=False,
                transform_result=TransformFailure(
                    error=ex,
                    traceback_str=traceback.format_exc(),
                    warning_messages=transformer.context.warnings,
                ),
            )
        )


class Progress:
    ERASE_CURRENT_LINE: str = "\r\033[2K"

    def __init__(self, *, enabled: bool, total: int) -> None:
        self.enabled = enabled
        self.total = total
        # 1/100 = 0, len("0") = 1, precision = 0, more digits for more files
        self.pretty_precision: int = len(str(self.total // 100)) - 1

    def print(self, finished: int) -> None:
        if not self.enabled:
            return
        left = self.total - finished
        percent = 100.0 * (float(finished) / float(self.total))
        print(
            f"{self.ERASE_CURRENT_LINE}{percent:.{self.pretty_precision}f}% complete, {left} files to go...",
            end="",
            file=sys.stderr,
        )

    def clear(self) -> None:
        if not self.enabled:
            return
        print(self.ERASE_CURRENT_LINE, end="", file=sys.stderr)


def _print_parallel_result(
    exec_result: ParallelExecResult,
    progress: Progress,
    *,
    unified_diff: bool,
    show_successes: bool,
    hide_generated: bool,
    hide_blacklisted: bool,
) -> None:
    filename = exec_result.filename
    result = exec_result.transform_result

    if isinstance(result, TransformSkip):
        # Skipped file, print message and don't write back since not changed.
        if not (
            (result.skip_reason is SkipReason.BLACKLISTED and hide_blacklisted)
            or (result.skip_reason is SkipReason.GENERATED and hide_generated)
        ):
            progress.clear()
            print(f"Codemodding {filename}", file=sys.stderr)
            print_execution_result(result)
            print(
                f"Skipped codemodding {filename}: {result.skip_description}\n",
                file=sys.stderr,
            )
    elif isinstance(result, TransformFailure):
        # Print any exception, don't write the file back.
        progress.clear()
        print(f"Codemodding {filename}", file=sys.stderr)
        print_execution_result(result)
        print(f"Failed to codemod {filename}\n", file=sys.stderr)
    elif isinstance(result, TransformSuccess):
        if show_successes or result.warning_messages:
            # Print any warnings, save the changes if there were any.
            progress.clear()
            print(f"Codemodding {filename}", file=sys.stderr)
            print_execution_result(result)
            print(
                f"Successfully codemodded {filename}"
                + (" with warnings\n" if result.warning_messages else "\n"),
                file=sys.stderr,
            )

        # In unified diff mode, the code is a diff we must print.
        if unified_diff:
            print(result.code)


@dataclass(frozen=True)
class ParallelTransformResult:
    # Number of files that we successfully transformed
    successes: int
    # Number of files that we failed to transform
    failures: int
    # Number of warnings generated when running transform across files
    warnings: int
    # Number of files skipped because they were blacklisted or generated
    skips: int


def parallel_exec_transform_with_prettyprint(  # noqa: C901
    transform: Codemod,
    files: Sequence[str],
    *,
    jobs: Optional[int] = None,
    unified_diff: Optional[int] = None,
    include_generated: bool = False,
    generated_code_marker: str = _DEFAULT_GENERATED_CODE_MARKER,
    format_code: bool = False,
    formatter_args: Sequence[str] = (),
    show_successes: bool = False,
    hide_generated: bool = False,
    hide_blacklisted: bool = False,
    hide_progress: bool = False,
    blacklist_patterns: Sequence[str] = (),
) -> ParallelTransformResult:
    """
    Given a list of files, and an instantiated transform we should apply to them,
    fork and apply in parallel to all of the files. "jobs" controls the maximum
    number of in-flight transforms, and needs to be at least 1. To make this API
    simpler, we take an instantiated transform. This means we're implicitly relying
    on fork behavior on *NIX systems, and this will not work on Windows.
    """

    # Ensure that we have no duplicates, otherwise we might get race conditions
    # on write.
    files = sorted(list(set(files)))
    total = len(files)
    progress = Progress(enabled=not hide_progress, total=total)

    # Grab number of cores if we need to
    jobs: int = jobs or cpu_count()

    if jobs < 1:
        raise Exception("Must have at least one job to process!")

    if total == 0:
        return ParallelTransformResult(successes=0, failures=0, skips=0, warnings=0)

    # We place results in this queue inside _parallel_exec_process_stub
    # so that we can control when things get printed to the console.
    queue = Queue()

    if total == 1:
        # Simple case, we should not pay for process overhead. Lets still
        # use the exec stub however, so we can share code.
        progress.print(0)
        _parallel_exec_process_stub(
            queue,
            transform,
            files[0],
            unified_diff=unified_diff,
            include_generated=include_generated,
            generated_code_marker=generated_code_marker,
            format_code=format_code,
            formatter_args=formatter_args,
            blacklist_patterns=blacklist_patterns,
        )
        result = queue.get()
        _print_parallel_result(
            result,
            progress,
            unified_diff=bool(unified_diff),
            show_successes=show_successes,
            hide_generated=hide_generated,
            hide_blacklisted=hide_blacklisted,
        )
        if isinstance(result.transform_result, TransformFailure):
            return ParallelTransformResult(
                successes=0,
                failures=1,
                skips=0,
                warnings=len(result.transform_result.warning_messages),
            )
        elif isinstance(result.transform_result, (TransformSkip, TransformExit)):
            return ParallelTransformResult(
                successes=0,
                failures=0,
                skips=1,
                warnings=len(result.transform_result.warning_messages),
            )
        elif isinstance(result.transform_result, TransformSuccess):
            return ParallelTransformResult(
                successes=1,
                failures=0,
                skips=0,
                warnings=len(result.transform_result.warning_messages),
            )
        else:
            raise Exception("Logic error, unaccounted for result!")

    # Warm the parser
    parse_module("")

    # Complex case, more than one file
    successes: int = 0
    failures: int = 0
    warnings: int = 0
    skips: int = 0
    pending_processes: List[Process] = []

    # Start processes
    for f in files:
        pending_processes.append(
            Process(
                target=_parallel_exec_process_stub,
                args=(
                    queue,
                    transform,
                    f,
                    unified_diff,
                    include_generated,
                    generated_code_marker,
                    format_code,
                    formatter_args,
                    blacklist_patterns,
                ),
            )
        )

    # Start the processes, allowing no more than num_processes to be running
    # at once.
    results_left = len(pending_processes)
    joinable_processes: List[Process] = []
    processes_started = 0

    interrupted = False
    while results_left > 0 and not interrupted:
        while processes_started < jobs and pending_processes:
            try:
                # Move this process to the joinables
                process = pending_processes.pop(0)
                joinable_processes.append(process)

                # Start it, bookkeep that we did
                process.start()
                processes_started += 1
            except KeyboardInterrupt:
                interrupted = True
                continue

        try:
            result = queue.get(block=True, timeout=0.005)
        except KeyboardInterrupt:
            interrupted = True
            continue
        except Empty:
            progress.print(successes + failures + skips)
            continue

        # Bookkeep the result, since we know the process that returned this is done.
        results_left -= 1
        processes_started -= 1

        # Print an execution result, keep track of failures
        _print_parallel_result(
            result,
            progress,
            unified_diff=bool(unified_diff),
            show_successes=show_successes,
            hide_generated=hide_generated,
            hide_blacklisted=hide_blacklisted,
        )
        progress.print(successes + failures + skips)

        if isinstance(result.transform_result, TransformFailure):
            failures += 1
        elif isinstance(result.transform_result, TransformSuccess):
            successes += 1
        elif isinstance(result.transform_result, (TransformExit, TransformSkip)):
            skips += 1

        warnings += len(result.transform_result.warning_messages)

    # Now, join on all of them so we don't leave zombies or hang
    for p in joinable_processes:
        p.join()

    # Return whether there was one or more failure.
    progress.clear()

    # If we caught an interrupt, raise that
    if interrupted:
        raise KeyboardInterrupt()
    return ParallelTransformResult(
        successes=successes, failures=failures, skips=skips, warnings=warnings
    )
