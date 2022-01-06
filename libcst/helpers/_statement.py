# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from typing import Optional

import libcst as cst
from libcst.helpers.expression import get_full_name_for_node


def get_absolute_module(
    current_module: Optional[str], module_name: Optional[str], num_dots: int
) -> Optional[str]:
    if num_dots == 0:
        # This is an absolute import, so the module is correct.
        return module_name
    if current_module is None:
        # We don't actually have the current module available, so we can't compute
        # the absolute module from relative.
        return None
    # We have the current module, as well as the relative, let's compute the base.
    modules = current_module.split(".")
    if len(modules) < num_dots:
        # This relative import goes past the base of the repository, so we can't calculate it.
        return None
    base_module = ".".join(modules[:-num_dots])
    # Finally, if the module name was supplied, append it to the end.
    if module_name is not None:
        # If we went all the way to the top, the base module should be empty, so we
        # should return the relative bit as absolute. Otherwise, combine the base
        # module and module name using a dot separator.
        base_module = (
            f"{base_module}.{module_name}" if len(base_module) > 0 else module_name
        )
    # If they tried to import all the way to the root, return None. Otherwise,
    # return the module itself.
    return base_module if len(base_module) > 0 else None


def get_absolute_module_for_import(
    current_module: Optional[str], import_node: cst.ImportFrom
) -> Optional[str]:
    # First, let's try to grab the module name, regardless of relative status.
    module = import_node.module
    module_name = get_full_name_for_node(module) if module is not None else None
    # Now, get the relative import location if it exists.
    num_dots = len(import_node.relative)
    return get_absolute_module(current_module, module_name, num_dots)


def get_absolute_module_for_import_or_raise(
    current_module: Optional[str], import_node: cst.ImportFrom
) -> str:
    module = get_absolute_module_for_import(current_module, import_node)
    if module is None:
        raise Exception(f"Unable to compute absolute module for {import_node}")
    return module
