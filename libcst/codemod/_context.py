# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# pyre-strict
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

import libcst as cst


@dataclass(frozen=True)
class CodemodContext:
    """
    A context holding all information that is shared amongst all transforms
    in a single codemod invocation. When chaining multiple transforms together,
    the context holds the state that needs to be passed between transforms.
    The context is responsible for keeping track of metadata wrappers and the
    filename of the file that is being modified (if available).
    """

    # List of warnings gathered up while running a codemod. Add to this list
    # by calling the `warn()` method on a transform.
    warnings: List[str] = field(default_factory=list)
    # Scratch dictionary available for codemods which spread across multiple
    # transforms. Codemods are free to add to this at will.
    scratch: Dict[str, Any] = field(default_factory=dict)
    # The current filename if a codemod is being executed against a file that
    # lives on disk.
    filename: Optional[str] = None
    # The current top level metadata wrapper for the module being modified.
    # To access computed metadata, use `self.get_metadata` to retrieve values
    # when inside an actively running transform.
    wrapper: Optional[cst.MetadataWrapper] = None

    @property
    def module(self) -> Optional[cst.Module]:
        """
        The current top level module being modified. As a convenience, you can
        use `self.module` to refer to this when inside an actively running
        transform.
        """

        wrapper = self.wrapper
        if wrapper is None:
            return None
        return wrapper.module
