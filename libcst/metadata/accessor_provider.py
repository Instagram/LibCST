# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses

from typing import Sequence

import libcst as cst

from libcst.metadata.base_provider import VisitorMetadataProvider


class AccessorProvider(VisitorMetadataProvider[str]):
    def on_visit(self, node: cst.CSTNode) -> bool:
        for f in dataclasses.fields(node):
            child = getattr(node, f.name)
            if isinstance(child, cst.CSTNode):
                self.set_metadata(child, f.name)
            elif isinstance(child, Sequence):
                for idx, subchild in enumerate(child):
                    self.set_metadata(subchild, f.name + "[" + str(idx) + "]")
        return True
