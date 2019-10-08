# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This holds a series of transforms that help prettify generated code.
# The design is such that any of them could be left out and the code
# in question will still be correct, but possibly uglier to look at.
# Great care should be taken to include only transforms that do not
# affect the behavior of generated code, only the style for readability.
# As a result, since these can be skipped without harm, it is okay to
# use features such as matchers which rely on previously generated
# code to function.

# pyre-strict
import libcst as cst
import libcst.matchers as m


class SimplifyUnionsTransformer(cst.CSTTransformer):
    def leave_Subscript(
        self, original_node: cst.Subscript, updated_node: cst.Subscript
    ) -> cst.BaseExpression:
        if updated_node.value.deep_equals(cst.Name("Union")):
            slc = updated_node.slice
            if isinstance(slc, cst.Slice):
                # We don't expect this in type annotations, so its better
                # to whine about it than do undefined behavior.
                raise Exception("Unexpected Slice in Union!")
            elif isinstance(slc, cst.Index):
                # This is a Union[SimpleType] which is equivalent to
                # just SimpleType
                return slc.value
            elif len(slc) == 1:
                # This is a Union[SimpleType,] which is equivalent to
                # just SimpleType
                return cst.ensure_type(slc[0].slice, cst.Index).value
        return updated_node


class DoubleQuoteForwardRefsTransformer(m.MatcherDecoratableTransformer):
    @m.call_if_inside(m.Annotation())
    def leave_SimpleString(
        self, original_node: cst.SimpleString, updated_node: cst.SimpleString
    ) -> cst.SimpleString:
        # For prettiness, convert all single-quoted forward refs to double-quoted.
        if updated_node.value.startswith("'") and updated_node.value.endswith("'"):
            return updated_node.with_changes(value=f'"{updated_node.value[1:-1]}"')
        return updated_node
