# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#

from typing import Iterable, Set, Tuple, Union

import libcst as cst
import libcst.matchers as m
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareVisitor
from libcst.codemod.visitors._gather_exports import GatherExportsVisitor
from libcst.codemod.visitors._gather_string_annotation_names import (
    GatherNamesFromStringAnnotationsVisitor,
)
from libcst.metadata import ProviderT, ScopeProvider
from libcst.metadata.scope_provider import _gen_dotted_names


class GatherUnusedImportsVisitor(ContextAwareVisitor):

    SUPPRESS_COMMENT_REGEX_CONTEXT_KEY = f"GatherUnusedImportsVisitor.suppress_regex"
    METADATA_DEPENDENCIES: Tuple[ProviderT] = (
        *GatherNamesFromStringAnnotationsVisitor.METADATA_DEPENDENCIES,
        ScopeProvider,
    )

    def __init__(self, context: CodemodContext) -> None:
        super().__init__(context)

        self._string_annotation_names: Set[str] = set()
        self._exported_names: Set[str] = set()
        self.unused_imports: Set[
            Tuple[cst.ImportAlias, Union[cst.Import, cst.ImportFrom]]
        ] = set()

    def visit_Module(self, node: cst.Module) -> bool:
        export_collector = GatherExportsVisitor(self.context)
        node.visit(export_collector)
        self._exported_names = export_collector.explicit_exported_objects
        annotation_visitor = GatherNamesFromStringAnnotationsVisitor(self.context)
        node.visit(annotation_visitor)
        self._string_annotation_names = annotation_visitor.names
        return True

    @m.visit(
        m.Import()
        | m.ImportFrom(
            module=m.DoesNotMatch(m.Name("__future__")),
            names=m.DoesNotMatch(m.ImportStar()),
        )
    )
    def handle_import(self, node: Union[cst.Import, cst.ImportFrom]) -> None:
        names = node.names
        assert not isinstance(names, cst.ImportStar)  # hello, type checker

        for alias in names:
            self.unused_imports.add((alias, node))

    def leave_Module(self, original_node: cst.Module) -> None:
        self.unused_imports = self.filter_unused_imports(self.unused_imports)

    def filter_unused_imports(
        self,
        candidates: Iterable[Tuple[cst.ImportAlias, Union[cst.Import, cst.ImportFrom]]],
    ) -> Set[Tuple[cst.ImportAlias, Union[cst.Import, cst.ImportFrom]]]:
        unused_imports = set()
        for (alias, parent) in candidates:
            scope = self.get_metadata(ScopeProvider, parent)
            if scope is None:
                continue
            if not self.is_in_use(scope, alias):
                unused_imports.add((alias, parent))
        return unused_imports

    def is_in_use(self, scope: cst.metadata.Scope, alias: cst.ImportAlias) -> bool:
        asname = alias.asname
        names = _gen_dotted_names(
            cst.ensure_type(asname.name, cst.Name) if asname is not None else alias.name
        )

        for name_or_alias, _ in names:
            if (
                name_or_alias in self._exported_names
                or name_or_alias in self._string_annotation_names
            ):
                return True

            for assignment in scope[name_or_alias]:
                if (
                    isinstance(assignment, cst.metadata.Assignment)
                    and isinstance(assignment.node, (cst.ImportFrom, cst.Import))
                    and len(assignment.references) > 0
                ):
                    return True
        return False