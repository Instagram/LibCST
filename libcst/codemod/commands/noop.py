from libcst import Module
from libcst.codemod import CodemodCommand


class NOOPCommand(CodemodCommand):
    def transform_module_impl(self, tree: Module) -> Module:
        # Return the tree as-is, with absolutely no modification
        return tree
