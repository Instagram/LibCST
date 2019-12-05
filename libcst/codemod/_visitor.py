import libcst as cst
from libcst import MetadataDependent
from libcst.codemod._codemod import Codemod
from libcst.codemod._context import CodemodContext
from libcst.matchers import MatcherDecoratableTransformer, MatcherDecoratableVisitor


class ContextAwareTransformer(Codemod, MatcherDecoratableTransformer):
    """
    A transformer which visits using LibCST. Allows visitor-based mutation of a tree.
    Classes wishing to do arbitrary non-visitor-based mutation on a tree should
    instead subclass from :class:`Codemod` and implement
    :meth:`~Codemod.transform_module_impl`.
    """

    def __init__(self, context: CodemodContext) -> None:
        Codemod.__init__(self, context)
        MatcherDecoratableTransformer.__init__(self)

    def _transform_module_impl(self, tree: cst.Module) -> cst.Module:
        return tree.visit(self)


class ContextAwareVisitor(MatcherDecoratableVisitor, MetadataDependent):
    """
    A collector which visits using LibCST. Allows visitor-based collecting of info
    on a tree. All codemods which wish to implement an information collector should
    subclass from this instead of directly from :class:`MatcherDecoratableVisitor`
    or :class:`CSTVisitor` since this provides access to the current codemod context.
    """

    def __init__(self, context: CodemodContext) -> None:
        MetadataDependent.__init__(self)
        MatcherDecoratableVisitor.__init__(self)
        self.context = context

        dependencies = self.get_inherited_dependencies()
        if dependencies:
            wrapper = self.context.wrapper
            if wrapper is None:
                raise Exception(
                    f"Attempting to instantiate {self.__class__.__name__} outside of "
                    + "an active transform. This means that metadata hasn't been "
                    + "calculated and we cannot successfully create this visitor."
                )
            for dep in dependencies:
                if dep not in wrapper._metadata:
                    raise Exception(
                        f"Attempting to access metadata {dep.__name__} that was not a "
                        + "declared dependency of parent transform! This means it is "
                        + "not possible to compute this value. Please ensure that all "
                        + f"parent transforms of {self.__class__.__name__} declare "
                        + f"{dep.__name__} as a metadata dependency."
                    )
            self.metadata = {dep: wrapper._metadata[dep] for dep in dependencies}

    @property
    def module(self) -> cst.Module:
        """
        Reference to the currently-traversed module. Note that this is only available
        during a transform itself.
        """
        module = self.context.module
        if module is None:
            raise Exception(
                f"Attempted access of {self.__class__.__name__}.module outside of "
                + "transform_module()."
            )
        return module
