from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import replace
from typing import Generator, Optional

from libcst import MetadataDependent, MetadataWrapper, Module, parse_module
from libcst.codemod._context import CodemodContext


class Codemod(MetadataDependent, ABC):
    """
    Abstract base class that all codemods must subclass from. Classes wishing
    to perform arbitrary mutations on a tree should subclass from this. Classes
    wishing to perform visitor-based mutation should instead subclass from
    ContextAwareTransformer.
    """

    def __init__(self, context: CodemodContext) -> None:
        MetadataDependent.__init__(self)
        self.context: CodemodContext = context

    def should_allow_multiple_passes(self) -> bool:
        """
        Override this and return True to allow your transform to be called
        repeatedly until the tree doesn't change between passes. By default,
        this is off, and should suffice for most transforms.
        """
        return False

    def warn(self, warning: str) -> None:
        self.context.warnings.append(warning)

    @property
    def module(self) -> Module:
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

    @abstractmethod
    def transform_module_impl(self, tree: Module) -> Module:
        """
        Override this with your transform. You should take in the tree,
        optionally mutate it and then return it.
        """
        ...

    @contextmanager
    def _handle_metadata_reference(
        self, module: Module
    ) -> Generator[Module, None, None]:
        oldwrapper = self.context.wrapper
        wrapper = MetadataWrapper(module)
        with self.resolve(wrapper):
            self.context = replace(self.context, wrapper=wrapper)
            try:
                yield wrapper.module
            finally:
                self.context = replace(self.context, wrapper=oldwrapper)

    def transform_module(self, tree: Module) -> Module:
        """
        Transform entrypoint which handles multi-pass logic and metadata calculation
        for you. This is the method that you should call if you wish to
        invoke a codemod directly.
        """

        if not self.should_allow_multiple_passes():
            with self._handle_metadata_reference(tree) as tree_with_metadata:
                return self.transform_module_impl(tree_with_metadata)

        # We allow multiple passes, so we execute 1+ passes until there are
        # no more changes.
        before: str = tree.code
        after: Optional[str] = None
        while before != after:
            if after is not None:
                tree = parse_module(after)
                before = after
            with self._handle_metadata_reference(tree) as tree_with_metadata:
                tree = self.transform_module_impl(tree_with_metadata)
            after = tree.code
        return tree
