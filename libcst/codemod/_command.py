import argparse
import inspect
from abc import ABC, abstractmethod
from typing import Any, Dict, Generator, List, Type, TypeVar

from libcst import Module
from libcst.codemod._codemod import Codemod
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareTransformer
from libcst.codemod.visitors._add_imports import AddImportsVisitor


_Codemod = TypeVar("_Codemod", bound=Codemod)


class CodemodCommand(Codemod, ABC):
    """
    A command is a type of transform that is normally instantiated and
    run from the command-line. It behaves like any other Codemod in
    that it can be used anywhere that any other Codemod can be used.
    However, it can also be used with 'run_command' to make a transform
    into a CLI tool. It also includes facilities for automatically running
    certain common transforms after executing your transform_module_impl.
    The following list of transforms are supported at this time:

     - AddImportsVisitor (adds needed imports to a file).

    """

    # An overrideable description attribute so that codemods can provide
    # a short summary of what they do.
    DESCRIPTION: str = "No description."

    @staticmethod
    def add_args(arg_parser: argparse.ArgumentParser) -> None:
        """
        Override this to add arguments to the CLI argument parser. These args
        will show up when the user invokes the 'run_command' script with
        --help. They will also be presented to your class's __init__ method.
        So, if you define a command with an argument 'foo', you should also
        have a corresponding 'foo' positional or keyword argument in your
        class's __init__ method.
        """

        pass

    def _instantiate_and_run(self, transform: Type[_Codemod], tree: Module) -> Module:
        inst = transform(self.context)
        return inst.transform_module(tree)

    def transform_module(self, tree: Module) -> Module:
        # Overrides (but then calls) Codemod's transform_module to provide
        # a spot where additional supported transforms can be attached and run.
        tree = super().transform_module(tree)

        # List of transforms we should run, with their context key they use
        # for storing in context.scratch. Typically, the transform will also
        # have a static method that other transforms can use which takes
        # a context and other optional args and modifies its own context key
        # accordingly. We import them here so that we don't have circular imports.
        supported_transforms: Dict[str, Type[Codemod]] = {
            AddImportsVisitor.CONTEXT_KEY: AddImportsVisitor
        }

        # For any visitors that we support auto-running, run them here if needed.
        for key, transform in supported_transforms.items():
            if key in self.context.scratch:
                # We have work to do, so lets run this.
                tree = self._instantiate_and_run(transform, tree)

        # We're finally done!
        return tree


class VisitorBasedCodemodCommand(ContextAwareTransformer, CodemodCommand, ABC):
    """
    A command that acts identically to a visitor-based transform, but also has
    the support of add_args and running supported helper transforms after
    execution. See CodemodCommand and ContextAwareTransformer for documentation.
    """

    pass


class MagicArgsCodemodCommand(CodemodCommand, ABC):
    """
    A "magic" args command, which auto-magically looks up the transforms that
    are yielded from get_transforms and instantiates them using values out
    of the context. Visitors yielded in get_transforms must have constructor
    arguments that match a key in the context scratch. The easiest way to
    guarantee that is to use add_args to add a command arg that will be parsed
    for each of the args. However, if you wish to chain transforms, adding
    to the scratch in one transform will make the value available in the
    constructor in subsequent transforms as well as the scratch for subsequent
    transforms.
    """

    def __init__(self, context: CodemodContext, **kwargs: Dict[str, Any]) -> None:
        super().__init__(context)
        self.context.scratch.update(kwargs)

    @abstractmethod
    def get_transforms(self) -> Generator[Type[Codemod], None, None]:
        ...

    def _instantiate(self, transform: Type[Codemod]) -> Codemod:
        # Grab the expected arguments
        argspec = inspect.getfullargspec(transform.__init__)
        args: List[Any] = []
        kwargs: Dict[str, Any] = {}
        # pyre-fixme[6]: Expected `Sized` for 1st param but got `Union[Tuple[],
        #  Tuple[Any, ...]]`.
        last_default_arg = len(argspec.args) - len(argspec.defaults or ())
        for i, arg in enumerate(argspec.args):
            if arg in ["self", "context"]:
                # Self is bound, and context we explicitly include below.
                continue
            if arg not in self.context.scratch:
                if i >= last_default_arg:
                    # This arg has a default, so the fact that its missing is fine.
                    continue
                raise KeyError(
                    f"Visitor {transform.__name__} requires positional arg {arg} but "
                    + "it is not in our context nor does it have a default! It should "
                    + "be provided by an argument returned from the 'add_args' method "
                    + "or populated into context.scratch by a previous transform!"
                )
            # No default, but we found something in scratch. So, forward it.
            args.append(self.context.scratch[arg])
        for kwarg in argspec.kwonlyargs:
            if (
                kwarg not in self.context.scratch
                and kwarg not in argspec.kwonlydefaults
            ):
                raise KeyError(
                    f"Visitor {transform.__name__} requires keyword arg {kwarg} but "
                    + "it is not in our context nor does it have a default! It should "
                    + "be provided by an argument returned from the 'add_args' method "
                    + "or populated into context.scratch by a previous transform!"
                )
            kwargs[kwarg] = self.context.scratch.get(
                kwarg, argspec.kwonlydefaults[kwarg]
            )

        # Return an instance of the transform with those arguments
        return transform(self.context, *args, **kwargs)

    def transform_module_impl(self, tree: Module) -> Module:
        for transform in self.get_transforms():
            inst = self._instantiate(transform)
            tree = inst.transform_module(tree)
        return tree
