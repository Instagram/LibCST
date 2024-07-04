=====================
Working With Codemods
=====================

Codemods are an abstraction on top of LibCST for performing large-scale changes
to an entire codebase. See :doc:`Codemods <codemods>` for the complete
documentation.

-------------------------------
Setting up and Running Codemods
-------------------------------

Let's say you were interested in converting legacy ``.format()`` calls to shiny new
Python 3.6 f-strings. LibCST ships with a command-line interface known as
``libcst.tool``. This includes a few provisions for working with codemods at the
command-line. It also includes a library of pre-defined codemods, one of which is
a transform that can convert most ``.format()`` calls to f-strings. So, let's use this
to give Python 3.6 f-strings a try.


You might be lucky enough that the defaults for LibCST perfectly match your coding
style, but chances are you want to customize LibCST to your repository. Initialize
your repository by running the following command in the root of your repository and
then edit the produced ``.libcst.codemod.yaml`` file::

    python3 -m libcst.tool initialize .

The file includes provisions for customizing any generated code marker, calling an
external code formatter such as `black <https://pypi.org/project/black/>`_, blackisting
patterns of files you never wish to touch and a list of modules that contain valid
codemods that can be executed. If you want to write and run codemods specific to your
repository or organization, you can add an in-repo module location to the list of
modules and LibCST will discover codemods in all locations.

Now that your repository is initialized, let's have a quick look at what's currently
available for running. Run the following command from the root of your repository::

    python3 -m libcst.tool list

You'll see several codemods available to you, one of which is
``convert_format_to_fstring.ConvertFormatStringCommand``. The description to the right
of this codemod indicates that it converts ``.format()`` calls to f-strings, so let's
give it a whirl! Execute the codemod from the root of your repository like so::

    python3 -m libcst.tool codemod convert_format_to_fstring.ConvertFormatStringCommand .

If you want to try it out on only one file or a specific subdirectory, you can replace
the ``.`` in the above command with a relative directory, file, list of directories or
list of files. While LibCST is walking through your repository and codemodding files
you will see a progress indicator. If there's anything the codemod can't do or any
unexpected syntax errors, you will also see them on your console as it progresses.

If everything works out, you'll notice that your ``.format()`` calls have been
converted to f-strings!

-----------------
Writing a Codemod
-----------------

Codemods use the same principles as the rest of LibCST. They take LibCST's core,
metadata and matchers and package them up as a simple command-line interface. So,
anything you can do with LibCST in isolation you can also do with a codemod.

Let's say you need to clean up some legacy code which used magic values instead
of constants. You've already got a constants module called ``utils.constants``
and you want to assume that every reference to a raw string matching a particular
constant should be converted to that constant. For the simplest version of this
codemod, you'll need a command-line tool that takes as arguments the string to
replace and the constant to replace it with. You'll also need to ensure that
modified modules import the constant itself.

So, you can write something similar to the following::

    import argparse
    from ast import literal_eval
    from typing import Union

    import libcst as cst
    from libcst.codemod import CodemodContext, VisitorBasedCodemodCommand
    from libcst.codemod.visitors import AddImportsVisitor


    class ConvertConstantCommand(VisitorBasedCodemodCommand):

        # Add a description so that future codemodders can see what this does.
        DESCRIPTION: str = "Converts raw strings to constant accesses."

        @staticmethod
        def add_args(arg_parser: argparse.ArgumentParser) -> None:
            # Add command-line args that a user can specify for running this
            # codemod.
            arg_parser.add_argument(
                "--string",
                dest="string",
                metavar="STRING",
                help="String contents that we should look for.",
                type=str,
                required=True,
            )
            arg_parser.add_argument(
                "--constant",
                dest="constant",
                metavar="CONSTANT",
                help="Constant identifier we should replace strings with.",
                type=str,
                required=True,
            )

        def __init__(self, context: CodemodContext, string: str, constant: str) -> None:
            # Initialize the base class with context, and save our args. Remember, the
            # "dest" for each argument we added above must match a parameter name in
            # this init.
            super().__init__(context)
            self.string = string
            self.constant = constant

        def leave_SimpleString(
            self, original_node: cst.SimpleString, updated_node: cst.SimpleString
        ) -> Union[cst.SimpleString, cst.Name]:
            if literal_eval(updated_node.value) == self.string:
                # Check to see if the string matches what we want to replace. If so,
                # then we do the replacement. We also know at this point that we need
                # to import the constant itself.
                AddImportsVisitor.add_needed_import(
                    self.context, "utils.constants", self.constant,
                )
                return cst.Name(self.constant)
            # This isn't a string we're concerned with, so leave it unchanged.
            return updated_node

This codemod is pretty simple. It defines a command-line description, sets up to parse
a few required command-line args, initializes its own member variables with the
command-line args that were parsed for it by ``libcst.tool codemod`` and finally
replaces any string which matches our string command-line argument with a constant.
It also takes care of adding the import required for the constant to be defined properly.

Cool! Let's look at the command-line help for this codemod. Let's assume you saved it
as ``constant_folding.py``. You can get help for the
codemod by running the following command::

    python3 -m libcst.tool codemod -x constant_folding.ConvertConstantCommand --help

Notice that along with the default arguments, the ``--string`` and ``--constant``
arguments are present in the help, and the command-line description has been updated
with the codemod's description string. You'll notice that the codemod also shows up
on ``libcst.tool list``.

And ``-x`` flag allows to load any module as a codemod in addition to the standard ones.

----------------
Testing Codemods
----------------

Instead of iterating on a codemod by running it repeatedly on a codebase and seeing
what happens, we can write a series of unit tests that assert on desired
transformations. Given the above constant folding codemod that we wrote, we can test
it with some code similar to the following::

    from libcst.codemod import CodemodTest
    from libcst.codemod.commands.constant_folding import ConvertConstantCommand


    class TestConvertConstantCommand(CodemodTest):

        # The codemod that will be instantiated for us in assertCodemod.
        TRANSFORM = ConvertConstantCommand

        def test_noop(self) -> None:
            before = """
                foo = "bar"
            """
            after = """
                foo = "bar"
            """

            # Verify that if we don't have a valid string match, we don't make
            # any substitutions.
            self.assertCodemod(before, after, string="baz", constant="BAZ")

        def test_substitution(self) -> None:
            before = """
                foo = "bar"
            """
            after = """
                from utils.constants import BAR

                foo = BAR
            """

            # Verify that if we do have a valid string match, we make a substitution
            # as well as import the constant.
            self.assertCodemod(before, after, string="bar", constant="BAR")

If we save this as ``test_constant_folding.py`` inside ``libcst.codemod.commands.tests``
then we can execute the tests with the following line::

    python3 -m unittest libcst.codemod.commands.tests.test_constant_folding

That's all there is to it!
