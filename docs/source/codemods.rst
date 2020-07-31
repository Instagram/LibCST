========
Codemods
========

LibCST defines a codemod as an automated refactor that can be applied to a codebase
of arbitrary size. Codemods are provided as a framework for writing higher-order
transforms that consist of other, simpler transforms. It includes provisions for
quickly creating a command-line interface to execute a codemod.

.. _libcst-codemod-base:

------------
Codemod Base
------------

All codemods derive from a common base, :class:`~libcst.codemod.Codemod`. This class
includes a context, automatic metadata resolution and multi-pass transform support.
Codemods are intended to be executed using the :func:`~libcst.codemod.transform_module`
interface.

.. autoclass:: libcst.codemod.Codemod
.. autoclass:: libcst.codemod.CodemodContext

As a convenience, LibCST-compatible visitors are provided which extend the feature-set
of :class:`~libcst.codemod.Codemod` to LibCST visitors and transforms. Remember that
:class:`~libcst.codemod.ContextAwareTransformer` is still a
:class:`~libcst.codemod.Codemod`, so you should still execute it using
:func:`~libcst.codemod.transform_module`.

.. autoclass:: libcst.codemod.ContextAwareTransformer
  :exclude-members: transform_module_impl
.. autoclass:: libcst.codemod.ContextAwareVisitor

It is often necessary to bail out of a codemod mid-operation when you realize that
you do not want to operate on a module. This can be for any reason such as realizing
the module includes some operation that you do not support. If you wish to skip a
module, you can raise the :class:`~libcst.codemod.SkipFile` exception. For codemods
executed using the :func:`~libcst.codemod.transform_module` interface, all warnings
emitted up to the exception being thrown will be preserved in the result.

.. autoclass:: libcst.codemod.SkipFile

Finally, its often easier to test codemods by writing verification tests instead of
running repeatedly on your project. LibCST makes this easy with
:class:`~libcst.codemod.CodemodTest`. Often you can develop the majority of your
codemod using just tests, augmenting functionality when you run into an unexpected
edge case when running it against your repository.

.. autoclass:: libcst.codemod.CodemodTest
  :inherited-members:
  :exclude-members: addCleanup, addTypeEqualityFunc, assertAlmostEqual, assertAlmostEquals, assertCountEqual, assertDictContainsSubset, assertDictEqual, assertEqual, assertEquals, assertFalse, assertGreater, assertGreaterEqual, assertIn, assertIs, assertIsInstance, assertIsNone, assertIsNot, assertIsNotNone, assertLess, assertLessEqual, assertListEqual, assertLogs, assertMultiLineEqual, assertNotAlmostEqual, assertNotAlmostEquals, assertNotEqual, assertNotEquals, assertNotIn, assertNotIsInstance, assertNotRegex, assertNotRegexpMatches, assertRaises, assertRaisesRegex, assertRaisesRegexp, assertRegex, assertRegexpMatches, assertSequenceEqual, assertSetEqual, assertTrue, assertTupleEqual, assertWarns, assertWarnsRegex, assert_, countTestCases, debug, defaultTestResult, doCleanups, fail, failIf, failIfAlmostEqual, failIfEqual, failUnless, failUnlessAlmostEqual, failUnlessEqual, failUnlessRaises, failureException, id, longMessage, maxDiff, run, setUp, classmethod, setUpClass, shortDescription, skipTest, subTest, tearDown, tearDownClass

-------------------
Execution Interface
-------------------

As documented in the Codemod Base section above, codemods are meant to be
programmatically executed using :func:`~libcst.codemod.transform_module`. Executing
in this manner handles all of the featureset of codemods, including metadata calculation
and exception handling.

.. autofunction:: libcst.codemod.transform_module
.. autoclass:: libcst.codemod.TransformResult
.. autoclass:: libcst.codemod.TransformSuccess
.. autoclass:: libcst.codemod.TransformFailure
.. autoclass:: libcst.codemod.TransformSkip
.. autoclass:: libcst.codemod.SkipReason
.. autoclass:: libcst.codemod.TransformExit

--------------------
Command-Line Support
--------------------

LibCST includes additional support to facilitate faster development of codemods which
are to be run at the command-line. This is achieved through the
:class:`~libcst.codemod.CodemodCommand` class and the ``codemod`` utility which lives
inside ``libcst.tool``. The :class:`~libcst.codemod.CodemodCommand` class provides a
codemod description and an interface to add arguments to the command-line. This is
translated to a custom help message and command-line options that a user can provide
when running a codemod at the command-line.

For a brief overview of supported universal options, run the ``codemod`` utility like so::

    python3 -m libcst.tool codemod --help

The utility provides support for gathering up and parallelizing codemods across a
series of files or directories, auto-formatting changed code according to a configured
formatter, generating a unified diff of changes instead of applying them to files,
taking code from stdin and codemodding it before returning to stdout, and printing
progress and warnings to stderr during execution of a codemod.

Help is auto-customized if a codemod class is provided, including any added options
and the codemod description. For an example, run the ``codemod`` utility like so::

    python3 -m libcst.tool codemod noop.NOOPCommand --help

A second utility, ``list``, can list all available codemods given your configuration.
Run it like so::

    python3 -m libcst.tool list

Finally, to set up a directory for codemodding using these tools, including additional
directories where codemods can be found, use the ``initialize`` utility. To see help
for how to use this, run the ``initialize`` utility like so::

    python3 -m libcst.tool initialize --help

The above tools operate against any codemod which subclasses from
:class:`~libcst.codemod.CodemodCommand`. Remember that :class:`~libcst.codemod.CodemodCommand`
is a subclass of :class:`~libcst.codemod.Codemod`, so all of the features documented
in the :ref:`libcst-codemod-base` section are available in addition to command-line
support. Any command-line enabled codemod can also be programmatically instantiated
and invoked using the above-documented :func:`~libcst.codemod.transform_module`
interface.

.. autoclass:: libcst.codemod.CodemodCommand
  :exclude-members: transform_module

Additionally, a few convenience classes have been provided which take the boilerplate
out of common types of codemods:

.. autoclass:: libcst.codemod.VisitorBasedCodemodCommand
.. autoclass:: libcst.codemod.MagicArgsCodemodCommand
  :exclude-members: transform_module_impl

--------------------
Command-Line Toolkit
--------------------

Several helpers for constructing a command-line interface are provided. These are used
in the ``codemod`` utility to provide LibCST's de-facto command-line interface but they
are also available to be used directly in the case that circumstances demand a custom
command-line tool.

.. autofunction:: libcst.codemod.gather_files
.. autofunction:: libcst.codemod.exec_transform_with_prettyprint
.. autofunction:: libcst.codemod.parallel_exec_transform_with_prettyprint
.. autoclass:: libcst.codemod.ParallelTransformResult
.. autofunction:: libcst.codemod.diff_code

---------------------
Library of Transforms
---------------------

LibCST additionally includes a library of transforms to reduce the need for boilerplate
inside codemods. As of now, the list includes the following helpers.

.. autoclass:: libcst.codemod.visitors.GatherImportsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.GatherExportsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.AddImportsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.RemoveImportsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.ApplyTypeAnnotationsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.GatherUnusedImportsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.GatherCommentsVisitor
  :no-undoc-members:
.. autoclass:: libcst.codemod.visitors.GatherNamesFromStringAnnotationsVisitor
  :no-undoc-members: