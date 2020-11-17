# 0.3.14 - 2020-11-18
## Fixed
- Fix is_annotation for types used in classdef base and assign value [#406](https://github.com/Instagram/LibCST/pull/406)
- Visit concatenated f-strings during scope analysis [#411](https://github.com/Instagram/LibCST/pull/411)
- Correct handling of walrus operator in function args [#417](https://github.com/Instagram/LibCST/pull/417)
- Allow generator expressions in f-strings [#419](https://github.com/Instagram/LibCST/pull/419)
- Keep track of assignment/access ordering during scope analysis [#413](https://github.com/Instagram/LibCST/pull/413)
- Handle string type references in cast() during scope analysis [#418](https://github.com/Instagram/LibCST/pull/418)

# 0.3.13 - 2020-10-12

## Fixed
- Use correct type for AugAssign and AnnAssign target [#396](https://github.com/Instagram/LibCST/pull/396)
- Support string annotations for type aliases [#401](https://github.com/Instagram/LibCST/pull/401)

# 0.3.12 - 2020-10-01

## Fixed
- fix RemoveImportsVisitor crash when ImportAlias is inserted without comma [#397](https://github.com/Instagram/LibCST/pull/397)
- Provide STORE for {Class,Function}Def.name in ExpressionContextProvider [#394](https://github.com/Instagram/LibCST/pull/394)

# 0.3.11 - 2020-09-29

## Added
- Implement TypeOf matcher [#384](https://github.com/Instagram/LibCST/pull/384)

## Updated
- Update return type of ParentNodeProvider to be CSTNode [#377](https://github.com/Instagram/LibCST/pull/377)
- Add source code links to each class/function [#378](https://github.com/Instagram/LibCST/pull/378)

## Fixed
- Removing an import alias with a trailing standalone comment should preserve the comment [#392](https://github.com/Instagram/LibCST/pull/392)

# 0.3.10 - 2020-09-17

## Added
- Handle string annotations in ScopeProvider [#373](https://github.com/Instagram/LibCST/pull/373)
- Add is_annotation subtype for Access inreferences. [#372](https://github.com/Instagram/LibCST/pull/372)

## Updated
- Call pyre query with noninteractive logging [#371](https://github.com/Instagram/LibCST/pull/371)
- Replace matchers with explicit visitation in gatherers [#366](https://github.com/Instagram/LibCST/pull/366)
- Include missing test data in install [#365](https://github.com/Instagram/LibCST/pull/365)

## Fixed
- Spaces around walrus operator are not required [#368](https://github.com/Instagram/LibCST/pull/368)
- SaveMachedNode now matches with trailing empty wildcards [#356](https://github.com/Instagram/LibCST/pull/356)
- Correctly extract wildcard matchers [#355](https://github.com/Instagram/LibCST/pull/355)

# 0.3.9 - 2020-09-07

## Added
 - Support string type annotations in RemoveUnusedImports [#353](https://github.com/Instagram/LibCST/pull/353)
 - Add scope to ImportAlias [#350](https://github.com/Instagram/LibCST/pull/350)
 - Add scope to ClassDef [#349](https://github.com/Instagram/LibCST/pull/349)

## Fixed
 - Fixed all pyre related errors [#360](https://github.com/Instagram/LibCST/pull/360)
 - Fixed enclosing attribute for attributes in call arguments [#362](https://github.com/Instagram/LibCST/pull/362)

# 0.3.8 - 2020-07-22

## Added
 - Handle type subscripts when applying annotations. [#335](https://github.com/Instagram/LibCST/pull/335)
 - Added FullRepoManager `cache` property [#330](https://github.com/Instagram/LibCST/pull/330)
 - Added optional args for tox commands [#327](https://github.com/Instagram/LibCST/pull/327)

## Updated
 - Only remove trailing comma if the last alias is removed [#334](https://github.com/Instagram/LibCST/pull/334)

## Fixed
 - Fixed inserting imports after module docstring [#343](https://github.com/Instagram/LibCST/pull/343)
 - Fixed ParenthesizedWhitespace before params in FuncDef [#342](https://github.com/Instagram/LibCST/pull/342)
 - Fixed validation for ImportAlias and Try statements [#340](https://github.com/Instagram/LibCST/pull/340)
 - Fixed NotEqual position issue [#325](https://github.com/Instagram/LibCST/pull/325)
 - Fixed minor typo in scope_provider.py [#324](https://github.com/Instagram/LibCST/pull/324)

# 0.3.7 - 2020-06-24

## Added
 - Added `RenameCommand` to rename all instances of a local or imported object to a specified new name. [#308](https://github.com/Instagram/LibCST/pull/308)

## Updated
 - Upgraded Codecov dev dependency to 2.1.4. [#311](https://github.com/Instagram/LibCST/pull/311)
 - Enabled Pyre `strict` mode by default. [#313](https://github.com/Instagram/LibCST/pull/313)

## Fixed
 - Fixed `ImportError` under Python 3.9. [#306](https://github.com/Instagram/LibCST/pull/306)
 - Fixed `stdout` being plugged into successfully codemod-ed files. [#309](https://github.com/Instagram/LibCST/pull/309)
 - Fixed `QualifiedName` retrieval for names with repeated substrings. [#312](https://github.com/Instagram/LibCST/pull/312)
 - Fixed default values of keyword-only and positional-only arguments in `ApplyTypeAnnotationsVisitor`. [#314](https://github.com/Instagram/LibCST/pull/314)
 - Fixed `ExpressionContextProvider` by giving subscript values a `LOAD`context. [#319](https://github.com/Instagram/LibCST/pull/319)

# 0.3.6 - 2020-05-27

## Added
 - Added `ConvertNamedTupleToDataclassCommand` to convert `NamedTuple` class declarations to Python 3.7 `dataclasses` using the `@dataclass(frozen=True)` decorator. [#299](https://github.com/Instagram/LibCST/pull/299)

## Fixed
 - Fixed typo in file name `libcst/codemod/commands/convert_percent_format_to_fstring.py`. [#301](https://github.com/Instagram/LibCST/pull/301)
 - Fixed `StopIteration` exception during scope analysis matching on import names. [#302](https://github.com/Instagram/LibCST/pull/302)

# 0.3.5 - 2020-05-12

## Updated
 - Expose more granular `Assignments` and `Accesses` for dotted imports in `ScopeProvider`. [#284](https://github.com/Instagram/LibCST/pull/284)
 - `get_qualified_names_for` returns the most appropriate qualified name. [#290](https://github.com/Instagram/LibCST/pull/290)
 - Surface `SyntaxError` raised by formatter in codemod run. [#288](https://github.com/Instagram/LibCST/pull/288) [#289](https://github.com/Instagram/LibCST/pull/289)
 - Rename `ApplyTypeAnnotationsVisitor.add_stub_to_context` as `ApplyTypeAnnotationsVisitor.store_stub_in_context` and add `overwrite_existing_annotations` to allow overwrite existing type annotations. [#289](https://github.com/Instagram/LibCST/pull/291)

## Fixed
 - Close opened file handles on finishing codemod to avoid `Too many open files` on OSX. [#283](https://github.com/Instagram/LibCST/pull/283)

## Deprecated
 - `ApplyTypeAnnotationsVisitor.add_stub_to_context` is renamed as `ApplyTypeAnnotationsVisitor.store_stub_in_context`.

# 0.3.4 - 2020-03-27

## Added
 - Supported CST parsing for Python 3.0, 3.1 and 3.3. [#261](https://github.com/Instagram/LibCST/pull/261)
 - Added `RemoveUnusedImportsCommand` for removing unused import codemod. [#266](https://github.com/Instagram/LibCST/pull/266)
 - Added `ApplyTypeAnnotationsVisitor.add_stub_to_context` for apply type annotations from stub modules. [#265](https://github.com/Instagram/LibCST/pull/265)

## Updated
 - Improved exception message of `get_metadata` when MetadataWrapper is not used. [#257](https://github.com/Instagram/LibCST/pull/257)
 - New steps for Pyre type check in README.rst which analyzes installed Python sources for better type checking. [#262](https://github.com/Instagram/LibCST/pull/262)

## Fixed
 - Parsed `except(Exception):` correctly while there is no space after except syntax. [#256](https://github.com/Instagram/LibCST/pull/256)
 - Fixed `RemoveImportsVisitor` to not remove imports when references still exist. [#264](https://github.com/Instagram/LibCST/pull/264)
 - Fixed missing type annotations. [#271](https://github.com/Instagram/LibCST/pull/271)
 - `AddImportsVisitor` generates deterministic order for added imports. [#274](https://github.com/Instagram/LibCST/pull/274)

# 0.3.3 - 2020-03-05

## Added
 - `ByteSpanPositionProvider` provides start offset and length of CSTNode as metadata.
 - `get_docstring` helper provides docstring from `Module`, `ClassDef` and `FunctionDef` node types.

## Updated
 - Optimized `ScopeProvider` performance to run faster and use less memory:
   - remove unnecessary `Assignment` of keyword `Arg`.
   - don't provide scope object for formatting information nodes.
   - batch set union updates in `infer_accesses` step.

## Fixed
 - Fixed `_assignments` mutation when calling read-only `Scope.get_qualified_names_for` and `__contains__`.

# 0.3.2 - 2020-02-24

## Added
 - Added `RemoveImportsVisitor` to remove an import if it's not used in a module.
 - Added `GatherExportsVisitor` to gather exports specified in `__all__`.
 - Added property helpers `evaluated_name` and `evaluated_name` in `ImportAlias`.
 - Added helper to get full module name: `get_absolute_module_for_import` and `get_absolute_module_for_import_or_raise`.
 - Added `CodemodContext.full_module_name` for full dotted module name.
 - Added format specifiers f-string conversion support to `ConvertFormatStringCommand`.

## Updated
 - Moved LibCST version to `_version.py` and can print it by `python -m libcst.tool --version`.
 - Improved `EnsureImportPresentCommand` with `--alias` option.
 - Improved `ConvertFormatStringCommand` with `--allow-strip-comments` and `--allow-await` options.

# 0.3.1 - 2020-02-06

## Added

 - Added helpers to get both the raw and evaluated value of a SimpleString.
 - Added helpers to get the quoting and prefix of SimpleString and FormattedString.
 - Added a helper to get the evaluated value of number types.
 - Added templated parsers for statement/expression/module to make constructing updated nodes in transforms easier.
 - FullRepoManager is now integrated into codemods, so metadata requiring full repo analysis can now be used.
 - Added `get_full_name_for_node_or_raise` helper to remove boilerplate of checking against `None`.

## Updated

 - Upgraded Pyre dependency to 0.0.41.
 - Added additional status to `libcst codemod` command.
 - `get_full_name_for_node` now supports decorators.

## Fixed

 - Clarified documentation around f-strings, fixed indentation.
 - Fixed `libcst list` crashing if a codemod does unsafe work on import.
 - Fixed deploy-time dependencies so pyyaml won't have to be manually installed to execute codemods.
 - QualifiedNameProvider no longer erroneously claims names inside attributes are built-ins.

# 0.3.0 - 2020-01-16

## Added

 - Added support for parsing and rendering Python 3.8 source code.
 - Added more documentation for codemods.
 - Added `get_full_name_for_expression` helper method.
 - Added `has_name` helper to `QualifiedNameProvider`.
 - Added a `--python-version` flag to `libcst.tool print` utility.

## Updated

 - Codemod command can now discover codemods in subdirectories of configured modules.
 - Updgraded Pyre dependency to 0.0.39.

## Fixed

 - Cleaned up some typos and formatting issues in comments and documentation.
 - Cleaned up a few redundant typevars.
 - Fixed callable typing in matchers implementation.
 - Fixed incorrect base class references in matcher decorator attribute visitors.
 - Fixed codemod test assertion failing for some whitespace edge cases.
 - Fixed scope analysis to track variable usage on `del` statements.

## Deprecated

 - Deprecated exporting `ensure_type` from `libcst` in favor of `libcst.helpers`.

## Removed

 - Removed `ExtSlice` and helper code in favor of `SubscriptElement`.
 - Removed `default_params` attribute on `Parameters`.
 - Removed `SyntacticPositionProvider` and `BasicPositionProvider`.
 - Removed `CodePosition` and `CodeRange` exports on `libcst` in favor of `libcst.metadata`.

# 0.2.7 - 2020-01-07

## Updated

 - Command-line interface now shows rough estimate of time remaining while executing a codemod.
 - Add needed import now supports import aliases.

# 0.2.6 - 2020-01-01

## Added

 - Added Codemod framework for running code transform over a codebase in parallel.
   - Codemod for code transform logic.
   - CodemodContext for preserving states across transforms.
   - CodemodCommand for CLI interface.
   - CodemodTest for testing codemod easily.
   - yaml codemod config.
   - Pre-build commands in codemod/commands/.
 - Added TypeInferenceProvider for inferred type info from Pyre. A regression test suite was included.
 - Added FullRepoManager for metadata inter-process cache handing.

## Fixed

 - Fixed usage link in README.
 - Fixed type annotation for Mypy compatibility.

## Updated

 - Upgraded Pyre to 0.0.38

# 0.2.5 - 2019-12-05

## Added

 - Added `extract`, `extractall` and `replace` functions to Matchers API.

## Fixed

 - Fixed length restrictions for `AllOf` and `OneOf` so that they can be used with sequence expansion operators.
 - Fixed batchable visitors not calling attribute visit functions.
 - Fixed typos in docstrings.
 - Fixed matcher type exception not being pickleable.

## Deprecated

 - Deprecated parsing function parameters with defaults into `default_params` attribute. They can be found in the `params` attribute instead.

# 0.2.4 - 2019-11-13

## Fixed

 - Fixed broken types for sequence matchers.

# 0.2.3 - 2019-11-11

## Added

 - Preliminary support for 3.8 walrus operator.
 - CI config and fuzz tests for 3.8.
 - Experimental re-entrant codegen API.
 - Added `unsafe_skip_copy` optimization to `MetadataWrapper`.
 - Matchers API now includes a `findall` function.
 - Matchers now have a `MatchMetadataIfTrue` special matcher.

## Updated

 - Updated to latest Black release.
 - Better type documentation for generated matchers.

## Fixed

 - Clarified matchers documentation in several confusing areas.
 - Drastically sped up codegen and tests.
 - `QualifiedName` now supports imported attributtes.
 - `ExpressionContext` properly marks loop variables as `STORE`.
 - Various typos in documentation are fixed.

## Deprecated

 - Deprecated `BasicPositionProvider` and `SyntacticPositionProvider` in favor of `WhitespaceInclusivePositionProvider` and `PositionProvider`.

# 0.2.2 - 2019-10-24

## Added

 - Added `deep_with_changes` helper method on CSTNode.
 - Added metadata support to matchers.
 - Added ability to get the defining node from a `LocalScope` (`FunctionScope`, `ClassScope` or `ComprehensionScope`).

## Updated

 - Many changes to LibCST documentation including a new best practices page and updated scope tutorial.
 - Exported `CodePosition` and `CodeRange` from `libcst.metadata` instead of `libcst`.

## Fixed

 - Disallowed decorating a concrete visit or leave method with `@visit` or `@leave` decorators.
 - Renamed position provider classes to be more self-explanatory.
 - Fixed trailing newline detection when the last character in a file was from a continuation.
 - Fixed `deep_clone` potentially blowing the stack with large LibCST trees.

## Deprecated

 - Deprecated `ExtSlice` in favor of `SubscriptElement`.
 - Deprecated parsing `Subscript` slices directly into `Index` or `Slice` nodes.

# 0.2.1 - 2019-10-14

## Added

 - `Scope.assignments` and `Scope.accesses` APIs to access all references in a scope.
 - Scope analysis tutorial.

## Updated

 - Supported `<comprehension>` in `Scope.get_qualified_names_for` and `QualifiedName`.
 - Enforced identity equality for matchers and immutability of non-dataclass matchers.
 - Generalize codegen cleanup steps for all codegen.

## Fixed
 - Made `BatchableMetadataProvider` typing covariant over its typevar.
 - Fixed LICENSE header on generated matcher file.
 - Cleanup unused internal noqa and on-call specification.

# 0.2.0 - 2019-10-04

## Added

 - Added matchers which allow comparing LibCST trees against arbitrary patterns.
 - Improved tree manipulation with `deep_remove` and `deep_replace` helper methods on CSTNode.
 - Added new metadata providers: parent node and qualified name.

## Updated

 - Updated Pyre to latest release.
 - Updated scope metadata to provide additional helpers.
 - Updated preferred method of removing a node from its parent in a visitor.

## Fixed

 - Metadata classes and methods are now exported from "libcst.metadata" instead of several submodules.
 - Fixed LICENSE file to explicitly reference individual files in the repo with different licenses.
 - Fixed `deep_clone` to correctly clone leaf nodes.
 - Fixed all parse entrypoints to always return a tree with no duplicated leaf nodes.

# 0.1.3 - 2019-09-18

## Added

 - Added preliminary support for parsing Python 3.5 and Python 3.6 source.
 - Added scope analysis metadata provider.
 - Added mypy type support for built package.

## Fixed

 - Several typos in documentation are fixed.

# 0.1.2 - 2019-08-29

## Added

 - Added attribute visitor hooks.
 - Added base visit/leave methods which can be subclassed.
 - Hypothesis fuzz testing suite, courtesy of Zac Hatfield-Dodds.

## Fixed

 - Metadata documentation is much more complete.
 - Fixed several whitespace validation issues caught by Hypothesis.
 - Parser syntax errors are now used inside parser.

# 0.1.1 - 2019-08-20

## Added

- Metadata interface is now exported.

## Fixed

- Dependencies are now specified with minimum revisions.
- Lots of documentation fixes.

# 0.1 - 2019-07-23

## Added

 - First public release of LibCST.
 - Complete, fully typed syntax tree for Python 3.6.
 - Full suite of tests for each defined node type.
