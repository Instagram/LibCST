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
 - Deprecated `BasicPositionProvider` and `SyntacticPositionProvider`. Replaced with new name `WhitespaceInclusivePositionProvider` and `PositionProvider`.

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
