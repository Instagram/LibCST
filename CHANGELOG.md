# 1.5.0 - 2024-10-10

## Added
* FullyQualifiedNameProvider: Optionally consider pyproject.toml files when determining a file's module name and package by @camillol in https://github.com/Instagram/LibCST/pull/1148
* Add validation for If node by @kiri11 in https://github.com/Instagram/LibCST/pull/1177
* include python 3.13 in build by @khameeteman in https://github.com/Instagram/LibCST/pull/1203

## Fixed
* fix various Match statement visitation errors by @zsol in https://github.com/Instagram/LibCST/pull/1161
* Mention codemod -x flag in docs by @kiri11 in https://github.com/Instagram/LibCST/pull/1169
* Clear warnings for each file in codemod cli by @kiri11 in https://github.com/Instagram/LibCST/pull/1184
* Typo fix in codemods_tutorial.rst (trivial) by @wimglenn in https://github.com/Instagram/LibCST/pull/1208
* fix certain matchers breaking under multiprocessing by initializing them late by @kiri11 in https://github.com/Instagram/LibCST/pull/1204

## Updated
* make libcst_native::tokenizer public by @zsol in https://github.com/Instagram/LibCST/pull/1182
* Use `license` instead of `license-file` by @michel-slm in https://github.com/Instagram/LibCST/pull/1189
* Drop codecov from CI and readme by @amyreese in https://github.com/Instagram/LibCST/pull/1192


## New Contributors
* @kiri11 made their first contribution in https://github.com/Instagram/LibCST/pull/1169
* @grievejia made their first contribution in https://github.com/Instagram/LibCST/pull/1174
* @michel-slm made their first contribution in https://github.com/Instagram/LibCST/pull/1189
* @wimglenn made their first contribution in https://github.com/Instagram/LibCST/pull/1208
* @khameeteman made their first contribution in https://github.com/Instagram/LibCST/pull/1203

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v1.4.0...v1.5.0

# 1.4.0 - 2024-05-22

## Fixed
* Fix Literal parse error in RemoveImportsVisitor by @camillol in https://github.com/Instagram/LibCST/pull/1130
* Don't reset context.scratch between files by @zsol in https://github.com/Instagram/LibCST/pull/1151
* Various documentation fixes
  * Typo fix FullRepoManager by @kit1980 in https://github.com/Instagram/LibCST/pull/1138
  * ✏️ Fix tiny typo in `docs/source/metadata.rst` by @tiangolo in https://github.com/Instagram/LibCST/pull/1134
  * ✏️ Fix typo in `docs/source/scope_tutorial.ipynb` by @tiangolo in https://github.com/Instagram/LibCST/pull/1135
  * Update CONTRIBUTING.md by @zaicruvoir1rominet in https://github.com/Instagram/LibCST/pull/1142

## Added

* Add helper functions for common ways of filtering nodes by @zaicruvoir1rominet in https://github.com/Instagram/LibCST/pull/1137
* Dump CST to .dot (graphviz) files by @zaicruvoir1rominet in https://github.com/Instagram/LibCST/pull/1147
* Implement PEP-696 by @thereversiblewheel in https://github.com/Instagram/LibCST/pull/1141

## New Contributors
* @tiangolo made their first contribution in https://github.com/Instagram/LibCST/pull/1134
* @camillol made their first contribution in https://github.com/Instagram/LibCST/pull/1130
* @zaicruvoir1rominet made their first contribution in https://github.com/Instagram/LibCST/pull/1142
* @thereversiblewheel made their first contribution in https://github.com/Instagram/LibCST/pull/1141

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v1.3.1...v1.4.0

# 1.3.1 - 2024-04-03

## Fixed
* ImportError due to missing `mypy_extensions` dependency by @zsol in https://github.com/Instagram/LibCST/pull/1128

# 1.3.0 - 2024-04-03

## Updated
* Removed dependencies on `typing_extensions` and `typing_inspect` by @zsol in https://github.com/Instagram/LibCST/pull/1126

# 1.2.0 - 2024-02-19

## Updated
* Support running LibCST on Python 3.12 and drop support for running it on 3.8
  * remove 3.8 support by @zsol in https://github.com/Instagram/LibCST/pull/1073
  * Remove reference to distutils by @zsol in https://github.com/Instagram/LibCST/pull/1099
  * Update pyproject.toml for Python 3.12 support by @itamaro in https://github.com/Instagram/LibCST/pull/1038

## Added
* Allow `Element::codegen` to be used by external users by @Wilfred in https://github.com/Instagram/LibCST/pull/1071

## Fixed
* Fix parsing list matchers without explicit brackets by @zsol in https://github.com/Instagram/LibCST/pull/1097
* installing rustc/cargo for mybinder demo by @aleivag in https://github.com/Instagram/LibCST/pull/1083
* fix filepathprovider generic type by @kinto0 in https://github.com/Instagram/LibCST/pull/1036

## New Contributors
* @itamaro made their first contribution in https://github.com/Instagram/LibCST/pull/1039
* @kinto0 made their first contribution in https://github.com/Instagram/LibCST/pull/1036
* @dtolnay made their first contribution in https://github.com/Instagram/LibCST/pull/1063
* @anonymousdouble made their first contribution in https://github.com/Instagram/LibCST/pull/1082
* @aleivag made their first contribution in https://github.com/Instagram/LibCST/pull/1083
* @Wilfred made their first contribution in https://github.com/Instagram/LibCST/pull/1071
* @diliop made their first contribution in https://github.com/Instagram/LibCST/pull/1106

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v1.1.0...v1.2.0

# 1.1.0 - 2023-10-05

## Added
* PEP 695 support
  * parser: PEP 695 - Type Parameter Syntax #1004
  * Scope provider: support for type annotations #1014
* PEP 701 support
  * parser: support arbitrarily nested f-strings #1026
  * parser: Parse multiline expressions in f-strings #1027
* parser: Support files with mixed newlines #1007
* [libcst](https://crates.io/crates/libcst) is now published to crates.io

## Fixed
* codemod/ApplyTypeAnnotationsVisitor: Do not annotate the same variable multiple times #956
* parser: Don't swallow trailing whitespace #976
* codemod/rename: Avoid duplicating import statements when the module name doesn't change #981

## Updated
* cli: Don't gather dirs ending .py #994
* drop support for Python 3.7 #997
* A few parser performance improvements:
  * Switch to using thread_local regular expressions to stop mutext contention #996
  * Remove need for regex in TextPosition::matches #1002
  * Remove Regexes from whitespace parser #1008

# 1.0.1 - 2023-06-07

## Fixed
* Fix type of `evaluated_value` on string to allow bytes by @ljodal in https://github.com/Instagram/LibCST/pull/721
* Fix Sentinal typo by @kit1980 in https://github.com/Instagram/LibCST/pull/948
* Allow no whitespace after lambda body in certain cases by @zsol in https://github.com/Instagram/LibCST/pull/939
* Fix whitespace, fstring, walrus related parse errors (#939, #938, #937,
#936, #935, #934, #933, #932, #931) by @zsol in https://github.com/Instagram/LibCST/pull/940
* Codemod CLI: Print diff only when there is a change by @kit1980 in https://github.com/Instagram/LibCST/pull/945

## New Contributors
* @ljodal made their first contribution in https://github.com/Instagram/LibCST/pull/721
* @kit1980 made their first contribution in https://github.com/Instagram/LibCST/pull/948

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v1.0.0...v1.0.1

# 1.0.0 - 2023-05-25

The first major release of LibCST is essentially the same as 0.4.10, but using the
newer, Rust-based parser implementation by default. The old, pure Python parser is
scheduled for removal in the next (non-patch) release. Until then, it is available with
the `LIBCST_PARSER_TYPE` environment variable set to `pure`.

## Updated

* Switch the default parser implementation to native by @zsol in https://github.com/Instagram/LibCST/pull/929

# 0.4.10 - 2023-05-23

## New Contributors
* @and-semakin made their first contribution in https://github.com/Instagram/LibCST/pull/816
* @carljm made their first contribution in https://github.com/Instagram/LibCST/pull/828
* @sagarbadiyani made their first contribution in https://github.com/Instagram/LibCST/pull/841
* @podtserkovskiy made their first contribution in https://github.com/Instagram/LibCST/pull/894
* @rchen152 made their first contribution in https://github.com/Instagram/LibCST/pull/903
* @Kludex made their first contribution in https://github.com/Instagram/LibCST/pull/913
* @jakkdl made their first contribution in https://github.com/Instagram/LibCST/pull/921

## Added
* Add py3.11 classifier by @and-semakin in https://github.com/Instagram/LibCST/pull/816
* Script to regenerate test fixtures, upgrade to Pyre 0.9.10 by @amyreese in https://github.com/Instagram/LibCST/pull/872
* Allow FullyQualifiedNameProvider to work with absolute paths by @amyreese in https://github.com/Instagram/LibCST/pull/867
* Allow running codemods without configuring in YAML by @akx in https://github.com/Instagram/LibCST/pull/879
* Support PEP 604 in ApplyTypeAnnotationsVisitor by @hauntsaninja in https://github.com/Instagram/LibCST/pull/868

## Fixed
* fix PEP 604 union annotations in decorators by @carljm in https://github.com/Instagram/LibCST/pull/828
* [AddImportsVisitor] Docstring Check Only for the Top Element of the Body by @sagarbadiyani in https://github.com/Instagram/LibCST/pull/841
* Fix [#855](https://github.com/Instagram/LibCST/issues/855) - fail to parse with statement by @stroxler in https://github.com/Instagram/LibCST/pull/861
* Add setuptools-rust to build requirements in setup.py by @amyreese in https://github.com/Instagram/LibCST/pull/873
* Relative imports from '' package are not allowed by @podtserkovskiy in https://github.com/Instagram/LibCST/pull/894
* Use subprocess.DEVNULL instead of opening os.devnull by hand by @akx in https://github.com/Instagram/LibCST/pull/897
* Ensure current Python interpreter is used for subprocesses by @akx in https://github.com/Instagram/LibCST/pull/898
* Fix ApplyTypeAnnotationsVisitor behavior on attribute assignments. by @rchen152 in https://github.com/Instagram/LibCST/pull/903
* Fix spelling and grammar in some comments by @stroxler in https://github.com/Instagram/LibCST/pull/908
* skip escaped backslash in rf-string by @jakkdl in https://github.com/Instagram/LibCST/pull/921
* relax validation rules on decorators by @jakkdl in https://github.com/Instagram/LibCST/pull/926

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v0.4.9...v0.4.10

# 0.4.9 - 2022-11-10

## Updated
* Bump setuptools-rust version by @zsol in https://github.com/Instagram/LibCST/pull/809

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v0.4.8...v0.4.9

# 0.4.8 - 2022-11-10

## New Contributors
* @dhruvmanila made their first contribution in https://github.com/Instagram/LibCST/pull/728
* @vfazio made their first contribution in https://github.com/Instagram/LibCST/pull/801
* @matthewshaer made their first contribution in https://github.com/Instagram/LibCST/pull/807


## Fixed
* Fix parse error message for number parsing by @zzl0 in https://github.com/Instagram/LibCST/pull/724
* Fix problematic doc build, due to the new builder image provided by readthedocs doesn't has the `graphviz-dev` package pre-installed any more by @MapleCCC in https://github.com/Instagram/LibCST/pull/751
* Fix docstring of `FullRepoManager` by @MapleCCC in https://github.com/Instagram/LibCST/pull/750
* Fix bug when `TypeOf` is one of options in `OneOf` / `AllOf` by @MapleCCC in https://github.com/Instagram/LibCST/pull/756
* Tighten the metadata type of `ExpressionContextProvider` by @MapleCCC in https://github.com/Instagram/LibCST/pull/760
* Fix the bug that the use of formatter in codemods has undetermined target Python version, resulting in hard-to-reason-with behavior by @MapleCCC in https://github.com/Instagram/LibCST/pull/771


## Added
* Python 3.11 rutime support
  * test using python 3.11 beta versions by @zsol in https://github.com/Instagram/LibCST/pull/723
  * Python 3.11 wheels by @vfazio in https://github.com/Instagram/LibCST/pull/801
* Raise informative exception when metadata is unresolved in a metadata-based match by @MapleCCC in https://github.com/Instagram/LibCST/pull/757
* Add AccessorProvider by @matthewshaer in https://github.com/Instagram/LibCST/pull/807

**Full Changelog**: https://github.com/Instagram/LibCST/compare/v0.4.7...v0.4.8

# 0.4.7 - 2022-07-12

## New Contributors
* @Chenguang-Zhu made their first contribution in https://github.com/Instagram/LibCST/pull/720

## Fixed
* Fix get_qualified_names_for matching on prefixes of the given name by @lpetre in https://github.com/Instagram/LibCST/pull/719

## Added
* Implement lazy loading mechanism for expensive metadata providers by @Chenguang-Zhu in https://github.com/Instagram/LibCST/pull/720

# 0.4.6 - 2022-07-04

## New Contributors
- @superbobry made their first contribution in https://github.com/Instagram/LibCST/pull/702

## Fixed
- convert_type_comments now preserves comments following type comments by @superbobry in https://github.com/Instagram/LibCST/pull/702
- QualifiedNameProvider optimizations
  - Cache the scope name prefix to prevent scope traversal in a tight loop by @lpetre in https://github.com/Instagram/LibCST/pull/708
  - Faster qualified name formatting by @lpetre in https://github.com/Instagram/LibCST/pull/710
  - Prevent unnecessary work in Scope.get_qualified_names_for_ by @lpetre in https://github.com/Instagram/LibCST/pull/709
- Fix parsing of parenthesized empty tuples by @zsol in https://github.com/Instagram/LibCST/pull/712
- Support whitespace after ParamSlash by @zsol in https://github.com/Instagram/LibCST/pull/713
- [parser] bail on deeply nested expressions by @zsol in https://github.com/Instagram/LibCST/pull/718

# 0.4.5 - 2022-06-17

## New Contributors

-   @zzl0 made their first contribution in https://github.com/Instagram/LibCST/pull/704

## Fixed

-   Only skip supported escaped characters in f-strings by @zsol in https://github.com/Instagram/LibCST/pull/700
-   Escaping quote characters in raw string literals causes a tokenizer error by @zsol in https://github.com/Instagram/LibCST/issues/668
-   Corrected a code example in the documentation by @zzl0 in https://github.com/Instagram/LibCST/pull/703
-   Handle multiline strings that start with quotes by @zzl0 in https://github.com/Instagram/LibCST/pull/704
-   Fixed a performance regression in libcst.metadata.ScopeProvider by @lpetre in https://github.com/Instagram/LibCST/pull/698

# 0.4.4 - 2022-06-13

## New Contributors

-   @adamchainz made their first contribution in https://github.com/Instagram/LibCST/pull/688

## Added

-   Add package links to PyPI by @adamchainz in https://github.com/Instagram/LibCST/pull/688
-   native: add overall benchmark by @zsol in https://github.com/Instagram/LibCST/pull/692
-   Add support for PEP-646 by @zsol in https://github.com/Instagram/LibCST/pull/696

## Updated

-   parser: use references instead of smart pointers for Tokens by @zsol in https://github.com/Instagram/LibCST/pull/691

# 0.4.3 - 2022-05-11

## Fixed

-   Restore the 0.4.1 behavior for libcst.helpers.get_absolute_module by @lpetre in https://github.com/Instagram/LibCST/pull/684

# 0.4.2 - 2022-05-04

## New Contributors

-   @stanislavlevin made their first contribution in https://github.com/Instagram/LibCST/pull/650
-   @dmitryvinn made their first contribution in https://github.com/Instagram/LibCST/pull/655
-   @wiyr made their first contribution in https://github.com/Instagram/LibCST/pull/669
-   @toofar made their first contribution in https://github.com/Instagram/LibCST/pull/675

## Fixed

-   native: Avoid crashing by making IntoPy conversion fallible by @zsol in https://github.com/Instagram/LibCST/pull/639
-   native: make sure ParserError's line is zero-indexed by @zsol in https://github.com/Instagram/LibCST/pull/681
-   Fix space validation for AsName and Await by @zsol in https://github.com/Instagram/LibCST/pull/641
-   Qualified Name Provider: Fix returned qname for symbols that are prefixes of each other by @wiyr in https://github.com/Instagram/LibCST/pull/669
-   Rename Codemod: Correct last renamed import from by @toofar in https://github.com/Instagram/LibCST/pull/675
-   Many changes to the Apply Type Comments codemod:
    -   Allow for skipping quotes when applying type comments by @stroxler in https://github.com/Instagram/LibCST/pull/644
    -   Port pyre fixes by @stroxler in https://github.com/Instagram/LibCST/pull/651
    -   Preserve as-imports when merging type annotations. by @martindemello in https://github.com/Instagram/LibCST/pull/664
    -   Qualify imported symbols when the dequalified form would cause a conflict by @martindemello in https://github.com/Instagram/LibCST/pull/674
    -   Add an argument to always qualify imported type annotations. by @martindemello in https://github.com/Instagram/LibCST/pull/676

## Added

-   Create an AddTrailingCommas codemod by @stroxler in https://github.com/Instagram/LibCST/pull/643
-   Define gather global names visitor by @shannonzhu in https://github.com/Instagram/LibCST/pull/657

## Updated

-   Support module and package names in the codemod context by @lpetre in https://github.com/Instagram/LibCST/pull/662
-   Drop support for running libcst using a python 3.6 interpreter by @lpetre in https://github.com/Instagram/LibCST/pull/663
-   Update relative import logic to match cpython by @lpetre in https://github.com/Instagram/LibCST/pull/660
-   Scope Provider: Consider access information when computing qualified names for nodes by @lpetre in https://github.com/Instagram/LibCST/pull/682

# 0.4.1 - 2022-01-28

## New Contributors

-   @ariebovenberg made their first contribution in https://github.com/Instagram/LibCST/pull/605
-   @sehz made their first contribution in https://github.com/Instagram/LibCST/pull/598

## Added

-   Add docs about the native parts by @zsol in https://github.com/Instagram/LibCST/pull/601
-   Specify minimum rust toolchain version by @zsol in https://github.com/Instagram/LibCST/pull/614
-   build wheels on main branch for linux/arm64 by @zsol in https://github.com/Instagram/LibCST/pull/630

## Updated

-   ApplyTypeAnnotationVisitor changes
    -   Add support for methods with func type comment excluding self/cls by @stroxler in https://github.com/Instagram/LibCST/pull/622
    -   Merge in TypeVars and Generic base classes in ApplyTypeAnnotationVisitor by @martindemello in https://github.com/Instagram/LibCST/pull/596
    -   Full handling for applying type comments to Assign by @stroxler in https://github.com/Instagram/LibCST/pull/599
    -   Add support for For and With by @stroxler in https://github.com/Instagram/LibCST/pull/607
    -   Support FunctionDef transformations by @stroxler in https://github.com/Instagram/LibCST/pull/610
-   change pyo3 as optional dependency in native Python Parser by @sehz in https://github.com/Instagram/LibCST/pull/598
-   add slots to base classes, @add_slots takes bases into account by @ariebovenberg in https://github.com/Instagram/LibCST/pull/605
-   [native] Box most enums by @zsol in https://github.com/Instagram/LibCST/pull/632
-   [native] Return tuples instead of lists in CST nodes by @zsol in https://github.com/Instagram/LibCST/pull/631

## Fixed

-   Allow trailing whitespace without newline at EOF by @zsol in https://github.com/Instagram/LibCST/pull/611
-   Handle ast.parse failures when converting function type comments by @stroxler in https://github.com/Instagram/LibCST/pull/616
-   [native] Don't redundantly nest StarredElement inside another Element by @isidentical in https://github.com/Instagram/LibCST/pull/624
-   [native] Allow unparenthesized tuples inside f-strings by @isidentical in https://github.com/Instagram/LibCST/pull/621
-   Don't require whitespace right after match by @isidentical in https://github.com/Instagram/LibCST/pull/628
-   Proxy both parentheses in some pattern matching nodes by @isidentical in https://github.com/Instagram/LibCST/pull/626

# 0.4.0 - 2022-01-12

This release contains a new parsing infrastructure that is turned off by default. You
can enable it by setting the `LIBCST_PARSER_TYPE` environment variable to `native`
before parsing an input with the usual LibCST APIs. Parsing Python 3.10 documents is
only supported in this new mode.

Note: the new parser is built as a native extension, so LibCST will ship with binary
wheels from now on.

## Added

-   Implement a Python PEG parser in Rust by @zsol in [#566](https://github.com/Instagram/LibCST/pull/566)
-   implement PEP-654: except\* by @zsol in [#571](https://github.com/Instagram/LibCST/pull/571)
-   Implement PEP-634 - Match statement by @zsol in [#568](https://github.com/Instagram/LibCST/pull/568)
-   Add instructions to codegen test failures by @stroxler in [#582](https://github.com/Instagram/LibCST/pull/582)
-   Support Parenthesized With Statements by @stroxler in [#584](https://github.com/Instagram/LibCST/pull/584)
-   Support relative imports in AddImportsVisitor by @martindemello in [#585](https://github.com/Instagram/LibCST/pull/585)
-   Codemod for PEP 484 Assign w / type comments -> PEP 526 AnnAssign by @stroxler in [#594](https://github.com/Instagram/LibCST/pull/594)

## Updated

-   Update license headers by @zsol in [#560](https://github.com/Instagram/LibCST/pull/560)
-   Use precise signature matching when inserting function type annotations by @martindemello in [#591](https://github.com/Instagram/LibCST/pull/591)

# 0.3.23 - 2021-11-23

## Fixed

-   Fix missing string annotation references [#561](https://github.com/Instagram/LibCST/pull/561)

# 0.3.22 - 2021-11-22

## Added

-   Add --indent-string option to `libcst.tool print` [#525](https://github.com/Instagram/LibCST/pull/525)
-   Publish pre-release packages to test.pypi.org [#550](https://github.com/Instagram/LibCST/pull/550)
-   Add ImportAssignment class extending Assignment to record assignments for import statements [#554](https://github.com/Instagram/LibCST/pull/554)

## Fixed

-   Various documentation fixes [#527](https://github.com/Instagram/LibCST/pull/527), [#529](https://github.com/Instagram/LibCST/pull/529)
-   Do not add imports if we added no type info in ApplyTypeAnnotationVisitor [(commit)](https://github.com/Instagram/LibCST/commit/87625d02b6cb321c9c29ba1c67d81ce954a1a396)
-   Support relative imports in ApplyTypeAnnotationVisitor qualifier handling [#538](https://github.com/Instagram/LibCST/pull/538)
-   Don't gather metadata if the wrapper already contains it [#545](https://github.com/Instagram/LibCST/pull/545)
-   Swallow parsing errors in string annotations [#548](https://github.com/Instagram/LibCST/pull/548)
-   Stop parsing string annotations when no longer in a typing call [#546](https://github.com/Instagram/LibCST/pull/546)

## Updated

-   Move find_qualified_names_for in the Assignment class [#557](https://github.com/Instagram/LibCST/pull/557)

# 0.3.21 - 2021-09-21

## Fixed

-   Fix pyre command for type inference provider [#523](https://github.com/Instagram/LibCST/pull/523)

## Updated

-   Change codegen to treat typing.Union[Foo, NoneType] and typing.Optional[Foo] as the same [#508]((https://github.com/Instagram/LibCST/pull/508)
-   Rewrite the MatchIfTrue type to be generic on \_MatchIfTrueT [#512](https://github.com/Instagram/LibCST/pull/512)
-   Add python3.9 to the CI [#506](https://github.com/Instagram/LibCST/pull/506)
-   Various CI changes [#471](https://github.com/Instagram/LibCST/pull/471) [#510](https://github.com/Instagram/LibCST/pull/510) [#505](https://github.com/Instagram/LibCST/pull/505) [#515](https://github.com/Instagram/LibCST/pull/515) [#516](https://github.com/Instagram/LibCST/pull/516)

# 0.3.20 - 2021-08-09

## Fixed

-   Don't reset subprocess environment to fix codemodding on windows [#495](https://github.com/Instagram/LibCST/pull/495)
-   TypeAnnotationsVisitor: don't truncate function return type [#499](https://github.com/Instagram/LibCST/pull/499)
-   Docs: Fix typo [#492](https://github.com/Instagram/LibCST/pull/492)

# 0.3.19 - 2021-05-12

# Updated

-   Return more specific QNames for assignments [#477](https://github.com/Instagram/LibCST/pull/477)
-   Tie accesses from string annotation to the string node [#483](https://github.com/Instagram/LibCST/pull/483)

## Fixed

-   Fix leaking processes from TypeInferenceProvider [#474](https://github.com/Instagram/LibCST/pull/474)
-   Fix TypeInferenceProvider breakage with empty cache [#476](https://github.com/Instagram/LibCST/pull/476)
-   Fix formatting for link to QualifiedName class in docs [#480](https://github.com/Instagram/LibCST/pull/480)

# 0.3.18 - 2021-03-29

## Added

-   Add FlattenSentinel to support replacing a statement with multiple statements [#455](https://github.com/Instagram/LibCST/pull/455)
-   Add BuiltinScope [#469](https://github.com/Instagram/LibCST/pull/469)
-   Add FullyQualifiedNameProvider [#465](https://github.com/Instagram/LibCST/pull/465)

## Updated

-   Split QualifiedNameProvider out from libcst.metadata.scope_provider [#464](https://github.com/Instagram/LibCST/pull/464)

## Fixed

-   Exception while parsing escape character in raw f-strings [#462](https://github.com/Instagram/LibCST/issues/462)

# 0.3.17 - 2021-02-08

## Updated

-   Optimization: reduce the number of unused parallel processes [#440](https://github.com/Instagram/LibCST/pull/440)

## Fixed

-   Walrus operator's left hand side now has STORE expression context [#443](https://github.com/Instagram/LibCST/pull/433)
-   ApplyTypeAnnotationsVisitor applies parameter annotations even if no return type is declared [#445](https://github.com/Instagram/LibCST/pull/445)
-   Work around Windows problem by using dummy pool for `jobs=1` [#436](https://github.com/Instagram/LibCST/pull/436)
-   Remove extra unused imports added in other files [#453](https://github.com/Instagram/LibCST/pull/453)

# 0.3.16 - 2020-12-16

## Added

-   Support PEP-604 style unions in decorator annotations [#429](https://github.com/Instagram/LibCST/pull/429)
-   Gathering exports in augmented assignment statements [#426](https://github.com/Instagram/LibCST/pull/426)

## Fixed

-   Don't allow out of order accesses in the global scope [#431](https://github.com/Instagram/LibCST/pull/431)
-   Handle scope ordering in For statements [#430](https://github.com/Instagram/LibCST/pull/430)
-   Fix for not parsing subscripts such as `cast()["from"]` [#428](https://github.com/Instagram/LibCST/pull/428)
-   Walrus operator's left hand side now has STORE expression context [#433](https://github.com/Instagram/LibCST/pull/433)

# 0.3.15 - 2020-12-01

## Added

-   Support Named Unicode Characters and yield in f-strings [#424](https://github.com/Instagram/LibCST/pull/424)

## Fixed

-   Assignment/access ordering in comprehensions [#423](https://github.com/Instagram/LibCST/pull/423)
-   Referencing of remaining objects in cast() [#422](https://github.com/Instagram/LibCST/pull/422)

# 0.3.14 - 2020-11-18

## Fixed

-   Fix is_annotation for types used in classdef base and assign value [#406](https://github.com/Instagram/LibCST/pull/406)
-   Visit concatenated f-strings during scope analysis [#411](https://github.com/Instagram/LibCST/pull/411)
-   Correct handling of walrus operator in function args [#417](https://github.com/Instagram/LibCST/pull/417)
-   Allow generator expressions in f-strings [#419](https://github.com/Instagram/LibCST/pull/419)
-   Keep track of assignment/access ordering during scope analysis [#413](https://github.com/Instagram/LibCST/pull/413)
-   Handle string type references in cast() during scope analysis [#418](https://github.com/Instagram/LibCST/pull/418)

# 0.3.13 - 2020-10-12

## Fixed

-   Use correct type for AugAssign and AnnAssign target [#396](https://github.com/Instagram/LibCST/pull/396)
-   Support string annotations for type aliases [#401](https://github.com/Instagram/LibCST/pull/401)

# 0.3.12 - 2020-10-01

## Fixed

-   fix RemoveImportsVisitor crash when ImportAlias is inserted without comma [#397](https://github.com/Instagram/LibCST/pull/397)
-   Provide STORE for {Class,Function}Def.name in ExpressionContextProvider [#394](https://github.com/Instagram/LibCST/pull/394)

# 0.3.11 - 2020-09-29

## Added

-   Implement TypeOf matcher [#384](https://github.com/Instagram/LibCST/pull/384)

## Updated

-   Update return type of ParentNodeProvider to be CSTNode [#377](https://github.com/Instagram/LibCST/pull/377)
-   Add source code links to each class/function [#378](https://github.com/Instagram/LibCST/pull/378)

## Fixed

-   Removing an import alias with a trailing standalone comment should preserve the comment [#392](https://github.com/Instagram/LibCST/pull/392)

# 0.3.10 - 2020-09-17

## Added

-   Handle string annotations in ScopeProvider [#373](https://github.com/Instagram/LibCST/pull/373)
-   Add is_annotation subtype for Access inreferences. [#372](https://github.com/Instagram/LibCST/pull/372)

## Updated

-   Call pyre query with noninteractive logging [#371](https://github.com/Instagram/LibCST/pull/371)
-   Replace matchers with explicit visitation in gatherers [#366](https://github.com/Instagram/LibCST/pull/366)
-   Include missing test data in install [#365](https://github.com/Instagram/LibCST/pull/365)

## Fixed

-   Spaces around walrus operator are not required [#368](https://github.com/Instagram/LibCST/pull/368)
-   SaveMachedNode now matches with trailing empty wildcards [#356](https://github.com/Instagram/LibCST/pull/356)
-   Correctly extract wildcard matchers [#355](https://github.com/Instagram/LibCST/pull/355)

# 0.3.9 - 2020-09-07

## Added

-   Support string type annotations in RemoveUnusedImports [#353](https://github.com/Instagram/LibCST/pull/353)
-   Add scope to ImportAlias [#350](https://github.com/Instagram/LibCST/pull/350)
-   Add scope to ClassDef [#349](https://github.com/Instagram/LibCST/pull/349)

## Fixed

-   Fixed all pyre related errors [#360](https://github.com/Instagram/LibCST/pull/360)
-   Fixed enclosing attribute for attributes in call arguments [#362](https://github.com/Instagram/LibCST/pull/362)

# 0.3.8 - 2020-07-22

## Added

-   Handle type subscripts when applying annotations. [#335](https://github.com/Instagram/LibCST/pull/335)
-   Added FullRepoManager `cache` property [#330](https://github.com/Instagram/LibCST/pull/330)
-   Added optional args for tox commands [#327](https://github.com/Instagram/LibCST/pull/327)

## Updated

-   Only remove trailing comma if the last alias is removed [#334](https://github.com/Instagram/LibCST/pull/334)

## Fixed

-   Fixed inserting imports after module docstring [#343](https://github.com/Instagram/LibCST/pull/343)
-   Fixed ParenthesizedWhitespace before params in FuncDef [#342](https://github.com/Instagram/LibCST/pull/342)
-   Fixed validation for ImportAlias and Try statements [#340](https://github.com/Instagram/LibCST/pull/340)
-   Fixed NotEqual position issue [#325](https://github.com/Instagram/LibCST/pull/325)
-   Fixed minor typo in scope_provider.py [#324](https://github.com/Instagram/LibCST/pull/324)

# 0.3.7 - 2020-06-24

## Added

-   Added `RenameCommand` to rename all instances of a local or imported object to a specified new name. [#308](https://github.com/Instagram/LibCST/pull/308)

## Updated

-   Upgraded Codecov dev dependency to 2.1.4. [#311](https://github.com/Instagram/LibCST/pull/311)
-   Enabled Pyre `strict` mode by default. [#313](https://github.com/Instagram/LibCST/pull/313)

## Fixed

-   Fixed `ImportError` under Python 3.9. [#306](https://github.com/Instagram/LibCST/pull/306)
-   Fixed `stdout` being plugged into successfully codemod-ed files. [#309](https://github.com/Instagram/LibCST/pull/309)
-   Fixed `QualifiedName` retrieval for names with repeated substrings. [#312](https://github.com/Instagram/LibCST/pull/312)
-   Fixed default values of keyword-only and positional-only arguments in `ApplyTypeAnnotationsVisitor`. [#314](https://github.com/Instagram/LibCST/pull/314)
-   Fixed `ExpressionContextProvider` by giving subscript values a `LOAD`context. [#319](https://github.com/Instagram/LibCST/pull/319)

# 0.3.6 - 2020-05-27

## Added

-   Added `ConvertNamedTupleToDataclassCommand` to convert `NamedTuple` class declarations to Python 3.7 `dataclasses` using the `@dataclass(frozen=True)` decorator. [#299](https://github.com/Instagram/LibCST/pull/299)

## Fixed

-   Fixed typo in file name `libcst/codemod/commands/convert_percent_format_to_fstring.py`. [#301](https://github.com/Instagram/LibCST/pull/301)
-   Fixed `StopIteration` exception during scope analysis matching on import names. [#302](https://github.com/Instagram/LibCST/pull/302)

# 0.3.5 - 2020-05-12

## Updated

-   Expose more granular `Assignments` and `Accesses` for dotted imports in `ScopeProvider`. [#284](https://github.com/Instagram/LibCST/pull/284)
-   `get_qualified_names_for` returns the most appropriate qualified name. [#290](https://github.com/Instagram/LibCST/pull/290)
-   Surface `SyntaxError` raised by formatter in codemod run. [#288](https://github.com/Instagram/LibCST/pull/288) [#289](https://github.com/Instagram/LibCST/pull/289)
-   Rename `ApplyTypeAnnotationsVisitor.add_stub_to_context` as `ApplyTypeAnnotationsVisitor.store_stub_in_context` and add `overwrite_existing_annotations` to allow overwrite existing type annotations. [#289](https://github.com/Instagram/LibCST/pull/291)

## Fixed

-   Close opened file handles on finishing codemod to avoid `Too many open files` on OSX. [#283](https://github.com/Instagram/LibCST/pull/283)

## Deprecated

-   `ApplyTypeAnnotationsVisitor.add_stub_to_context` is renamed as `ApplyTypeAnnotationsVisitor.store_stub_in_context`.

# 0.3.4 - 2020-03-27

## Added

-   Supported CST parsing for Python 3.0, 3.1 and 3.3. [#261](https://github.com/Instagram/LibCST/pull/261)
-   Added `RemoveUnusedImportsCommand` for removing unused import codemod. [#266](https://github.com/Instagram/LibCST/pull/266)
-   Added `ApplyTypeAnnotationsVisitor.add_stub_to_context` for apply type annotations from stub modules. [#265](https://github.com/Instagram/LibCST/pull/265)

## Updated

-   Improved exception message of `get_metadata` when MetadataWrapper is not used. [#257](https://github.com/Instagram/LibCST/pull/257)
-   New steps for Pyre type check in README.rst which analyzes installed Python sources for better type checking. [#262](https://github.com/Instagram/LibCST/pull/262)

## Fixed

-   Parsed `except(Exception):` correctly while there is no space after except syntax. [#256](https://github.com/Instagram/LibCST/pull/256)
-   Fixed `RemoveImportsVisitor` to not remove imports when references still exist. [#264](https://github.com/Instagram/LibCST/pull/264)
-   Fixed missing type annotations. [#271](https://github.com/Instagram/LibCST/pull/271)
-   `AddImportsVisitor` generates deterministic order for added imports. [#274](https://github.com/Instagram/LibCST/pull/274)

# 0.3.3 - 2020-03-05

## Added

-   `ByteSpanPositionProvider` provides start offset and length of CSTNode as metadata.
-   `get_docstring` helper provides docstring from `Module`, `ClassDef` and `FunctionDef` node types.

## Updated

-   Optimized `ScopeProvider` performance to run faster and use less memory:
    -   remove unnecessary `Assignment` of keyword `Arg`.
    -   don't provide scope object for formatting information nodes.
    -   batch set union updates in `infer_accesses` step.

## Fixed

-   Fixed `_assignments` mutation when calling read-only `Scope.get_qualified_names_for` and `__contains__`.

# 0.3.2 - 2020-02-24

## Added

-   Added `RemoveImportsVisitor` to remove an import if it's not used in a module.
-   Added `GatherExportsVisitor` to gather exports specified in `__all__`.
-   Added property helpers `evaluated_name` and `evaluated_name` in `ImportAlias`.
-   Added helper to get full module name: `get_absolute_module_for_import` and `get_absolute_module_for_import_or_raise`.
-   Added `CodemodContext.full_module_name` for full dotted module name.
-   Added format specifiers f-string conversion support to `ConvertFormatStringCommand`.

## Updated

-   Moved LibCST version to `_version.py` and can print it by `python -m libcst.tool --version`.
-   Improved `EnsureImportPresentCommand` with `--alias` option.
-   Improved `ConvertFormatStringCommand` with `--allow-strip-comments` and `--allow-await` options.

# 0.3.1 - 2020-02-06

## Added

-   Added helpers to get both the raw and evaluated value of a SimpleString.
-   Added helpers to get the quoting and prefix of SimpleString and FormattedString.
-   Added a helper to get the evaluated value of number types.
-   Added templated parsers for statement/expression/module to make constructing updated nodes in transforms easier.
-   FullRepoManager is now integrated into codemods, so metadata requiring full repo analysis can now be used.
-   Added `get_full_name_for_node_or_raise` helper to remove boilerplate of checking against `None`.

## Updated

-   Upgraded Pyre dependency to 0.0.41.
-   Added additional status to `libcst codemod` command.
-   `get_full_name_for_node` now supports decorators.

## Fixed

-   Clarified documentation around f-strings, fixed indentation.
-   Fixed `libcst list` crashing if a codemod does unsafe work on import.
-   Fixed deploy-time dependencies so pyyaml won't have to be manually installed to execute codemods.
-   QualifiedNameProvider no longer erroneously claims names inside attributes are built-ins.

# 0.3.0 - 2020-01-16

## Added

-   Added support for parsing and rendering Python 3.8 source code.
-   Added more documentation for codemods.
-   Added `get_full_name_for_expression` helper method.
-   Added `has_name` helper to `QualifiedNameProvider`.
-   Added a `--python-version` flag to `libcst.tool print` utility.

## Updated

-   Codemod command can now discover codemods in subdirectories of configured modules.
-   Updgraded Pyre dependency to 0.0.39.

## Fixed

-   Cleaned up some typos and formatting issues in comments and documentation.
-   Cleaned up a few redundant typevars.
-   Fixed callable typing in matchers implementation.
-   Fixed incorrect base class references in matcher decorator attribute visitors.
-   Fixed codemod test assertion failing for some whitespace edge cases.
-   Fixed scope analysis to track variable usage on `del` statements.

## Deprecated

-   Deprecated exporting `ensure_type` from `libcst` in favor of `libcst.helpers`.

## Removed

-   Removed `ExtSlice` and helper code in favor of `SubscriptElement`.
-   Removed `default_params` attribute on `Parameters`.
-   Removed `SyntacticPositionProvider` and `BasicPositionProvider`.
-   Removed `CodePosition` and `CodeRange` exports on `libcst` in favor of `libcst.metadata`.

# 0.2.7 - 2020-01-07

## Updated

-   Command-line interface now shows rough estimate of time remaining while executing a codemod.
-   Add needed import now supports import aliases.

# 0.2.6 - 2020-01-01

## Added

-   Added Codemod framework for running code transform over a codebase in parallel.
    -   Codemod for code transform logic.
    -   CodemodContext for preserving states across transforms.
    -   CodemodCommand for CLI interface.
    -   CodemodTest for testing codemod easily.
    -   yaml codemod config.
    -   Pre-build commands in codemod/commands/.
-   Added TypeInferenceProvider for inferred type info from Pyre. A regression test suite was included.
-   Added FullRepoManager for metadata inter-process cache handing.

## Fixed

-   Fixed usage link in README.
-   Fixed type annotation for Mypy compatibility.

## Updated

-   Upgraded Pyre to 0.0.38

# 0.2.5 - 2019-12-05

## Added

-   Added `extract`, `extractall` and `replace` functions to Matchers API.

## Fixed

-   Fixed length restrictions for `AllOf` and `OneOf` so that they can be used with sequence expansion operators.
-   Fixed batchable visitors not calling attribute visit functions.
-   Fixed typos in docstrings.
-   Fixed matcher type exception not being pickleable.

## Deprecated

-   Deprecated parsing function parameters with defaults into `default_params` attribute. They can be found in the `params` attribute instead.

# 0.2.4 - 2019-11-13

## Fixed

-   Fixed broken types for sequence matchers.

# 0.2.3 - 2019-11-11

## Added

-   Preliminary support for 3.8 walrus operator.
-   CI config and fuzz tests for 3.8.
-   Experimental re-entrant codegen API.
-   Added `unsafe_skip_copy` optimization to `MetadataWrapper`.
-   Matchers API now includes a `findall` function.
-   Matchers now have a `MatchMetadataIfTrue` special matcher.

## Updated

-   Updated to latest Black release.
-   Better type documentation for generated matchers.

## Fixed

-   Clarified matchers documentation in several confusing areas.
-   Drastically sped up codegen and tests.
-   `QualifiedName` now supports imported attributtes.
-   `ExpressionContext` properly marks loop variables as `STORE`.
-   Various typos in documentation are fixed.

## Deprecated

-   Deprecated `BasicPositionProvider` and `SyntacticPositionProvider` in favor of `WhitespaceInclusivePositionProvider` and `PositionProvider`.

# 0.2.2 - 2019-10-24

## Added

-   Added `deep_with_changes` helper method on CSTNode.
-   Added metadata support to matchers.
-   Added ability to get the defining node from a `LocalScope` (`FunctionScope`, `ClassScope` or `ComprehensionScope`).

## Updated

-   Many changes to LibCST documentation including a new best practices page and updated scope tutorial.
-   Exported `CodePosition` and `CodeRange` from `libcst.metadata` instead of `libcst`.

## Fixed

-   Disallowed decorating a concrete visit or leave method with `@visit` or `@leave` decorators.
-   Renamed position provider classes to be more self-explanatory.
-   Fixed trailing newline detection when the last character in a file was from a continuation.
-   Fixed `deep_clone` potentially blowing the stack with large LibCST trees.

## Deprecated

-   Deprecated `ExtSlice` in favor of `SubscriptElement`.
-   Deprecated parsing `Subscript` slices directly into `Index` or `Slice` nodes.

# 0.2.1 - 2019-10-14

## Added

-   `Scope.assignments` and `Scope.accesses` APIs to access all references in a scope.
-   Scope analysis tutorial.

## Updated

-   Supported `<comprehension>` in `Scope.get_qualified_names_for` and `QualifiedName`.
-   Enforced identity equality for matchers and immutability of non-dataclass matchers.
-   Generalize codegen cleanup steps for all codegen.

## Fixed

-   Made `BatchableMetadataProvider` typing covariant over its typevar.
-   Fixed LICENSE header on generated matcher file.
-   Cleanup unused internal noqa and on-call specification.

# 0.2.0 - 2019-10-04

## Added

-   Added matchers which allow comparing LibCST trees against arbitrary patterns.
-   Improved tree manipulation with `deep_remove` and `deep_replace` helper methods on CSTNode.
-   Added new metadata providers: parent node and qualified name.

## Updated

-   Updated Pyre to latest release.
-   Updated scope metadata to provide additional helpers.
-   Updated preferred method of removing a node from its parent in a visitor.

## Fixed

-   Metadata classes and methods are now exported from "libcst.metadata" instead of several submodules.
-   Fixed LICENSE file to explicitly reference individual files in the repo with different licenses.
-   Fixed `deep_clone` to correctly clone leaf nodes.
-   Fixed all parse entrypoints to always return a tree with no duplicated leaf nodes.

# 0.1.3 - 2019-09-18

## Added

-   Added preliminary support for parsing Python 3.5 and Python 3.6 source.
-   Added scope analysis metadata provider.
-   Added mypy type support for built package.

## Fixed

-   Several typos in documentation are fixed.

# 0.1.2 - 2019-08-29

## Added

-   Added attribute visitor hooks.
-   Added base visit/leave methods which can be subclassed.
-   Hypothesis fuzz testing suite, courtesy of Zac Hatfield-Dodds.

## Fixed

-   Metadata documentation is much more complete.
-   Fixed several whitespace validation issues caught by Hypothesis.
-   Parser syntax errors are now used inside parser.

# 0.1.1 - 2019-08-20

## Added

-   Metadata interface is now exported.

## Fixed

-   Dependencies are now specified with minimum revisions.
-   Lots of documentation fixes.

# 0.1 - 2019-07-23

## Added

-   First public release of LibCST.
-   Complete, fully typed syntax tree for Python 3.6.
-   Full suite of tests for each defined node type.
