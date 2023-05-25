# libcst/native

A native extension to enable parsing of new Python grammar in LibCST.

The extension is written in Rust, and exposed to Python using [PyO3](https://pyo3.rs/).
This is packaged together with libcst, and can be imported from `libcst.native`. By default
the LibCST APIs use this module for all parsing.

Later on, the parser library might be packaged separately as 
[a Rust crate](https://crates.io). Pull requests towards this are much appreciated.

## Goals

1. Adopt the CPython grammar definition as closely as possible to reduce maintenance
   burden. This means using a PEG parser.
2. Feature-parity with the pure-python LibCST parser: the API should be easy to use from
   Python, support parsing with a target version, bytes and strings as inputs, etc.
3. [future] Performance. The aspirational goal is to be within 2x CPython performance,
   which would enable LibCST to be used in interactive use cases (think IDEs).
4. [future] Error recovery. The parser should be able to handle partially complete
   documents, returning a CST for the syntactically correct parts, and a list of errors
   found.

## Structure

The extension is organized into two rust crates: `libcst_derive` contains some macros to
facilitate various features of CST nodes, and `libcst` contains the `parser` itself
(including the Python grammar), a `tokenizer` implementation by @bgw, and a very basic
representation of CST `nodes`. Parsing is done by
1. **tokenizing** the input utf-8 string (bytes are not supported at the Rust layer,
   they are converted to utf-8 strings by the python wrapper)
2. running the **PEG parser** on the tokenized input, which also captures certain anchor
   tokens in the resulting syntax tree
3. using the anchor tokens to **inflate** the syntax tree into a proper CST

These steps are wrapped into a high-level `parse_module` API
[here](https://github.com/Instagram/LibCST/blob/main/native/libcst/src/lib.rs#L43),
along with `parse_statement` and `parse_expression` functions which all just accept the
input string and an optional encoding.

These Rust functions are exposed to Python
[here](https://github.com/Instagram/LibCST/blob/main/native/libcst/src/py.rs) using the
excellent [PyO3](https://pyo3.rs/) library, plus an `IntoPy` trait which is mostly
implemented via a macro in `libcst_derive`.


## Hacking

### Nodes
All CST nodes are marked with the `#[cst_node]` proc macro, which duplicates the node types; for a node named `Foo`, there's:

- `DeflatedFoo`, which is the output of the parsing phase and isn't exposed through the
  API of the crate.
  - it has two lifetime parameters: `'r` (or `'input` in the grammar) is the lifetime of
    `Token` references, and `'a` is the lifetime of `str` slices from the original input
  - `TokenRef` fields are contained here, while whitespace fields aren't
  - if there aren't any fields that refer to other CST nodes or `TokenRef`s, there's an
    extra (private) `_phantom` field that "contains" the two lifetime parameters (this
    is to make the type parameters of all `DeflatedFoo` types uniform)
  - it implements the `Inflate` trait, which converts `DeflatedFoo` into `Foo`
- `Foo`, which is what's publicly exposed in the crate and is the output of `Inflate`ing `DeflatedFoo`.
   - it only retains the second (`'a`) lifetime parameter of `DeflatedFoo` to refer back to slices of the original input string
   - whitespace fields are contained here, but `TokenRef`s aren't
   - `IntoPy` is implemented for it (assuming the `py` crate feature is enabled), which contains code to translate `Foo` back into a Python object; hence, the fields on `Foo` match the Python CST node implementations (barring fields marked with `#[skip_py]`)

### Grammar

The grammar is mostly a straightforward translation from the [CPython
grammar](https://github.com/python/cpython/blob/main/Grammar/python.gram), with some
exceptions:

* The output of grammar rules are deflated CST nodes that capture the AST plus
  additional anchor token references used for whitespace parsing later on.
* Rules in the grammar must be strongly typed, as enforced by the Rust compiler. The
  CPython grammar rules are a bit more loosely-typed in comparison.
* Some features in the CPython peg parser are not supported by rust-peg: keywords,
  mutually recursive rules, special `invalid_` rules, the `~` operator, terminating the
  parser early.

The PEG parser is run on a `Vec` of `Token`s (more precisely `&'input Vec<Token<'a>>`),
and tries its best to avoid allocating any strings, working only with references. As
such, the output nodes don't own any strings, but refer to slices of the original input
(hence the `'input, 'a` lifetime parameters on almost all nodes).

### Whitespace parsing

The `Inflate` trait is responsible for taking a "deflated", skeleton CST node, and
parsing out the relevant whitespace from the anchor tokens to produce an "inflated"
(normal) CST node. In addition to the deflated node, inflation requires a whitespace
config object which contains global information required for certain aspects of
whitespace parsing, like the default indentation.

Inflation consumes the deflated node, while mutating the tokens referenced by it. This
is important to make sure whitespace is only ever assigned to at most one CST node. The
`Inflate` trait implementation needs to ensure that all whitespace is assigned to a CST
node; this is generally verified using roundtrip tests (i.e. parsing code and then
generating it back to then assert the original and generated are byte-by-byte equal).

The general convention is that the top-most possible node owns a certain piece of
whitespace, which should be straightforward to achieve in a top-down parser like
`Inflate`. In cases where whitespace is shared between sibling nodes, usually the
leftmost node owns the whitespace except in the case of trailing commas and closing
parentheses, where the latter owns the whitespace (for backwards compatibility with the
pure python parser). See the implementation of `inflate_element` for how this is done.

### Tests

In addition to running the python test suite, you can run some tests written in rust
with

```
cd native
cargo test
```

These include unit and roundtrip tests.

Additionally, some benchmarks can be run on x86-based architectures using `cargo bench`.

### Code Formatting

Use `cargo fmt` to format your code.
