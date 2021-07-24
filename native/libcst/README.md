# libcst_native

A very experimental native extension to speed up LibCST. This does not currently provide
much performance benefit and is therefore not recommended for general use.

The extension is written in Rust using [PyO3](https://pyo3.rs/).

This installs as a separate python package that LibCST looks for and will import if it's
available.


## Using with LibCST

[Set up a rust development environment](https://www.rust-lang.org/tools/install). Using
`rustup` is recommended, but not necessary. Rust 1.45.0+ should work.

Follow the instructions for setting up a virtualenv in the top-level README, then:

```
cd libcst_native
maturin develop  # install libcst_native to the virtualenv
cd ..            # cd back into the main project
python -m unittest
```

This will run the python test suite. Nothing special is required to use `libcst_native`,
since `libcst` will automatically use the native extension when it's installed.

When benchmarking this code, make sure to run `maturin develop` with the `--release`
flag to enable compiler optimizations.

You can disable the native extension by uninstalling the package from your virtualenv:

```
pip uninstall libcst_native
```


## Rust Tests

In addition to running the python test suite, you can run some tests written in rust
with

```
cargo test --no-default-features
```

The `--no-default-features` flag needed to work around an incompatibility between tests
and pyo3's `extension-module` feature.


## Code Formatting

Use `cargo fmt` to format your code.


## Release

This isn't currently supported, so there's no releases available, but the end-goal would
be to publish this on PyPI.

Because this is a native extension, it must be re-built for each platform/architecture.
The per-platform build could be automated using a CI system, [like github
actions][gh-actions].

[gh-actions]: https://github.com/PyO3/maturin/blob/master/.github/workflows/release.yml
