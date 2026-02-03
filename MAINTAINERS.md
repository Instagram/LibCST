# How to make a new release

1. Add a new entry to `CHANGELOG.md` (I normally use the [new release page](https://github.com/Instagram/LibCST/releases/new) to generate a changelog, then manually group)
    1. Follow the existing format: `Fixed`, `Added`, `Updated`, `Deprecated`, `Removed`, `New Contributors` sections, and the full changelog link at the bottom.
    1. Mention only user-visible changes - improvements to CI, tests, or development workflow aren't noteworthy enough
    1. Version bumps are generally not worth mentioning with some notable exceptions (like pyo3)
    1. Group related PRs into one bullet point if it makes sense
2. manually bump versions in `Cargo.toml` files in the repo
3. run `cargo update -p libcst`
4. make a new PR with the above changes, get it reviewed and landed
5. make a new release on Github, create a new tag on publish, and copy the contents of the changelog entry in there
6. after publishing, check out the repo at the new tag, and run `cd native; cargo +nightly publish -Z package-workspace -p libcst_derive -p libcst`
