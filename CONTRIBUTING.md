# Contributing to LibCST
We want to make contributing to this project as easy and transparent as
possible.

## Our Development Process
This github repo is the source of truth and all changes need to be reviewed in
pull requests.

## Pull Requests
We actively welcome your pull requests.

### Setup Your Environment

1. Install a [Rust toolchain](https://rustup.rs) and [hatch](https://hatch.pypa.io)
2. Fork the repo on your side
3. Clone the repo
   > git clone [your fork.git] libcst  
   > cd libcst
4. Sync with the main libcst version package
   > git fetch --tags https://github.com/instagram/libcst
5. Setup the env
   > hatch env create

You are now ready to create your own branch from main, and contribute.
Please provide tests (using unittest), and update the documentation (both docstrings
and sphinx doc), if applicable.

### Before Submitting Your Pull Request

1. Format your code
   > hatch run format
2. Run the type checker
   > hatch run typecheck
3. Test your changes
   > hatch run test
4. Check linters
   > hatch run lint

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Coding Style
We use flake8 and ufmt to enforce coding style.

## License
By contributing to LibCST, you agree that your contributions will be licensed
under the MIT LICENSE file in the root directory of this source tree.
