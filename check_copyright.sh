#!/bin/bash

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -eu
EXITCODE=0
error() { echo "$1"; EXITCODE=1; }

EXCEPTION_PATTERNS=(
    "^native/libcst/tests/fixtures/"
    "^libcst/_add_slots\.py$"
    "^libcst/tests/test_\(e2e\|fuzz\)\.py$"
    "^libcst/_parser/base_parser\.py$"
    "^libcst/_parser/parso/utils\.py$"
    "^libcst/_parser/parso/pgen2/\(generator\|grammar_parser\)\.py$"
    "^libcst/_parser/parso/python/\(py_token\|tokenize\)\.py$"
    "^libcst/_parser/parso/tests/test_\(fstring\|tokenize\|utils\)\.py$"
)


while read filename; do \
    if ! head -n 16 "$filename" | grep -q "Copyright (c) Meta Platforms, Inc. and affiliates."; then
        error "Missing copyright in $filename"
    fi
done < <( git ls-tree -r --name-only HEAD | grep "\(.py\|\.sh\|\.rs\)$" | \
            grep -v "${EXCEPTION_PATTERNS[@]/#/-e}" )
exit $EXITCODE