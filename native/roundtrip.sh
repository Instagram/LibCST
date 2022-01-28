#!/bin/bash

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

PARSE=$(dirname $0)/target/release/parse

exec diff -u "$1" <($PARSE < "$1")
