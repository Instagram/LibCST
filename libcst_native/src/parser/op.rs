// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::SimpleWhitespace;

#[derive(Debug, Eq, PartialEq)]
pub struct Semicolon<'a> {
    pub whitespace_before: SimpleWhitespace<'a>,
    pub whitespace_after: SimpleWhitespace<'a>,
}
