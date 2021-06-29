// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use super::SimpleWhitespace;

#[derive(Debug, Eq, PartialEq)]
pub struct Semicolon<'a> {
    /// Any space that appears directly before this semicolon.
    pub whitespace_before: SimpleWhitespace<'a>,
    /// Any space that appears directly after this semicolon.
    pub whitespace_after: SimpleWhitespace<'a>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Comma<'a> {
    /// Any space that appears directly before this comma.
    pub whitespace_before: SimpleWhitespace<'a>,
    /// Any space that appears directly after this comma.
    pub whitespace_after: SimpleWhitespace<'a>,
}
