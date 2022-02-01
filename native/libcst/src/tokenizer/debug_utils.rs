// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::fmt;

/// An empty struct that when writes "..." when using `fmt::Debug`. Useful for omitting fields when
/// using `fmt::Formatter::debug_struct`.
pub struct EllipsisDebug;

impl fmt::Debug for EllipsisDebug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("...")
    }
}
