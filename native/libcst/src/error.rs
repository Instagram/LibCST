// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// The following is vendored from `chic` by Yoshua Wuyts which is licensed under the MIT and
// Apache-2.0 licenses.
use annotate_snippets::{Annotation, Level, Message, Snippet};

/// An error formatter.
pub struct Error<'a> {
    message: Message<'a>,
}

impl<'a> Error<'a> {
    /// Create a new `Error` formatter.
    pub fn new(label: &'a str) -> Self {
        Self {
            message: Level::Error.title(label),
        }
    }

    /// Pass a new error to the formatter.
    pub fn error(
        mut self,
        line_start: usize,
        start: usize,
        end: usize,
        source: &'a str,
        label: &'a str,
    ) -> Self {
        self.message = self.message.snippet(
            Snippet::source(source)
                .line_start(line_start)
                .fold(false)
                .annotations(vec![Level::Error.span(start..end).label(label)]),
        );
        self
    }

    pub fn to_string(self) -> String {
        Renderer::styled().render(self.message).to_string()
    }
}
