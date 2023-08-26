// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
//
// Part of this file is derived from the CPython documentation, which is available under the
// zero-clause BSD license. That license does not require that derivative works cite the original
// code or that we retain the original work's copyright information.
// https://docs.python.org/3/license.html#zero-clause-bsd-license-for-code-in-the-python-release-documentation

use regex::Regex;

/// A list of strings that make up all the possible operators in a specific version of Python.
/// Derived from the [CPython's token documentation](https://docs.python.org/3/library/token.html).
pub const OPERATORS: &[&str] = &[
    "(",   // LPAR
    ")",   // RPAR
    "[",   // LSQB
    "]",   // RSQB
    ":",   // COLON
    ",",   // COMMA
    ";",   // SEMI
    "+",   // PLUS
    "-",   // MINUS
    "*",   // STAR
    "/",   // SLASH
    "|",   // VBAR
    "&",   // AMPER
    "<",   // LESS
    ">",   // GREATER
    "=",   // EQUAL
    ".",   // DOT
    "%",   // PERCENT
    "{",   // LBRACE
    "}",   // RBRACE
    "==",  // EQEQUAL
    "!=",  // NOTEQUAL
    "<=",  // LESSEQUAL
    ">=",  // GREATEREQUAL
    "~",   // TILDE
    "^",   // CIRCUMFLEX
    "<<",  // LEFTSHIFT
    ">>",  // RIGHTSHIFT
    "**",  // DOUBLESTAR
    "+=",  // PLUSEQUAL
    "-=",  // MINEQUAL
    "*=",  // STAREQUAL
    "/=",  // SLASHEQUAL
    "%=",  // PERCENTEQUAL
    "&=",  // AMPEREQUAL
    "|=",  // VBAREQUAL
    "^=",  // CIRCUMFLEXEQUAL
    "<<=", // LEFTSHIFTEQUAL
    ">>=", // RIGHTSHIFTEQUAL
    "**=", // DOUBLESTAREQUAL
    "//",  // DOUBLESLASH
    "//=", // DOUBLESLASHEQUAL
    "@",   // AT
    "@=",  // ATEQUAL
    "->",  // RARROW
    "...", // ELLIPSIS
    ":=",  // COLONEQUAL
    // Not a real operator, but needed to support the split_fstring feature
    "!",
    // The fake operator added by PEP 401. Technically only valid if used with:
    //
    //     from __future__ import barry_as_FLUFL
    "<>",
];

thread_local! {
pub static OPERATOR_RE: Regex = {
    // sort operators so that we try to match the longest ones first
    let mut sorted_operators: Box<[&str]> = OPERATORS.into();
    sorted_operators.sort_unstable_by_key(|op| usize::MAX - op.len());
    Regex::new(&format!(
        r"\A({})",
        sorted_operators
            .iter()
            .map(|op| regex::escape(op))
            .collect::<Vec<_>>()
            .join("|")
    ))
    .expect("regex")
};
}
