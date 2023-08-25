// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use regex::Regex;

use crate::nodes::deflated::{Expression, Float, Imaginary, Integer};

static HEX: &str = r"0[xX](?:_?[0-9a-fA-F])+";
static BIN: &str = r"0[bB](?:_?[01])+";
static OCT: &str = r"0[oO](?:_?[0-7])+";
static DECIMAL: &str = r"(?:0(?:_?0)*|[1-9](?:_?[0-9])*)";

static EXPONENT: &str = r"[eE][-+]?[0-9](?:_?[0-9])*";
// Note: these don't exactly match the python implementation (exponent is not included)
static POINT_FLOAT: &str = r"([0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?|\.[0-9](?:_?[0-9])*)";
static EXP_FLOAT: &str = r"[0-9](?:_?[0-9])*";

thread_local! {
    static INTEGER_RE: Regex =
        Regex::new(format!("^({}|{}|{}|{})$", HEX, BIN, OCT, DECIMAL).as_str()).expect("regex");
    static FLOAT_RE: Regex =
        Regex::new(
            format!(
                "^({}({})?|{}{})$",
                POINT_FLOAT, EXPONENT, EXP_FLOAT, EXPONENT
            )
            .as_str(),
        )
        .expect("regex");
    static IMAGINARY_RE: Regex =
        Regex::new(
            format!(
                r"^([0-9](?:_?[0-9])*[jJ]|({}({})?|{}{})[jJ])$",
                POINT_FLOAT, EXPONENT, EXP_FLOAT, EXPONENT
            )
            .as_str(),
        )
        .expect("regex");
}

pub(crate) fn parse_number(raw: &str) -> Expression {
    if INTEGER_RE.with(|r| r.is_match(raw)) {
        Expression::Integer(Box::new(Integer {
            value: raw,
            lpar: Default::default(),
            rpar: Default::default(),
        }))
    } else if FLOAT_RE.with(|r| r.is_match(raw)) {
        Expression::Float(Box::new(Float {
            value: raw,
            lpar: Default::default(),
            rpar: Default::default(),
        }))
    } else if IMAGINARY_RE.with(|r| r.is_match(raw)) {
        Expression::Imaginary(Box::new(Imaginary {
            value: raw,
            lpar: Default::default(),
            rpar: Default::default(),
        }))
    } else {
        Expression::Integer(Box::new(Integer {
            value: raw,
            lpar: Default::default(),
            rpar: Default::default(),
        }))
    }
}
