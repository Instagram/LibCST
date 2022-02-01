// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use libcst_native::*;
use std::{
    env,
    io::{self, Read},
    process::exit,
};

pub fn main() {
    let mut str = std::string::String::new();
    io::stdin().read_to_string(&mut str).unwrap();
    match parse_module(str.as_ref(), None) {
        Err(e) => {
            eprintln!("{}", prettify_error(e, "stdin"));
            exit(1);
        }
        Ok(m) => {
            let first_arg = env::args().nth(1).unwrap_or_else(|| "".to_string());
            if first_arg == "-d" {
                println!("{:#?}", m);
            }
            if first_arg != "-n" {
                let mut state = Default::default();
                m.codegen(&mut state);
                print!("{}", state.to_string());
            }
        }
    };
}
