use libcst_native::parser::*;
use std::io::{self, Read};

pub fn main() {
    let mut str = String::new();
    io::stdin().read_to_string(&mut str).unwrap();
    match parse_module(str.as_ref()) {
        Ok(_) => {}
        Err(e) => eprintln!("{}", prettify_error(str.as_ref(), e, "stdin")),
    };
}
