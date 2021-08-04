use libcst::*;
use std::{
    env,
    io::{self, Read},
};

pub fn main() {
    let mut str = std::string::String::new();
    io::stdin().read_to_string(&mut str).unwrap();
    match parse_module(str.as_ref()) {
        Err(e) => eprintln!("{}", prettify_error(str.as_ref(), e, "stdin")),
        Ok(m) => {
            let first_arg = env::args().next().unwrap_or_else(|| "".to_string());
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
