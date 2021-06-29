use std::path::PathBuf;

use libcst_native::parser::{parse_module, Codegen};

fn all_fixtures() -> impl Iterator<Item = (PathBuf, String)> {
    let mut path = PathBuf::from(file!());
    path.pop();
    path.push("fixtures");

    path.read_dir().expect("read_dir").into_iter().map(|file| {
        let path = file.unwrap().path();
        let contents = std::fs::read_to_string(&path).expect("reading file");
        (path, contents)
    })
}

#[test]
fn roundtrip_fixtures() {
    for (path, input) in all_fixtures() {
        let m = match parse_module(&input) {
            Ok(m) => m,
            Err(e) => {
                panic!("Failed to parse {:#?} due to: {}", path, e);
            }
        };
        let mut state = Default::default();
        m.codegen(&mut state);
        assert_eq!(
            state.to_string(),
            input,
            "failed to roundtrip {}",
            path.to_str().unwrap()
        );
    }
}
