// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use difference::assert_diff;
use itertools::Itertools;
use libcst_native::{parse_module, prettify_error, Codegen};
use std::{
    iter::once,
    path::{Component, PathBuf},
};

fn all_fixtures() -> impl Iterator<Item = (PathBuf, String)> {
    let mut path = PathBuf::from(file!());
    path.pop();
    path = path
        .components()
        .skip(1)
        .chain(once(Component::Normal("fixtures".as_ref())))
        .collect();

    path.read_dir().expect("read_dir").into_iter().map(|file| {
        let path = file.unwrap().path();
        let contents = std::fs::read_to_string(&path).expect("reading file");
        (path, contents)
    })
}

#[test]
fn roundtrip_fixtures() {
    for (path, input) in all_fixtures() {
        let input = if let Some(stripped) = input.strip_prefix('\u{feff}') {
            stripped
        } else {
            &input
        };
        let m = match parse_module(input, None) {
            Ok(m) => m,
            Err(e) => panic!("{}", prettify_error(e, format!("{:#?}", path).as_ref())),
        };
        let mut state = Default::default();
        m.codegen(&mut state);
        let generated = state.to_string();
        if generated != input {
            let got = visualize(&generated);
            let expected = visualize(input);
            assert_diff!(expected.as_ref(), got.as_ref(), "", 0);
        }
    }
}

fn visualize(s: &str) -> String {
    s.replace(' ', "▩").lines().join("↩\n")
}
