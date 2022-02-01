// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use std::{
    path::{Component, PathBuf},
    time::Duration,
};

use criterion::{
    black_box, criterion_group, criterion_main, measurement::Measurement, BatchSize, Criterion,
};
use criterion_cycles_per_byte::CyclesPerByte;
use itertools::Itertools;
use libcst_native::{
    parse_module, parse_tokens_without_whitespace, tokenize, Codegen, Config, Inflate,
};

fn load_all_fixtures() -> String {
    let mut path = PathBuf::from(file!());
    path.pop();
    path.pop();
    path = path
        .components()
        .skip(1)
        .chain(
            vec!["tests".as_ref(), "fixtures".as_ref()]
                .into_iter()
                .map(Component::Normal),
        )
        .collect();

    path.read_dir()
        .expect("read_dir")
        .into_iter()
        .map(|file| {
            let path = file.unwrap().path();
            std::fs::read_to_string(&path).expect("reading_file")
        })
        .join("\n")
}

pub fn inflate_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let fixture = load_all_fixtures();
    let tokens = tokenize(fixture.as_str()).expect("tokenize failed");
    let mut group = c.benchmark_group("inflate");
    group.bench_function("all", |b| {
        b.iter_batched(
            || {
                let conf = Config::new(fixture.as_str(), &tokens);
                let m = parse_tokens_without_whitespace(tokens.clone(), fixture.as_str(), None)
                    .expect("parse failed");
                (conf, m)
            },
            |(conf, m)| black_box(m.inflate(&conf)),
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

pub fn parser_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let fixture = load_all_fixtures();
    let mut group = c.benchmark_group("parse");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("all", |b| {
        b.iter_batched(
            || tokenize(fixture.as_str()).expect("tokenize failed"),
            |tokens| {
                black_box(parse_tokens_without_whitespace(
                    tokens,
                    fixture.as_str(),
                    None,
                ))
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

pub fn codegen_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let input = load_all_fixtures();
    let m = parse_module(&input, None).expect("parse failed");
    let mut group = c.benchmark_group("codegen");
    group.bench_function("all", |b| {
        b.iter(|| {
            let mut state = Default::default();
            #[allow(clippy::unit_arg)]
            black_box(m.codegen(&mut state));
        })
    });
    group.finish();
}

pub fn tokenize_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let input = load_all_fixtures();
    let mut group = c.benchmark_group("tokenize");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("all", |b| b.iter(|| black_box(tokenize(input.as_str()))));
    group.finish();
}

criterion_group!(
    name=benches;
    config = Criterion::default().with_measurement(CyclesPerByte);
    targets=parser_benchmarks, codegen_benchmarks, inflate_benchmarks, tokenize_benchmarks
);
criterion_main!(benches);
