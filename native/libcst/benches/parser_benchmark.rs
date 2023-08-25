// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use std::{
    path::{Component, PathBuf},
    time::Duration,
};

use criterion::{
    black_box, criterion_group, criterion_main, measurement::Measurement, BatchSize, BenchmarkId,
    Criterion, Throughput,
};
use itertools::Itertools;
use rayon::prelude::*;

use libcst_native::{
    parse_module, parse_tokens_without_whitespace, tokenize, Codegen, Config, Inflate,
};

#[cfg(not(windows))]
const NEWLINE: &str = "\n";
#[cfg(windows)]
const NEWLINE: &str = "\r\n";

fn load_all_fixtures_vec() -> Vec<String> {
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
        .collect()
}

fn load_all_fixtures() -> String {
    load_all_fixtures_vec().join(NEWLINE)
}

pub fn inflate_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let fixture = load_all_fixtures();
    let tokens = tokenize(fixture.as_str()).expect("tokenize failed");
    let tokvec = tokens.clone().into();
    let mut group = c.benchmark_group("inflate");
    group.bench_function("all", |b| {
        b.iter_batched(
            || {
                let conf = Config::new(fixture.as_str(), &tokens);
                let m = parse_tokens_without_whitespace(&tokvec, fixture.as_str(), None)
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
            || tokenize(fixture.as_str()).expect("tokenize failed").into(),
            |tokens| {
                black_box(drop(parse_tokens_without_whitespace(
                    &tokens,
                    fixture.as_str(),
                    None,
                )))
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

pub fn codegen_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let input = load_all_fixtures();
    let m = parse_module(input.as_str(), None).expect("parse failed");
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

pub fn parse_into_cst_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let fixture = load_all_fixtures();
    let mut group = c.benchmark_group("parse_into_cst");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("all", |b| {
        b.iter(|| black_box(parse_module(&fixture, None)))
    });
    group.finish();
}

pub fn parse_into_cst_multithreaded_benchmarks<T: Measurement + std::marker::Sync>(
    c: &mut Criterion<T>,
) where
    <T as Measurement>::Value: Send,
{
    let fixtures = load_all_fixtures_vec();
    let mut group = c.benchmark_group("parse_into_cst_parallel");
    group.measurement_time(Duration::from_secs(15));
    group.warm_up_time(Duration::from_secs(5));

    for thread_count in 1..10 {
        let expanded_fixtures = (0..thread_count)
            .flat_map(|_| fixtures.clone())
            .collect_vec();
        group.throughput(Throughput::Elements(expanded_fixtures.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(thread_count),
            &thread_count,
            |b, thread_count| {
                let thread_pool = rayon::ThreadPoolBuilder::new()
                    .num_threads(*thread_count)
                    .build()
                    .unwrap();
                thread_pool.install(|| {
                    b.iter_with_large_drop(|| {
                        expanded_fixtures
                            .par_iter()
                            .map(|contents| black_box(parse_module(&contents, None)))
                            .collect::<Vec<_>>()
                    });
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    name=benches;
    config=Criterion::default();
    targets=parser_benchmarks, codegen_benchmarks, inflate_benchmarks, tokenize_benchmarks, parse_into_cst_benchmarks, parse_into_cst_multithreaded_benchmarks
);
criterion_main!(benches);
