use std::{
    path::{Component, PathBuf},
    time::Duration,
};

use criterion::{black_box, criterion_group, criterion_main, measurement::Measurement, Criterion};
use criterion_cycles_per_byte::CyclesPerByte;
use itertools::Itertools;
use libcst::{parse_module, Codegen};

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

pub fn parser_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let input = load_all_fixtures();
    let mut group = c.benchmark_group("parser");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("decorated-function", |b| {
        b.iter(|| parse_module(black_box(&input)))
    });
    group.finish();
}

pub fn codegen_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let input = load_all_fixtures();
    let m = parse_module(&input).expect("parse failed");
    let mut group = c.benchmark_group("codegen");
    group.bench_function("roundtrip", |b| {
        b.iter(|| {
            let mut state = Default::default();
            m.codegen(&mut state);
        })
    });
    group.finish();
}

criterion_group!(
    name=benches;
    config = Criterion::default().with_measurement(CyclesPerByte);
    targets=parser_benchmarks, codegen_benchmarks
);
criterion_main!(benches);
