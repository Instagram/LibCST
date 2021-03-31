use criterion::{black_box, criterion_group, criterion_main, Criterion};
use libcst_native::parser::parse_module;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("decorated-function", |b| {
        b.iter(|| parse_module(black_box("@hello\ndef foo(bar): ...")))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
