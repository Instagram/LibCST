use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, measurement::Measurement, Criterion};
use criterion_cycles_per_byte::CyclesPerByte;
use indoc::indoc;
use libcst_native::parser::{parse_module, Codegen};

pub fn parser_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let mut group = c.benchmark_group("parser");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("decorated-function", |b| {
        b.iter(|| parse_module(black_box("@hello\ndef foo(bar): ...")))
    });
    group.finish();
}

pub fn codegen_benchmarks<T: Measurement>(c: &mut Criterion<T>) {
    let m = parse_module(indoc! {r"
        
        @hello

        def foo(
            a, #foo
            b=1,
            /,
            c=3,
            *,
            d, # bar
            **kwargs,
        ):
            pass
        
        
        def bar(): ...
    "})
    .expect("parse failed");
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
