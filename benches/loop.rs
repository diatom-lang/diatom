#![allow(unused)]

use std::{ffi::OsStr, io};

use diatom::Interpreter;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn loop_10million() {
    let mut interpreter = Interpreter::new(io::stdout());
    interpreter
        .exec(
            r#"
            a = 0
            until a >= 10_000_000 do 
                a = a + 1
            end
        "#,
            OsStr::new("<loop>"),
            false,
        )
        .expect("Should work");
}

fn loop_native(n: usize) {
    let mut a = 0;
    while a <= n {
        a += 1
    }
    format!("{a}");
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("loop 10 million", |b| b.iter(loop_10million));
    c.bench_function("loop 10 million native", |b| {
        b.iter(|| loop_native(black_box(10_000_000)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
