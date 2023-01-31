#![allow(unused)]

use diatom::Interpreter;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn loop_1million() {
    let mut interpreter = Interpreter::new();
    interpreter
        .exec(
            r#"
            a = 0
            until a > 1_000_000 do 
                a = a + 1
            end
        "#,
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
    c.bench_function("loop 1 million", |b| b.iter(loop_1million));
    c.bench_function("loop 1 million native", |b| {
        b.iter(|| loop_native(black_box(1_000_000)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
