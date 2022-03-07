use std::{f64::consts::E, time::Duration};

use criterion::{
    black_box, criterion_group, criterion_main, measurement::Measurement, BenchmarkGroup, Criterion,
};

use nm_math::{
    expression::{Evaluable, Expr},
    parser::parse,
    variable::{Scope, Var},
};

fn settings<'a>(
    mut g: BenchmarkGroup<'a, impl Measurement>,
) -> BenchmarkGroup<'a, impl Measurement> {
    g.warm_up_time(Duration::from_secs(1))
        .measurement_time(Duration::from_secs(3));
    g
}

fn main_bench(c: &mut Criterion) {
    let expr_math = parse("x * sin(x)^e").unwrap();
    let expr_rust = |x: f64| x * x.sin().powf(E);
    let scope = Scope::Single(Var::new("x", 2.0));

    let mut g = settings(c.benchmark_group("main"));
    g.bench_function("math", |b| {
        b.iter_with_large_drop(|| black_box(expr_math.eval(&scope)))
    });
    g.bench_function("rust", |b| b.iter(|| expr_rust(black_box(2.0))));
    g.finish();
}

fn neg_bench(c: &mut Criterion) {
    let expr_math_old = (-2.0 * Expr::var("x")).sin() * -1.0;
    let expr_math_new = Expr::Neg(Box::new((-2.0 * Expr::var("x")).sin()));
    let expr_rust_old = |x: f64| (-2.0 * x).sin() * -1.0; // Old style
    let expr_rust_new = |x: f64| -(-2.0 * x).sin(); // New style (will be implemented soon)
    let scope = Scope::Single(Var::new("x", 1.0));

    let mut g = settings(c.benchmark_group("neg"));
    g.bench_function("math_old", |b| {
        b.iter_with_large_drop(|| black_box(expr_math_old.eval(&scope)))
    });
    g.bench_function("math_new", |b| {
        b.iter_with_large_drop(|| black_box(expr_math_new.eval(&scope)))
    });
    g.bench_function("rust_old", |b| b.iter(|| expr_rust_old(black_box(1.0))));
    g.bench_function("rust_new", |b| b.iter(|| expr_rust_new(black_box(1.0))));
    g.finish();
}

fn parse_bench(c: &mut Criterion) {
    let mut g = settings(c.benchmark_group("parse"));
    g.bench_function("complex", |b| {
        b.iter(|| parse(black_box("-4.5x^2 * (sin(x) + 2x) * (sqrt(12) / -x)")).unwrap())
    });
    g.finish();
}

criterion_group!(benches, main_bench, neg_bench, parse_bench);
criterion_main!(benches);
