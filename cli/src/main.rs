use core::f64::consts::E;

use nm_math::{
    expression::{Expr, MathConst},
    method::{Method, MethodEquation, MethodError},
    variable::OpType,
};

fn main() -> Result<(), MethodError> {
    // Rust function
    // Insert equation in func closure
    let func = Box::new(|x: OpType| x + E.powf(x));

    let result = Method::new(100).very_verbose().bisection(
        MethodEquation::External(func),
        (-1.0, 0.0),
        0.01,
    )?;

    println!(
        "\nResult: x = {} f(x) = {}\nIterations: {}\nElapsed time: {:?}",
        result.inner.0, result.inner.1, result.stats.iterations, result.stats.elapsed
    );

    // Separator
    println!("\n\n{:-^16}\n\n", "");

    // Virtual expressions
    let func_expr = Expr::var("x") + Expr::MathConst(MathConst::E).pow(Expr::var("x"));

    let result_expr = Method::new(100).very_verbose().bisection(
        MethodEquation::Math(func_expr),
        (-1.0, 0.0),
        0.01,
    )?;

    println!(
        "\nResult (Expr): x = {} f(x) = {}\nIterations: {}\nElapsed time: {:?}",
        result_expr.inner.0,
        result_expr.inner.1,
        result_expr.stats.iterations,
        result_expr.stats.elapsed
    );

    Ok(())
}
