use std::f32::consts::E;

use nm_math::{
    method::{Method, MethodEquation, MethodError},
    variable::OperableType,
};

fn main() -> Result<(), MethodError> {
    // Insert equation in func closure
    let func = |x: OperableType| x + E.powf(x);

    let result = Method::new(100).very_verbose().bisection(
        MethodEquation::External(func),
        (-1.0, 0.0),
        0.01,
    )?;

    println!(
        "\nResult: x = {} f(x) = {}\nIterations: {}\nElapsed time: {:?}",
        result.inner.0, result.inner.1, result.stats.iterations, result.stats.elapsed
    );

    Ok(())
}
