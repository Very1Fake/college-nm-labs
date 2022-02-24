use std::time::{Duration, Instant};

use crate::{
    expression::{Expression, ExpressionError},
    variable::{OperableType, Scope, Variable},
};

const MAX_ITERS_DEFAULT: u128 = 1_000_000;

#[derive(Debug)]
pub enum MethodError {
    IterationsExceed(u128),
    ExpressionError(ExpressionError),
    Other(String),
}

#[derive(Debug)]
pub struct CallStats {
    pub elapsed: Duration,
    pub iterations: u128,
}

#[derive(Debug)]
pub struct MethodResult {
    pub inner: (OperableType, OperableType),
    pub stats: CallStats,
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum MethodEquation<F>
where
    F: Fn(OperableType) -> OperableType,
{
    Math(Box<Expression>),
    External(F),
}

impl<F> MethodEquation<F>
where
    F: Fn(OperableType) -> OperableType,
{
    pub fn eval(&self, x: OperableType) -> Result<OperableType, ExpressionError> {
        match self {
            MethodEquation::Math(expr) => {
                let mut scope = Scope::default();
                scope.insert(
                    "x".into(),
                    Variable {
                        name: "x".into(),
                        inner: x,
                    },
                );

                expr.eval(&scope)
            }
            MethodEquation::External(f) => Ok(f(x)),
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Method {
    start: Instant,
    iterations: u128,
    verbose: u8,

    // Limits
    max_iters: u128,
}

impl Method {
    pub fn new(max_iters: u128) -> Self {
        Method {
            start: Instant::now(),
            iterations: 0,
            verbose: 0,
            max_iters,
        }
    }

    #[inline]
    pub fn verbose(mut self) -> Self {
        self.verbose = 1;
        self
    }

    #[inline]
    pub fn very_verbose(mut self) -> Self {
        self.verbose = 2;
        self
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// Bisection method
    ////////////////////////////////////////////////////////////////////////////////////////////////

    pub fn bisection(
        mut self,
        f: MethodEquation<impl Fn(OperableType) -> OperableType>,
        range: (OperableType, OperableType),
        epsilon: OperableType,
    ) -> Result<MethodResult, MethodError> {
        let mut a = range.0;
        let mut b = range.1;

        if sign_diff(
            f.eval(a).map_err(MethodError::ExpressionError)?,
            f.eval(b).map_err(MethodError::ExpressionError)?,
        ) {
            while self.iterations != self.max_iters {
                self.iterations += 1;

                let xi = (a + b) / 2.0;

                // Print iterations result
                if self.verbose >= 1 {
                    println!(
                        "Iter({}): [{a}; {b}], xi = {xi} ([{}; {}] {})",
                        self.iterations,
                        f.eval(a).map_err(MethodError::ExpressionError)?,
                        f.eval(b).map_err(MethodError::ExpressionError)?,
                        f.eval(xi).map_err(MethodError::ExpressionError)?
                    );
                }

                // Print iterations precision check
                if self.verbose == 2 {
                    println!(
                        "Iter({}): Precision check |{}| < {epsilon}",
                        self.iterations,
                        f.eval(b).map_err(MethodError::ExpressionError)?
                            - f.eval(a).map_err(MethodError::ExpressionError)?
                    );
                }

                // Compare signs to clip the range
                if sign_diff(
                    f.eval(a).map_err(MethodError::ExpressionError)?,
                    f.eval(xi).map_err(MethodError::ExpressionError)?,
                ) {
                    b = xi;
                } else {
                    a = xi;
                }

                // Check precision
                if (f.eval(b).map_err(MethodError::ExpressionError)?
                    - f.eval(a).map_err(MethodError::ExpressionError)?)
                .abs()
                    < epsilon
                {
                    let result = (b + a) / 2.0;
                    return Ok(MethodResult {
                        inner: (
                            result,
                            f.eval(result).map_err(MethodError::ExpressionError)?,
                        ),
                        stats: CallStats {
                            elapsed: self.start.elapsed(),
                            iterations: self.iterations,
                        },
                    });
                }
            }

            Err(MethodError::IterationsExceed(self.iterations))
        } else {
            Err(MethodError::Other(format!(
                "Equation has no roots in range {range:?}"
            )))
        }
    }
}

impl Default for Method {
    fn default() -> Self {
        Self::new(MAX_ITERS_DEFAULT)
    }
}

// -------------------------------------------------------------------------------------------------

#[inline(always)]
fn sign_diff(lhs: OperableType, rhs: OperableType) -> bool {
    lhs * rhs < 0.0
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    #[test]
    fn method_bisection() {} // TODO: Write tests
}
