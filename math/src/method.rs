use core::{
    fmt::{self, Display},
    time::Duration,
};
use std::time::Instant;

use crate::{
    expression::{Evaluable, EvaluationError, Expr},
    variable::{OpType, Scope, Var},
};

const MAX_ITERS_DEFAULT: u128 = 1_000_000;

#[derive(Debug)]
pub enum MethodError {
    IterationsExceed(u128),
    EvaluationError(EvaluationError),
    Other(String),
}

#[derive(Debug)]
pub struct CallStats {
    pub elapsed: Duration,
    pub iterations: u128,
}

#[derive(Debug)]
pub struct MethodResult {
    pub inner: (OpType, OpType),
    pub stats: CallStats,
}

// -------------------------------------------------------------------------------------------------

type ExternalFunction = fn(OpType) -> OpType;

#[derive(Clone)]
pub enum MethodEquation {
    Math(Expr),
    Internal(Box<ExternalFunction>),
}

impl Default for MethodEquation {
    fn default() -> Self {
        Self::Math(Expr::var("x") + 1.0)
    }
}

// Auto generated
impl fmt::Debug for MethodEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Math(arg0) => f.debug_tuple("Math").field(arg0).finish(),
            Self::Internal(_) => f.debug_tuple("External").finish(),
        }
    }
}

impl Display for MethodEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MethodEquation::Math(math) => math.fmt(f),
            MethodEquation::Internal(_) => f.write_str("Internal rust function"),
        }
    }
}

impl MethodEquation {
    pub fn eval(&self, x: OpType) -> Result<OpType, EvaluationError> {
        match self {
            MethodEquation::Math(expr) => {
                let mut scope = Scope::default();
                scope.insert(
                    "x".into(),
                    Var {
                        name: "x".into(),
                        inner: x,
                    },
                );

                expr.eval(&scope)
            }
            MethodEquation::Internal(f) => Ok(f(x)),
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
        f: MethodEquation,
        range: (OpType, OpType),
        epsilon: OpType,
    ) -> Result<MethodResult, MethodError> {
        let mut a = range.0;
        let mut b = range.1;

        if sign_diff(
            f.eval(a).map_err(MethodError::EvaluationError)?,
            f.eval(b).map_err(MethodError::EvaluationError)?,
        ) {
            while self.iterations != self.max_iters {
                self.iterations += 1;

                let xi = (a + b) / 2.0;

                // Print iterations result
                if self.verbose >= 1 {
                    println!(
                        "Iter({}): [{a}; {b}], xi = {xi} ([{}; {}] {})",
                        self.iterations,
                        f.eval(a).map_err(MethodError::EvaluationError)?,
                        f.eval(b).map_err(MethodError::EvaluationError)?,
                        f.eval(xi).map_err(MethodError::EvaluationError)?
                    );
                }

                // Print iterations precision check
                if self.verbose == 2 {
                    println!(
                        "Iter({}): Precision check |{}| < {epsilon}",
                        self.iterations,
                        f.eval(b).map_err(MethodError::EvaluationError)?
                            - f.eval(a).map_err(MethodError::EvaluationError)?
                    );
                }

                // Compare signs to clip the range
                if sign_diff(
                    f.eval(a).map_err(MethodError::EvaluationError)?,
                    f.eval(xi).map_err(MethodError::EvaluationError)?,
                ) {
                    b = xi;
                } else {
                    a = xi;
                }

                // Check precision
                if (f.eval(b).map_err(MethodError::EvaluationError)?
                    - f.eval(a).map_err(MethodError::EvaluationError)?)
                .abs()
                    < epsilon
                {
                    let result = (b + a) / 2.0;
                    return Ok(MethodResult {
                        inner: (
                            result,
                            f.eval(result).map_err(MethodError::EvaluationError)?,
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
fn sign_diff(lhs: OpType, rhs: OpType) -> bool {
    lhs * rhs < 0.0
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    #[test]
    fn method_bisection() {} // TODO: Write tests
}
