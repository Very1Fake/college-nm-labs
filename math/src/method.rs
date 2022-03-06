use std::{
    fmt::{self, Display},
    time::Duration,
    time::Instant,
};

use thiserror::Error;

use crate::{
    expression::{Evaluable, EvaluationError, Expr},
    variable::{OpType, Scope, Var},
};

const MAX_ITERS_DEFAULT: u128 = 1_000_000;

pub type Interval = (OpType, OpType);

#[derive(Error, Debug)]
pub enum MethodError {
    #[error("Iterations limit exceeded")]
    IterationsExceeded(u128),
    #[error("Evaluation error while execution method: {0}")]
    EvaluationError(#[from] EvaluationError),
    #[error("{0}")]
    Other(String),
}

#[derive(Debug)]
pub struct CallStats {
    pub elapsed: Duration,
    pub iterations: u128,
}

#[derive(Debug)]
pub struct MethodResult {
    pub root: (OpType, OpType),
    pub stats: CallStats,
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum OutPut<'a> {
    Stdout,
    Vec(&'a mut Vec<String>),
}

impl<'a> OutPut<'a> {
    #[inline]
    fn print(&mut self, text: String) {
        match self {
            OutPut::Stdout => println!("{text}"),
            OutPut::Vec(vec) => vec.push(text),
        }
    }
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
                scope.insert(Var::new("x", x));

                expr.eval(&scope)
            }
            MethodEquation::Internal(f) => Ok(f(x)),
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Method<'a> {
    start: Instant,
    iterations: u128,
    out: OutPut<'a>,
    verbose: u8,

    // Limits
    max_iters: u128,
}

impl<'a> Method<'a> {
    pub fn new(max_iters: u128, out: OutPut<'a>) -> Self {
        Method {
            start: Instant::now(),
            iterations: 0,
            out,
            verbose: 0,
            max_iters,
        }
    }

    #[inline]
    pub fn verbose(mut self, verbose: u8) -> Self {
        self.verbose = verbose;
        self
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// Bisection method
    ////////////////////////////////////////////////////////////////////////////////////////////////

    pub fn bisection(
        mut self,
        f: MethodEquation,
        interval: Interval,
        precision: OpType,
    ) -> Result<MethodResult, MethodError> {
        let (mut a, mut b) = interval;

        if sign_diff(
            f.eval(a).map_err(MethodError::EvaluationError)?,
            f.eval(b).map_err(MethodError::EvaluationError)?,
        ) {
            while self.iterations != self.max_iters {
                self.iterations += 1;

                let xi = (a + b) / 2.0;

                // Print iterations result
                if self.verbose >= 1 {
                    self.out.print(format!(
                        "Iter({}): [{a}; {b}], xi = {xi} ([{}; {}] {})",
                        self.iterations,
                        f.eval(a).map_err(MethodError::EvaluationError)?,
                        f.eval(b).map_err(MethodError::EvaluationError)?,
                        f.eval(xi).map_err(MethodError::EvaluationError)?
                    ));
                }

                // Print iterations precision check
                if self.verbose == 2 {
                    self.out.print(format!(
                        "Iter({}): Precision check |{}| < {precision}",
                        self.iterations,
                        f.eval(b).map_err(MethodError::EvaluationError)?
                            - f.eval(a).map_err(MethodError::EvaluationError)?
                    ));
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
                    < precision
                {
                    let result = (b + a) / 2.0;
                    return Ok(MethodResult {
                        root: (
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

            Err(MethodError::IterationsExceeded(self.iterations))
        } else {
            Err(MethodError::Other(format!(
                "Equation has no roots on this interval {interval:?}"
            )))
        }
    }
}

impl Default for Method<'_> {
    fn default() -> Self {
        Self::new(MAX_ITERS_DEFAULT, OutPut::Stdout)
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
