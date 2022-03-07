use core::ops::{Add, Div, Mul, Sub};

use crate::expression::{EvaluationError, EvaluationResult};

#[derive(PartialOrd, PartialEq, Clone, Debug)]
pub enum Op {
    Sub = 1,
    Add,
    Mul,
    Div,
    Pow,
}

impl Op {
    pub fn as_str(&self) -> &str {
        use Op::*;

        match self {
            Sub => "-",
            Add => "+",
            Mul => "*",
            Div => "/",
            Pow => "^",
        }
    }

    pub fn calc(&self, lhs: f64, rhs: f64) -> EvaluationResult<f64> {
        use Op::*;

        Ok(match self {
            Sub => lhs.sub(rhs),
            Add => lhs.add(rhs),
            Mul => lhs.mul(rhs),
            Div => {
                if rhs != 0.0 {
                    lhs.div(rhs)
                } else {
                    return Err(EvaluationError::ZeroDivision);
                }
            }
            Pow => lhs.powf(rhs),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ops::Op;

    #[test]
    fn ops_ord() {
        assert!(Op::Add < Op::Mul);
        assert!(Op::Sub < Op::Div);
        assert!(Op::Pow > Op::Mul);
    }
}
