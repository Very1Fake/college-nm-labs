use std::{fmt, ops::{Sub, Add, Mul, Div}};

use itertools::{EitherOrBoth, Itertools};
use smallvec::SmallVec;

use super::{
    ops::Operation,
    term::{Term, TermError},
    variable::{Scope, VariableName, OperableType},
    INLINED,
};

#[derive(Debug)]
pub enum ExpressionError {
    VariableNotFound(VariableName),
    TermNotFound((Operation, usize)),
    TermError(TermError),
}

#[derive(Default, Debug)]
pub struct Expression {
    terms: SmallVec<[Term; INLINED]>,
    ops: SmallVec<[(Operation, (usize, usize)); INLINED]>,
}

impl Expression {
    pub fn eval(&self, scope: &Scope) -> Result<OperableType, ExpressionError> {
        if self.is_zero() {
            Ok(0.0)
        } else {
            // Operation calculation
            let result = self.ops.iter().try_fold(0.0, |_, (op, (lhs, rhs))| {
                let lhs =  match self.terms.get(*lhs) {
                    Some(value) => value.eval(scope).map_err(|err| ExpressionError::TermError(err))?,
                    None => return Err(ExpressionError::TermNotFound((op.clone(), *lhs))),
                };
                let rhs =  match self.terms.get(*rhs) {
                    Some(value) => value.eval(scope).map_err(|err| ExpressionError::TermError(err))?,
                    None => return Err(ExpressionError::TermNotFound((op.clone(), *rhs))),
                };

                Ok(match op {
                    Operation::Sub => lhs.sub(rhs),
                    Operation::Add => lhs.add(rhs),
                    Operation::Mul => lhs.mul(rhs),
                    Operation::Div => lhs.div(rhs),
                })
            })?;

            Ok(result)
        }
    }

    pub fn is_zero(&self) -> bool {
        self.terms.is_empty() && self.ops.is_empty()
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self.terms.iter().zip_longest(self.ops.clone()) {
            match i {
                EitherOrBoth::Both(term, (op, _)) => {
                    term.fmt(f)?;
                    f.write_str(op.as_pretty())?;
                }
                EitherOrBoth::Left(term) => term.fmt(f)?,
                EitherOrBoth::Right(_) => unreachable!(),
            };
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Equation {
    lhs: Expression,
    rhs: Expression,
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use smallvec::smallvec;

    use crate::math::{ops::Operation, term::Term, variable::Variable};

    use super::{Expression, ExpressionError};

    #[test]
    fn print_expr() {
        let expr = Expression {
            terms: smallvec![Term::var(2.0, "x"), Term::Constant(12.75)],
            ops: smallvec![(Operation::Add, (0, 1))],
        };

        assert_eq!(format!("{expr}"), "2x + 12.75");
    }

    #[test]
    fn eval() -> Result<(), ExpressionError> {
        let expr_zero = Expression::default();
        let expr = Expression {
            terms: smallvec![Term::var(2.0, "x"), Term::Constant(12.75)],
            ops: smallvec![(Operation::Add, (0, 1))],
        };
        let mut scope = BTreeMap::default();
        scope.insert("x".into(), Variable {
            name: "x".into(),
            inner: 4.5,
        });

        assert_eq!(expr_zero.eval(&scope)?, 0.0);
        assert_eq!(expr.eval(&scope)?, 21.75);

        Ok(())
    }
}
