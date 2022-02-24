use std::{fmt, ops::Mul};

use super::{
    expression::Expression,
    variable::{OperableType, Scope, VariableName},
};

#[derive(Debug)]
pub enum TermError {
    VariableNotFound(VariableName),
}

#[derive(Debug)]
pub enum Function {
    Sin,
    Cos,
    Tan,
}

#[derive(Debug)]
pub enum Term {
    Constant(OperableType),
    Variable(OperableType, VariableName),
    // TODO: Variable as exponent
    // TODO: e as exponent
    Exponent(Box<Term>, OperableType),
    Function(Function, Vec<Term>),
    Brackets(Box<Expression>),
}

// Constructors
impl Term {
    pub fn eval(&self, scope: &Scope) -> Result<OperableType, TermError> {
        match &self {
            Term::Constant(value) => Ok(*value),
            Term::Variable(coef, name) => match scope.get(name) {
                Some(var) => {
                    if *coef == 0.0 {
                        Ok(var.inner)
                    } else {
                        Ok(var.inner.mul(coef))
                    }
                }
                None => Err(TermError::VariableNotFound(name.clone())),
            },
            Term::Exponent(term, exp) => Ok(term.eval(scope)?.powf(*exp)),
            Term::Function(_, _) => unreachable!(),
            Term::Brackets(_) => unreachable!(),
        }
    }

    #[inline]
    pub fn var(num: OperableType, name: impl Into<VariableName>) -> Self {
        Self::Variable(num, name.into())
    }
}

impl Default for Term {
    fn default() -> Self {
        Self::Constant(1.0)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use Term::*;

        match self {
            Constant(num) => f.write_str(&num.to_string()),
            Variable(num, name) => {
                if *num != 1.0 {
                    f.write_str(&num.to_string())?;
                }
                f.write_str(name.as_str())
            }
            Exponent(term, exp) => {
                term.fmt(f)?;
                write!(f, "^{exp}")
            }
            Function(_, _) => unreachable!(),
            Brackets(_) => unreachable!(),
        }
    }
}

// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::variable::Scope;

    use super::{Term, TermError};

    #[test]
    fn print_const() {
        let constant = Term::Constant(2.5);

        assert_eq!(format!("{constant}"), "2.5".to_string());
    }

    #[test]
    fn print_var() {
        let var_no_coef = Term::var(1.0, "x");
        let var = Term::var(4.75, "y");

        assert_eq!(format!("{var_no_coef}"), "x".to_string());
        assert_eq!(format!("{var}"), "4.75y".to_string());
    }

    #[test]
    fn print_exp() {
        let exp = Term::Exponent(Box::new(Term::var(4.0, "z")), 3.0);

        assert_eq!(format!("{exp}"), "4z^3".to_string());
    }

    #[test]
    fn eval_const() -> Result<(), TermError> {
        let term = Term::Constant(2.0);

        assert_eq!(term.eval(&Scope::default())?, 2.0);

        Ok(())
    }
}
