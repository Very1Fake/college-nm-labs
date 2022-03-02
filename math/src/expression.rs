use core::{
    f64::consts::E,
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{function::Func, token::Token};

use super::{
    ops::Op,
    variable::{OpType, Scope, VarName},
};

#[derive(Debug)]
pub enum EvaluationError {
    VariableNotFound(VarName),
    ZeroDivision,
}

pub trait Evaluable {
    fn eval(&self, scope: &Scope) -> Result<OpType, EvaluationError>;
}

// -------------------------------------------------------------------------------------------------

#[derive(PartialEq, Clone, Debug)]
pub enum MathConst {
    E,
}

impl MathConst {
    /// Get math constant value
    pub fn value(&self) -> OpType {
        match self {
            MathConst::E => E,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            MathConst::E => "e",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "e" => Some(Self::E),
            _ => None,
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Const(OpType),
    MathConst(MathConst),
    Var(VarName),
    Op(Op, Box<(Self, Self)>),
    Func(Func, Vec<Self>),
    Brackets(Box<Expr>),
}

impl Expr {
    #[inline]
    pub fn var(name: impl Into<VarName>) -> Self {
        Self::Var(name.into())
    }

    #[must_use]
    pub fn is_valid_postfix(&self, token: &Token) -> bool {
        match self {
            Expr::Const(_) => matches!(token, Token::Identifier(_)),
            Expr::Var(_) => matches!(token, Token::LeftParen),
            Expr::Op(Op::Mul, ops) => {
                matches!(ops.0, Expr::Const(_))
                    && matches!(ops.1, Expr::Var(_))
                    && matches!(token, Token::LeftParen)
            }
            _ => false,
        }
    }
}

impl Evaluable for Expr {
    fn eval(&self, scope: &Scope) -> Result<OpType, EvaluationError> {
        use Expr::*;

        match self {
            Const(value) => Ok(*value),
            MathConst(constant) => Ok(constant.value()),
            Var(name) => match scope.get(name) {
                Some(var) => Ok(var.inner),
                None => Err(EvaluationError::VariableNotFound(name.clone())),
            },
            Op(op, ops) => Ok(op.calc(ops.0.eval(scope)?, ops.1.eval(scope)?)),
            Func(func, args) => Ok(func.eval(
                args.iter()
                    .map(|expr| expr.eval(scope))
                    .collect::<Result<Vec<OpType>, EvaluationError>>()?,
            )),
            Brackets(expr) => expr.eval(scope),
        }
    }
}

impl Default for Expr {
    fn default() -> Self {
        Self::Const(1.0)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(num) => f.write_str(&num.to_string()),
            Self::MathConst(constant) => f.write_str(constant.as_str()),
            Self::Var(name) => f.write_str(name),
            Self::Op(op, ops) => match op {
                Op::Pow => write!(f, "{}{}{}", ops.0, op.as_str(), ops.1),
                Op::Mul => {
                    if let Expr::Var(_) = ops.1 {
                        ops.0.fmt(f)?;
                        ops.1.fmt(f)
                    } else {
                        write!(f, "{} {} {}", ops.0, op.as_str(), ops.1)
                    }
                }
                _ => write!(f, "{} {} {}", ops.0, op.as_str(), ops.1),
            },
            Self::Func(func, args) => {
                f.write_str(func.as_str())?;
                f.write_str("(")?;
                for (i, expr) in args.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    expr.fmt(f)?;
                }
                f.write_str(")")
            }
            Self::Brackets(expr) => write!(f, "({expr})"),
        }
    }
}

// -------------------------------------------------------------------------------------------------

impl Add for Expr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Op(
            Op::Add,
            Box::new((self.brace_if(&Op::Add), rhs.brace_if(&Op::Sub))),
        )
    }
}

impl Sub for Expr {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Op(
            Op::Sub,
            Box::new((self.brace_if(&Op::Sub), rhs.brace_if(&Op::Sub))),
        )
    }
}

impl Mul for Expr {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Op(
            Op::Mul,
            Box::new((self.brace_if(&Op::Mul), rhs.brace_if(&Op::Mul))),
        )
    }
}

impl Div for Expr {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Op(
            Op::Div,
            Box::new((self.brace_if(&Op::Div), rhs.brace_if(&Op::Div))),
        )
    }
}

impl Neg for Expr {
    type Output = Self;

    fn neg(self) -> Self::Output {
        // TODO: Optimize further
        match self {
            Expr::Const(num) => Expr::Const(-num),
            _ => self,
        }
    }
}

impl Add<OpType> for Expr {
    type Output = Self;

    fn add(self, rhs: OpType) -> Self::Output {
        Self::Op(
            Op::Add,
            Box::new((self.brace_if(&Op::Add), Self::Const(rhs))),
        )
    }
}

impl Sub<OpType> for Expr {
    type Output = Self;

    fn sub(self, rhs: OpType) -> Self::Output {
        Self::Op(
            Op::Sub,
            Box::new((self.brace_if(&Op::Sub), Self::Const(rhs))),
        )
    }
}

impl Mul<OpType> for Expr {
    type Output = Self;

    fn mul(self, rhs: OpType) -> Self::Output {
        Self::Op(
            Op::Mul,
            Box::new((self.brace_if(&Op::Mul), Self::Const(rhs))),
        )
    }
}

impl Div<OpType> for Expr {
    type Output = Self;

    fn div(self, rhs: OpType) -> Self::Output {
        Self::Op(
            Op::Div,
            Box::new((self.brace_if(&Op::Div), Self::Const(rhs))),
        )
    }
}

impl Add<Expr> for OpType {
    type Output = Expr;

    fn add(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Add,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Add))),
        )
    }
}

impl Sub<Expr> for OpType {
    type Output = Expr;

    fn sub(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Sub,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Sub))),
        )
    }
}

impl Mul<Expr> for OpType {
    type Output = Expr;

    fn mul(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Mul,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Mul))),
        )
    }
}

impl Div<Expr> for OpType {
    type Output = Expr;

    fn div(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Div,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Div))),
        )
    }
}

impl From<OpType> for Expr {
    fn from(num: OpType) -> Self {
        Expr::Const(num)
    }
}

impl Expr {
    /// Wraps self with parentheses if the given `Ops` has a higher priority
    pub fn brace_if(self, op: &Op) -> Self {
        match &self {
            Self::Op(inner, _) if inner < op => return Self::Brackets(Box::new(self)),
            _ => (),
        }

        self
    }

    pub fn pow(self, rhs: Self) -> Self {
        Expr::Op(
            Op::Pow,
            Box::new((self.brace_if(&Op::Pow), rhs.brace_if(&Op::Pow))),
        )
    }

    pub fn powf(self, rhs: OpType) -> Self {
        Expr::Op(Op::Pow, Box::new((self.brace_if(&Op::Pow), rhs.into())))
    }

    pub fn sin(self) -> Self {
        Expr::Func(Func::Sin, vec![self])
    }

    pub fn cos(self) -> Self {
        Expr::Func(Func::Cos, vec![self])
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        expression::{Evaluable, MathConst},
        variable::{Scope, Var},
    };

    use super::{EvaluationError, Expr};

    // Formats

    #[test]
    fn fmt_const() {
        let constant = Expr::Const(2.5);
        let math_const = Expr::MathConst(MathConst::E);

        assert_eq!(format!("{constant}"), "2.5".to_string());
        assert_eq!(format!("{math_const}"), "e".to_string());
    }

    #[test]
    fn fmt_var() {
        let var = Expr::var("x");
        let var_coef = 4.75 * Expr::var("y");
        let var_no_coef = 12.5 + Expr::var("z");

        assert_eq!(format!("{var}"), "x".to_string());
        assert_eq!(format!("{var_coef}"), "4.75y".to_string());
        assert_eq!(format!("{var_no_coef}"), "12.5 + z".to_string());
    }

    #[test]
    fn fmt_exp() {
        let exp_const = Expr::Const(25.0).powf(2.0);
        let exp_var = Expr::var("z").powf(4.0);
        let exp_e = Expr::MathConst(MathConst::E).pow(Expr::var("x"));

        assert_eq!(format!("{exp_const}"), "25^2".to_string());
        assert_eq!(format!("{exp_var}"), "z^4".to_string());
        assert_eq!(format!("{exp_e}"), "e^x".to_string());
    }

    #[test]
    fn fmt_expr() {
        let expr_1 = Expr::Const(2.0) * Expr::var("x") + Expr::Const(12.75);
        let expr_2 = (4.5 + Expr::var("x")).powf(2.0) / 2.5;
        let expr_3 = Expr::var("x") * Expr::MathConst(MathConst::E).pow(Expr::var("x")).sin();

        assert_eq!(format!("{expr_1}"), "2x + 12.75");
        assert_eq!(format!("{expr_2}"), "(4.5 + x)^2 / 2.5");
        assert_eq!(format!("{expr_3}"), "x * sin(e^x)");
    }

    // Evaluations

    #[test]
    fn eval_const() -> Result<(), EvaluationError> {
        let c = Expr::Const(2.0);

        assert_eq!(c.eval(&Scope::default())?, 2.0);

        Ok(())
    }

    #[test]
    fn eval_var() -> Result<(), EvaluationError> {
        let var = Expr::var("x");
        let var_coef = 3.2 * Expr::var("x");

        let mut scope = Scope::default();
        scope.insert(Var {
            name: "x".into(),
            inner: 5.0,
        });

        assert_eq!(var.eval(&scope)?, 5.0);
        assert_eq!(var_coef.eval(&scope)?, 16.0);

        Ok(())
    }

    #[test]
    fn eval_exp() -> Result<(), EvaluationError> {
        let exp = Expr::Const(2.0).powf(10.0);

        assert_eq!(exp.eval(&Scope::default())?, 1024.0);

        Ok(())
    }

    #[test]
    fn eval_func() -> Result<(), EvaluationError> {
        let func_sin = Expr::Const(4.0).sin();
        let func_cos = Expr::Const(5.0).cos();

        assert_eq!(func_sin.eval(&Scope::default())?, -0.7568024953079282);
        assert_eq!(func_cos.eval(&Scope::default())?, 0.28366218546322625);

        Ok(())
    }

    #[test]
    fn eval_expr() -> Result<(), EvaluationError> {
        let expr_1 = 2.0 * Expr::var("x") + 12.75;
        let expr_2 = (Expr::Const(12.0) - 3.0) * 3.0 * Expr::var("x");
        let expr_3 = (4.5 + Expr::var("x")).powf(2.0) / 2.5;
        let mut scope = Scope::default();
        scope.insert(Var {
            name: "x".into(),
            inner: 4.5,
        });

        assert_eq!(expr_1.eval(&scope)?, 21.75);
        assert_eq!(expr_2.eval(&scope)?, 121.5);
        assert_eq!(expr_3.eval(&scope)?, 32.4);

        Ok(())
    }
}
