use core::{
    f64::consts::{E, PI},
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

use thiserror::Error;

use crate::{function::Func, token::Token};

use super::{
    ops::Op,
    variable::{Scope, VarName},
};

pub type EvaluationResult<T> = Result<T, EvaluationError>;

#[inline]
fn get_arg(args: &Vec<Expr>, id: usize) -> Expr {
    args.get(id).expect("Arg not found").clone()
}

// -------------------------------------------------------------------------------------------------

#[derive(Error, Debug)]
pub enum EvaluationError {
    #[error("Variable '{0}' not found")]
    VariableNotFound(VarName),
    #[error("Division by zero")]
    ZeroDivision,
}

pub trait Evaluable {
    fn eval(&self, scope: &Scope) -> EvaluationResult<f64>;
}

// -------------------------------------------------------------------------------------------------

#[derive(PartialEq, Clone, Debug)]
pub enum MathConst {
    E,
    Pi,
}

impl MathConst {
    /// Get math constant value
    pub fn value(&self) -> f64 {
        match self {
            Self::E => E,
            Self::Pi => PI,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::E => "e",
            Self::Pi => "π",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "e" => Some(Self::E),
            "pi" | "π" => Some(Self::Pi),
            _ => None,
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Const(f64),
    MathConst(MathConst),
    Var(VarName),
    Op(Op, Box<(Self, Self)>),
    Fn(Func, Vec<Self>),
    Brackets(Box<Expr>),
    Neg(Box<Expr>),
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

    /// Check if `Expr` is `coef * variable[^exp]` (coef must be constant)
    fn is_coef_term(&self) -> bool {
        if let Self::Op(Op::Mul, ops) = self {
            match &**ops {
                (
                    Self::Const(_) | Self::MathConst(_),
                    var @ (Self::Var(_) | Self::Op(Op::Pow, _)),
                ) => match var {
                    Self::Var(_) => true,
                    Self::Op(_, ops)
                        if matches!(ops.0, Self::Var(_)) && matches!(ops.1, Self::Const(_)) =>
                    {
                        true
                    }
                    _ => false,
                },
                _ => false,
            }
        } else {
            false
        }
    }

    /// Fix redundancy of the expression
    pub fn fix(&self) -> Self {
        match self {
            Self::Op(op, ops) => {
                match op {
                    Op::Add | Op::Sub if matches!(ops.1, Self::Const(_)) => match ops.1 {
                        // x ± 0 = x
                        Self::Const(num) if num == 0.0 => ops.0.fix(),
                        _ => match ops.0 {
                            // 0 ± x = x
                            Self::Const(num) if num == 0.0 => ops.1.fix(),
                            _ => self.clone(),
                        },
                    },
                    Op::Mul => match ops.1 {
                        // x * 1 = x
                        Self::Const(num) if num == 1.0 => ops.0.fix(),
                        _ => match ops.0 {
                            // 1 * x = x
                            Self::Const(num) if num == 1.0 => ops.1.fix(),
                            _ => Self::Op(*op, Box::new((ops.0.fix(), ops.1.fix()))),
                        },
                    },
                    Op::Pow => match ops.1 {
                        // x^1 = x
                        Self::Const(num) if num == 1.0 => ops.0.fix(),
                        _ => Self::Op(*op, Box::new((ops.0.fix(), ops.1.fix()))),
                    },
                    _ => Self::Op(*op, Box::new((ops.0.fix(), ops.1.fix()))),
                }
            }
            Self::Fn(name, args) => {
                Self::Fn(*name, args.into_iter().map(|expr| expr.fix()).collect())
            }
            Self::Brackets(expr) => expr.fix(),
            Self::Neg(expr) => match &**expr {
                // -(2) = -2
                num @ Self::Const(_) => -num.clone(),
                _ => Self::Neg(Box::new(expr.fix())),
            },
            Self::Const(_) | Self::MathConst(_) | Self::Var(_) => self.clone(),
        }
    }

    // Shorten the expression
    pub fn optimize(&self) -> EvaluationResult<Self> {
        Ok(match self {
            // Execute operation if both operands are constants
            op @ Self::Op(_, ops) if matches!(**ops, (Self::Const(_), Self::Const(_))) => {
                Self::Const(op.eval(&Scope::Empty)?)
            }
            // a * bx = abx
            Self::Op(Op::Mul, ops)
                if (matches!(ops.0, Self::Const(_)) || ops.0.is_coef_term())
                    && (matches!(ops.1, Self::Const(_)) || ops.1.is_coef_term()) =>
            {
                match &**ops {
                    (a @ Self::Const(_), Self::Op(Op::Mul, ops))
                    | (Self::Op(Op::Mul, ops), a @ Self::Const(_)) => Self::Op(
                        Op::Mul,
                        Box::new((
                            Self::Const((a.clone() * ops.0.clone()).eval(&Scope::Empty)?),
                            ops.1.clone(),
                        )),
                    ),
                    _ => unreachable!(),
                }
            }
            Self::Op(op, ops) => {
                let mut ops = ops.clone();
                ops.0 = ops.0.optimize()?;
                ops.1 = ops.1.optimize()?;
                Self::Op(*op, ops).fix()
            }
            Self::Const(_)
            | Self::MathConst(_)
            | Self::Var(_)
            | Self::Fn(..)
            | Self::Brackets(_)
            | Self::Neg(_) => self.clone(),
        })
    }

    // TODO: Add u' table
    pub fn derivative(&self) -> Self {
        #[inline]
        fn uv_mul(uv: &(Expr, Expr)) -> Expr {
            let (u, v) = uv.clone();
            Expr::Op(
                Op::Mul,
                Box::new((
                    Expr::Op(Op::Pow, Box::new((u.clone(), v.clone()))),
                    Expr::Op(Op::Mul, Box::new((v, Expr::Fn(Func::Ln, vec![u])))).derivative(),
                )),
            )
        }

        match self {
            // a' = 0
            Self::Const(_) => Self::Const(0.0),
            // e' = 0
            Self::MathConst(_) => Self::Const(0.0),
            // x' = 1
            Self::Var(_) => Self::Const(1.0),
            Self::Op(op, ops) => match op {
                // (a ± b)' = a' ± b'
                Op::Sub | Op::Add => Self::Op(
                    op.clone(),
                    Box::new((ops.0.derivative(), ops.1.derivative())),
                ),
                Op::Mul => match &**ops {
                    // (cx)' = c * x'
                    (
                        coef @ (Self::Const(_) | Self::MathConst(_)),
                        f @ (Self::Var(_)
                        | Self::Op(..)
                        | Self::Fn(..)
                        | Self::Brackets(_)
                        | Self::Neg(_)),
                    ) => Self::Op(Op::Mul, Box::new((coef.clone(), f.derivative()))),
                    // (uv)' = u'v + uv'
                    _ => Self::Op(
                        Op::Add,
                        Box::new((
                            Self::Op(Op::Mul, Box::new((ops.0.derivative(), ops.1.clone()))),
                            Self::Op(Op::Mul, Box::new((ops.0.clone(), ops.1.derivative()))),
                        )),
                    ),
                },
                // (u / v)' = (u'v - uv') / v^2
                Op::Div => Self::Op(
                    Op::Div,
                    Box::new((
                        Self::Brackets(Box::new(Self::Op(
                            Op::Sub,
                            Box::new((
                                Self::Op(Op::Mul, Box::new((ops.0.derivative(), ops.1.clone()))),
                                Self::Op(Op::Mul, Box::new((ops.0.clone(), ops.1.derivative()))),
                            )),
                        ))),
                        Self::Op(Op::Pow, Box::new((ops.1.clone(), Self::Const(2.0)))),
                    )),
                ),
                Op::Pow => match &**ops {
                    // (e^x)' = e^x
                    (Self::MathConst(MathConst::E), x @ Self::Var(_)) => Self::Op(
                        Op::Pow,
                        Box::new((Self::MathConst(MathConst::E), x.clone())),
                    ),
                    // (x^a)' = ax^a-1
                    (
                        x @ (Self::Var(_) | Self::Op(Op::Mul, _)),
                        a @ (Self::Const(_) | Self::MathConst(_)),
                    ) if matches!(x, Self::Var(_)) || x.is_coef_term() => Self::Op(
                        Op::Pow,
                        Box::new((
                            Self::Op(Op::Mul, Box::new((a.clone(), x.clone()))),
                            a.clone() - 1.0,
                        )),
                    ),
                    // (a^x)' = a^x * ln(a)
                    (
                        a @ (Self::Const(_) | Self::MathConst(_)),
                        x @ (Self::Var(_) | Self::Op(Op::Mul, _)),
                    ) if matches!(x, Self::Var(_)) || x.is_coef_term() => Self::Op(
                        Op::Mul,
                        Box::new((
                            Self::Op(Op::Pow, Box::new((a.clone(), x.clone()))),
                            Self::Fn(Func::Ln, vec![a.clone()]),
                        )),
                    ),
                    // (u^v)' = u^v * (v * ln(u))'
                    uv => uv_mul(uv),
                },
            },
            // TODO: Add a new match arm to find derivate of function argument if its not x
            Self::Fn(func, args) => {
                // Find derivatives of arguments if it's not x
                // g(f(x))' =
                let args = args
                    .iter()
                    .map(|expr| {
                        if !matches!(expr, Self::Const(_) | Self::MathConst(_) | Self::Var(_)) {
                            expr.derivative()
                        } else {
                            expr.clone()
                        }
                    })
                    .collect::<Vec<Expr>>();

                match func {
                    // (√x)' = 1 / 2√x
                    Func::Sqrt => Self::Op(
                        Op::Div,
                        Box::new((
                            Self::Const(1.0),
                            Self::Op(
                                Op::Mul,
                                Box::new((Self::Const(2.0), Self::Fn(Func::Sqrt, args.clone()))),
                            ),
                        )),
                    ),
                    // (|f|)' = |f'|
                    Func::Abs => Self::Fn(Func::Abs, vec![get_arg(&args, 0).derivative()]),
                    // (sin x)' = cos x
                    Func::Sin => Self::Fn(Func::Cos, args.clone()),
                    // (cos x)' = -sin x
                    Func::Cos => Self::Neg(Box::new(Self::Fn(Func::Sin, args.clone()))),
                    // (tan x)' = 1 / cos(x)^2
                    Func::Tan => Self::Op(
                        Op::Div,
                        Box::new((
                            Self::Const(1.0),
                            Self::Op(
                                Op::Pow,
                                Box::new((Self::Fn(Func::Cos, args.clone()), Self::Const(2.0))),
                            ),
                        )),
                    ),
                    // (asin x)' = 1 / √(1 - x^2)
                    Func::ASin => Self::Op(
                        Op::Div,
                        Box::new((
                            Self::Const(1.0),
                            Self::Fn(
                                Func::Sqrt,
                                vec![Self::Op(
                                    Op::Sub,
                                    Box::new((
                                        Self::Const(1.0),
                                        Self::Op(
                                            Op::Pow,
                                            Box::new((get_arg(&args, 0), Self::Const(2.0))),
                                        ),
                                    )),
                                )],
                            ),
                        )),
                    ),
                    // (acos x)' = -(1 / √(1 - x^2))
                    Func::ACos => Self::Neg(Box::new(Self::Op(
                        Op::Div,
                        Box::new((
                            Self::Const(1.0),
                            Self::Fn(
                                Func::Sqrt,
                                vec![Self::Op(
                                    Op::Sub,
                                    Box::new((
                                        Self::Const(1.0),
                                        Self::Op(
                                            Op::Pow,
                                            Box::new((get_arg(&args, 0), Self::Const(2.0))),
                                        ),
                                    )),
                                )],
                            ),
                        )),
                    ))),
                    // (atan x)' = 1 / (1 + x^2)
                    Func::ATan => Self::Op(
                        Op::Div,
                        Box::new((
                            Self::Const(1.0),
                            Self::Op(
                                Op::Add,
                                Box::new((
                                    Self::Const(1.0),
                                    Self::Op(
                                        Op::Pow,
                                        Box::new((get_arg(&args, 0), Self::Const(2.0))),
                                    ),
                                )),
                            ),
                        )),
                    ),
                    // (ln x)' = 1 / x
                    Func::Ln => Self::Op(Op::Div, Box::new((Self::Const(1.0), get_arg(&args, 0)))),
                    // (log_a x)' = 1 / (x * ln a)
                    Func::Log => Self::Op(
                        Op::Div,
                        Box::new((
                            Self::Const(1.0),
                            Self::Op(
                                Op::Mul,
                                Box::new((
                                    get_arg(&args, 0),
                                    Self::Fn(Func::Ln, vec![get_arg(&args, 1)]),
                                )),
                            ),
                        )),
                    ),
                }
            }
            Self::Brackets(expr) => Self::Brackets(Box::new(expr.derivative())),
            Self::Neg(expr) => expr.derivative(),
        }
    }
}

impl Evaluable for Expr {
    fn eval(&self, scope: &Scope) -> EvaluationResult<f64> {
        use Expr::*;

        match self {
            Const(value) => Ok(*value),
            MathConst(constant) => Ok(constant.value()),
            Var(name) => match scope.get(name) {
                Some(var) => Ok(var.value),
                None => Err(EvaluationError::VariableNotFound(name.clone())),
            },
            Op(op, ops) => Ok(op.calc(ops.0.eval(scope)?, ops.1.eval(scope)?)?),
            Fn(func, args) => Ok(func.eval(
                &args
                    .iter()
                    .map(|expr| expr.eval(scope))
                    .collect::<EvaluationResult<Vec<f64>>>()?,
            )),
            Brackets(expr) => expr.eval(scope),
            Neg(expr) => Ok(expr.eval(scope)? * -1.0),
        }
    }
}

impl Default for Expr {
    fn default() -> Self {
        Self::Const(0.0)
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
                Op::Mul => match **ops {
                    (Expr::Const(_), Expr::Var(_) | Expr::Fn(..)) => {
                        ops.0.fmt(f)?;
                        ops.1.fmt(f)
                    }
                    (Expr::Var(_) | Expr::Fn(..), Expr::Const(_)) => {
                        ops.1.fmt(f)?;
                        ops.0.fmt(f)
                    }
                    _ => write!(f, "{} {} {}", ops.0, op.as_str(), ops.1),
                },
                Op::Div => {
                    match &ops.0 {
                        lhs @ Self::Op(Op::Sub | Op::Add | Op::Mul | Op::Div, _) => {
                            write!(f, "({lhs})")?
                        }
                        lhs => lhs.fmt(f)?,
                    }

                    f.write_str(" / ")?;

                    match &ops.1 {
                        rhs @ Self::Op(Op::Sub | Op::Add | Op::Mul | Op::Div, _) => {
                            write!(f, "({rhs})")
                        }
                        rhs => rhs.fmt(f),
                    }
                }
                _ => write!(f, "{} {} {}", ops.0, op.as_str(), ops.1),
            },
            Self::Fn(func, args) => {
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
            Self::Neg(expr) => match &**expr {
                expr @ Self::Op(..) => write!(f, "-({expr})"),
                expr => {
                    f.write_str("-")?;
                    expr.fmt(f)
                }
            },
        }
    }
}

// -------------------------------------------------------------------------------------------------

impl Add for Expr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Op(
            Op::Add,
            Box::new((self.brace_if(&Op::Add), rhs.brace_if(&Op::Add))),
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
        match self {
            // For a constant, reverse sign
            Expr::Const(num) => Expr::Const(-num),
            // For terms like '-2x', reverse sign for coefficient
            Expr::Op(Op::Mul, mut ops)
                if matches!(ops.0, Expr::Const(_)) && matches!(ops.1, Expr::Var(_)) =>
            {
                ops.0 = -ops.0;
                Expr::Op(Op::Mul, ops)
            }
            // Otherwise, multiply by -1
            _ => Expr::Neg(Box::new(self)),
        }
    }
}

impl Add<f64> for Expr {
    type Output = Self;

    fn add(self, rhs: f64) -> Self::Output {
        Self::Op(
            Op::Add,
            Box::new((self.brace_if(&Op::Add), Self::Const(rhs))),
        )
    }
}

impl Sub<f64> for Expr {
    type Output = Self;

    fn sub(self, rhs: f64) -> Self::Output {
        Self::Op(
            Op::Sub,
            Box::new((self.brace_if(&Op::Sub), Self::Const(rhs))),
        )
    }
}

impl Mul<f64> for Expr {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        Self::Op(
            Op::Mul,
            Box::new((self.brace_if(&Op::Mul), Self::Const(rhs))),
        )
    }
}

impl Div<f64> for Expr {
    type Output = Self;

    fn div(self, rhs: f64) -> Self::Output {
        Self::Op(
            Op::Div,
            Box::new((self.brace_if(&Op::Div), Self::Const(rhs))),
        )
    }
}

impl Add<Expr> for f64 {
    type Output = Expr;

    fn add(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Add,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Add))),
        )
    }
}

impl Sub<Expr> for f64 {
    type Output = Expr;

    fn sub(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Sub,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Sub))),
        )
    }
}

impl Mul<Expr> for f64 {
    type Output = Expr;

    fn mul(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Mul,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Mul))),
        )
    }
}

impl Div<Expr> for f64 {
    type Output = Expr;

    fn div(self, rhs: Expr) -> Self::Output {
        Expr::Op(
            Op::Div,
            Box::new((Expr::Const(self), rhs.brace_if(&Op::Div))),
        )
    }
}

impl From<f64> for Expr {
    fn from(num: f64) -> Self {
        Expr::Const(num)
    }
}

impl Expr {
    /// Wraps self with parentheses if the given `Ops` has a higher priority
    pub fn brace_if(self, op: &Op) -> Self {
        if let Self::Op(inner, _) = &self {
            match op {
                Op::Pow if self.is_coef_term() => (),
                _ if inner < op => return Self::Brackets(Box::new(self)),
                _ => (),
            }
        }

        self
    }

    pub fn pow(self, rhs: Self) -> Self {
        Self::Op(
            Op::Pow,
            Box::new((self.brace_if(&Op::Pow), rhs.brace_if(&Op::Pow))),
        )
    }

    pub fn powf(self, rhs: f64) -> Self {
        Self::Op(Op::Pow, Box::new((self.brace_if(&Op::Pow), rhs.into())))
    }

    pub fn sqrt(self) -> Self {
        Self::Fn(Func::Sqrt, vec![self])
    }

    pub fn sin(self) -> Self {
        Self::Fn(Func::Sin, vec![self])
    }

    pub fn cos(self) -> Self {
        Self::Fn(Func::Cos, vec![self])
    }

    pub fn tan(self) -> Self {
        Self::Fn(Func::Tan, vec![self])
    }

    pub fn asin(self) -> Self {
        Self::Fn(Func::ASin, vec![self])
    }

    pub fn acos(self) -> Self {
        Self::Fn(Func::ACos, vec![self])
    }

    pub fn atan(self) -> Self {
        Self::Fn(Func::ATan, vec![self])
    }

    pub fn ln(self) -> Self {
        Self::Fn(Func::Ln, vec![self])
    }

    pub fn log(self, base: f64) -> Self {
        Self::Fn(Func::Log, vec![self, Self::Const(base)])
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use crate::{
        expression::{Evaluable, MathConst::E},
        variable::{Scope, Var},
    };

    use super::{EvaluationResult, Expr};

    // Formats

    #[test]
    fn fmt_const() {
        let constant = Expr::Const(2.5);
        let math_const = Expr::MathConst(E);

        assert_eq!(format!("{constant}"), "2.5");
        assert_eq!(format!("{math_const}"), "e");
    }

    #[test]
    fn fmt_var() {
        let var = Expr::var("x");
        let var_neg = -Expr::var("z");
        let var_var = Expr::var("y") * Expr::var("x");
        let var_coef = 4.75 * Expr::var("y");
        let var_no_coef = 12.5 + Expr::var("z");

        assert_eq!(format!("{var}"), "x");
        assert_eq!(format!("{var_neg}"), "-z");
        assert_eq!(format!("{var_var}"), "y * x");
        assert_eq!(format!("{var_coef}"), "4.75y");
        assert_eq!(format!("{var_no_coef}"), "12.5 + z");
    }

    #[test]
    fn fmt_exp() {
        let exp_const = Expr::Const(25.0).powf(2.0);
        let exp_var = Expr::var("z").powf(4.0);
        let exp_e = Expr::MathConst(E).pow(Expr::var("x"));

        assert_eq!(format!("{exp_const}"), "25^2");
        assert_eq!(format!("{exp_var}"), "z^4");
        assert_eq!(format!("{exp_e}"), "e^x");
    }

    #[test]
    fn fmt_func() {
        let func = Expr::Const(1.0).sin();
        let func_coef = 2.0 * Expr::Const(0.5).cos();

        assert_eq!(format!("{func}"), "sin(1)");
        assert_eq!(format!("{func_coef}"), "2cos(0.5)");
    }

    #[test]
    fn fmt_expr() {
        let expr_1 = Expr::Const(2.0) * Expr::var("x") + Expr::Const(12.75);
        let expr_2 = (4.5 + Expr::var("x")).powf(2.0) / 2.5;
        let expr_3 = Expr::var("x") * Expr::MathConst(E).pow(Expr::var("x")).sin();

        assert_eq!(format!("{expr_1}"), "2x + 12.75");
        assert_eq!(format!("{expr_2}"), "(4.5 + x)^2 / 2.5");
        assert_eq!(format!("{expr_3}"), "x * sin(e^x)");
    }

    // Evaluations

    #[test]
    fn eval_const() -> Result<()> {
        let c = Expr::Const(2.0);

        assert_eq!(c.eval(&Scope::Empty)?, 2.0);

        Ok(())
    }

    #[test]
    fn eval_var() -> Result<()> {
        let var = Expr::var("x");
        let var_coef = 3.2 * Expr::var("x");

        let scope = Scope::Single(Var::new("x", 5.0));

        assert_eq!(var.eval(&scope)?, 5.0);
        assert_eq!(var_coef.eval(&scope)?, 16.0);

        Ok(())
    }

    #[test]
    fn eval_exp() -> Result<()> {
        let exp = Expr::Const(2.0).powf(10.0);

        assert_eq!(exp.eval(&Scope::Empty)?, 1024.0);

        Ok(())
    }

    #[test]
    fn eval_func() -> Result<()> {
        let func_sin = Expr::Const(4.0).sin();
        let func_cos = Expr::Const(5.0).cos();

        assert_eq!(func_sin.eval(&Scope::Empty)?, -0.7568024953079282);
        assert_eq!(func_cos.eval(&Scope::Empty)?, 0.28366218546322625);

        Ok(())
    }

    #[test]
    fn eval_expr() -> Result<()> {
        let expr_1 = 2.0 * Expr::var("x") + 12.75;
        let expr_2 = (Expr::Const(12.0) - 3.0) * 3.0 * Expr::var("x");
        let expr_3 = (4.5 + Expr::var("x")).powf(2.0) / 2.5;
        let scope = Scope::Single(Var::new("x", 4.5));

        assert_eq!(expr_1.eval(&scope)?, 21.75);
        assert_eq!(expr_2.eval(&scope)?, 121.5);
        assert_eq!(expr_3.eval(&scope)?, 32.4);

        Ok(())
    }

    // Derivative

    #[test]
    fn derivative_x_table() {
        let cases = vec![
            // c'
            (Expr::Const(2.0), "0"),
            // (√x)'
            (Expr::var("x").sqrt(), "1 / (2sqrt(x))"),
            // (x^a)'
            (Expr::var("x").powf(4.0), "4x^4 - 1"),
            // (a^x)'
            (Expr::Const(3.0).pow(Expr::var("x")), "3^x * ln(3)"),
            // (e^x)'
            (Expr::MathConst(E).pow(Expr::var("x")), "e^x"),
            // (ln x)'
            (Expr::var("x").ln(), "1 / x"),
            // (log x)'
            (Expr::var("x").log(4.0), "1 / (x * ln(4))"),
            // (sin x)'
            (Expr::var("x").sin(), "cos(x)"),
            // (cos x)'
            (Expr::var("x").cos(), "-sin(x)"),
            // (tg x)'
            (Expr::var("x").tan(), "1 / cos(x)^2"),
            // (asin x)'
            (Expr::var("x").asin(), "1 / sqrt(1 - x^2)"),
            // (acos x)'
            (Expr::var("x").acos(), "-(1 / sqrt(1 - x^2))"),
            // (atg x)'
            (Expr::var("x").atan(), "1 / (1 + x^2)"),
        ];

        for (expr, expect) in cases {
            assert_eq!(format!("{}", expr.derivative().fix()), expect);
        }
    }

    #[test]
    fn differentiation() -> EvaluationResult<()> {
        let cases = vec![
            // cx' = c * x'
            (3.0 * Expr::var("x"), "3"),
            // (u+v)' = u' + v'
            (
                (2.0 * Expr::var("x")).powf(2.0) + 4.0 * Expr::var("x"),
                "4x + 4",
            ),
            // (uv)' = u' * v'
            (
                Expr::var("x").powf(4.0) * Expr::var("x").sin(),
                "4x^3 * sin(x) + x^4 * cos(x)",
            ),
        ];

        for (expr, expect) in cases {
            assert_eq!(format!("{}", expr.derivative().fix().optimize()?), expect);
        }

        Ok(())
    }
}
