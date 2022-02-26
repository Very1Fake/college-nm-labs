use core::ops::{Add, Div, Mul, Sub};

use crate::variable::OpType;

// TODO: Add modulo
#[derive(PartialOrd, PartialEq, Clone, Debug)]
pub enum Ops {
    Sub = 0,
    Add,
    Mul,
    Div,
    Pow,
}

impl Ops {
    pub fn as_str(&self) -> &str {
        use Ops::*;

        match self {
            Sub => "-",
            Add => "+",
            Mul => "*",
            Div => "/",
            Pow => "^",
        }
    }

    pub fn calc(&self, lhs: OpType, rhs: OpType) -> OpType {
        use Ops::*;

        match self {
            Sub => lhs.sub(rhs),
            Add => lhs.add(rhs),
            Mul => lhs.mul(rhs),
            Div => lhs.div(rhs),
            Pow => lhs.powf(rhs),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ops::Ops;

    #[test]
    fn ops_ord() {
        assert!(Ops::Add < Ops::Mul);
        assert!(Ops::Sub < Ops::Div);
        assert!(Ops::Pow > Ops::Mul);
    }
}
