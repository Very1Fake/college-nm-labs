use core::ops::{Add, Div, Mul, Sub};

use crate::variable::OpType;

// TODO: Add modulo
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

    pub fn calc(&self, lhs: OpType, rhs: OpType) -> OpType {
        use Op::*;

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
    use crate::ops::Op;

    #[test]
    fn ops_ord() {
        assert!(Op::Add < Op::Mul);
        assert!(Op::Sub < Op::Div);
        assert!(Op::Pow > Op::Mul);
    }
}
