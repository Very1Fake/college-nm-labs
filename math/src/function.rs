use crate::variable::OpType;

#[derive(Debug)]
pub enum Func {
    Sin,
    Cos,
}

impl Func {
    pub fn eval(&self, args: Vec<OpType>) -> OpType {
        match self {
            Func::Sin => args[0].sin(),
            Func::Cos => args[0].cos(),
        }
    }

    /// How much arguments function need
    pub fn hint(&self) -> u8 {
        use Func::*;

        match self {
            Sin => 1,
            Cos => 1,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Func::Sin => "sin",
            Func::Cos => "cos",
        }
    }
}
