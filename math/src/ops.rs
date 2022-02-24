// TODO: Add modulo
#[derive(PartialEq, Clone, Debug)]
pub enum Operation {
    Sub,
    Add,
    Mul,
    Div,
}

impl Operation {
    pub fn priority(&self) -> u8 {
        use Operation::*;

        match self {
            Sub => 0,
            Add => 1,
            Mul => 2,
            Div => 3,
        }
    }

    pub fn as_pretty(&self) -> &str {
        use Operation::*;

        match self {
            Sub => " - ",
            Add => " + ",
            Mul => " * ",
            Div => " / ",
        }
    }
}
