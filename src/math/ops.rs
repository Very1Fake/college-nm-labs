#[derive(PartialEq, Clone, Debug)]
pub enum Operation {
    Sub,
    Add,
    Mul,
    Div,
}

impl Operation {
    pub fn priority(&self) -> u8 {
        match self {
            Operation::Sub => 0,
            Operation::Add => 1,
            Operation::Mul => 2,
            Operation::Div => 3,
        }
    }

    pub fn as_pretty(&self) -> &str {
        match self {
            Operation::Sub => " - ",
            Operation::Add => " + ",
            Operation::Mul => " * ",
            Operation::Div => " / ",
        }
    }
}
