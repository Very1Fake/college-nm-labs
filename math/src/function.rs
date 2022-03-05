use crate::variable::OpType;

#[derive(PartialEq, Clone, Debug)]
pub enum Func {
    Sin,
    Cos,
    Tan,
}

impl Func {
    pub fn eval(&self, args: Vec<OpType>) -> OpType {
        const MSG: &str = "Arg not found";

        // TODO: Enhance handling
        match self {
            Func::Sin => args.get(0).expect(MSG).sin(),
            Func::Cos => args.get(0).expect(MSG).cos(),
            Func::Tan => args.get(0).expect(MSG).tan(),
        }
    }

    /// Hint for parser about how much arguments function need
    pub fn hint(&self) -> u8 {
        use Func::*;

        match self {
            Sin => 1,
            Cos => 1,
            Tan => 1,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Func::Sin => "sin",
            Func::Cos => "cos",
            Func::Tan => "tan",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "sin" => Some(Func::Sin),
            "cos" => Some(Func::Cos),
            "tan" => Some(Func::Tan),
            _ => None,
        }
    }
}
