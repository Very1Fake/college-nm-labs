use crate::variable::OpType;

#[derive(PartialEq, Clone, Debug)]
pub enum Func {
    Sqrt,
    Sin,
    Cos,
    Tan,
}

impl Func {
    pub fn eval(&self, args: Vec<OpType>) -> OpType {
        use Func::*;
        const MSG: &str = "Arg not found";

        // TODO: Enhance handling
        match self {
            Sqrt => args.get(0).expect(MSG).sqrt(),
            Sin => args.get(0).expect(MSG).sin(),
            Cos => args.get(0).expect(MSG).cos(),
            Tan => args.get(0).expect(MSG).tan(),
        }
    }

    /// Hint for parser about how much arguments function need
    pub fn hint(&self) -> u8 {
        use Func::*;

        match self {
            Sqrt => 1,
            Sin => 1,
            Cos => 1,
            Tan => 1,
        }
    }

    pub fn as_str(&self) -> &str {
        use Func::*;

        match self {
            Sqrt => "sqrt",
            Sin => "sin",
            Cos => "cos",
            Tan => "tan",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        use Func::*;

        match s {
            "sqrt" => Some(Sqrt),
            "sin" => Some(Sin),
            "cos" => Some(Cos),
            "tan" => Some(Tan),
            _ => None,
        }
    }
}
