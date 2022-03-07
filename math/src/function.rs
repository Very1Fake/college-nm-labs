#[derive(PartialEq, Clone, Debug)]
pub enum Func {
    Sqrt,
    Abs, // TODO: Add literal `|expr|` to parser
    Sin,
    Cos,
    Tan,
}

impl Func {
    pub fn eval(&self, args: Vec<f64>) -> f64 {
        use Func::*;
        const MSG: &str = "Arg not found";

        // TODO: Enhance handling
        match self {
            Sqrt => args.get(0).expect(MSG).sqrt(),
            Abs => args.get(0).expect(MSG).abs(),
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
            Abs => 1,
            Sin => 1,
            Cos => 1,
            Tan => 1,
        }
    }

    pub fn as_str(&self) -> &str {
        use Func::*;

        match self {
            Sqrt => "sqrt",
            Abs => "abs",
            Sin => "sin",
            Cos => "cos",
            Tan => "tan",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        use Func::*;

        match s {
            "sqrt" => Some(Sqrt),
            "abs" => Some(Abs),
            "sin" => Some(Sin),
            "cos" => Some(Cos),
            "tan" => Some(Tan),
            _ => None,
        }
    }
}
