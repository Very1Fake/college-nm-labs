#[inline]
fn get_arg(args: &Vec<f64>, id: usize) -> f64 {
    *args.get(id).expect("Arg not found")
}

// -------------------------------------------------------------------------------------------------

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Func {
    Sqrt,
    Abs, // TODO: Add literal `|expr|` to parser
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
    Log,
    Ln,
}

impl Func {
    pub fn eval(&self, args: &Vec<f64>) -> f64 {
        use Func::*;

        // TODO: Enhance handling
        match self {
            Sqrt => get_arg(args, 0).sqrt(),
            Abs => get_arg(args, 0).abs(),
            Sin => get_arg(args, 0).sin(),
            Cos => get_arg(args, 0).cos(),
            Tan => get_arg(args, 0).tan(),
            ASin => get_arg(args, 0).asin(),
            ACos => get_arg(args, 0).acos(),
            ATan => get_arg(args, 0).atan(),
            Log => {
                let n = get_arg(args, 1);
                let a = get_arg(args, 0);
                if a == 2.0 {
                    n.log2()
                } else if a == 10.0 {
                    n.log10()
                } else {
                    n.log(a)
                }
            }
            Ln => get_arg(args, 0).ln(),
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
            ASin => 1,
            ACos => 1,
            ATan => 1,
            Log => 2,
            Ln => 1,
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
            ASin => "asin",
            ACos => "acos",
            ATan => "atan",
            Log => "log",
            Ln => "ln",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        use Func::*;

        match s {
            "sqrt" => Some(Sqrt),
            "abs" => Some(Abs),
            "sin" => Some(Sin),
            "cos" => Some(Cos),
            "asin" | "arcsin" => Some(ASin),
            "acos" | "arccos" => Some(ACos),
            "atan" | "atg" => Some(ATan),
            "tan" | "tg" => Some(Tan),
            "log" => Some(Log),
            "ln" => Some(Ln),
            _ => None,
        }
    }
}
