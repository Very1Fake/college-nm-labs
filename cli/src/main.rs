use anyhow::Result;
use clap::{Arg, Command, PossibleValue};

use nm_math::{
    method::{Method, MethodEquation, OutPut},
    parser::parse,
    token::LexError,
};

fn main() -> Result<()> {
    let app = Command::new("nm-cli")
        .author("Timur Israpilov <very1fake.coder@gmail.com>")
        .version(env!("CARGO_PKG_VERSION"))
        .arg_required_else_help(true)
        .arg(
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .multiple_occurrences(true)
                .max_occurrences(3)
                .help("Use verbose output (use -vv for very verbose)."),
        )
        .arg(
            Arg::new("expression")
                .short('e')
                .long("expression")
                .help("Expression to be parsed and used in calculations.")
                .takes_value(true),
        )
        .arg(Arg::new("interval")
            .short('i')
            .long("interval")
            .help("Set internal. Used in some method.")
            .value_names(&["a", "b"])
            .number_of_values(2)
            .default_values(&["-1", "1"])
            .validator(|x| x.parse::<f64>())
            .allow_hyphen_values(true)
        )
        .arg(
            Arg::new("precision")
                .short('p')
                .long("precision")
                .help("Set precision (Epsilon). Used in some methods")
                .value_name("epsilon")
                .takes_value(true)
                .default_value("0.001")
                .validator(|x| if let Ok(f) = x.parse::<f64>() {
                    if f < 0.0 {
                        Err(LexError::MalformedNumber(f.to_string()))
                    } else {
                        Ok(())
                    }
                } else {
                    Err(LexError::MalformedNumber(String::new()))
                })
        )
        .arg(
            Arg::new("limit")
                .short('l')
                .long("limit")
                .help("Set iterations limit")
                .value_name("iters count")
                .takes_value(true)
                .default_value("10000")
                .validator(|x| x.parse::<u128>())
        )
        .arg(
            Arg::new("operation")
                .short('o')
                .long("op")
                .help("Operations that need to be calculated")
                .multiple_values(true)
                .possible_values([
                    PossibleValue::new("bisection")
                        .help("The bisection method is an approximation method to find the roots of the given equation by repeatedly dividing the interval")
                ])
                .min_values(1)
                .requires_all(&["expression", "interval", "precision", "limit"]),
        );

    let args = app.get_matches();
    let verbose = args.occurrences_of("verbose") as u8;

    fn separator(name: &str) {
        print!("\n\n{name:-^42 }\n\n");
    }

    if let Some(ops) = args.values_of("operation") {
        let expression = parse(args.value_of("expression").expect("Was required"))?;

        println!("\nParsed expression: {expression}");

        for op in ops {
            match op {
                "bisection" => {
                    separator("Bisection method");

                    let precision = args.value_of_t::<f64>("precision").expect("Was required");
                    let interval = args.values_of_t::<f64>("interval").expect("Was required");
                    let limit = args.value_of_t::<u128>("limit").expect("Was required");

                    println!("Precision: {precision}\nInterval: {interval:?}\nIterations limit: {limit}\n");

                    let result = Method::new(limit, OutPut::Stdout)
                        .verbose(verbose)
                        .bisection(
                            MethodEquation::Math(expression.clone()),
                            (interval[0], interval[1]),
                            precision,
                        )?;

                    println!(
                        "Result: x = {} (f(x) = {})\nIterations: {}\nElapsed time: {:?}",
                        result.root.0, result.root.1, result.stats.iterations, result.stats.elapsed
                    );
                }
                _ => panic!("Unknown operation: '{op}'"),
            }
        }

        println!("\n");
    };

    Ok(())
}
