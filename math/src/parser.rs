use thiserror::Error;

use crate::expression::MathConst;
use crate::ops::Op;
use crate::{expression::Expr, function::Func};

use crate::token::{LexError, Precedence, Token, TokenStream, Tokenizer};

const NEVER_ENDS: &str = "`TokenStream` never ends";

const LEVEL_LIMIT: usize = 10;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Nesting level limit exceeded")]
    LevelLimitExceeded,
    #[error("Lex error: {0}")]
    LexError(#[from] LexError),
    #[error("Unexpected End of File")]
    UnexpectedEOF,
    #[error("Missing token '{0}' use {1}")]
    MissingToken(String, String),
    #[error("Function '{0}' not found")]
    FunctionNotFound(String),
}

// -------------------------------------------------------------------------------------------------

#[derive(Default, Debug)]
pub struct ParserState {
    level: usize,
}

impl ParserState {
    #[inline]
    pub fn level_up(&self) -> Self {
        Self {
            level: self.level + 1,
        }
    }

    #[inline]
    fn ensure_level_max_limit(&self) -> ParseResult<()> {
        if self.level == LEVEL_LIMIT {
            Err(ParseError::LevelLimitExceeded)
        } else {
            Ok(())
        }
    }
}

/// Parse a function call
fn parse_fn_call(state: &ParserState, stream: &mut TokenStream, name: String) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    // TODO: Arguments count checks
    let mut args = Vec::with_capacity(1);
    let peek = stream.peek().expect(NEVER_ENDS);

    // DRY
    #[inline(always)]
    fn pack_fn(stream: &mut TokenStream, name: String, args: Vec<Expr>) -> ParseResult<Expr> {
        skip_token(stream, Token::RightParen);

        if let Some(func) = Func::parse(&name) {
            Ok(Expr::Func(func, args))
        } else {
            Err(ParseError::FunctionNotFound(name))
        }
    }

    // DRY
    #[inline(always)]
    fn right_paren_missing(name: String) -> ParseResult<Expr> {
        Err(ParseError::MissingToken(
            Token::RightParen.as_str().to_owned(),
            format!(
                "to close the arguments list of this function call '{}'",
                name
            ),
        ))
    }

    match peek {
        // name( <EOF>
        Token::Eof => return right_paren_missing(name),
        // name( <error>
        Token::LexError(err) => return Err(err.clone().into()),
        // name()
        Token::RightParen => return pack_fn(stream, name, args),
        // id..
        _ => (),
    }

    loop {
        match stream.peek().expect(NEVER_ENDS) {
            // name(...args, )
            Token::RightParen => (),
            _ => args.push(parse_expr(&state.level_up(), stream)?),
        }

        match stream.peek().expect(NEVER_ENDS) {
            // name(...args)
            Token::RightParen => return pack_fn(stream, name, args),
            // name(...,
            Token::Comma => skip_token(stream, Token::Comma),
            // name(... <EOF>
            Token::Eof => return right_paren_missing(name),
            // name(... <error>
            Token::LexError(err) => return Err(err.clone().into()),
            // name(... ???
            _ => {
                return Err(ParseError::MissingToken(
                    Token::Comma.as_str().to_owned(),
                    format!("to separate arguments to function call '{}'", name),
                ))
            }
        }
    }
}

/// Parse
fn parse_postfix(
    state: &ParserState,
    stream: &mut TokenStream,
    mut lhs: Expr,
) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    loop {
        let token = stream.peek().expect(NEVER_ENDS);

        if !lhs.is_valid_postfix(token) {
            break;
        }

        let token = stream.next().expect(NEVER_ENDS);

        lhs = match (lhs, token) {
            // 2id - Coefficient
            (num @ Expr::Const(_), Token::Identifier(id)) => Expr::Op(
                Op::Mul,
                Box::new((
                    num,
                    if let Some(constant) = MathConst::parse(&id) {
                        Expr::MathConst(constant)
                    } else {
                        Expr::var(id)
                    },
                )),
            ),
            // name( - Function call
            (Expr::Var(name), Token::LeftParen) => {
                parse_fn_call(&state.level_up(), stream, name.into())?
            }
            // 2name( - Function call after coefficient
            (Expr::Op(Op::Mul, mut ops), Token::LeftParen)
                if matches!(ops.0, Expr::Const(_)) && matches!(ops.1, Expr::Var(_)) =>
            {
                if let Expr::Var(name) = ops.1 {
                    ops.1 = parse_fn_call(&state.level_up(), stream, name.into())?;
                    Expr::Op(Op::Mul, ops)
                } else {
                    unreachable!("Expr::Var if expected but gets {}", ops.1)
                }
            }
            (expr, token) => unreachable!("unknown postfix op `{}` for {expr:?}", token.syntax()),
        }
    }

    Ok(lhs)
}

/// `(` expr `)`
fn parse_paren_expr(state: &ParserState, stream: &mut TokenStream) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    skip_token(stream, Token::LeftParen);
    let expr = parse_expr(&state.level_up(), stream)?;

    match stream.next().expect(NEVER_ENDS) {
        Token::RightParen => Ok(expr),
        Token::LexError(err) => Err(err.into()),
        _ => Err(ParseError::MissingToken(
            Token::RightParen.as_str().to_owned(),
            String::from("for matching ( in this expression"),
        )),
    }
}

/// Parse binary operation tree
fn parse_binary_op(
    state: &ParserState,
    stream: &mut TokenStream,
    parent_precedence: Option<Precedence>,
    lhs: Expr,
) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    let mut lhs = lhs;

    loop {
        let op = stream.peek().expect(NEVER_ENDS);
        let precedence = op.precedence();
        let bind_to_right = op.is_bind_right();

        if precedence < parent_precedence || (precedence == parent_precedence && !bind_to_right) {
            return Ok(lhs);
        }

        let op = stream.next().expect(NEVER_ENDS);
        let rhs = parse_unary(state, stream)?;

        let next_op = stream.peek().expect(NEVER_ENDS);
        let next_precedence = next_op.precedence();

        let rhs =
            if (precedence == next_precedence && bind_to_right) || precedence < next_precedence {
                parse_binary_op(state, stream, parent_precedence, rhs)?
            } else {
                rhs
            };

        let state = state.level_up();
        state.ensure_level_max_limit()?;

        if let Some(op) = op.as_ops() {
            lhs = Expr::Op(op.clone(), Box::new((lhs.brace_if(&op), rhs.brace_if(&op))));
        } else {
            unreachable!("Unknown operator '{op:?}'");
        }
    }
}

/// Parse a primary expression
fn parse_primary(state: &ParserState, stream: &mut TokenStream) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    let peek = stream.peek().expect(NEVER_ENDS);

    let root_expr = match peek {
        Token::Number(_) => match stream.next().expect(NEVER_ENDS) {
            Token::Number(num) => Expr::Const(num),
            t => unreachable!("Token::Number is expected but gets {t:?}"),
        },
        Token::Identifier(_) => {
            let id = match stream.next().expect(NEVER_ENDS) {
                Token::Identifier(s) => s,
                t => unreachable!("Token::Identifier is expected but gets {t:?}"),
            };

            if let Some(constant) = MathConst::parse(&id) {
                Expr::MathConst(constant)
            } else {
                Expr::var(id)
            }
        }
        // ( - grouped expression
        Token::LeftParen => parse_paren_expr(&state.level_up(), stream)?,
        Token::LexError(_) => match stream.next().expect(NEVER_ENDS) {
            Token::LexError(err) => return Err(err.into()),
            t => unreachable!("Token::LexError is expected but gets {t:?}"),
        },
        _ => return Err(LexError::UnexpectedInput(peek.syntax()).into()),
    };

    parse_postfix(&state.level_up(), stream, root_expr)
}

/// Parse a potential unary operator
fn parse_unary(state: &ParserState, stream: &mut TokenStream) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    let peek = stream.peek().expect(NEVER_ENDS);

    match peek {
        // -expr
        Token::Minus => {
            skip_token(stream, Token::Minus);
            Ok(-parse_unary(&state.level_up(), stream)?)
        }
        // +expr
        Token::Plus => {
            skip_token(stream, Token::Plus);
            Ok(parse_unary(&state.level_up(), stream)?)
        }
        // Other tokens
        _ => parse_primary(&state.level_up(), stream),
    }
}

/// Parse expression
fn parse_expr(state: &ParserState, stream: &mut TokenStream) -> ParseResult<Expr> {
    state.ensure_level_max_limit()?;

    let lhs = parse_unary(&state.level_up(), stream)?;
    parse_binary_op(&state.level_up(), stream, Precedence::new(1), lhs)
}

pub fn parse(script: impl AsRef<str>) -> ParseResult<Expr> {
    let parser = ParserState::default();
    let mut stream = Tokenizer::new(&script).peekable();

    parse_expr(&parser.level_up(), &mut stream)
}

#[inline]
fn skip_token(stream: &mut TokenStream, token: Token) {
    let t = stream.next().expect(NEVER_ENDS);

    if t != token {
        unreachable!("{} expected but gets {}", token.syntax(), t.syntax());
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use crate::{
        expression::{Evaluable, Expr, MathConst},
        function::Func,
        ops::Op,
        variable::{Scope, Var},
    };

    use super::parse;

    #[test]
    fn parse_number() -> Result<()> {
        let input_1 = "2";
        let input_2 = "-2.45";

        assert_eq!(parse(&input_1)?, Expr::Const(2.0));
        assert_eq!(parse(&input_2)?, Expr::Const(-2.45));

        Ok(())
    }

    #[test]
    fn parse_ops() -> Result<()> {
        let input_1 = "2 + 2";
        let input_2 = "4 + 5^2";
        let input_3 = "(2 + 2) * 5";
        let input_4 = "7.6 * (24 + -23)";

        assert_eq!(parse(input_1)?, Expr::Const(2.0) + 2.0);
        assert_eq!(parse(input_2)?, 4.0 + Expr::Const(5.0).powf(2.0));
        assert_eq!(parse(input_3)?, (Expr::Const(2.0) + 2.0) * 5.0);
        assert_eq!(parse(input_4)?, 7.6 * (Expr::Const(24.0) + -23.0));

        Ok(())
    }

    #[test]
    fn parse_var_const() -> Result<()> {
        let input_1 = "2y";
        let input_2 = "x^e";
        let input_3 = "2e";

        assert_eq!(parse(input_1)?, 2.0 * Expr::var("y"));
        assert_eq!(
            parse(input_2)?,
            Expr::var("x").pow(Expr::MathConst(MathConst::E))
        );
        assert_eq!(parse(input_3)?, 2.0 * Expr::MathConst(MathConst::E));

        Ok(())
    }

    #[test]
    fn parse_func() -> Result<()> {
        let input_1 = "cos(2)";
        let input_2 = "2sin(x)";

        assert_eq!(
            parse(input_1)?,
            Expr::Func(Func::Cos, vec![Expr::Const(2.0)])
        );
        assert_eq!(
            parse(input_2)?,
            Expr::Op(
                Op::Mul,
                Box::new((
                    Expr::Const(2.0),
                    Expr::Func(Func::Sin, vec![Expr::var("x")])
                ))
            )
        );

        Ok(())
    }

    #[test]
    fn parse_full() -> Result<()> {
        let script = "2x^3+2";

        let mut scope = Scope::default();
        scope.insert(Var::new("x", 4.0));

        let expected = (2.0 * Expr::var("x")).powf(3.0) + 2.0; // Answer: 514

        assert_eq!(parse(script)?.eval(&scope)?, expected.eval(&scope)?);

        Ok(())
    }
}
