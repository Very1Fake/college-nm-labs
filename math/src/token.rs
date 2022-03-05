use core::{iter::Peekable, num::NonZeroU8, str::Chars};

use smallvec::SmallVec;
use thiserror::Error;

use crate::{ops::Op, variable::OpType};

const INLINE: usize = 16;

const NUMBER_SEPARATOR: char = '_';

pub type Precedence = NonZeroU8;

#[derive(PartialEq, Clone, Error, Debug)]
pub enum LexError {
    #[error("Unexpected input '{0}' received")]
    UnexpectedInput(String),
    #[error("Malformed number literal '{0}'")]
    MalformedNumber(String),
    #[error("Malformed identifier '{0}'")]
    MalformedIdentifier(String),
}

#[derive(PartialEq, Debug)]
pub enum Token {
    /// `\n` symbol
    NewLine,
    /// Number constant
    Number(OpType),
    /// Identifier (variable or function name)
    Identifier(String),
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `^` or `**`
    PowerOf,
    /// `;`
    Semicolon,
    /// `,`
    Comma,
    /// End Of File
    Eof,
    LexError(LexError),
}

impl Token {
    pub fn precedence(&self) -> Option<Precedence> {
        use Token::*;

        Precedence::new(match self {
            Minus => 10,
            Plus => 20,
            Multiply => 30,
            Divide => 40,
            PowerOf => 50,
            _ => 0,
        })
    }

    pub fn is_bind_right(&self) -> bool {
        matches!(self, Token::PowerOf)
    }

    pub fn as_ops(&self) -> Option<Op> {
        use Token::*;

        match self {
            Minus => Some(Op::Sub),
            Plus => Some(Op::Add),
            Multiply => Some(Op::Mul),
            Divide => Some(Op::Div),
            PowerOf => Some(Op::Pow),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        use Token::*;

        match self {
            NewLine => "\\n",
            Number(_) => "Token::Number",
            Identifier(_) => "`Token::Identifier`",
            LeftParen => "(",
            RightParen => ")",
            Minus => "+",
            Plus => "-",
            Multiply => "*",
            Divide => "/",
            PowerOf => "^",
            Semicolon => ";",
            Comma => ",",
            Eof => "EOF",
            LexError(_) => "`Token::Error`",
        }
    }

    pub fn syntax(&self) -> String {
        use Token::*;

        match self {
            Number(n) => n.to_string(),
            Identifier(s) => s.clone(),
            t => t.as_str().to_owned(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

pub(crate) type TokenStream<'a> = Peekable<Tokenizer<'a>>;

pub(crate) struct Tokenizer<'a> {
    stream: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a (impl AsRef<str> + 'a)) -> Self {
        Self {
            stream: input.as_ref().chars().peekable(),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(char) = self.stream.next() {
            match (char, &self.stream.peek().unwrap_or(&'\0')) {
                ('\n', _) => return Some(Token::NewLine),

                ('0'..='9', _) => {
                    let mut result = SmallVec::<[char; INLINE]>::new();
                    result.push(char);

                    while let Some(next_char) = self.stream.peek() {
                        // TODO: add hexadecimal/binary format
                        match next_char {
                            next_char
                                if next_char.is_digit(10)
                                    || *next_char == '.'
                                    || *next_char == NUMBER_SEPARATOR =>
                            {
                                result.push(*next_char);
                                skip(&mut self.stream);
                            }
                            _ => break,
                        }
                    }

                    return Some({
                        let out: String =
                            result.iter().filter(|&&c| c != NUMBER_SEPARATOR).collect();

                        out.parse::<OpType>()
                            .map(Token::Number)
                            .unwrap_or_else(|_| Token::LexError(LexError::MalformedNumber(out)))
                    });
                }

                (ch, _) if ch.is_ascii_lowercase() => {
                    let mut result = SmallVec::<[char; INLINE]>::new();
                    result.push(ch);

                    while let Some(next_char) = self.stream.peek() {
                        match next_char {
                            x if x.is_ascii_lowercase() => {
                                result.push(*x);
                                skip(&mut self.stream);
                            }
                            _ => break,
                        }
                    }

                    let identifier: String = result.into_iter().collect();

                    if identifier.len() <= 6 {
                        return Some(Token::Identifier(identifier));
                    } else {
                        return Some(Token::LexError(LexError::MalformedIdentifier(identifier)));
                    }
                }

                // Parentheses
                ('(', _) => return Some(Token::LeftParen),
                (')', _) => return Some(Token::RightParen),

                // Operators
                ('-', _) => return Some(Token::Minus),
                ('+', _) => return Some(Token::Plus),
                ('*', _) => return Some(Token::Multiply),
                ('/', _) => return Some(Token::Divide),
                ('^', _) => return Some(Token::PowerOf),

                // Reserved
                (';', _) => return Some(Token::Semicolon),
                (',', _) => return Some(Token::Comma),

                (ch, _) if ch.is_whitespace() => (),
                (ch, _) => return Some(Token::LexError(LexError::UnexpectedInput(ch.to_string()))),
            }
        }

        Some(Token::Eof)
    }
}

#[inline(always)]
fn skip(stream: &mut impl Iterator) {
    stream.next();
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::{LexError, Token, Tokenizer};

    #[test]
    fn stream_simple() {
        let script = "( -+*/^ ) \n ;,";

        assert_eq!(
            Tokenizer::new(&script)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![
                Token::LeftParen,
                Token::Minus,
                Token::Plus,
                Token::Multiply,
                Token::Divide,
                Token::PowerOf,
                Token::RightParen,
                Token::NewLine,
                Token::Semicolon,
                Token::Comma,
            ]
        );
    }

    #[test]
    fn stream_number() {
        let integer = "12345";
        let float = "24.25";
        let big = "1_000_000";
        let script = "1.0 + 1.25 - 35 * 0.2";

        assert_eq!(
            Tokenizer::new(&integer)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![Token::Number(12345.0),]
        );

        assert_eq!(
            Tokenizer::new(&float)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![Token::Number(24.25),]
        );

        assert_eq!(
            Tokenizer::new(&big)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![Token::Number(1_000_000.0),]
        );

        assert_eq!(
            Tokenizer::new(&script)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![
                Token::Number(1.0),
                Token::Plus,
                Token::Number(1.25),
                Token::Minus,
                Token::Number(35.0),
                Token::Multiply,
                Token::Number(0.2)
            ]
        );
    }

    #[test]
    fn stream_identifier() {
        let valid = "2x";
        let invalid = "x_y^2";
        let script = "x * 2y + 3abc^4";

        assert_eq!(
            Tokenizer::new(&valid)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![Token::Number(2.0), Token::Identifier(String::from("x")),]
        );

        assert_eq!(
            Tokenizer::new(&invalid)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![
                Token::Identifier(String::from("x")),
                Token::LexError(LexError::UnexpectedInput(String::from("_"))),
                Token::Identifier(String::from("y")),
                Token::PowerOf,
                Token::Number(2.0)
            ]
        );

        assert_eq!(
            Tokenizer::new(&script)
                .take_while(|t| t != &Token::Eof)
                .collect::<Vec<Token>>(),
            vec![
                Token::Identifier(String::from("x")),
                Token::Multiply,
                Token::Number(2.0),
                Token::Identifier(String::from("y")),
                Token::Plus,
                Token::Number(3.0),
                Token::Identifier(String::from("abc")),
                Token::PowerOf,
                Token::Number(4.0)
            ]
        );
    }
}
