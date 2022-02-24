use std::{iter::Peekable, str::Chars};

use smallvec::SmallVec;

use crate::variable::OperableType;

const INLINE: usize = 16;

const NUMBER_SEPARATOR: char = '_';

#[derive(PartialEq, Debug)]
pub enum TokenError {
    UnexpectedInput(String),
    MalformedNumber(String),
    MalformedIdentifier(String),
}

#[derive(PartialEq, Debug)]
pub enum Token {
    /// `\n` symbol
    NewLine,
    /// Number constant
    Number(OperableType),
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
    EOF,
    Error(TokenError),
}

// -------------------------------------------------------------------------------------------------

pub struct Tokenizer<'a> {
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
            println!("{char:?},{:?}", self.stream.peek().unwrap_or(&'\0'));
            match (char, &self.stream.peek().unwrap_or(&'\0')) {
                ('\n', _) => return Some(Token::NewLine),

                // TODO: Number
                ('0'..='9', _) => {
                    let mut result = SmallVec::<[char; INLINE]>::new();
                    result.push(char);

                    while let Some(next_char) = self.stream.peek() {
                        println!("{next_char}");
                        // TODO: add e support
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

                        out.parse::<OperableType>()
                            .map(Token::Number)
                            .unwrap_or_else(|_| {
                                eprintln!("{}{out}", result.iter().collect::<String>());
                                Token::Error(TokenError::MalformedNumber(out))
                            })
                    });
                }

                // TODO: Identifier
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
                        return Some(Token::Error(TokenError::MalformedIdentifier(identifier)));
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
                (ch, _) => return Some(Token::Error(TokenError::UnexpectedInput(ch.to_string()))),
            }
        }

        Some(Token::EOF)
    }
}

#[inline(always)]
fn skip(stream: &mut impl Iterator) {
    stream.next();
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::{Token, TokenError, Tokenizer};

    #[test]
    fn stream_simple() {
        let script = "( -+*/^ ) \n ;,";

        assert_eq!(
            Tokenizer::new(&script)
                .take_while(|t| t != &Token::EOF)
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
                .take_while(|t| t != &Token::EOF)
                .collect::<Vec<Token>>(),
            vec![Token::Number(12345.0),]
        );

        assert_eq!(
            Tokenizer::new(&float)
                .take_while(|t| t != &Token::EOF)
                .collect::<Vec<Token>>(),
            vec![Token::Number(24.25),]
        );

        assert_eq!(
            Tokenizer::new(&big)
                .take_while(|t| t != &Token::EOF)
                .collect::<Vec<Token>>(),
            vec![Token::Number(1_000_000.0),]
        );

        assert_eq!(
            Tokenizer::new(&script)
                .take_while(|t| t != &Token::EOF)
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
                .take_while(|t| t != &Token::EOF)
                .collect::<Vec<Token>>(),
            vec![Token::Number(2.0), Token::Identifier("x".to_string()),]
        );

        assert_eq!(
            Tokenizer::new(&invalid)
                .take_while(|t| t != &Token::EOF)
                .collect::<Vec<Token>>(),
            vec![
                Token::Identifier("x".to_string()),
                Token::Error(TokenError::UnexpectedInput("_".to_string())),
                Token::Identifier("y".to_string()),
                Token::PowerOf,
                Token::Number(2.0)
            ]
        );

        assert_eq!(
            Tokenizer::new(&script)
                .take_while(|t| t != &Token::EOF)
                .collect::<Vec<Token>>(),
            vec![
                Token::Identifier("x".to_string()),
                Token::Multiply,
                Token::Number(2.0),
                Token::Identifier("y".to_string()),
                Token::Plus,
                Token::Number(3.0),
                Token::Identifier("abc".to_string()),
                Token::PowerOf,
                Token::Number(4.0)
            ]
        );
    }
}
