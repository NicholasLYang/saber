use crate::ast::{Expr, Pat, Stmt, TypeSig};
use crate::lexer::{Lexer, Token};
use crate::types::Result;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

pub struct Parser<'input> {
    lexer: Lexer<'input>,
    pushedback_tokens: Vec<(usize, Token, usize)>,
}

#[derive(Debug, PartialEq)]
struct Location(usize, usize);

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}---{})", self.0, self.1)
    }
}

#[derive(Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "Reached end of file without completing parse")]
    EndOfFile,
    #[fail(
        display = "({}): Unexpected token {:?}, expected {:?}",
        location, token, expected_tokens
    )]
    UnexpectedToken {
        token: Token,
        expected_tokens: Vec<&'static str>,
        location: Location,
    },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
    #[fail(display = "Should not be reachable")]
    NotReachable,
}

impl<'input> Parser<'input> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            pushedback_tokens: Vec::new(),
        }
    }

    fn pushback(&mut self, token: (usize, Token, usize)) {
        self.pushedback_tokens.push(token);
    }

    fn get_next_token(&mut self) -> Result<(usize, Token, usize)> {
        if !self.pushedback_tokens.is_empty() {
            match self.pushedback_tokens.pop() {
                Some(tok) => Ok(tok),
                None => Err(ParseError::NotReachable)?,
            }
        } else {
            match self.lexer.next() {
                Some(tok) => Ok(tok?),
                None => Err(ParseError::EndOfFile)?,
            }
        }
    }
    pub fn parse_statement(&mut self) -> Result<Stmt> {
        let tok = self.get_next_token()?;
        match tok {
            (_, Token::Let, _) => self.parse_let_statement(),
            _ => {
                println!("PARSING SOMETHING ELSE");
                Err(ParseError::NotImplemented)?
            }
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<Stmt> {
        let pat = self.parse_pattern();
        println!("{:?}", pat);
        Err(ParseError::NotImplemented)?
    }

    pub fn parse_pattern(&mut self) -> Result<Pat> {
        let tok = self.get_next_token()?;
        match tok {
            (_, Token::LParen, _) => Ok(Pat::Tuple(
                self.comma::<Pat>(&Self::parse_pattern, Token::RParen)?,
            )),
            (_, Token::LBrace, _) => Ok(Pat::Record(Vec::new())),
            (_, Token::Ident(name), _) => Ok(Pat::Id(name, self.parse_type_sig()?)),
            (start, token, end) => Err(ParseError::UnexpectedToken {
                token,
                location: Location(start, end),
                expected_tokens: vec!["(", "{", "identifier"],
            })?,
        }
    }

    fn parse_type_sig(&mut self) -> Result<Option<TypeSig>> {
        let token = self.get_next_token()?;
        match token {
            (_, Token::Colon, _) => Ok(Some(self.parse_type()?)),
            token => {
                self.pushback(token);
                Ok(None)
            }
        }
    }

    fn parse_type(&mut self) -> Result<TypeSig> {
        let tok = self.get_next_token()?;
        match tok {
            (_, Token::Ident(name), _) => Ok(TypeSig::Name(name)),
            (_, Token::LBracket, _) => {
                let array_type = self.parse_type()?;
                let (start, bracket, end) = self.get_next_token()?;
                if bracket != Token::RBracket {
                    Err(ParseError::UnexpectedToken {
                        token: bracket,
                        location: Location(start, end),
                        expected_tokens: vec!["]"],
                    })?
                } else {
                    Ok(TypeSig::Array(Box::new(array_type)))
                }
            }
            (start, token, end) => Err(ParseError::UnexpectedToken {
                token,
                location: Location(start, end),
                expected_tokens: vec!["[", "identifier"],
            })?,
        }
    }

    fn comma<T: Debug>(
        &mut self,
        parse_fn: &Fn(&mut Self) -> Result<T>,
        delimiter: Token,
    ) -> Result<Vec<T>> {
        let mut parsed: Vec<T> = Vec::new();
        loop {
            parsed.push(parse_fn(self)?);
            let (start, token, end) = self.get_next_token()?;
            if token == delimiter {
                return Ok(parsed);
            } else if token != Token::Comma {
                return Err(ParseError::UnexpectedToken {
                    token,
                    location: Location(start, end),
                    expected_tokens: vec![","],
                })?;
            }
        }
    }
}
