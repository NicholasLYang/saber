use crate::ast::{Expr, Pat, Stmt};
use crate::lexer::{Lexer, Token};
use crate::types::Result;

pub struct Parser<'input> {
    lexer: Lexer<'input>,
}

#[derive(Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "Reached end of file without completing parse")]
    EndOfFile,
    #[fail(
        display = "Unexpected token {:?}, expected {:?}",
        token, expected_tokens
    )]
    UnexpectedToken {
        token: Token,
        expected_tokens: Vec<&'static str>,
    },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
}

impl<'input> Parser<'input> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
    fn get_next_token(&mut self) -> Result<(usize, Token, usize)> {
        match self.lexer.next() {
            Some(tok) => Ok(tok?),
            None => Err(ParseError::EndOfFile)?,
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
            (_, Token::LParen, _) => Ok(Pat::Tuple(Vec::new())),
            (_, Token::LBrace, _) => Ok(Pat::Record(Vec::new())),
            (_, Token::Ident(name), _) => Ok(Pat::Id(name, None)),
            (_, token, _) => Err(ParseError::UnexpectedToken {
                token,
                expected_tokens: vec!["(", "{", "identifier"],
            })?,
        }
    }
}
