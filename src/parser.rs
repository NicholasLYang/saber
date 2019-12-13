use crate::ast::{Expr, Op, Pat, Stmt, TypeSig, Value};
use crate::lexer::{Lexer, LexicalError, Token, TokenDiscriminants};
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
    #[fail(
        display = "Reached end of file without completing parse. Expected these tokens: {:?}",
        expected_tokens
    )]
    EndOfFile {
        expected_tokens: Vec<TokenDiscriminants>,
    },
    #[fail(
        display = "({}): Unexpected token {:?}, expected {:?}",
        location, token, expected_tokens
    )]
    UnexpectedToken {
        token: Token,
        expected_tokens: Vec<TokenDiscriminants>,
        location: Location,
    },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
    #[fail(display = "Lexer error: {}", err)]
    LexicalError { err: LexicalError },
    #[fail(display = "Should not be reachable")]
    NotReachable,
}

impl From<LexicalError> for ParseError {
    fn from(err: LexicalError) -> Self {
        ParseError::LexicalError { err }
    }
}

impl<'input> Parser<'input> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            pushedback_tokens: Vec::new(),
        }
    }

    fn expect(
        &mut self,
        expected: TokenDiscriminants,
    ) -> Result<(usize, Token, usize), ParseError> {
        let token = self.bump()?;
        if let Some((start, token, end)) = token {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == expected {
                Ok((start, token, end))
            } else {
                Err(ParseError::UnexpectedToken {
                    token,
                    location: Location(start, end),
                    expected_tokens: vec![expected],
                })?
            }
        } else {
            Err(ParseError::EndOfFile {
                expected_tokens: vec![expected],
            })?
        }
    }

    fn pushback(&mut self, token: (usize, Token, usize)) {
        self.pushedback_tokens.push(token);
    }

    fn lookahead_match(
        &mut self,
        lookahead: TokenDiscriminants,
    ) -> Result<Option<Token>, ParseError> {
        let token = self.bump()?;
        if let Some((start, token, end)) = token {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == lookahead {
                Ok(Some(token))
            } else {
                self.pushback((start, token, end));
                Ok(None)
            }
        } else {
            Err(ParseError::EndOfFile {
                expected_tokens: vec![lookahead],
            })?
        }
    }

    fn match_multiple(&mut self, tokens: Vec<Token>) -> Result<Option<Token>, ParseError> {
        for token in tokens {
            if let Some(token) = self.lookahead_match((&token).into())? {
                return Ok(Some(token));
            }
        }
        Ok(None)
    }

    fn bump(&mut self) -> Result<Option<(usize, Token, usize)>, ParseError> {
        match self.pushedback_tokens.pop() {
            Some(tok) => Ok(Some(tok)),
            None => match self.lexer.next() {
                Some(tok) => Ok(Some(tok?)),
                None => Ok(None),
            },
        }
    }

    fn peek(&mut self) -> Result<(), ParseError> {
        let tok = self.bump()?;
        if let Some((start, token, end)) = tok {
            self.pushback((start, token, end));
        }
        Ok(())
    }

    fn lookup_op_token(&mut self, token: Token) -> Result<Op, ParseError> {
        match token {
            Token::EqualEqual => Ok(Op::EqualEqual),
            Token::BangEqual => Ok(Op::BangEqual),
            Token::Times => Ok(Op::Times),
            Token::Div => Ok(Op::Div),
            Token::Plus => Ok(Op::Plus),
            Token::Minus => Ok(Op::Minus),
            Token::Greater => Ok(Op::Greater),
            Token::GreaterEqual => Ok(Op::GreaterEqual),
            Token::Less => Ok(Op::Less),
            Token::LessEqual => Ok(Op::LessEqual),
            _ => Err(ParseError::NotReachable)?,
        }
    }

    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        let mut stmts = Vec::new();
        while let None = self.lookahead_match(TokenDiscriminants::RBrace)? {
            stmts.push(self.parse_stmt()?);
        }
        Ok(Stmt::Block(stmts))
    }

    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(ParseError::EndOfFile { expected_tokens }) => return Ok(stmts),
                Err(err) => return Err(err),
            }
        }
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((_, Token::Let, _)) => self.parse_let_stmt(),
            Some((_, Token::Return, _)) => self.parse_return_stmt(),
            Some((_, Token::Export, _)) => self.parse_export_stmt(),
            Some(token) => {
                self.pushback(token);
                self.parse_expression_stmt()
            }
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::Let,
                    TokenDiscriminants::Return,
                    TokenDiscriminants::Export,
                ],
            })?,
        }
    }

    fn parse_export_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expression()?;
        self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Stmt::Export(expr))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expression()?;
        self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Stmt::Return(expr))
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParseError> {
        let pat = self.parse_pattern()?;
        self.expect(TokenDiscriminants::Equal)?;
        let rhs_expr = self.parse_expression()?;
        match rhs_expr {
            Expr::Function {
                params,
                return_type,
                body,
            } => Ok(Stmt::Asgn(
                pat,
                Expr::Function {
                    params,
                    return_type,
                    body,
                },
            )),
            _ => {
                self.expect(TokenDiscriminants::Semicolon)?;
                Ok(Stmt::Asgn(pat, rhs_expr))
            }
        }
    }

    fn parse_expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expression()?;
        self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        if let Some(_) = self.lookahead_match(TokenDiscriminants::Slash)? {
            self.parse_function()
        } else {
            self.parse_equality()
        }
    }
    fn parse_function(&mut self) -> Result<Expr, ParseError> {
        let params = self.parse_pattern()?;
        let return_type = self.parse_type_sig()?;

        self.expect(TokenDiscriminants::FatArrow)?;
        let token = self.bump()?;
        let body = match token {
            Some((_, Token::LBrace, _)) => self.parse_block()?,
            Some((_, Token::LParen, _)) => {
                let expr = self.parse_expression()?;
                self.expect(TokenDiscriminants::RParen)?;
                Stmt::Return(expr)
            }
            Some((start, token, end)) => {
                self.pushback((start, token, end));
                Stmt::Return(self.parse_expression()?)
            }
            // TODO: Streamline error reporting. I should group the
            // tokens into ones expected for each kind of syntax rule.
            // For instance, these are a combo of the block lookahead
            // token, the expression grouping lookahead token and the
            // expression lookahead token
            None => {
                return Err(ParseError::EndOfFile {
                    expected_tokens: vec![
                        TokenDiscriminants::LBrace,
                        TokenDiscriminants::LParen,
                        TokenDiscriminants::True,
                        TokenDiscriminants::False,
                        TokenDiscriminants::Integer,
                        TokenDiscriminants::Float,
                        TokenDiscriminants::String,
                    ],
                })?;
            }
        };
        Ok(Expr::Function {
            params,
            return_type,
            body: Box::new(body),
        })
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_comparison()?;
        if let Some(token) = self.match_multiple(vec![Token::EqualEqual, Token::BangEqual])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.parse_comparison()?;
            Ok(Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        } else {
            Ok(lhs)
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_addition()?;
        if let Some(token) = self.match_multiple(vec![
            Token::GreaterEqual,
            Token::Greater,
            Token::Less,
            Token::LessEqual,
        ])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.parse_addition()?;
            Ok(Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        } else {
            Ok(lhs)
        }
    }

    fn parse_addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_multiplication()?;
        while let Some(token) = self.match_multiple(vec![Token::Plus, Token::Minus])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.parse_multiplication()?;
            expr = Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            }
        }
        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        while let Some(token) = self.match_multiple(vec![Token::Times, Token::Div])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.parse_unary()?;
            expr = Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.match_multiple(vec![Token::Bang, Token::Minus])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.parse_unary()?;
            Ok(Expr::UnaryOp {
                op,
                rhs: Box::new(rhs),
            })
        } else {
            Ok(self.parse_primary()?)
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.bump()?;
        match token {
            Some((_, Token::True, _)) => Ok(Expr::Primary {
                value: Value::Bool(true),
            }),
            Some((_, Token::False, _)) => Ok(Expr::Primary {
                value: Value::Bool(false),
            }),
            Some((_, Token::Integer(int), _)) => Ok(Expr::Primary {
                value: Value::Integer(int),
            }),
            Some((_, Token::Float(float), _)) => Ok(Expr::Primary {
                value: Value::Float(float),
            }),
            Some((_, Token::String(s), _)) => Ok(Expr::Primary {
                value: Value::String(s),
            }),
            Some((_, Token::LParenBrace, _)) => self.parse_record_literal(),
            // Parsing tuple or grouping
            Some((start, Token::LParen, end)) => {
                let expr = self.parse_expression()?;
                if let Some(_) = self.lookahead_match(TokenDiscriminants::Comma)? {
                    let mut elems = vec![expr];
                    let mut rest = self.comma::<Expr>(&Self::parse_expression, Token::RParen)?;
                    elems.append(&mut rest);
                    Ok(Expr::Tuple(elems))
                } else {
                    self.expect(TokenDiscriminants::RParen)?;
                    Ok(expr)
                }
            }
            Some((start, Token::Ident(name), end)) => Ok(Expr::Var { name }),
            Some((start, token, end)) => Err(ParseError::UnexpectedToken {
                token,
                location: Location(start, end),
                expected_tokens: vec![
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                ],
            })?,
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                ],
            })?,
        }
    }

    fn parse_record_literal(&mut self) -> Result<Expr, ParseError> {
        let mut entries = Vec::new();
        loop {
            match self.bump()? {
                Some((_, Token::RParenBrace, _)) => {
                    return Ok(Expr::Record { entries });
                }
                Some((_, Token::Ident(name), _)) => {
                    let field_val = self.parse_expression()?;
                    entries.push((name, field_val));
                }
                Some((start, tok, end)) => Err(ParseError::UnexpectedToken {
                    token: tok.clone(),
                    expected_tokens: vec![
                        TokenDiscriminants::RParenBrace,
                        TokenDiscriminants::Ident,
                    ],
                    location: Location(start, end),
                })?,
                None => Err(ParseError::EndOfFile {
                    expected_tokens: vec![
                        TokenDiscriminants::RParenBrace,
                        TokenDiscriminants::Ident,
                    ],
                })?,
            }
        }
    }

    fn parse_pattern(&mut self) -> Result<Pat, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((_, Token::LParen, _)) => {
                if let Some(_) = self.lookahead_match(TokenDiscriminants::RParen)? {
                    return Ok(Pat::Empty);
                }

                let mut tuple_patterns = self.comma::<Pat>(&Self::parse_pattern, Token::RParen)?;
                // If the pattern is singular, i.e. let (a) = 10, then
                // we treat it as a single id
                if tuple_patterns.len() == 1 {
                    match tuple_patterns.pop() {
                        Some(pat) => Ok(pat),
                        None => Err(ParseError::NotReachable)?,
                    }
                } else {
                    Ok(Pat::Tuple(tuple_patterns))
                }
            }
            Some((_, Token::LBrace, _)) => Ok(Pat::Record(
                self.comma::<Pat>(&Self::parse_record_pattern, Token::RBrace)?,
            )),
            Some((_, Token::Ident(name), _)) => {
                let type_sig = self.parse_type_sig()?;
                Ok(Pat::Id(name, type_sig))
            }
            Some((start, token, end)) => Err(ParseError::UnexpectedToken {
                token,
                location: Location(start, end),
                expected_tokens: vec![
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::Ident,
                ],
            })?,
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::Ident,
                ],
            })?,
        }
    }

    fn parse_record_pattern(&mut self) -> Result<Pat, ParseError> {
        let token = self.bump()?;
        match token {
            Some((_start, Token::Ident(name), _end)) => {
                let type_sig = self.parse_type_sig()?;
                Ok(Pat::Id(name, type_sig))
            }
            Some((start, token, end)) => Err(ParseError::UnexpectedToken {
                token,
                location: Location(start, end),
                expected_tokens: vec![TokenDiscriminants::Ident],
            })?,
            _ => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            })?,
        }
    }

    fn parse_type_sig(&mut self) -> Result<Option<TypeSig>, ParseError> {
        if let Some(_) = self.lookahead_match(TokenDiscriminants::Colon)? {
            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }

    fn parse_type(&mut self) -> Result<TypeSig, ParseError> {
        let token = self.bump()?;
        match token {
            Some((_, Token::Ident(name), _)) => Ok(TypeSig::Name(name)),
            Some((_, Token::LBracket, _)) => {
                let array_type = self.parse_type()?;
                self.expect(TokenDiscriminants::RBracket)?;
                Ok(TypeSig::Array(Box::new(array_type)))
            }
            Some((start, token, end)) => Err(ParseError::UnexpectedToken {
                token,
                location: Location(start, end),
                expected_tokens: vec![TokenDiscriminants::LBracket, TokenDiscriminants::Ident],
            })?,
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::LBracket, TokenDiscriminants::Ident],
            })?,
        }
    }

    fn comma<T: Debug>(
        &mut self,
        parse_fn: &dyn Fn(&mut Self) -> Result<T, ParseError>,
        end_token: Token,
    ) -> Result<Vec<T>, ParseError> {
        let mut parsed: Vec<T> = Vec::new();
        loop {
            parsed.push(parse_fn(self)?);
            if let Some(_) = self.lookahead_match((&end_token).into())? {
                return Ok(parsed);
            }
            self.expect(TokenDiscriminants::Comma)?;
        }
    }
}
