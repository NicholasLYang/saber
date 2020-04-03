use crate::ast::{Expr, Name, Op, Pat, Stmt, TypeSig, Value};
use crate::lexer::{Lexer, LexicalError, Token, TokenDiscriminants};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use utils::SymbolTable;

pub struct Parser<'input> {
    lexer: Lexer<'input>,
    pushedback_tokens: Vec<(usize, Token, usize)>,
}

#[derive(Debug, PartialEq)]
pub struct Location(usize, usize);

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

    // Gets the symbol table. Drops the parser though
    pub fn get_symbol_table(self) -> SymbolTable {
        self.lexer.get_symbol_table()
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

    fn match_one(&mut self, lookahead: TokenDiscriminants) -> Result<Option<Token>, ParseError> {
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
            if let Some(token) = self.match_one((&token).into())? {
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
        println!("TOK: {:?}", tok);
        if let Some(tok) = tok {
            self.pushback(tok);
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

    fn block(&mut self) -> Result<Stmt, ParseError> {
        let mut stmts = Vec::new();
        while let None = self.match_one(TokenDiscriminants::RBrace)? {
            stmts.push(self.stmt()?);
        }
        Ok(Stmt::Block(stmts))
    }

    pub fn stmts(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            match self.stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(ParseError::EndOfFile { expected_tokens: _ }) => return Ok(stmts),
                Err(err) => return Err(err),
            }
        }
    }

    // Returns multiple stmts because of desugaring.
    pub fn stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((_, Token::Let, _)) => self.let_stmt(),
            Some((_, Token::Return, _)) => Ok(self.return_stmt()?),
            Some((_, Token::Export, _)) => Ok(self.export_stmt()?),
            Some(token) => {
                self.pushback(token);
                Ok(self.expression_stmt()?)
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

    fn export_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((_, Token::Ident(name), _)) => {
                self.expect(TokenDiscriminants::Semicolon)?;
                Ok(Stmt::Export(name))
            }
            Some((start, tok, end)) => {
                self.pushback((start, tok.clone(), end));
                Err(ParseError::UnexpectedToken {
                    token: tok,
                    expected_tokens: vec![TokenDiscriminants::Ident],
                    location: Location(start, end),
                })
            }
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            }),
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expr()?;
        self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Stmt::Return(expr))
    }

    fn let_stmt(&mut self) -> Result<Stmt, ParseError> {
        let pat = self.pattern()?;
        self.expect(TokenDiscriminants::Equal)?;
        let rhs_expr = self.expr()?;
        if let Pat::Id(name, type_sig) = pat {
            return Ok(Stmt::Asgn(Pat::Id(name, type_sig), rhs_expr));
        }

        // NOTE: Maybe function asgn's should be a separate type?
        // FuncAsgn or something. That way we can validate the pattern
        // is just an Id at parse-time
        if let Expr::Function {
            params,
            return_type,
            body,
        } = rhs_expr
        {
            Ok(Stmt::Asgn(
                pat,
                Expr::Function {
                    params,
                    return_type,
                    body,
                },
            ))
        } else {
            self.expect(TokenDiscriminants::Semicolon)?;
            Ok(Stmt::Asgn(pat, rhs_expr))
        }
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expr()?;
        self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        if let Some(_) = self.match_one(TokenDiscriminants::Slash)? {
            self.function()
        } else {
            self.equality()
        }
    }

    fn function(&mut self) -> Result<Expr, ParseError> {
        let params = self.pattern()?;
        let return_type = self.type_sig()?;
        self.expect(TokenDiscriminants::FatArrow)?;
        let token = self.bump()?;
        let body = match token {
            Some((_, Token::LBrace, _)) => self.block()?,
            Some((_, Token::LParen, _)) => {
                let expr = self.expr()?;
                self.expect(TokenDiscriminants::RParen)?;
                Stmt::Return(expr)
            }
            Some((start, token, end)) => {
                self.pushback((start, token, end));
                Stmt::Return(self.expr()?)
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

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.comparison()?;
        if let Some(token) = self.match_multiple(vec![Token::EqualEqual, Token::BangEqual])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.comparison()?;
            Ok(Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        } else {
            Ok(lhs)
        }
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.addition()?;
        if let Some(token) = self.match_multiple(vec![
            Token::GreaterEqual,
            Token::Greater,
            Token::Less,
            Token::LessEqual,
        ])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.addition()?;
            Ok(Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        } else {
            Ok(lhs)
        }
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication()?;
        while let Some(token) = self.match_multiple(vec![Token::Plus, Token::Minus])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.multiplication()?;
            expr = Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            }
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while let Some(token) = self.match_multiple(vec![Token::Times, Token::Div])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.unary()?;
            expr = Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.match_multiple(vec![Token::Bang, Token::Minus])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.unary()?;
            Ok(Expr::UnaryOp {
                op,
                rhs: Box::new(rhs),
            })
        } else {
            Ok(self.call()?)
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if let Some(_) = self.match_one(TokenDiscriminants::LParen)? {
                expr = self.finish_call(expr)?;
            } else if let Some(_) = self.match_one(TokenDiscriminants::Dot)? {
                let tok = self.bump()?;
                if let Some((_, Token::Ident(name), _)) = tok {
                    expr = Expr::Field(Box::new(expr), name);
                } else {
                    tok.map(|tok| self.pushback(tok));
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let args = self.comma::<Expr>(&Self::expr, Token::RParen)?;
        Ok(Expr::Call {
            callee: Box::new(callee),
            args,
        })
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
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
            Some((_, Token::LParenBrace, _)) => self.record_literal(),
            // Parsing tuple or grouping
            Some((_, Token::LParen, _)) => {
                let expr = self.expr()?;
                if let Some(_) = self.match_one(TokenDiscriminants::Comma)? {
                    let mut elems = vec![expr];
                    let mut rest = self.comma::<Expr>(&Self::expr, Token::RParen)?;
                    elems.append(&mut rest);
                    Ok(Expr::Tuple(elems))
                } else {
                    self.expect(TokenDiscriminants::RParen)?;
                    Ok(expr)
                }
            }
            Some((_, Token::Ident(name), _)) => Ok(Expr::Var { name }),
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

    fn record_literal(&mut self) -> Result<Expr, ParseError> {
        let mut entries = Vec::new();
        loop {
            match self.bump()? {
                Some((_, Token::RParenBrace, _)) => {
                    return Ok(Expr::Record { entries });
                }
                Some((_, Token::Ident(name), _)) => {
                    let field_val = self.expr()?;
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

    fn pattern(&mut self) -> Result<Pat, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((_, Token::LParen, _)) => {
                if let Some(_) = self.match_one(TokenDiscriminants::RParen)? {
                    return Ok(Pat::Empty);
                }

                let mut pats = self.comma::<Pat>(&Self::pattern, Token::RParen)?;
                // If the pattern is singular, i.e. let (a) = 10, then
                // we treat it as a single id
                if pats.len() == 1 {
                    Ok(pats.pop().unwrap())
                } else {
                    Ok(Pat::Tuple(pats))
                }
            }
            Some((_, Token::LBrace, _)) => Ok(Pat::Record(
                self.comma::<Name>(&Self::record_pattern, Token::RBrace)?,
            )),
            Some((_, Token::Ident(name), _)) => {
                let type_sig = self.type_sig()?;
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

    fn record_pattern(&mut self) -> Result<Name, ParseError> {
        let token = self.bump()?;
        match token {
            Some((_start, Token::Ident(name), _end)) => Ok(name),
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

    fn type_sig(&mut self) -> Result<Option<TypeSig>, ParseError> {
        if let Some(_) = self.match_one(TokenDiscriminants::Colon)? {
            Ok(Some(self.type_()?))
        } else {
            Ok(None)
        }
    }

    fn type_(&mut self) -> Result<TypeSig, ParseError> {
        let token = self.bump()?;
        match token {
            Some((_, Token::Ident(name), _)) => Ok(TypeSig::Name(name)),
            Some((_, Token::LBracket, _)) => {
                let array_type = self.type_()?;
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
        if let Some(_) = self.match_one((&end_token).into())? {
            return Ok(parsed);
        }
        loop {
            parsed.push(parse_fn(self)?);
            if let Some(_) = self.match_one((&end_token).into())? {
                return Ok(parsed);
            }
            self.expect(TokenDiscriminants::Comma)?;
        }
    }
}
