use crate::ast::{Expr, Name, Op, Pat, Stmt, TypeSig, Value};
use crate::lexer::{Lexer, LexicalError, LocationRange, Token, TokenDiscriminants};
use ast::{Loc, Program, TypeDef, UnaryOp};
use printer::token_to_string;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use utils::NameTable;

pub struct Parser<'input> {
    pub lexer: Lexer<'input>,
    errors: Vec<ParseError>,
    pushedback_tokens: Vec<(Token, LocationRange)>,
}

#[derive(Debug, Fail, PartialEq, Clone, Serialize, Deserialize)]
pub enum ParseError {
    #[fail(
        display = "Reached end of file without completing parse. Expected these tokens: {:?}",
        expected_tokens
    )]
    EndOfFile {
        expected_tokens: Vec<TokenDiscriminants>,
    },
    #[fail(
        display = "{}: Unexpected token {:?}, expected {:?}",
        location, token, expected_tokens
    )]
    UnexpectedToken {
        token: String,
        token_type: TokenDiscriminants,
        expected_tokens: String,
        location: LocationRange,
    },
    #[fail(display = "{}: Cannot destructure a function", location)]
    DestructureFunction { location: LocationRange },
    #[fail(
        display = "{}: Cannot have a type signature on a let function binding (use type signatures in the function!)",
        location
    )]
    FuncBindingTypeSig { location: LocationRange },
    #[fail(display = "{}", err)]
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
            errors: Vec::new(),
            pushedback_tokens: Vec::new(),
        }
    }

    // Gets the name table. Drops the parser though
    pub fn get_name_table(self) -> NameTable {
        self.lexer.name_table
    }

    fn expect(
        &mut self,
        expected: TokenDiscriminants,
    ) -> Result<(Token, LocationRange), ParseError> {
        let token = self.bump()?;
        if let Some((token, location)) = token {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == expected {
                Ok((token, location))
            } else {
                Err(ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    location,
                    expected_tokens: format!("{}", expected),
                })
            }
        } else {
            Err(ParseError::EndOfFile {
                expected_tokens: vec![expected],
            })
        }
    }

    fn pushback(&mut self, token: (Token, LocationRange)) {
        self.pushedback_tokens.push(token);
    }

    fn match_one(
        &mut self,
        lookahead: TokenDiscriminants,
    ) -> Result<Option<(Token, LocationRange)>, ParseError> {
        if let Some((token, location)) = self.bump()? {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == lookahead {
                Ok(Some((token, location)))
            } else {
                self.pushback((token, location));
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn match_multiple(
        &mut self,
        tokens: Vec<Token>,
    ) -> Result<Option<(Token, LocationRange)>, ParseError> {
        for token in tokens {
            if let Some(token) = self.match_one((&token).into())? {
                return Ok(Some(token));
            }
        }
        Ok(None)
    }

    fn bump(&mut self) -> Result<Option<(Token, LocationRange)>, ParseError> {
        let tok = match self.pushedback_tokens.pop() {
            Some(tok) => Ok(Some(tok)),
            None => match self.lexer.next() {
                Some(tok) => Ok(Some(tok?)),
                None => Ok(None),
            },
        };
        tok
    }

    #[allow(dead_code)]
    fn peek(&mut self) -> Result<(), ParseError> {
        let tok = self.bump()?;
        println!("TOK: {:?}", tok);
        if let Some(tok) = tok {
            self.pushback(tok);
        }
        Ok(())
    }

    // Pop tokens until we reach the end token. For example, when parsing a stmt
    // this is semicolon
    fn recover_from_error(&mut self, end_token: TokenDiscriminants) -> Result<(), ParseError> {
        while let Some((token, _)) = self.bump()? {
            if end_token == token.into() {
                return Ok(());
            }
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
            _ => Err(ParseError::NotReachable),
        }
    }

    pub fn program(&mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();
        let mut type_defs = Vec::new();
        loop {
            if let Some((_, left)) = self.match_one(TokenDiscriminants::Struct)? {
                match self.type_def(left) {
                    Ok(def) => type_defs.push(def),
                    Err(err) => {
                        self.errors.push(err);
                        // Our recover token for type defs is RBrace. This isn't ideal
                        // cause if the bug is that there is no RBrace, then we basically
                        // fail at parsing the rest of the code. But w/e
                        self.recover_from_error(TokenDiscriminants::RBrace)?;
                    }
                }
            } else {
                match self.stmt() {
                    Ok(Some(stmt)) => stmts.push(stmt),
                    Ok(None) => {}
                    Err(ParseError::EndOfFile { expected_tokens: _ }) => {
                        let mut errors = Vec::new();
                        std::mem::swap(&mut errors, &mut self.errors);
                        return Ok(Program {
                            stmts,
                            type_defs,
                            errors,
                        });
                    }
                    Err(err) => {
                        self.errors.push(err);
                    }
                }
            }
        }
    }

    fn id(&mut self) -> Result<(Name, LocationRange), ParseError> {
        match self.bump()? {
            Some((Token::Ident(id), loc)) => Ok((id, loc)),
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                location,
                token: token_to_string(&self.lexer.name_table, &token),
                token_type: token.into(),
                expected_tokens: format!("{}", TokenDiscriminants::Ident),
            }),
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            }),
        }
    }

    fn type_def(&mut self, left: LocationRange) -> Result<Loc<TypeDef>, ParseError> {
        let (id, _) = self.id()?;
        self.expect(TokenDiscriminants::LBrace)?;
        let (fields, right) =
            self.comma::<(Name, Loc<TypeSig>)>(&Self::record_type_field, Token::RBrace)?;
        Ok(Loc {
            location: LocationRange(left.0, right.1),
            inner: TypeDef::Struct(id, fields),
        })
    }

    fn record_type_field(&mut self) -> Result<(Name, Loc<TypeSig>), ParseError> {
        let (id, _) = self.id()?;
        self.expect(TokenDiscriminants::Colon)?;
        let type_sig = self.type_()?;
        Ok((id, type_sig))
    }

    pub fn stmt(&mut self) -> Result<Option<Loc<Stmt>>, ParseError> {
        let tok = self.bump()?;
        let res = match tok {
            Some((Token::Let, loc)) => self.let_stmt(loc),
            Some((Token::Return, loc)) => self.return_stmt(loc),
            Some((Token::Export, loc)) => self.export_stmt(loc),
            Some((Token::If, loc)) => {
                let if_expr = self.if_expr(loc)?;
                Ok(Loc {
                    location: if_expr.location,
                    inner: Stmt::Expr(if_expr),
                })
            }
            Some((token, loc)) => {
                self.pushback((token, loc));
                self.expression_stmt()
            }
            None => {
                return Err(ParseError::EndOfFile {
                    expected_tokens: vec![
                        TokenDiscriminants::Let,
                        TokenDiscriminants::Return,
                        TokenDiscriminants::Export,
                    ],
                })
            }
        };
        match res {
            Ok(res) => Ok(Some(res)),
            err @ Err(ParseError::EndOfFile { expected_tokens: _ }) => return err.map(Some),
            Err(err) => {
                // Special case if the unexpected token is a semicolon
                // since that's our recovery token
                if let ParseError::UnexpectedToken {
                    token: _,
                    token_type,
                    location: _,
                    expected_tokens: _,
                } = &err
                {
                    if token_type == &TokenDiscriminants::Semicolon {
                        self.errors.push(err);
                        return Ok(None);
                    }
                }
                self.errors.push(err);
                self.recover_from_error(TokenDiscriminants::Semicolon)?;
                Ok(None)
            }
        }
    }

    fn export_stmt(&mut self, left: LocationRange) -> Result<Loc<Stmt>, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((Token::Ident(name), _)) => {
                let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
                Ok(Loc {
                    location: LocationRange(left.0, right.1),
                    inner: Stmt::Export(name),
                })
            }
            Some((token, location)) => {
                self.pushback((token.clone(), location.clone()));
                Err(ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    expected_tokens: format!("{}", TokenDiscriminants::Ident),
                    location,
                })
            }
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            }),
        }
    }

    fn return_stmt(&mut self, left: LocationRange) -> Result<Loc<Stmt>, ParseError> {
        let expr = self.expr()?;
        let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Loc {
            location: LocationRange(left.0, right.1),
            inner: Stmt::Return(expr),
        })
    }

    fn let_stmt(&mut self, left: LocationRange) -> Result<Loc<Stmt>, ParseError> {
        let pat = self.pattern()?;
        self.expect(TokenDiscriminants::Equal)?;
        let rhs_expr = self.expr()?;
        if let Loc {
            location,
            inner:
                Expr::Function {
                    params,
                    return_type,
                    body,
                },
        } = rhs_expr
        {
            match pat {
                Pat::Id(name, None, _) => Ok(Loc {
                    location: LocationRange(left.0, location.1),
                    inner: Stmt::Function(name, params, return_type, body),
                }),
                Pat::Id(_, _, _) => Err(ParseError::FuncBindingTypeSig { location }),
                Pat::Record(_, _, location) | Pat::Empty(location) | Pat::Tuple(_, location) => {
                    Err(ParseError::DestructureFunction { location })
                }
            }
        } else {
            self.expect(TokenDiscriminants::Semicolon)?;
            Ok(Loc {
                location: LocationRange(left.0, rhs_expr.location.1),
                inner: Stmt::Asgn(pat, rhs_expr),
            })
        }
    }

    fn expression_stmt(&mut self) -> Result<Loc<Stmt>, ParseError> {
        let expr = self.expr()?;
        let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Loc {
            location: LocationRange(expr.location.0, right.1),
            inner: Stmt::Expr(expr),
        })
    }

    fn expr(&mut self) -> Result<Loc<Expr>, ParseError> {
        match self.bump()? {
            Some((Token::Slash, left)) => self.function(left),
            Some((Token::LBrace, left)) => self.expr_block(left),
            Some((Token::If, left)) => self.if_expr(left),
            Some((Token::Ident(id), left)) => {
                if self.match_one(TokenDiscriminants::LBrace)?.is_some() {
                    self.record_literal(id, left)
                } else {
                    self.pushback((Token::Ident(id), left));
                    self.equality()
                }
            }
            Some(span) => {
                self.pushback(span);
                self.equality()
            }
            None => self.equality(),
        }
    }

    fn if_expr(&mut self, left: LocationRange) -> Result<Loc<Expr>, ParseError> {
        // Yeah...I'm not allowing functions or blocks in the cond spot
        let cond = self.equality()?;
        let (_, block_left) = self.expect(TokenDiscriminants::LBrace)?;
        let then_block = self.expr_block(block_left)?;
        let else_block = if let Some((_, else_left)) = self.match_one(TokenDiscriminants::Else)? {
            self.expect(TokenDiscriminants::LBrace)?;
            Some(Box::new(self.expr_block(else_left)?))
        } else {
            None
        };
        Ok(Loc {
            location: LocationRange(
                left.0,
                (&else_block)
                    .as_ref()
                    .map(|else_block| else_block.location.1)
                    .unwrap_or(then_block.location.1),
            ),
            inner: Expr::If(Box::new(cond), Box::new(then_block), else_block),
        })
    }

    fn expr_block(&mut self, left: LocationRange) -> Result<Loc<Expr>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            if let Some((_, right)) = self.match_one(TokenDiscriminants::RBrace)? {
                return Ok(Loc {
                    location: LocationRange(left.0, right.1),
                    inner: Expr::Block(stmts, None),
                });
            }
            // If we're undeniably starting a statement then
            // parse it and push onto the vec
            if let Some(span) =
                self.match_multiple(vec![Token::Let, Token::Return, Token::Export])?
            {
                self.pushback(span);
                self.stmt()?.map(|stmt| stmts.push(stmt));
            } else {
                // Otherwise we could either be in an expr stmt or an ending expr situation
                let expr = self.expr()?;
                if let Some((_, right)) = self.match_one(TokenDiscriminants::Semicolon)? {
                    stmts.push(Loc {
                        location: LocationRange(expr.location.0, right.1),
                        inner: Stmt::Expr(expr),
                    });
                } else {
                    let (_, right) = self.expect(TokenDiscriminants::RBrace)?;
                    return Ok(Loc {
                        location: LocationRange(left.0, right.1),
                        inner: Expr::Block(stmts, Some(Box::new(expr))),
                    });
                }
            }
        }
    }

    fn function(&mut self, left: LocationRange) -> Result<Loc<Expr>, ParseError> {
        let params = self.pattern()?;
        let return_type = self.type_sig()?;
        self.expect(TokenDiscriminants::FatArrow)?;
        let token = self.bump()?;
        let body = match token {
            Some((Token::LBrace, left)) => self.expr_block(left)?,
            Some((Token::LParen, left)) => {
                let mut expr = self.expr()?;
                let (_, right) = self.expect(TokenDiscriminants::RParen)?;
                expr.location = LocationRange(left.0, right.1);
                expr
            }
            Some((token, left)) => {
                self.pushback((token, left));
                self.expr()?
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
                });
            }
        };
        Ok(Loc {
            location: LocationRange(left.0, body.location.1),
            inner: Expr::Function {
                params,
                return_type: return_type.map(|(return_type, _)| return_type),
                body: Box::new(body),
            },
        })
    }

    fn equality(&mut self) -> Result<Loc<Expr>, ParseError> {
        let lhs = self.comparison()?;
        if let Some((token, _)) = self.match_multiple(vec![Token::EqualEqual, Token::BangEqual])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.comparison()?;
            Ok(Loc {
                location: LocationRange(lhs.location.0, rhs.location.1),
                inner: Expr::BinOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Ok(lhs)
        }
    }

    fn comparison(&mut self) -> Result<Loc<Expr>, ParseError> {
        let lhs = self.addition()?;
        if let Some((token, _)) = self.match_multiple(vec![
            Token::GreaterEqual,
            Token::Greater,
            Token::Less,
            Token::LessEqual,
        ])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.addition()?;
            Ok(Loc {
                location: LocationRange(lhs.location.0, rhs.location.1),
                inner: Expr::BinOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Ok(lhs)
        }
    }

    fn addition(&mut self) -> Result<Loc<Expr>, ParseError> {
        let mut expr = self.multiplication()?;
        while let Some((token, _)) = self.match_multiple(vec![Token::Plus, Token::Minus])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.multiplication()?;
            expr = Loc {
                location: LocationRange(expr.location.0, rhs.location.1),
                inner: Expr::BinOp {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Loc<Expr>, ParseError> {
        let mut expr = self.unary()?;
        while let Some((token, _)) = self.match_multiple(vec![Token::Times, Token::Div])? {
            let op = self.lookup_op_token(token)?;
            let rhs = self.unary()?;
            expr = Loc {
                location: LocationRange(expr.location.0, rhs.location.1),
                inner: Expr::BinOp {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                },
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Loc<Expr>, ParseError> {
        if let Some((token, left)) = self.match_multiple(vec![Token::Bang, Token::Minus])? {
            let op = match token {
                Token::Bang => UnaryOp::Not,
                Token::Minus => UnaryOp::Minus,
                _ => return Err(ParseError::NotReachable),
            };
            let rhs = self.unary()?;
            Ok(Loc {
                location: LocationRange(left.0, rhs.location.1),
                inner: Expr::UnaryOp {
                    op,
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Ok(self.call()?)
        }
    }

    fn call(&mut self) -> Result<Loc<Expr>, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if let Some((_, left)) = self.match_one(TokenDiscriminants::LParen)? {
                expr = self.finish_call(expr, left)?;
            } else if self.match_one(TokenDiscriminants::Dot)?.is_some() {
                match self.bump()? {
                    Some((Token::Ident(name), right)) => {
                        expr = Loc {
                            location: LocationRange(expr.location.0, right.1),
                            inner: Expr::Field(Box::new(expr), name),
                        };
                    }
                    Some((token, location)) => {
                        return Err(ParseError::UnexpectedToken {
                            token: token_to_string(&self.lexer.name_table, &token),
                            token_type: token.into(),
                            location,
                            expected_tokens: format!("{}", TokenDiscriminants::Ident),
                        })
                    }
                    None => {
                        return Err(ParseError::EndOfFile {
                            expected_tokens: vec![TokenDiscriminants::Ident],
                        })
                    }
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(
        &mut self,
        callee: Loc<Expr>,
        left: LocationRange,
    ) -> Result<Loc<Expr>, ParseError> {
        let args = {
            let (mut exprs, right) = self.comma::<Loc<Expr>>(&Self::expr, Token::RParen)?;
            if exprs.len() == 1 {
                exprs.remove(0)
            } else {
                Loc {
                    location: LocationRange(left.0, right.1),
                    inner: Expr::Tuple(exprs),
                }
            }
        };
        Ok(Loc {
            location: LocationRange(callee.location.0, args.location.1),
            inner: Expr::Call {
                callee: Box::new(callee),
                args: Box::new(args),
            },
        })
    }

    fn primary(&mut self) -> Result<Loc<Expr>, ParseError> {
        let (token, location) = if let Some(span) = self.bump()? {
            span
        } else {
            return Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                ],
            });
        };
        match token {
            Token::True => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Bool(true),
                },
            }),
            Token::False => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Bool(false),
                },
            }),
            Token::Integer(int) => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Integer(int),
                },
            }),
            Token::Float(float) => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Float(float),
                },
            }),
            Token::String(s) => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::String(s),
                },
            }),
            // Parsing tuple or grouping
            Token::LParen => {
                let expr = self.expr()?;
                if self.match_one(TokenDiscriminants::Comma)?.is_some() {
                    let mut elems = vec![expr];
                    let (mut rest, right) = self.comma::<Loc<Expr>>(&Self::expr, Token::RParen)?;
                    elems.append(&mut rest);
                    Ok(Loc {
                        location: LocationRange(location.0, right.1),
                        inner: Expr::Tuple(elems),
                    })
                } else {
                    self.expect(TokenDiscriminants::RParen)?;
                    Ok(expr)
                }
            }
            Token::Ident(name) => Ok(Loc {
                location,
                inner: Expr::Var { name },
            }),
            token => {
                let expected_tokens = format!(
                    "{}, {}, {}, {}, {}, {}",
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                );
                Err(ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    location,
                    expected_tokens,
                })
            }
        }
    }

    fn record_literal(
        &mut self,
        name: Name,
        name_loc: LocationRange,
    ) -> Result<Loc<Expr>, ParseError> {
        let (fields, end_loc) =
            self.comma::<(Name, Loc<Expr>)>(&Self::record_field, Token::RBrace)?;
        Ok(Loc {
            location: LocationRange(name_loc.0, end_loc.1),
            inner: Expr::Record { name, fields },
        })
    }

    fn record_field(&mut self) -> Result<(Name, Loc<Expr>), ParseError> {
        let (field_name, name_loc) = self.id()?;
        // If we find a comma, we treat `foo,` as `foo: foo,`
        let expr = if self.match_one(TokenDiscriminants::Comma)?.is_some() {
            Loc {
                location: name_loc,
                inner: Expr::Var { name: field_name },
            }
        } else {
            self.expect(TokenDiscriminants::Colon)?;
            self.expr()?
        };
        Ok((field_name, expr))
    }

    fn pattern(&mut self) -> Result<Pat, ParseError> {
        let (token, left) = if let Some((token, loc)) = self.bump()? {
            (token, loc)
        } else {
            return Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::Ident,
                ],
            });
        };
        match token {
            Token::LParen => {
                if let Some((_, right)) = self.match_one(TokenDiscriminants::RParen)? {
                    return Ok(Pat::Empty(LocationRange(left.0, right.0)));
                }

                let (mut pats, right) = self.comma::<Pat>(&Self::pattern, Token::RParen)?;
                // If the pattern is singular, i.e. let (a) = 10, then
                // we treat it as a single id
                if pats.len() == 1 {
                    Ok(pats.pop().unwrap())
                } else {
                    Ok(Pat::Tuple(pats, LocationRange(left.0, right.1)))
                }
            }
            Token::LBrace => {
                let (fields, right) = self.comma::<Name>(&Self::record_pattern, Token::RBrace)?;
                if let Some((type_sig, right)) = self.type_sig()? {
                    Ok(Pat::Record(
                        fields,
                        Some(type_sig),
                        LocationRange(left.0, right.1),
                    ))
                } else {
                    Ok(Pat::Record(fields, None, LocationRange(left.0, right.1)))
                }
            }
            Token::Ident(name) => {
                if let Some((type_sig, right)) = self.type_sig()? {
                    Ok(Pat::Id(
                        name,
                        Some(type_sig),
                        LocationRange(left.0, right.1),
                    ))
                } else {
                    Ok(Pat::Id(name, None, left))
                }
            }
            token => {
                return Err(ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    location: left,
                    expected_tokens: format!(
                        "{}, {}, {}",
                        TokenDiscriminants::LParen,
                        TokenDiscriminants::LBrace,
                        TokenDiscriminants::Ident,
                    ),
                })
            }
        }
    }

    fn record_pattern(&mut self) -> Result<Name, ParseError> {
        let token = self.bump()?;
        match token {
            Some((Token::Ident(name), _)) => Ok(name),
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                token: token_to_string(&self.lexer.name_table, &token),
                token_type: token.into(),
                location,
                expected_tokens: format!("{}", TokenDiscriminants::Ident),
            }),
            _ => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            }),
        }
    }

    fn type_sig(&mut self) -> Result<Option<(Loc<TypeSig>, LocationRange)>, ParseError> {
        if let Some((_, left)) = self.match_one(TokenDiscriminants::Colon)? {
            let type_sig = self.type_()?;
            let right = type_sig.location;
            Ok(Some((type_sig, LocationRange(left.0, right.1))))
        } else {
            Ok(None)
        }
    }

    fn type_(&mut self) -> Result<Loc<TypeSig>, ParseError> {
        let token = self.bump()?;
        match token {
            Some((Token::Ident(name), location)) => {
                let sig = Loc {
                    location,
                    inner: TypeSig::Name(name),
                };
                if self.match_one(TokenDiscriminants::Arrow)?.is_some() {
                    let return_type = self.type_()?;
                    Ok(Loc {
                        location: LocationRange(location.0, return_type.location.1),
                        inner: TypeSig::Arrow(vec![sig], Box::new(return_type)),
                    })
                } else {
                    Ok(sig)
                }
            }
            Some((Token::LBracket, left)) => {
                let array_type = self.type_()?;
                let (_, right) = self.expect(TokenDiscriminants::RBracket)?;
                Ok(Loc {
                    location: LocationRange(left.0, right.1),
                    inner: TypeSig::Array(Box::new(array_type)),
                })
            }
            Some((Token::LParen, left)) => {
                let (param_types, _) = self.comma(&Self::type_, Token::RParen)?;
                self.expect(TokenDiscriminants::Arrow)?;
                let return_type = self.type_()?;
                Ok(Loc {
                    location: LocationRange(left.0, return_type.location.1),
                    inner: TypeSig::Arrow(param_types, Box::new(return_type)),
                })
            }
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                token: token_to_string(&self.lexer.name_table, &token),
                token_type: token.into(),
                location,
                expected_tokens: format!(
                    "{}, {}",
                    TokenDiscriminants::LBracket,
                    TokenDiscriminants::Ident
                ),
            }),
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::LBracket, TokenDiscriminants::Ident],
            }),
        }
    }

    fn comma<T: Debug>(
        &mut self,
        parse_fn: &dyn Fn(&mut Self) -> Result<T, ParseError>,
        end_token: Token,
    ) -> Result<(Vec<T>, LocationRange), ParseError> {
        let mut elems: Vec<T> = Vec::new();
        if let Some((_, right)) = self.match_one((&end_token).into())? {
            return Ok((elems, right));
        }
        loop {
            elems.push(parse_fn(self)?);
            if let Some((_, right)) = self.match_one((&end_token).into())? {
                return Ok((elems, right));
            }
            self.expect(TokenDiscriminants::Comma)?;
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expr, Loc, Op, Pat, Stmt, TypeSig, Value};
    use lexer::{Lexer, Location, LocationRange};
    use parser::{ParseError, Parser};
    use std::ffi::OsStr;
    use std::fs;
    use std::fs::File;
    use std::io::Write;
    use std::path::PathBuf;

    #[test]
    #[ignore]
    fn generate_baseline() -> Result<(), failure::Error> {
        for entry in fs::read_dir("tests/parser")? {
            let entry = &entry?.path();
            if entry.extension() == Some(OsStr::new("sbr")) {
                let source = fs::read_to_string(entry)?;
                let lexer = Lexer::new(&source);
                let mut parser = Parser::new(lexer);
                let output = match parser.program() {
                    Ok(stmts) => serde_json::to_string_pretty(&stmts)?,
                    Err(err) => err.to_string(),
                };
                let mut out_path = PathBuf::new();
                out_path.push("tests/parser/");
                out_path.push(entry.file_stem().unwrap());
                out_path.set_extension("json");
                let mut out_file = File::create(out_path)?;
                out_file.write_all(output.as_bytes())?;
            }
        }
        Ok(())
    }

    #[test]
    fn literal() -> Result<(), failure::Error> {
        let expected = vec![
            Loc {
                location: LocationRange(Location(1, 1), Location(1, 3)),
                inner: Expr::Primary {
                    value: Value::Integer(10),
                },
            },
            Loc {
                location: LocationRange(Location(1, 4), Location(1, 8)),
                inner: Expr::Primary {
                    value: Value::Float(10.2),
                },
            },
            Loc {
                location: LocationRange(Location(1, 9), Location(1, 13)),
                inner: Expr::Primary {
                    value: Value::Bool(true),
                },
            },
            Loc {
                location: LocationRange(Location(1, 14), Location(1, 19)),
                inner: Expr::Primary {
                    value: Value::Bool(false),
                },
            },
            Loc {
                location: LocationRange(Location(1, 20), Location(1, 27)),
                inner: Expr::Primary {
                    value: Value::String("hello".into()),
                },
            },
        ];
        let source = "10 10.2 true false \"hello\"";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        for i in 0..5 {
            assert_eq!(expected[i], parser.primary()?)
        }
        Ok(())
    }

    #[test]
    fn id() -> Result<(), ParseError> {
        let expected = vec![
            Loc {
                location: LocationRange(Location(1, 1), Location(1, 4)),
                inner: Expr::Var { name: 0 },
            },
            Loc {
                location: LocationRange(Location(1, 5), Location(1, 8)),
                inner: Expr::Var { name: 1 },
            },
            Loc {
                location: LocationRange(Location(1, 9), Location(1, 12)),
                inner: Expr::Var { name: 1 },
            },
            Loc {
                location: LocationRange(Location(1, 13), Location(1, 16)),
                inner: Expr::Var { name: 2 },
            },
            Loc {
                location: LocationRange(Location(1, 17), Location(1, 20)),
                inner: Expr::Var { name: 3 },
            },
        ];
        let source = "foo bar bar baz bat";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        for i in 0..5 {
            assert_eq!(expected[i], parser.primary()?);
        }
        assert_eq!("foo", parser.lexer.name_table.get_str(&0));
        assert_eq!("bar", parser.lexer.name_table.get_str(&1));
        assert_eq!("baz", parser.lexer.name_table.get_str(&2));
        assert_eq!("bat", parser.lexer.name_table.get_str(&3));
        Ok(())
    }

    #[test]
    fn pattern() -> Result<(), ParseError> {
        let expected = vec![
            Pat::Id(0, None, LocationRange(Location(1, 1), Location(1, 4))),
            Pat::Id(
                1,
                Some(TypeSig::Name(2)),
                LocationRange(Location(1, 5), Location(1, 13)),
            ),
            Pat::Tuple(
                vec![
                    Pat::Id(0, None, LocationRange(Location(1, 15), Location(1, 18))),
                    Pat::Id(1, None, LocationRange(Location(1, 20), Location(1, 23))),
                ],
                LocationRange(Location(1, 14), Location(1, 24)),
            ),
            Pat::Record(
                vec![0, 1, 2],
                Some(TypeSig::Name(3)),
                LocationRange(Location(1, 26), Location(1, 46)),
            ),
        ];
        let source = "foo bar: int (foo, bar) { foo, bar, baz }: A";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        for i in 0..3 {
            assert_eq!(expected[i], parser.pattern()?);
        }
        assert_eq!("foo", parser.lexer.name_table.get_str(&0));
        assert_eq!("bar", parser.lexer.name_table.get_str(&1));
        Ok(())
    }

    #[test]
    fn arithmetic() -> Result<(), ParseError> {
        let expected = Loc {
            location: LocationRange(Location(1, 1), Location(1, 16)),
            inner: Expr::BinOp {
                op: Op::Plus,
                lhs: Box::new(Loc {
                    location: LocationRange(Location(1, 1), Location(1, 7)),
                    inner: Expr::BinOp {
                        op: Op::Times,
                        lhs: Box::new(Loc {
                            location: LocationRange(Location(1, 1), Location(1, 3)),
                            inner: Expr::Primary {
                                value: Value::Integer(10),
                            },
                        }),
                        rhs: Box::new(Loc {
                            location: LocationRange(Location(1, 6), Location(1, 7)),
                            inner: Expr::Primary {
                                value: Value::Integer(2),
                            },
                        }),
                    },
                }),
                rhs: Box::new(Loc {
                    location: LocationRange(Location(1, 10), Location(1, 16)),
                    inner: Expr::BinOp {
                        op: Op::Div,
                        lhs: Box::new(Loc {
                            location: LocationRange(Location(1, 10), Location(1, 11)),
                            inner: Expr::Primary {
                                value: Value::Integer(3),
                            },
                        }),
                        rhs: Box::new(Loc {
                            location: LocationRange(Location(1, 14), Location(1, 16)),
                            inner: Expr::UnaryOp {
                                op: Op::Minus,
                                rhs: Box::new(Loc {
                                    location: LocationRange(Location(1, 15), Location(1, 16)),
                                    inner: Expr::Primary {
                                        value: Value::Integer(4),
                                    },
                                }),
                            },
                        }),
                    },
                }),
            },
        };
        let source = "10 * 2 + 3 / -4";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        assert_eq!(expected, parser.expr()?);
        Ok(())
    }

    #[test]
    fn function() -> Result<(), ParseError> {
        let expected = Loc {
            location: LocationRange(Location(1, 1), Location(1, 12)),
            inner: Expr::Function {
                params: Pat::Id(0, None, LocationRange(Location(1, 2), Location(1, 3))),
                return_type: None,
                body: Box::new(Loc {
                    location: LocationRange(Location(1, 7), Location(1, 12)),
                    inner: Stmt::Return(Loc {
                        location: LocationRange(Location(1, 7), Location(1, 12)),
                        inner: Expr::BinOp {
                            op: Op::Plus,
                            lhs: Box::new(Loc {
                                location: LocationRange(Location(1, 7), Location(1, 8)),
                                inner: Expr::Var { name: 0 },
                            }),
                            rhs: Box::new(Loc {
                                location: LocationRange(Location(1, 11), Location(1, 12)),
                                inner: Expr::Primary {
                                    value: Value::Integer(1),
                                },
                            }),
                        },
                    }),
                }),
            },
        };
        let source = "\\a => a + 1";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        assert_eq!(expected, parser.expr()?);
        Ok(())
    }
}
