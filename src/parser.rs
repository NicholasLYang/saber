use crate::ast::{Expr, Name, Op, Pat, Stmt, TypeSig, Value};
use crate::ast::{Loc, Program, TypeDef, UnaryOp};
use crate::lexer::{Lexer, LexicalError, LocationRange, SyntaxToken, TokenDiscriminants};
use crate::loc;
use crate::printer::token_to_string;
use crate::utils::NameTable;
use serde::{Deserialize, Serialize};
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::result::Result;

pub struct Parser<'input> {
    pub lexer: Lexer<'input>,
    errors: Vec<Loc<ParseError>>,
    pushedback_tokens: Vec<Loc<SyntaxToken>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum ParseError {
    EndOfFile {
        expected_tokens: Vec<TokenDiscriminants>,
    },
    UnexpectedToken {
        token: String,
        token_type: TokenDiscriminants,
        expected_tokens: String,
    },
    DestructureFunction,
    FuncBindingTypeSig,
    LexicalError {
        err: LexicalError,
    },
    AmbiguousArrowFunc,
    InvalidIndex {
        index: String,
    },
    InvalidFloat {
        float: String,
    },
    NotReachable,
    TupleTypeSig,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::EndOfFile {
                expected_tokens,
            } => write!(f, "Reached end of file without completing parse. Expected these tokens: {:?}",
                        expected_tokens),
            ParseError::UnexpectedToken {
                token,
                token_type: _,
                expected_tokens,
            } => write!(f, "Unexpected token {:?}, expected {:?}", token, expected_tokens),
            ParseError::DestructureFunction => write!(f, "Cannot destructure a function"),
            ParseError::FuncBindingTypeSig => write!(f, "Cannot have a type signature on a let function binding (use type signatures in the function!)"),
            ParseError::LexicalError { err } => write!(f, "{}", err),
            ParseError::AmbiguousArrowFunc => write!(f, "You could be writing either a tuple or an arrow function. This implies that you're writing an arrow function, but something later on contradicts that"),
            ParseError::InvalidIndex { index } => write!(f, "{} is not a valid index", index),
            ParseError::InvalidFloat { float } => write!(f, "{} is not a valid float", float),
            ParseError::NotReachable => write!(f, "Not reachable"),
            ParseError::TupleTypeSig => write!(f, "It appears that you've added a type signature to a tuple. Did you mean to type a function")
        }
    }
}

impl From<Loc<LexicalError>> for Loc<ParseError> {
    fn from(error: Loc<LexicalError>) -> Self {
        loc!(
            ParseError::LexicalError { err: error.inner },
            error.location
        )
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
    ) -> Result<(SyntaxToken, LocationRange), Loc<ParseError>> {
        let token = self.bump()?;
        if let Some(Loc {
            inner: token,
            location,
        }) = token
        {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == expected {
                Ok((token, location))
            } else {
                Err(Loc {
                    location,
                    inner: ParseError::UnexpectedToken {
                        token: token_to_string(&self.lexer.name_table, &token),
                        token_type: token.into(),
                        expected_tokens: format!("{}", expected),
                    },
                })
            }
        } else {
            Err(Loc {
                location: LocationRange(self.lexer.current_location, self.lexer.current_location),
                inner: ParseError::EndOfFile {
                    expected_tokens: vec![expected],
                },
            })
        }
    }

    fn pushback(&mut self, token: Loc<SyntaxToken>) {
        self.pushedback_tokens.push(token);
    }

    fn match_one(
        &mut self,
        lookahead: TokenDiscriminants,
    ) -> Result<Option<Loc<SyntaxToken>>, Loc<ParseError>> {
        if let Some(Loc {
            inner: token,
            location,
        }) = self.bump()?
        {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == lookahead {
                Ok(Some(loc!(token, location)))
            } else {
                self.pushback(loc!(token, location));
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn match_multiple(
        &mut self,
        tokens: Vec<SyntaxToken>,
    ) -> Result<Option<Loc<SyntaxToken>>, Loc<ParseError>> {
        for token in tokens {
            if let Some(token) = self.match_one((&token).into())? {
                return Ok(Some(token));
            }
        }
        Ok(None)
    }

    fn bump(&mut self) -> Result<Option<Loc<SyntaxToken>>, Loc<ParseError>> {
        match self.pushedback_tokens.pop() {
            Some(tok) => Ok(Some(tok)),
            None => match self.lexer.next() {
                Some(tok) => Ok(Some(tok?)),
                None => Ok(None),
            },
        }
    }

    fn bump_or_err(
        &mut self,
        expected_tokens: Vec<TokenDiscriminants>,
    ) -> Result<Loc<SyntaxToken>, Loc<ParseError>> {
        self.bump()?.ok_or(loc!(
            ParseError::EndOfFile { expected_tokens },
            LocationRange(self.lexer.current_location, self.lexer.current_location)
        ))
    }

    #[allow(dead_code)]
    fn peek(&mut self) -> Result<(), Loc<ParseError>> {
        let tok = self.bump()?;
        if let Some(tok) = tok {
            println!(
                "TOKEN: {}",
                token_to_string(&self.lexer.name_table, &tok.inner)
            );
            self.pushback(tok);
        } else {
            println!("NO TOKEN");
        }
        Ok(())
    }

    // Pop tokens until we reach the end token. For example, when parsing a stmt
    // this is semicolon
    fn recover_from_error(&mut self, end_token: TokenDiscriminants) -> Result<(), Loc<ParseError>> {
        while let Some(Loc {
            inner: token,
            location: _,
        }) = self.bump()?
        {
            if end_token == token.into() {
                return Ok(());
            }
        }
        Ok(())
    }

    fn lookup_op_token(&mut self, span: Loc<SyntaxToken>) -> Result<Op, Loc<ParseError>> {
        match span.inner {
            SyntaxToken::EqualEqual => Ok(Op::EqualEqual),
            SyntaxToken::BangEqual => Ok(Op::BangEqual),
            SyntaxToken::Times => Ok(Op::Times),
            SyntaxToken::Div => Ok(Op::Div),
            SyntaxToken::Plus => Ok(Op::Plus),
            SyntaxToken::Minus => Ok(Op::Minus),
            SyntaxToken::Greater => Ok(Op::Greater),
            SyntaxToken::GreaterEqual => Ok(Op::GreaterEqual),
            SyntaxToken::Less => Ok(Op::Less),
            SyntaxToken::LessEqual => Ok(Op::LessEqual),
            SyntaxToken::AmpAmp => Ok(Op::LogicalAnd),
            SyntaxToken::PipePipe => Ok(Op::LogicalOr),
            _ => Err(loc!(ParseError::NotReachable, span.location)),
        }
    }

    pub fn program(&mut self) -> Result<Program, Loc<ParseError>> {
        let mut stmts = Vec::new();
        let mut type_defs = Vec::new();
        loop {
            if let Some(Loc {
                inner: _,
                location: left,
            }) = self.match_one(TokenDiscriminants::Struct)?
            {
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
                    Err(Loc {
                        location: _,
                        inner: ParseError::EndOfFile { expected_tokens: _ },
                    }) => {
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

    fn id(&mut self) -> Result<(Name, LocationRange), Loc<ParseError>> {
        match self.bump_or_err(vec![TokenDiscriminants::Ident])? {
            Loc {
                inner: SyntaxToken::Ident(id),
                location,
            } => Ok((id, location)),
            Loc {
                inner: token,
                location,
            } => Err(loc!(
                ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    expected_tokens: format!("{}", TokenDiscriminants::Ident),
                },
                location
            )),
        }
    }

    fn type_def(&mut self, left: LocationRange) -> Result<Loc<TypeDef>, Loc<ParseError>> {
        let (id, _) = self.id()?;
        self.expect(TokenDiscriminants::LBrace)?;
        let (fields, right) =
            self.comma::<(Name, Loc<TypeSig>)>(&Self::record_type_field, SyntaxToken::RBrace)?;
        Ok(Loc {
            location: LocationRange(left.0, right.1),
            inner: TypeDef::Struct(id, fields),
        })
    }

    fn record_type_field(&mut self) -> Result<(Name, Loc<TypeSig>), Loc<ParseError>> {
        let (id, _) = self.id()?;
        self.expect(TokenDiscriminants::Colon)?;
        let type_sig = self.type_()?;
        Ok((id, type_sig))
    }

    pub fn stmt(&mut self) -> Result<Option<Loc<Stmt>>, Loc<ParseError>> {
        let span = self.bump_or_err(vec![
            TokenDiscriminants::Let,
            TokenDiscriminants::Return,
            TokenDiscriminants::Export,
            TokenDiscriminants::Loop,
            TokenDiscriminants::Break,
        ])?;
        let location = span.location;
        let res = match span.inner {
            SyntaxToken::Break => {
                let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
                Ok(loc!(Stmt::Break, LocationRange(location.0, right.1)))
            }
            SyntaxToken::Let => self.let_stmt(location),
            SyntaxToken::Return => self.return_stmt(location),
            SyntaxToken::Export => self.export_stmt(location),
            SyntaxToken::If => {
                let if_expr = self.if_expr(location)?;
                let location = if_expr.location;
                Ok(loc!(Stmt::Expr(if_expr), location))
            }
            SyntaxToken::Loop => {
                let (_, left) = self.expect(TokenDiscriminants::LBrace)?;
                let block = self.expr_block(left)?;
                let location = LocationRange(left.0, block.location.1);
                Ok(loc!(Stmt::Loop(block), location))
            }
            token => {
                self.pushback(loc!(token, location));
                self.expression_stmt()
            }
        };
        match res {
            Ok(res) => Ok(Some(res)),
            Err(Loc {
                location,
                inner: ParseError::EndOfFile { expected_tokens },
            }) => Err(Loc {
                location,
                inner: ParseError::EndOfFile { expected_tokens },
            }),
            Err(err) => {
                // Special case if the unexpected token is a semicolon
                // since that's our recovery token
                if let ParseError::UnexpectedToken {
                    token: _,
                    token_type,
                    expected_tokens: _,
                } = &err.inner
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

    fn export_stmt(&mut self, left: LocationRange) -> Result<Loc<Stmt>, Loc<ParseError>> {
        let span = self.bump_or_err(vec![TokenDiscriminants::Ident])?;
        match span.inner {
            SyntaxToken::Ident(name) => {
                let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
                Ok(Loc {
                    location: LocationRange(left.0, right.1),
                    inner: Stmt::Export(name),
                })
            }
            token => {
                self.pushback(loc!(token.clone(), span.location));
                Err(loc!(
                    ParseError::UnexpectedToken {
                        token: token_to_string(&self.lexer.name_table, &token),
                        token_type: token.into(),
                        expected_tokens: format!("{}", TokenDiscriminants::Ident),
                    },
                    span.location
                ))
            }
        }
    }

    fn return_stmt(&mut self, left: LocationRange) -> Result<Loc<Stmt>, Loc<ParseError>> {
        let expr = self.expr()?;
        let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Loc {
            location: LocationRange(left.0, right.1),
            inner: Stmt::Return(expr),
        })
    }

    fn let_stmt(&mut self, left: LocationRange) -> Result<Loc<Stmt>, Loc<ParseError>> {
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
                Pat::Id(_, _, _) => Err(loc!(ParseError::FuncBindingTypeSig, location)),
                Pat::Record(_, _, location) | Pat::Empty(location) | Pat::Tuple(_, location) => {
                    Err(loc!(ParseError::DestructureFunction, location))
                }
            }
        } else {
            self.expect(TokenDiscriminants::Semicolon)?;
            let right = rhs_expr.location;
            Ok(loc!(
                Stmt::Let(pat, rhs_expr),
                LocationRange(left.0, right.1)
            ))
        }
    }

    fn expression_stmt(&mut self) -> Result<Loc<Stmt>, Loc<ParseError>> {
        let expr = self.expr()?;
        let (_, right) = self.expect(TokenDiscriminants::Semicolon)?;
        Ok(Loc {
            location: LocationRange(expr.location.0, right.1),
            inner: Stmt::Expr(expr),
        })
    }

    fn convert_params(
        &mut self,
        exprs: Vec<(Loc<Expr>, Option<Loc<TypeSig>>)>,
        left: LocationRange,
    ) -> Result<Pat, Loc<ParseError>> {
        let mut params = Vec::new();
        let mut params_location = left;
        for (expr, sig) in exprs {
            match &expr.inner {
                Expr::Var { name } => {
                    let loc = LocationRange(
                        expr.location.0,
                        sig.as_ref()
                            .map(|s| s.location.1)
                            .unwrap_or(expr.location.1),
                    );
                    params_location = LocationRange(params_location.0, loc.1);
                    params.push(Pat::Id(*name, sig, loc))
                }
                _ => return Err(loc!(ParseError::NotReachable, expr.location)),
            }
        }
        Ok(Pat::Tuple(params, params_location))
    }

    fn expr(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        if let Some(Loc {
            inner: token,
            location: left,
        }) = self.bump()?
        {
            match token {
                SyntaxToken::LBrace => self.expr_block(left),
                SyntaxToken::If => self.if_expr(left),
                SyntaxToken::Ident(id) => {
                    if self.match_one(TokenDiscriminants::LBrace)?.is_some() {
                        self.record_literal(id, left)
                    } else {
                        self.pushback(loc!(SyntaxToken::Ident(id), left));
                        self.asgn()
                    }
                }
                token => {
                    self.pushback(loc!(token, left));
                    self.asgn()
                }
            }
        } else {
            self.asgn()
        }
    }

    fn if_expr(&mut self, left: LocationRange) -> Result<Loc<Expr>, Loc<ParseError>> {
        // Yeah...I'm not allowing functions or blocks in the cond spot
        let cond = self.asgn();
        let cond = cond?;
        let (_, block_left) = self.expect(TokenDiscriminants::LBrace)?;
        let then_block = self.expr_block(block_left);
        let then_block = then_block?;
        let else_block = if let Some(Loc {
            inner: _,
            location: else_left,
        }) = self.match_one(TokenDiscriminants::Else)?
        {
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

    fn expr_block(&mut self, left: LocationRange) -> Result<Loc<Expr>, Loc<ParseError>> {
        let mut stmts = Vec::new();
        loop {
            if let Some(Loc {
                inner: _,
                location: right,
            }) = self.match_one(TokenDiscriminants::RBrace)?
            {
                return Ok(Loc {
                    location: LocationRange(left.0, right.1),
                    inner: Expr::Block(stmts, None),
                });
            }
            // If we're undeniably starting a statement then
            // parse it and push onto the vec
            if let Some(span) = self.match_multiple(vec![
                SyntaxToken::Let,
                SyntaxToken::Return,
                SyntaxToken::Export,
                SyntaxToken::Loop,
                SyntaxToken::Break,
            ])? {
                self.pushback(span);

                if let Some(stmt) = self.stmt()? {
                    stmts.push(stmt);
                }
            } else {
                // Otherwise we could either be in an expr stmt or an ending expr situation
                let expr = self.expr()?;
                if let Some(Loc {
                    inner: _,
                    location: right,
                }) = self.match_one(TokenDiscriminants::Semicolon)?
                {
                    stmts.push(Loc {
                        location: LocationRange(expr.location.0, right.1),
                        inner: Stmt::Expr(expr),
                    });
                } else if let Some(span) = self.match_one(TokenDiscriminants::RBrace)? {
                    return Ok(Loc {
                        location: LocationRange(left.0, span.location.1),
                        inner: Expr::Block(stmts, Some(Box::new(expr))),
                    });
                } else if matches!(expr.inner, Expr::If(_, _, _)) {
                    // If it's not the end of the block and we have an if expression,
                    // we don't need a semicolon
                    stmts.push(Loc {
                        location: expr.location,
                        inner: Stmt::Expr(expr),
                    });
                }
            }
        }
    }

    fn function_body(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let span = self.bump()?.ok_or(loc!(
            ParseError::EndOfFile {
                // TODO: Streamline error reporting. I should group the
                // tokens into ones expected for each kind of syntax rule.
                // For instance, these are a combo of the block lookahead
                // token, the expression grouping lookahead token and the
                // expression lookahead token
                expected_tokens: vec![
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                ],
            },
            LocationRange(self.lexer.current_location, self.lexer.current_location)
        ))?;
        match span.inner {
            SyntaxToken::LBrace => self.expr_block(span.location),
            SyntaxToken::LParen => {
                let mut expr = self.expr()?;
                let (_, right) = self.expect(TokenDiscriminants::RParen)?;
                expr.location = LocationRange(span.location.0, right.1);
                Ok(expr)
            }
            _ => {
                self.pushback(span);
                self.expr()
            }
        }
    }

    fn asgn(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let mut expr = self.logical()?;
        if self.match_one(TokenDiscriminants::Equal)?.is_some() {
            let rhs = self.expr()?;
            expr = Loc {
                location: LocationRange(expr.location.0, rhs.location.1),
                inner: Expr::Asgn {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                },
            }
        }
        Ok(expr)
    }

    fn logical(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let mut expr = self.equality()?;
        while let Some(span) = self.match_multiple(vec![SyntaxToken::AmpAmp, SyntaxToken::PipePipe])? {
            let op = self.lookup_op_token(span)?;
            let rhs = self.equality()?;
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

    fn equality(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let lhs = self.comparison()?;
        if let Some(span) = self.match_multiple(vec![SyntaxToken::EqualEqual, SyntaxToken::BangEqual])? {
            let op = self.lookup_op_token(span)?;
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

    fn comparison(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let lhs = self.addition()?;
        if let Some(span) = self.match_multiple(vec![
            SyntaxToken::GreaterEqual,
            SyntaxToken::Greater,
            SyntaxToken::Less,
            SyntaxToken::LessEqual,
        ])? {
            let op = self.lookup_op_token(span)?;
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

    fn addition(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let mut expr = self.multiplication()?;
        while let Some(span) = self.match_multiple(vec![SyntaxToken::Plus, SyntaxToken::Minus])? {
            let op = self.lookup_op_token(span)?;
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

    fn multiplication(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let mut expr = self.unary()?;
        while let Some(span) = self.match_multiple(vec![SyntaxToken::Times, SyntaxToken::Div])? {
            let op = self.lookup_op_token(span)?;
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

    fn unary(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        if let Some(Loc {
            inner: token,
            location: left,
        }) = self.match_multiple(vec![SyntaxToken::Bang, SyntaxToken::Minus])?
        {
            let op = match token {
                SyntaxToken::Bang => UnaryOp::Not,
                SyntaxToken::Minus => UnaryOp::Minus,
                _ => return Err(loc!(ParseError::NotReachable, left)),
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

    fn call(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let mut expr = self.primary()?;
        while let Some(span) = self.bump()? {
            match span.inner {
                SyntaxToken::LParen => {
                    expr = self.finish_call(expr, span.location)?;
                }
                SyntaxToken::LBracket => {
                    let index = self.expr()?;
                    let location = LocationRange(span.location.0, index.location.1);
                    self.expect(TokenDiscriminants::RBracket)?;
                    expr = loc!(
                        Expr::Index {
                            lhs: Box::new(expr),
                            index: Box::new(index)
                        },
                        location
                    )
                }
                SyntaxToken::Dot => {
                    let span = self.bump_or_err(vec![
                        TokenDiscriminants::Ident,
                        TokenDiscriminants::Integer,
                    ])?;
                    match span.inner {
                        SyntaxToken::Ident(name) => {
                            expr = Loc {
                                location: LocationRange(expr.location.0, span.location.1),
                                inner: Expr::Field(Box::new(expr), name),
                            };
                        }
                        SyntaxToken::Integer(index) => {
                            let left = expr.location.0;
                            expr = loc!(
                                Expr::TupleField(
                                    Box::new(expr),
                                    index.try_into().map_err(|_| loc!(
                                        ParseError::InvalidIndex {
                                            index: format!("{}", index)
                                        },
                                        span.location
                                    ))?
                                ),
                                LocationRange(left, span.location.1)
                            );
                        }
                        SyntaxToken::Float(f_str) => {
                            let left = expr.location.0;
                            let fields: Vec<&str> = f_str.split('.').collect();
                            let first_index: u32 = fields[0].parse().expect("Invalid u32");
                            let second_index: u32 = fields[1].parse().expect("Invalid u32");
                            expr = loc!(
                                Expr::TupleField(Box::new(expr), first_index),
                                LocationRange(left, span.location.1)
                            );
                            expr = loc!(
                                Expr::TupleField(Box::new(expr), second_index),
                                LocationRange(left, span.location.1)
                            );
                        }
                        token => {
                            return Err(loc!(
                                ParseError::UnexpectedToken {
                                    token: token_to_string(&self.lexer.name_table, &token),
                                    token_type: token.into(),

                                    expected_tokens: format!("{}", TokenDiscriminants::Ident),
                                },
                                span.location
                            ))
                        }
                    };
                }
                token => {
                    self.pushback(loc!(token, span.location));
                    break;
                }
            }
        }
        Ok(expr)
    }

    fn finish_call(
        &mut self,
        callee: Loc<Expr>,
        left: LocationRange,
    ) -> Result<Loc<Expr>, Loc<ParseError>> {
        let args = {
            let (mut exprs, right) = self.comma::<Loc<Expr>>(&Self::expr, SyntaxToken::RParen)?;
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

    fn primary(&mut self) -> Result<Loc<Expr>, Loc<ParseError>> {
        let span = self.bump()?.ok_or(loc!(
            ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                ],
            },
            LocationRange(self.lexer.current_location, self.lexer.current_location)
        ))?;
        let location = span.location;
        match span.inner {
            SyntaxToken::True => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Bool(true),
                },
            }),
            SyntaxToken::False => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Bool(false),
                },
            }),
            SyntaxToken::Integer(int) => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Integer(int),
                },
            }),
            SyntaxToken::Float(float) => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::Float(
                        float
                            .parse()
                            .map_err(|_| loc!(ParseError::InvalidFloat { float }, location))?,
                    ),
                },
            }),
            SyntaxToken::String(s) => Ok(Loc {
                location,
                inner: Expr::Primary {
                    value: Value::String(s),
                },
            }),
            SyntaxToken::Ident(id) => {
                let span = self.bump_or_err(vec![
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                ])?;
                match span.inner {
                    SyntaxToken::FatArrow => {
                        let body = self.function_body()?;
                        let location = LocationRange(location.0, body.location.1);
                        Ok(loc!(
                            Expr::Function {
                                params: Pat::Id(id, None, location),
                                return_type: None,
                                body: Box::new(body)
                            },
                            location
                        ))
                    }
                    token => {
                        self.pushback(loc!(token, span.location));
                        Ok(loc!(Expr::Var { name: id }, location))
                    }
                }
            }
            SyntaxToken::LParen => {
                let mut exprs = Vec::new();
                let mut right = location.1;
                let mut is_params = true;
                loop {
                    if self.match_one(TokenDiscriminants::RParen)?.is_some() {
                        break;
                    }
                    let expr = self.expr()?;
                    match &expr.inner {
                        Expr::Var { name: _ } => {}
                        _ => {
                            is_params = false;
                        }
                    }
                    right = expr.location.1;
                    exprs.push((expr, self.type_sig()?));
                    if self.match_one(TokenDiscriminants::RParen)?.is_some() {
                        break;
                    }
                    self.expect(TokenDiscriminants::Comma)?;
                }
                if is_params {
                    let return_type = self.type_sig()?;
                    if self.match_one(TokenDiscriminants::FatArrow)?.is_some() {
                        let body = self.function_body()?;
                        let location = LocationRange(location.0, body.location.1);
                        let params = self.convert_params(exprs, location)?;
                        return Ok(loc!(
                            Expr::Function {
                                params,
                                return_type,
                                body: Box::new(body)
                            },
                            location
                        ));
                    }
                    if let Some(return_type) = return_type {
                        return Err(loc!(ParseError::TupleTypeSig, return_type.location));
                    }
                }
                let mut entries: Vec<Loc<Expr>> = exprs.into_iter().map(|(e, _)| e).collect();
                if entries.len() == 1 {
                    Ok(entries.pop().unwrap())
                } else {
                    Ok(loc!(Expr::Tuple(entries), LocationRange(location.0, right)))
                }
            }
            SyntaxToken::LBracket => {
                let (entries, right) = self.comma(&Self::expr, SyntaxToken::RBracket)?;
                Ok(loc!(
                    Expr::Array(entries),
                    LocationRange(location.0, right.1)
                ))
            }
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
                Err(loc!(
                    ParseError::UnexpectedToken {
                        token: token_to_string(&self.lexer.name_table, &token),
                        token_type: token.into(),
                        expected_tokens,
                    },
                    location
                ))
            }
        }
    }

    fn record_literal(
        &mut self,
        name: Name,
        name_loc: LocationRange,
    ) -> Result<Loc<Expr>, Loc<ParseError>> {
        let (fields, end_loc) =
            self.comma::<(Name, Loc<Expr>)>(&Self::record_field, SyntaxToken::RBrace)?;
        Ok(Loc {
            location: LocationRange(name_loc.0, end_loc.1),
            inner: Expr::Record { name, fields },
        })
    }

    fn record_field(&mut self) -> Result<(Name, Loc<Expr>), Loc<ParseError>> {
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

    fn pattern(&mut self) -> Result<Pat, Loc<ParseError>> {
        let span = self.bump()?.ok_or(loc!(
            ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::Ident,
                ],
            },
            LocationRange(self.lexer.current_location, self.lexer.current_location)
        ))?;
        let left = span.location;
        match span.inner {
            SyntaxToken::LParen => {
                if let Some(Loc {
                    inner: _,
                    location: right,
                }) = self.match_one(TokenDiscriminants::RParen)?
                {
                    return Ok(Pat::Empty(LocationRange(left.0, right.0)));
                }

                let (mut pats, right) = self.comma::<Pat>(&Self::pattern, SyntaxToken::RParen)?;
                // If the pattern is singular, i.e. let (a) = 10, then
                // we treat it as a single id
                if pats.len() == 1 {
                    Ok(pats.pop().unwrap())
                } else {
                    Ok(Pat::Tuple(pats, LocationRange(left.0, right.1)))
                }
            }
            SyntaxToken::LBrace => {
                let (fields, right) = self.comma::<Name>(&Self::record_pattern, SyntaxToken::RBrace)?;
                if let Some(type_sig) = self.type_sig()? {
                    Ok(Pat::Record(
                        fields,
                        Some(type_sig),
                        LocationRange(left.0, right.1),
                    ))
                } else {
                    Ok(Pat::Record(fields, None, LocationRange(left.0, right.1)))
                }
            }
            SyntaxToken::Ident(name) => {
                if let Some(type_sig) = self.type_sig()? {
                    let loc = LocationRange(left.0, type_sig.location.1);
                    Ok(Pat::Id(name, Some(type_sig), loc))
                } else {
                    Ok(Pat::Id(name, None, left))
                }
            }
            token => {
                return Err(loc!(
                    ParseError::UnexpectedToken {
                        token: token_to_string(&self.lexer.name_table, &token),
                        token_type: token.into(),
                        expected_tokens: format!(
                            "{}, {}, {}",
                            TokenDiscriminants::LParen,
                            TokenDiscriminants::LBrace,
                            TokenDiscriminants::Ident,
                        ),
                    },
                    left
                ))
            }
        }
    }

    fn record_pattern(&mut self) -> Result<Name, Loc<ParseError>> {
        let span = self.bump()?.ok_or(loc!(
            ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            },
            LocationRange(self.lexer.current_location, self.lexer.current_location)
        ))?;
        match span.inner {
            SyntaxToken::Ident(name) => Ok(name),
            token => Err(loc!(
                ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    expected_tokens: format!("{}", TokenDiscriminants::Ident),
                },
                span.location
            )),
        }
    }

    fn type_sig(&mut self) -> Result<Option<Loc<TypeSig>>, Loc<ParseError>> {
        if self.match_one(TokenDiscriminants::Colon)?.is_some() {
            let type_sig = self.type_()?;
            Ok(Some(type_sig))
        } else {
            Ok(None)
        }
    }

    fn type_(&mut self) -> Result<Loc<TypeSig>, Loc<ParseError>> {
        let token = self.bump()?.ok_or(loc!(
            ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::LBracket, TokenDiscriminants::Ident],
            },
            LocationRange(self.lexer.current_location, self.lexer.current_location)
        ))?;
        let location = token.location;
        match token.inner {
            SyntaxToken::Ident(name) => {
                let mut sig = Loc {
                    location,
                    inner: TypeSig::Name(name),
                };
                while self.match_one(TokenDiscriminants::LBracket)?.is_some() {
                    let (_, right) = self.expect(TokenDiscriminants::RBracket)?;
                    sig = loc!(
                        TypeSig::Array(Box::new(sig)),
                        LocationRange(location.0, right.1)
                    );
                }
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
            SyntaxToken::LParen => {
                let (param_types, _) = self.comma(&Self::type_, SyntaxToken::RParen)?;
                self.expect(TokenDiscriminants::Arrow)?;
                let return_type = self.type_()?;
                Ok(Loc {
                    location: LocationRange(location.0, return_type.location.1),
                    inner: TypeSig::Arrow(param_types, Box::new(return_type)),
                })
            }
            token => Err(loc!(
                ParseError::UnexpectedToken {
                    token: token_to_string(&self.lexer.name_table, &token),
                    token_type: token.into(),
                    expected_tokens: format!(
                        "{}, {}",
                        TokenDiscriminants::LBracket,
                        TokenDiscriminants::Ident
                    ),
                },
                location
            )),
        }
    }

    fn comma<T: Debug>(
        &mut self,
        parse_fn: &dyn Fn(&mut Self) -> Result<T, Loc<ParseError>>,
        end_token: SyntaxToken,
    ) -> Result<(Vec<T>, LocationRange), Loc<ParseError>> {
        let mut elems: Vec<T> = Vec::new();
        if let Some(Loc {
            inner: _,
            location: right,
        }) = self.match_one((&end_token).into())?
        {
            return Ok((elems, right));
        }
        loop {
            elems.push(parse_fn(self)?);
            if let Some(Loc {
                inner: _,
                location: right,
            }) = self.match_one((&end_token).into())?
            {
                return Ok((elems, right));
            }
            self.expect(TokenDiscriminants::Comma)?;
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::ast::{Expr, Loc, Op, Pat, Stmt, TypeSig, Value};
//     use crate::lexer::{Lexer, LocationRange};
//     use crate::parser::{ParseError, Parser};
//     use anyhow::Result;
//     use std::ffi::OsStr;
//     use std::fs;
//     use std::fs::File;
//     use std::io::Write;
//     use std::path::PathBuf;
//
//     #[test]
//     #[ignore]
//     fn generate_baseline() -> anyhow::Result<()> {
//         for entry in fs::read_dir("tests/parser")? {
//             let entry = &entry?.path();
//             if entry.extension() == Some(OsStr::new("sbr")) {
//                 let source = fs::read_to_string(entry)?;
//                 let lexer = Lexer::new(&source);
//                 let mut parser = Parser::new(lexer);
//                 let output = match parser.program() {
//                     Ok(stmts) => serde_json::to_string_pretty(&stmts)?,
//                     Err(err) => err.to_string(),
//                 };
//                 let mut out_path = PathBuf::new();
//                 out_path.push("tests/parser/");
//                 out_path.push(entry.file_stem().unwrap());
//                 out_path.set_extension("json");
//                 let mut out_file = File::create(out_path)?;
//                 out_file.write_all(output.as_bytes())?;
//             }
//         }
//         Ok(())
//     }
//
//     #[test]
//     fn literal() -> Result<()> {
//         let expected = vec![
//             Loc {
//                 location: LocationRange(Location(1, 1), Location(1, 3)),
//                 inner: Expr::Primary {
//                     value: Value::Integer(10),
//                 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 4), Location(1, 8)),
//                 inner: Expr::Primary {
//                     value: Value::Float(10.2),
//                 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 9), Location(1, 13)),
//                 inner: Expr::Primary {
//                     value: Value::Bool(true),
//                 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 14), Location(1, 19)),
//                 inner: Expr::Primary {
//                     value: Value::Bool(false),
//                 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 20), Location(1, 27)),
//                 inner: Expr::Primary {
//                     value: Value::String("hello".into()),
//                 },
//             },
//         ];
//         let source = "10 10.2 true false \"hello\"";
//         let lexer = Lexer::new(&source);
//         let mut parser = Parser::new(lexer);
//         for i in 0..5 {
//             assert_eq!(expected[i], parser.primary()?)
//         }
//         Ok(())
//     }
//
//     #[test]
//     fn id() -> Result<(), Loc<ParseError>> {
//         let expected = vec![
//             Loc {
//                 location: LocationRange(Location(1, 1), Location(1, 4)),
//                 inner: Expr::Var { name: 0 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 5), Location(1, 8)),
//                 inner: Expr::Var { name: 1 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 9), Location(1, 12)),
//                 inner: Expr::Var { name: 1 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 13), Location(1, 16)),
//                 inner: Expr::Var { name: 2 },
//             },
//             Loc {
//                 location: LocationRange(Location(1, 17), Location(1, 20)),
//                 inner: Expr::Var { name: 3 },
//             },
//         ];
//         let source = "foo bar bar baz bat";
//         let lexer = Lexer::new(&source);
//         let mut parser = Parser::new(lexer);
//         for i in 0..5 {
//             assert_eq!(expected[i], parser.primary()?);
//         }
//         assert_eq!("foo", parser.lexer.name_table.get_str(&0));
//         assert_eq!("bar", parser.lexer.name_table.get_str(&1));
//         assert_eq!("baz", parser.lexer.name_table.get_str(&2));
//         assert_eq!("bat", parser.lexer.name_table.get_str(&3));
//         Ok(())
//     }
//
//     #[test]
//     fn pattern() -> Result<(), Loc<ParseError>> {
//         let expected = vec![
//             Pat::Id(0, None, LocationRange(Location(1, 1), Location(1, 4))),
//             Pat::Id(
//                 1,
//                 Some(TypeSig::Name(2)),
//                 LocationRange(Location(1, 5), Location(1, 13)),
//             ),
//             Pat::Tuple(
//                 vec![
//                     Pat::Id(0, None, LocationRange(Location(1, 15), Location(1, 18))),
//                     Pat::Id(1, None, LocationRange(Location(1, 20), Location(1, 23))),
//                 ],
//                 LocationRange(Location(1, 14), Location(1, 24)),
//             ),
//             Pat::Record(
//                 vec![0, 1, 2],
//                 Some(TypeSig::Name(3)),
//                 LocationRange(Location(1, 26), Location(1, 46)),
//             ),
//         ];
//         let source = "foo bar: int (foo, bar) { foo, bar, baz }: A";
//         let lexer = Lexer::new(&source);
//         let mut parser = Parser::new(lexer);
//         for i in 0..3 {
//             assert_eq!(expected[i], parser.pattern()?);
//         }
//         assert_eq!("foo", parser.lexer.name_table.get_str(&0));
//         assert_eq!("bar", parser.lexer.name_table.get_str(&1));
//         Ok(())
//     }
//
//     #[test]
//     fn arithmetic() -> Result<(), Loc<ParseError>> {
//         let expected = Loc {
//             location: LocationRange(Location(1, 1), Location(1, 16)),
//             inner: Expr::BinOp {
//                 op: Op::Plus,
//                 lhs: Box::new(Loc {
//                     location: LocationRange(Location(1, 1), Location(1, 7)),
//                     inner: Expr::BinOp {
//                         op: Op::Times,
//                         lhs: Box::new(Loc {
//                             location: LocationRange(Location(1, 1), Location(1, 3)),
//                             inner: Expr::Primary {
//                                 value: Value::Integer(10),
//                             },
//                         }),
//                         rhs: Box::new(Loc {
//                             location: LocationRange(Location(1, 6), Location(1, 7)),
//                             inner: Expr::Primary {
//                                 value: Value::Integer(2),
//                             },
//                         }),
//                     },
//                 }),
//                 rhs: Box::new(Loc {
//                     location: LocationRange(Location(1, 10), Location(1, 16)),
//                     inner: Expr::BinOp {
//                         op: Op::Div,
//                         lhs: Box::new(Loc {
//                             location: LocationRange(Location(1, 10), Location(1, 11)),
//                             inner: Expr::Primary {
//                                 value: Value::Integer(3),
//                             },
//                         }),
//                         rhs: Box::new(Loc {
//                             location: LocationRange(Location(1, 14), Location(1, 16)),
//                             inner: Expr::UnaryOp {
//                                 op: Op::Minus,
//                                 rhs: Box::new(Loc {
//                                     location: LocationRange(Location(1, 15), Location(1, 16)),
//                                     inner: Expr::Primary {
//                                         value: Value::Integer(4),
//                                     },
//                                 }),
//                             },
//                         }),
//                     },
//                 }),
//             },
//         };
//         let source = "10 * 2 + 3 / -4";
//         let lexer = Lexer::new(&source);
//         let mut parser = Parser::new(lexer);
//         assert_eq!(expected, parser.expr()?);
//         Ok(())
//     }
//
//     #[test]
//     fn function() -> Result<(), Loc<ParseError>> {
//         let expected = Loc {
//             location: LocationRange(Location(1, 1), Location(1, 12)),
//             inner: Expr::Function {
//                 params: Pat::Id(0, None, LocationRange(Location(1, 2), Location(1, 3))),
//                 return_type: None,
//                 body: Box::new(Loc {
//                     location: LocationRange(Location(1, 7), Location(1, 12)),
//                     inner: Stmt::Return(Loc {
//                         location: LocationRange(Location(1, 7), Location(1, 12)),
//                         inner: Expr::BinOp {
//                             op: Op::Plus,
//                             lhs: Box::new(Loc {
//                                 location: LocationRange(Location(1, 7), Location(1, 8)),
//                                 inner: Expr::Var { name: 0 },
//                             }),
//                             rhs: Box::new(Loc {
//                                 location: LocationRange(Location(1, 11), Location(1, 12)),
//                                 inner: Expr::Primary {
//                                     value: Value::Integer(1),
//                                 },
//                             }),
//                         },
//                     }),
//                 }),
//             },
//         };
//         let source = "\\a => a + 1";
//         let lexer = Lexer::new(&source);
//         let mut parser = Parser::new(lexer);
//         assert_eq!(expected, parser.expr()?);
//         Ok(())
//     }
// }
