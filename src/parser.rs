use crate::ast::{Expr, Name, Op, Pat, Stmt, TypeSig, Value};
use crate::lexer::{Lexer, LexicalError, LocationRange, Token, TokenDiscriminants};
use std::fmt::Debug;
use utils::NameTable;

pub struct Parser<'input> {
    pub lexer: Lexer<'input>,
    pushedback_tokens: Vec<(Token, LocationRange)>,
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
        location: LocationRange,
    },
    #[fail(display = "Cannot destructure a function")]
    DestructureFunction,
    #[fail(
        display = "Cannot have a type signature on a let function binding (use type signatures in the function!)"
    )]
    FuncBindingTypeSig,
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
                    token,
                    location,
                    expected_tokens: vec![expected],
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

    fn match_one(&mut self, lookahead: TokenDiscriminants) -> Result<Option<Token>, ParseError> {
        if let Some((token, location)) = self.bump()? {
            let token_discriminant: TokenDiscriminants = (&token).into();
            if token_discriminant == lookahead {
                Ok(Some(token))
            } else {
                self.pushback((token, location));
                Ok(None)
            }
        } else {
            Err(ParseError::EndOfFile {
                expected_tokens: vec![lookahead],
            })
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

    fn bump(&mut self) -> Result<Option<(Token, LocationRange)>, ParseError> {
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
            _ => Err(ParseError::NotReachable),
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

    pub fn stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((Token::Let, _)) => self.let_stmt(),
            Some((Token::Return, _)) => Ok(self.return_stmt()?),
            Some((Token::Export, _)) => Ok(self.export_stmt()?),
            Some((Token::If, _)) => Ok(self.if_stmt()?),
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
            }),
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let cond = self.expr()?;
        self.expect(TokenDiscriminants::LBrace)?;
        let result_block = self.block()?;
        let else_block = if self.match_one(TokenDiscriminants::Else)?.is_some() {
            if self.match_one(TokenDiscriminants::If)?.is_some() {
                Some(self.if_stmt()?)
            } else {
                self.expect(TokenDiscriminants::LBrace)?;
                Some(self.block()?)
            }
        } else {
            None
        };
        Ok(Stmt::If(
            cond,
            Box::new(result_block),
            else_block.map(Box::new),
        ))
    }

    fn export_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((Token::Ident(name), _)) => {
                self.expect(TokenDiscriminants::Semicolon)?;
                Ok(Stmt::Export(name))
            }
            Some((token, location)) => {
                self.pushback((token.clone(), location.clone()));
                Err(ParseError::UnexpectedToken {
                    token,
                    expected_tokens: vec![TokenDiscriminants::Ident],
                    location,
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
        if let Expr::Function {
            params,
            return_type,
            body,
        } = rhs_expr
        {
            match pat {
                Pat::Id(name, None) => Ok(Stmt::Function(name, params, return_type, body)),
                Pat::Id(_, _) => Err(ParseError::FuncBindingTypeSig),
                _ => Err(ParseError::DestructureFunction),
            }
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
        if self.match_one(TokenDiscriminants::Slash)?.is_some() {
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
            Some((Token::LBrace, _)) => self.block()?,
            Some((Token::LParen, _)) => {
                let expr = self.expr()?;
                self.expect(TokenDiscriminants::RParen)?;
                Stmt::Return(expr)
            }
            Some((token, location)) => {
                self.pushback((token, location));
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
                });
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
            if self.match_one(TokenDiscriminants::LParen)?.is_some() {
                expr = self.finish_call(expr)?;
            } else if self.match_one(TokenDiscriminants::Dot)?.is_some() {
                let tok = self.bump()?;
                if let Some((Token::Ident(name), _)) = tok {
                    expr = Expr::Field(Box::new(expr), name);
                } else if let Some(tok) = tok {
                    self.pushback(tok)
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let args = {
            let mut exprs = self.comma::<Expr>(&Self::expr, Token::RParen)?;
            if exprs.len() == 1 {
                exprs.remove(0)
            } else {
                Expr::Tuple(exprs)
            }
        };
        Ok(Expr::Call {
            callee: Box::new(callee),
            args: Box::new(args),
        })
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.bump()?;
        match token {
            Some((Token::True, _)) => Ok(Expr::Primary {
                value: Value::Bool(true),
            }),
            Some((Token::False, _)) => Ok(Expr::Primary {
                value: Value::Bool(false),
            }),
            Some((Token::Integer(int), _)) => Ok(Expr::Primary {
                value: Value::Integer(int),
            }),
            Some((Token::Float(float), _)) => Ok(Expr::Primary {
                value: Value::Float(float),
            }),
            Some((Token::String(s), _)) => Ok(Expr::Primary {
                value: Value::String(s),
            }),
            Some((Token::LParenBrace, _)) => self.record_literal(),
            // Parsing tuple or grouping
            Some((Token::LParen, _)) => {
                let expr = self.expr()?;
                if self.match_one(TokenDiscriminants::Comma)?.is_some() {
                    let mut elems = vec![expr];
                    let mut rest = self.comma::<Expr>(&Self::expr, Token::RParen)?;
                    elems.append(&mut rest);
                    Ok(Expr::Tuple(elems))
                } else {
                    self.expect(TokenDiscriminants::RParen)?;
                    Ok(expr)
                }
            }
            Some((Token::Ident(name), _)) => Ok(Expr::Var { name }),
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                token,
                location,
                expected_tokens: vec![
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                ],
            }),
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::True,
                    TokenDiscriminants::False,
                    TokenDiscriminants::Integer,
                    TokenDiscriminants::Float,
                    TokenDiscriminants::String,
                    TokenDiscriminants::LParen,
                ],
            }),
        }
    }

    fn record_literal(&mut self) -> Result<Expr, ParseError> {
        let mut entries = Vec::new();
        loop {
            match self.bump()? {
                Some((Token::RParenBrace, _)) => {
                    return Ok(Expr::Record { entries });
                }
                Some((Token::Ident(name), _)) => {
                    let field_val = self.expr()?;
                    entries.push((name, field_val));
                }
                Some((token, location)) => {
                    return Err(ParseError::UnexpectedToken {
                        token,
                        location,
                        expected_tokens: vec![
                            TokenDiscriminants::RParenBrace,
                            TokenDiscriminants::Ident,
                        ],
                    })
                }
                None => {
                    return Err(ParseError::EndOfFile {
                        expected_tokens: vec![
                            TokenDiscriminants::RParenBrace,
                            TokenDiscriminants::Ident,
                        ],
                    })
                }
            }
        }
    }

    fn pattern(&mut self) -> Result<Pat, ParseError> {
        let tok = self.bump()?;
        match tok {
            Some((Token::LParen, _)) => {
                if self.match_one(TokenDiscriminants::RParen)?.is_some() {
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
            Some((Token::LBrace, _)) => {
                let fields = self.comma::<Name>(&Self::record_pattern, Token::RBrace)?;
                let type_sig = self.type_sig()?;
                Ok(Pat::Record(fields, type_sig))
            }
            Some((Token::Ident(name), _)) => {
                let type_sig = self.type_sig()?;
                Ok(Pat::Id(name, type_sig))
            }
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                token,
                location,
                expected_tokens: vec![
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::Ident,
                ],
            }),
            None => Err(ParseError::EndOfFile {
                expected_tokens: vec![
                    TokenDiscriminants::LParen,
                    TokenDiscriminants::LBrace,
                    TokenDiscriminants::Ident,
                ],
            }),
        }
    }

    fn record_pattern(&mut self) -> Result<Name, ParseError> {
        let token = self.bump()?;
        match token {
            Some((Token::Ident(name), _)) => Ok(name),
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                token,
                location,
                expected_tokens: vec![TokenDiscriminants::Ident],
            }),
            _ => Err(ParseError::EndOfFile {
                expected_tokens: vec![TokenDiscriminants::Ident],
            }),
        }
    }

    fn type_sig(&mut self) -> Result<Option<TypeSig>, ParseError> {
        if self.match_one(TokenDiscriminants::Colon)?.is_some() {
            Ok(Some(self.type_()?))
        } else {
            Ok(None)
        }
    }

    fn type_(&mut self) -> Result<TypeSig, ParseError> {
        let token = self.bump()?;
        match token {
            Some((Token::Ident(name), _)) => Ok(TypeSig::Name(name)),
            Some((Token::LBracket, _)) => {
                let array_type = self.type_()?;
                self.expect(TokenDiscriminants::RBracket)?;
                Ok(TypeSig::Array(Box::new(array_type)))
            }
            Some((token, location)) => Err(ParseError::UnexpectedToken {
                token,
                location,
                expected_tokens: vec![TokenDiscriminants::LBracket, TokenDiscriminants::Ident],
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
    ) -> Result<Vec<T>, ParseError> {
        let mut parsed: Vec<T> = Vec::new();
        if self.match_one((&end_token).into())?.is_some() {
            return Ok(parsed);
        }
        loop {
            parsed.push(parse_fn(self)?);
            if self.match_one((&end_token).into())?.is_some() {
                return Ok(parsed);
            }
            self.expect(TokenDiscriminants::Comma)?;
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expr, Pat, TypeSig, Value};
    use lexer::Lexer;
    use parser::{ParseError, Parser};

    #[test]
    fn literal() -> Result<(), ParseError> {
        let expected = vec![
            Expr::Primary {
                value: Value::Integer(10),
            },
            Expr::Primary {
                value: Value::Float(10.2),
            },
            Expr::Primary {
                value: Value::Bool(true),
            },
            Expr::Primary {
                value: Value::Bool(false),
            },
            Expr::Primary {
                value: Value::String("hello".into()),
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
            Expr::Var { name: 0 },
            Expr::Var { name: 1 },
            Expr::Var { name: 1 },
            Expr::Var { name: 2 },
            Expr::Var { name: 3 },
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
    fn pat() -> Result<(), ParseError> {
        let expected = vec![
            Pat::Id(0, None),
            Pat::Id(1, Some(TypeSig::Name(2))),
            Pat::Tuple(vec![Pat::Id(0, None), Pat::Id(1, None)]),
            Pat::Record(vec![0, 1, 2], Some(TypeSig::Name(3))),
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
}
