use crate::ast::Loc;
use crate::loc;
use crate::utils::NameTable;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Display, Formatter};
use std::str::CharIndices;

#[derive(Clone, Debug, PartialEq, EnumDiscriminants, Serialize, Deserialize)]
#[strum_discriminants(derive(Serialize, Deserialize))]
pub enum Token {
    False,
    True,
    Else,
    Export,
    For,
    If,
    Return,
    Struct,
    Let,
    While,
    Ident(usize),
    Float(f32),
    Integer(i32),
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Semicolon,
    Colon,
    Comma,
    Dot,
    Amp,
    AmpAmp,
    Pipe,
    PipePipe,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Div,
    DivEqual,
    Times,
    TimesEqual,
    Arrow,
    FatArrow,
    Slash,
    String(String),
}

impl Display for TokenDiscriminants {
    fn fmt<'a>(&self, f: &'a mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenDiscriminants::False => "false",
                TokenDiscriminants::True => "true",
                TokenDiscriminants::Else => "else",
                TokenDiscriminants::Export => "export",
                TokenDiscriminants::For => "for",
                TokenDiscriminants::If => "if",
                TokenDiscriminants::Return => "return",
                TokenDiscriminants::Struct => "struct",
                TokenDiscriminants::Let => "let",
                TokenDiscriminants::While => "while",
                TokenDiscriminants::Ident => "identifier",
                TokenDiscriminants::Float => "float",
                TokenDiscriminants::Integer => "int",
                TokenDiscriminants::LBrace => "{",
                TokenDiscriminants::RBrace => "}",
                TokenDiscriminants::LBracket => "[",
                TokenDiscriminants::RBracket => "]",
                TokenDiscriminants::LParen => "(",
                TokenDiscriminants::RParen => ")",
                TokenDiscriminants::Semicolon => ";",
                TokenDiscriminants::Colon => ":",
                TokenDiscriminants::Comma => ",",
                TokenDiscriminants::Dot => ".",
                TokenDiscriminants::Amp => "&",
                TokenDiscriminants::AmpAmp => "&&",
                TokenDiscriminants::Pipe => "|",
                TokenDiscriminants::PipePipe => "||",
                TokenDiscriminants::Greater => ">",
                TokenDiscriminants::GreaterEqual => ">=",
                TokenDiscriminants::Less => "<",
                TokenDiscriminants::LessEqual => "<=",
                TokenDiscriminants::Bang => "!",
                TokenDiscriminants::BangEqual => "!=",
                TokenDiscriminants::Equal => "=",
                TokenDiscriminants::EqualEqual => "==",
                TokenDiscriminants::Plus => "+",
                TokenDiscriminants::PlusEqual => "+=",
                TokenDiscriminants::Minus => "-",
                TokenDiscriminants::MinusEqual => "-=",
                TokenDiscriminants::Div => "/",
                TokenDiscriminants::DivEqual => "/=",
                TokenDiscriminants::Times => "*",
                TokenDiscriminants::TimesEqual => "*=",
                TokenDiscriminants::FatArrow => "=>",
                TokenDiscriminants::Arrow => "->",
                TokenDiscriminants::Slash => "\\",
                TokenDiscriminants::String => "string",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Deserialize, Serialize)]
pub struct LocationRange(pub usize, pub usize);

impl Display for LocationRange {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let l1 = &self.0;
        let l2 = &self.1;
        write!(f, "({}---{})", l1, l2)
    }
}

impl Debug for LocationRange {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let l1 = &self.0;
        let l2 = &self.1;
        write!(f, "({}---{})", l1, l2)
    }
}

impl Into<std::ops::Range<usize>> for LocationRange {
    fn into(self) -> std::ops::Range<usize> {
        std::ops::Range {
            start: self.0,
            end: self.1,
        }
    }
}

#[inline]
fn is_id_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[inline]
fn is_id_body(ch: char) -> bool {
    ch == '_' || ch.is_ascii_digit() || ch.is_ascii_alphabetic()
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum LexicalError {
    InvalidCharacter { ch: char },
    UnterminatedString,
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            LexicalError::InvalidCharacter { ch } => write!(f, "Invalid character '{}'", ch),
            LexicalError::UnterminatedString => write!(f, "String was not terminated"),
        }
    }
}

pub struct Lexer<'input> {
    source: &'input str,
    chars: CharIndices<'input>,
    pub name_table: NameTable,
    pub current_location: usize,
    lookahead: Option<(usize, char)>,
    lookahead2: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Lexer<'input> {
        let mut chars = source.char_indices();
        let lookahead = chars.next();
        let lookahead2 = chars.next();

        Lexer {
            source,
            chars,
            name_table: NameTable::new(),
            current_location: 0,
            lookahead,
            lookahead2,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let next = self.lookahead;
        self.lookahead = self.lookahead2;
        self.lookahead2 = self.chars.next();
        self.current_location += 1;
        next
    }

    #[allow(dead_code)]
    fn peek(&self) {
        println!("{:?}", self.lookahead);
    }

    fn lookahead_match(
        &mut self,
        start_loc: usize,
        matched_token: Token,
        alt_token: Token,
        match_ch: char,
    ) -> <Lexer<'input> as Iterator>::Item {
        match self.lookahead {
            Some((i, ch)) => {
                if match_ch == ch {
                    self.bump();
                    Ok(loc!(matched_token, LocationRange(start_loc, i)))
                } else {
                    Ok(loc!(alt_token, LocationRange(start_loc, i - 1)))
                }
            }
            None => Ok(loc!(alt_token, LocationRange(start_loc, start_loc))),
        }
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Option<usize>
    where
        F: FnMut(char) -> bool,
    {
        while let Some((i, ch)) = self.lookahead {
            if terminate(ch) {
                return Some(i);
            } else {
                self.bump();
            }
        }
        None
    }

    fn take_while<F>(&mut self, mut condition: F) -> Option<usize>
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(|ch| !condition(ch))
    }

    fn skip_to_line_end(&mut self) {
        self.take_while(|ch| ch != '\n');
    }

    fn skip_whitespace(&mut self) {
        self.take_while(|ch| ch.is_whitespace());
    }

    fn read_string(&mut self, start_index: usize) -> <Lexer<'input> as Iterator>::Item {
        match self.take_until(|ch| ch == '"') {
            Some(i) => {
                self.bump();
                let end_index = self.current_location;
                Ok(loc!(
                    Token::String(self.source[start_index + 1..i].to_string()),
                    LocationRange(start_index, end_index),
                ))
            }
            None => Err(loc!(
                LexicalError::UnterminatedString,
                LocationRange(start_index, self.current_location)
            )),
        }
    }

    fn read_number(&mut self, start_index: usize) -> <Lexer<'input> as Iterator>::Item {
        let mut end_index = self.take_while(|ch| ch.is_ascii_digit());
        let mut is_decimal = false;

        if let Some((_, '.')) = self.lookahead {
            // Check if it's a decimal or a field access
            if let Some((_, next_ch)) = self.lookahead2 {
                if next_ch.is_ascii_digit() {
                    is_decimal = true;
                    self.bump();
                    end_index = self.take_while(|ch| ch.is_ascii_digit());
                }
            }
        }
        let end_index = end_index.unwrap_or_else(|| self.source.len());
        if is_decimal {
            Ok(loc!(
                Token::Float(
                    self.source[start_index..end_index]
                        .parse()
                        .expect("unparseable number"),
                ),
                LocationRange(start_index, end_index),
            ))
        } else {
            Ok(loc!(
                Token::Integer(
                    self.source[start_index..end_index]
                        .parse()
                        .expect("unparseable number"),
                ),
                LocationRange(start_index, end_index),
            ))
        }
    }

    fn read_identifier(&mut self, start_index: usize) -> <Lexer<'input> as Iterator>::Item {
        let end_index = self
            .take_while(|ch| is_id_start(ch) || is_id_body(ch))
            .unwrap_or_else(|| self.source.len());
        let token = match &self.source[start_index..end_index] {
            "else" => Token::Else,
            "false" => Token::False,
            "for" => Token::For,
            "if" => Token::If,
            "struct" => Token::Struct,
            "return" => Token::Return,
            "true" => Token::True,
            "let" => Token::Let,
            "while" => Token::While,
            "export" => Token::Export,
            ident => {
                let ident = ident.to_string();
                if let Some(id) = self.name_table.get_id(&ident) {
                    Token::Ident(*id)
                } else {
                    let id = self.name_table.insert(ident);
                    Token::Ident(id)
                }
            }
        };
        Ok(loc!(token, LocationRange(start_index, end_index)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Loc<Token>, Loc<LexicalError>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let start_index = self.current_location;
        if let Some((end_index, ch)) = self.bump() {
            match ch {
                '{' => Some(Ok(loc!(
                    Token::LBrace,
                    LocationRange(start_index, end_index)
                ))),
                '}' => Some(Ok(loc!(
                    Token::RBrace,
                    LocationRange(start_index, end_index)
                ))),
                '(' => Some(Ok(loc!(
                    Token::LParen,
                    LocationRange(start_index, end_index)
                ))),
                ')' => Some(Ok(loc!(
                    Token::RParen,
                    LocationRange(start_index, end_index)
                ))),
                '[' => Some(Ok(loc!(
                    Token::LBracket,
                    LocationRange(start_index, end_index)
                ))),
                ']' => Some(Ok(loc!(
                    Token::RBracket,
                    LocationRange(start_index, end_index)
                ))),
                ';' => Some(Ok(loc!(
                    Token::Semicolon,
                    LocationRange(start_index, end_index)
                ))),
                ',' => Some(Ok(loc!(
                    Token::Comma,
                    LocationRange(start_index, end_index)
                ))),
                '.' => Some(Ok(loc!(Token::Dot, LocationRange(start_index, end_index)))),
                '\\' => Some(Ok(loc!(
                    Token::Slash,
                    LocationRange(start_index, end_index)
                ))),
                ':' => Some(Ok(loc!(
                    Token::Colon,
                    LocationRange(start_index, end_index)
                ))),
                '+' => Some(self.lookahead_match(start_index, Token::PlusEqual, Token::Plus, '=')),
                '-' => match self.lookahead {
                    Some((_, '>')) => {
                        self.bump();
                        Some(Ok(loc!(
                            Token::Arrow,
                            LocationRange(start_index, self.current_location),
                        )))
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok(loc!(
                            Token::MinusEqual,
                            LocationRange(start_index, self.current_location),
                        )))
                    }
                    _ => Some(Ok(loc!(
                        Token::Minus,
                        LocationRange(start_index, end_index)
                    ))),
                },
                '*' => {
                    Some(self.lookahead_match(start_index, Token::TimesEqual, Token::Times, '='))
                }
                '/' => match self.lookahead {
                    Some((_, '/')) => {
                        self.skip_to_line_end();
                        self.next()
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok(loc!(
                            Token::DivEqual,
                            LocationRange(start_index, self.current_location),
                        )))
                    }
                    _ => Some(Ok(loc!(Token::Div, LocationRange(start_index, end_index)))),
                },
                '!' => Some(self.lookahead_match(start_index, Token::BangEqual, Token::Bang, '=')),
                '=' => match self.lookahead {
                    Some((_, '>')) => {
                        self.bump();
                        Some(Ok(loc!(
                            Token::FatArrow,
                            LocationRange(start_index, self.current_location),
                        )))
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok(loc!(
                            Token::EqualEqual,
                            LocationRange(start_index, self.current_location),
                        )))
                    }
                    _ => Some(Ok(loc!(
                        Token::Equal,
                        LocationRange(start_index, end_index)
                    ))),
                },
                '>' => Some(self.lookahead_match(
                    start_index,
                    Token::GreaterEqual,
                    Token::Greater,
                    '=',
                )),
                '<' => Some(self.lookahead_match(start_index, Token::LessEqual, Token::Less, '=')),
                '&' => Some(self.lookahead_match(start_index, Token::AmpAmp, Token::Amp, '&')),
                '|' => Some(self.lookahead_match(start_index, Token::PipePipe, Token::Pipe, '|')),
                '"' => Some(self.read_string(end_index)),
                ch if is_id_start(ch) => Some(self.read_identifier(end_index)),
                ch if ch.is_ascii_digit() => Some(self.read_number(end_index)),
                ch => {
                    let error = Loc {
                        location: LocationRange(start_index, end_index),
                        inner: LexicalError::InvalidCharacter { ch },
                    };
                    Some(Err(error))
                }
            }
        } else {
            None
        }
    }
}
