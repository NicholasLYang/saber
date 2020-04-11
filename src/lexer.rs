use std::fmt::{self, Debug, Display, Formatter};
use std::str::CharIndices;
use utils::NameTable;

#[derive(Clone, Debug, PartialEq, EnumDiscriminants)]
pub enum Token {
    False,
    True,
    Else,
    Export,
    For,
    If,
    Print,
    Return,
    This,
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
    // ({
    LParenBrace,
    // })
    RParenBrace,
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
    FatArrow,
    Slash,
    String(String),
}

#[derive(PartialEq, Clone, Copy)]
pub struct Location(pub usize, pub usize);

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct LocationRange(pub Location, pub Location);

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

#[inline]
fn is_id_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[inline]
fn is_id_body(ch: char) -> bool {
    ch == '_' || ch.is_ascii_digit() || ch.is_ascii_alphabetic()
}

#[derive(Debug, Fail, PartialEq)]
pub enum LexicalError {
    #[fail(display = "{}: Invalid character '{}'", location, ch)]
    InvalidCharacter { ch: char, location: LocationRange },

    #[fail(display = "String starting at {} was not terminated", location)]
    UnterminatedString { location: Location },
}

pub struct Lexer<'input> {
    source: &'input str,
    chars: CharIndices<'input>,
    pub name_table: NameTable,
    row: usize,
    column: usize,
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
            row: 1,
            column: 1,
            name_table: NameTable::new(),
            lookahead,
            lookahead2,
        }
    }

    pub fn get_location(&self) -> Location {
        Location(self.row, self.column)
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let next = self.lookahead;
        self.lookahead = self.lookahead2;
        self.lookahead2 = self.chars.next();
        if let Some((_, '\n')) = next {
            self.row += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
        next
    }

    fn peek(&self) {
        println!("{:?}", self.lookahead);
    }

    fn lookahead_match(
        &mut self,
        start_loc: Location,
        matched_token: Token,
        alt_token: Token,
        match_ch: char,
    ) -> <Lexer<'input> as Iterator>::Item {
        match self.lookahead {
            Some((_, ch)) => {
                if match_ch == ch {
                    self.bump();
                    Ok((matched_token, LocationRange(start_loc, self.get_location())))
                } else {
                    Ok((alt_token, LocationRange(start_loc, self.get_location())))
                }
            }
            None => Ok((alt_token, LocationRange(start_loc, start_loc))),
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

    fn read_string(
        &mut self,
        start_index: usize,
        start_loc: Location,
    ) -> <Lexer<'input> as Iterator>::Item {
        match self.take_until(|ch| ch == '"') {
            Some(i) => {
                self.bump();
                let end_loc = self.get_location();
                Ok((
                    Token::String(self.source[start_index + 1..i].to_string()),
                    LocationRange(start_loc, end_loc),
                ))
            }
            None => Err(LexicalError::UnterminatedString {
                location: start_loc,
            }),
        }
    }

    fn read_number(
        &mut self,
        start_index: usize,
        start_loc: Location,
    ) -> <Lexer<'input> as Iterator>::Item {
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
        let end_loc = self.get_location();
        let end_index = end_index.unwrap_or_else(|| self.source.len());
        if is_decimal {
            Ok((
                Token::Float(
                    self.source[start_index..end_index]
                        .parse()
                        .expect("unparseable number"),
                ),
                LocationRange(start_loc, end_loc),
            ))
        } else {
            Ok((
                Token::Integer(
                    self.source[start_index..end_index]
                        .parse()
                        .expect("unparseable number"),
                ),
                LocationRange(start_loc, end_loc),
            ))
        }
    }

    fn read_identifier(
        &mut self,
        start_index: usize,
        start_loc: Location,
    ) -> <Lexer<'input> as Iterator>::Item {
        let end_index = self
            .take_while(|ch| is_id_start(ch) || is_id_body(ch))
            .unwrap_or_else(|| self.source.len());
        let end_loc = self.get_location();
        match &self.source[start_index..end_index] {
            "else" => Ok((Token::Else, LocationRange(start_loc, end_loc))),
            "false" => Ok((Token::False, LocationRange(start_loc, end_loc))),
            "for" => Ok((Token::For, LocationRange(start_loc, end_loc))),
            "if" => Ok((Token::If, LocationRange(start_loc, end_loc))),
            "print" => Ok((Token::Print, LocationRange(start_loc, end_loc))),
            "return" => Ok((Token::Return, LocationRange(start_loc, end_loc))),
            "this" => Ok((Token::This, LocationRange(start_loc, end_loc))),
            "true" => Ok((Token::True, LocationRange(start_loc, end_loc))),
            "let" => Ok((Token::Let, LocationRange(start_loc, end_loc))),
            "while" => Ok((Token::While, LocationRange(start_loc, end_loc))),
            "export" => Ok((Token::Export, LocationRange(start_loc, end_loc))),
            ident => {
                let ident = ident.to_string();
                if let Some(id) = self.name_table.get_id(&ident) {
                    Ok((Token::Ident(*id), LocationRange(start_loc, end_loc)))
                } else {
                    let id = self.name_table.insert(ident);
                    Ok((Token::Ident(id), LocationRange(start_loc, end_loc)))
                }
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(Token, LocationRange), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let start_loc = self.get_location();
        if let Some((i, ch)) = self.bump() {
            let end_loc = self.get_location();
            match ch {
                '{' => Some(Ok((Token::LBrace, LocationRange(start_loc, end_loc)))),
                '}' => {
                    Some(self.lookahead_match(start_loc, Token::RParenBrace, Token::RBrace, ')'))
                }
                '(' => {
                    Some(self.lookahead_match(start_loc, Token::LParenBrace, Token::LParen, '{'))
                }
                ')' => Some(Ok((Token::RParen, LocationRange(start_loc, end_loc)))),
                '[' => Some(Ok((Token::LBracket, LocationRange(start_loc, end_loc)))),
                ']' => Some(Ok((Token::RBracket, LocationRange(start_loc, end_loc)))),
                ';' => Some(Ok((Token::Semicolon, LocationRange(start_loc, end_loc)))),
                ',' => Some(Ok((Token::Comma, LocationRange(start_loc, end_loc)))),
                '.' => Some(Ok((Token::Dot, LocationRange(start_loc, end_loc)))),
                '\\' => Some(Ok((Token::Slash, LocationRange(start_loc, end_loc)))),
                ':' => Some(Ok((Token::Colon, LocationRange(start_loc, end_loc)))),
                '+' => Some(self.lookahead_match(start_loc, Token::PlusEqual, Token::Plus, '=')),
                '-' => Some(self.lookahead_match(start_loc, Token::MinusEqual, Token::Minus, '=')),
                '*' => Some(self.lookahead_match(start_loc, Token::TimesEqual, Token::Times, '=')),
                '/' => match self.lookahead {
                    Some((_, '/')) => {
                        self.skip_to_line_end();
                        self.next()
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok((
                            Token::DivEqual,
                            LocationRange(start_loc, self.get_location()),
                        )))
                    }
                    _ => Some(Ok((Token::Div, LocationRange(start_loc, end_loc)))),
                },
                '!' => Some(self.lookahead_match(start_loc, Token::BangEqual, Token::Bang, '=')),
                '=' => match self.lookahead {
                    Some((_, '>')) => {
                        self.bump();
                        Some(Ok((
                            Token::FatArrow,
                            LocationRange(start_loc, self.get_location()),
                        )))
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok((
                            Token::EqualEqual,
                            LocationRange(start_loc, self.get_location()),
                        )))
                    }
                    _ => Some(Ok((Token::Equal, LocationRange(start_loc, end_loc)))),
                },
                '>' => {
                    Some(self.lookahead_match(start_loc, Token::GreaterEqual, Token::Greater, '='))
                }
                '<' => Some(self.lookahead_match(start_loc, Token::LessEqual, Token::Less, '=')),
                '&' => Some(self.lookahead_match(start_loc, Token::AmpAmp, Token::Amp, '&')),
                '|' => Some(self.lookahead_match(start_loc, Token::PipePipe, Token::Pipe, '|')),
                '"' => Some(self.read_string(i, start_loc)),
                ch if is_id_start(ch) => Some(self.read_identifier(i, start_loc)),
                ch if ch.is_ascii_digit() => Some(self.read_number(i, start_loc)),
                ch => {
                    let error = LexicalError::InvalidCharacter {
                        ch,
                        location: LocationRange(start_loc, end_loc),
                    };
                    Some(Err(error))
                }
            }
        } else {
            None
        }
    }
}
