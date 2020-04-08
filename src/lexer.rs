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

pub type Spanned<'a, Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

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
    #[fail(display = "Invalid character '{}' found at {}", ch, location)]
    InvalidCharacter { ch: char, location: usize },

    #[fail(display = "String starting at {} was not terminated", location)]
    UnterminatedString { location: usize },
}

pub struct Lexer<'input> {
    source: &'input str,
    chars: CharIndices<'input>,
    name_table: NameTable,
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
            lookahead,
            lookahead2,
        }
    }

    pub fn get_name_table(self) -> NameTable {
        self.name_table
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let next = self.lookahead;
        self.lookahead = self.lookahead2;
        self.lookahead2 = self.chars.next();
        next
    }

    fn lookahead_match(
        &mut self,
        start_pos: usize,
        matched_token: Token,
        alt_token: Token,
        match_ch: char,
    ) -> <Lexer<'input> as Iterator>::Item {
        match self.lookahead {
            Some((i, ch)) => {
                if match_ch == ch {
                    self.bump();
                    Ok((start_pos, matched_token, i))
                } else {
                    Ok((start_pos, alt_token, start_pos))
                }
            }
            None => Ok((start_pos, alt_token, start_pos)),
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

    fn read_string(&mut self, start_pos: usize) -> <Lexer<'input> as Iterator>::Item {
        match self.take_until(|ch| ch == '"') {
            Some(i) => {
                self.bump();
                Ok((
                    start_pos,
                    Token::String(self.source[start_pos + 1..i].to_string()),
                    i,
                ))
            }
            None => Err(LexicalError::UnterminatedString {
                location: start_pos,
            })?,
        }
    }

    fn read_number(&mut self, start_pos: usize) -> <Lexer<'input> as Iterator>::Item {
        let mut end = self.take_while(|ch| ch.is_ascii_digit());
        let mut is_decimal = false;

        if let Some((_, '.')) = self.lookahead {
            // Check if it's a decimal or a field access
            if let Some((_, next_ch)) = self.lookahead2 {
                if next_ch.is_ascii_digit() {
                    is_decimal = true;
                    self.bump();
                    end = self.take_while(|ch| ch.is_ascii_digit());
                }
            }
        }

        let end = end.unwrap_or_else(|| self.source.len());
        if is_decimal {
            Ok((
                start_pos,
                Token::Float(
                    self.source[start_pos..end]
                        .parse()
                        .expect("unparseable number"),
                ),
                end,
            ))
        } else {
            Ok((
                start_pos,
                Token::Integer(
                    self.source[start_pos..end]
                        .parse()
                        .expect("unparseable number"),
                ),
                end,
            ))
        }
    }

    fn read_identifier(&mut self, start: usize) -> <Lexer<'input> as Iterator>::Item {
        let end = self
            .take_while(|ch| is_id_start(ch) || is_id_body(ch))
            .unwrap_or_else(|| self.source.len());
        match &self.source[start..end] {
            "else" => Ok((start, Token::Else, end)),
            "false" => Ok((start, Token::False, end)),
            "for" => Ok((start, Token::For, end)),
            "if" => Ok((start, Token::If, end)),
            "print" => Ok((start, Token::Print, end)),
            "return" => Ok((start, Token::Return, end)),
            "this" => Ok((start, Token::This, end)),
            "true" => Ok((start, Token::True, end)),
            "let" => Ok((start, Token::Let, end)),
            "while" => Ok((start, Token::While, end)),
            "export" => Ok((start, Token::Export, end)),
            ident => {
                let ident = ident.to_string();
                if let Some(id) = self.name_table.get_id(&ident) {
                    Ok((start, Token::Ident(*id), end))
                } else {
                    let id = self.name_table.insert(ident);
                    Ok((start, Token::Ident(id), end))
                }
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<'input, Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((location, ch)) = self.bump() {
            match ch {
                '{' => Some(Ok((location, Token::LBrace, location))),
                '}' => Some(self.lookahead_match(location, Token::RParenBrace, Token::RBrace, ')')),
                '(' => Some(self.lookahead_match(location, Token::LParenBrace, Token::LParen, '{')),
                ')' => Some(Ok((location, Token::RParen, location))),
                '[' => Some(Ok((location, Token::LBracket, location))),
                ']' => Some(Ok((location, Token::RBracket, location))),
                ';' => Some(Ok((location, Token::Semicolon, location))),
                ',' => Some(Ok((location, Token::Comma, location))),
                '.' => Some(Ok((location, Token::Dot, location))),
                '\\' => Some(Ok((location, Token::Slash, location))),
                ':' => Some(Ok((location, Token::Colon, location))),
                '+' => Some(self.lookahead_match(location, Token::PlusEqual, Token::Plus, '=')),
                '-' => Some(self.lookahead_match(location, Token::MinusEqual, Token::Minus, '=')),
                '*' => Some(self.lookahead_match(location, Token::TimesEqual, Token::Times, '=')),
                '/' => match self.lookahead {
                    Some((_, '/')) => {
                        self.skip_to_line_end();
                        self.next()
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok((location, Token::DivEqual, location)))
                    }
                    _ => Some(Ok((location, Token::Div, location))),
                },
                '!' => Some(self.lookahead_match(location, Token::BangEqual, Token::Bang, '=')),
                '=' => match self.lookahead {
                    Some((_, '>')) => {
                        self.bump();
                        Some(Ok((location, Token::FatArrow, location)))
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok((location, Token::EqualEqual, location)))
                    }
                    _ => Some(Ok((location, Token::Equal, location))),
                },
                '>' => {
                    Some(self.lookahead_match(location, Token::GreaterEqual, Token::Greater, '='))
                }
                '<' => Some(self.lookahead_match(location, Token::LessEqual, Token::Less, '=')),
                '&' => Some(self.lookahead_match(location, Token::AmpAmp, Token::Amp, '&')),
                '|' => Some(self.lookahead_match(location, Token::PipePipe, Token::Pipe, '|')),
                '"' => Some(self.read_string(location)),
                ch if is_id_start(ch) => Some(self.read_identifier(location)),
                ch if ch.is_ascii_digit() => Some(self.read_number(location)),
                ch => {
                    let error = LexicalError::InvalidCharacter { ch, location };
                    Some(Err(error))
                }
            }
        } else {
            None
        }
    }
}
