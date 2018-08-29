use failure::Error;
use std::fs::File;
use std::io::Read;
use std::str::CharIndices;

#[derive(Debug)]
pub struct Token<'input> {
    tokenType: TokenType<'input>,
    location: Location,
}

#[derive(Debug)]
enum TokenType<'input> {
    Illegal,
    EndOfFile,
    False,
    True,
    Else,
    Fun,
    For,
    If,
    Print,
    Return,
    This,
    Let,
    While,
    Ident(String),
    Number(f64),
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Semicolon,
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
    Slash,
    SlashEqual,
    Star,
    StarEqual,
    String { content: &'input str, end: Location },
}

#[derive(Debug)]
enum OpType {
    Plus,
    Minus,
    Times,
    Div,
}

#[inline]
fn is_id_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[inline]
fn is_id_body(ch: char) -> bool {
    ch == '_' || ch.is_ascii_digit() || ch.is_ascii_alphabetic()
}

pub type Location = (usize, usize);

#[derive(Debug, Fail, PartialEq)]
pub enum LexicalError {
    #[fail(display = "Invalid character '{}' found at {}:{}", ch, row, col)]
    InvalidCharacter { ch: char, row: usize, col: usize },

    #[fail(display = "String starting at {}:{} was not terminated", row, col)]
    UnterminatedString { row: usize, col: usize },
}

pub type SpanResult<'input> = Result<Token<'input>, LexicalError>;

struct Lexer<'input> {
    source: &'input str,
    location: Location,
    chars: CharIndices<'input>,
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
            lookahead,
            lookahead2,
            location: (0, 0),
        }
    }
    fn bump(&mut self) -> Option<(usize, char)> {
        let next = self.lookahead;
        if let Some((_, '\n')) = next {
            self.location.0 += 1;
            self.location.1 = 0;
        }
        self.lookahead = self.lookahead2;
        self.lookahead2 = self.chars.next();
        next
    }

    fn lookahead_match<'a>(
        &mut self,
        matched_tokentype: TokenType<'a>,
        alt_tokentype: TokenType<'a>,
        ch: char,
    ) -> Token<'a> {
        if let Some((_, ch)) = self.lookahead {
            self.bump();
            Token {
                tokenType: matched_tokentype,
                location: self.location,
            }
        } else {
            Token {
                tokenType: alt_tokentype,
                location: self.location,
            }
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

    fn read_string(&mut self, start_index: usize) -> SpanResult<'input> {
        let location = self.location.clone();
        match self.take_until(|ch| ch == '"') {
            Some(i) => {
                self.bump();
                Ok(Token {
                    location,
                    tokenType: TokenType::String {
                        content: &self.source[start_index + 1..i],
                        end: (self.location.0, i + 1),
                    },
                })
            }
            None => Err(LexicalError::UnterminatedString {
                row: location.0,
                col: location.1,
            }),
        }
    }

    fn read_number(&mut self, start_index: usize) -> SpanResult<'input> {
        let location = self.location.clone();
        let mut end = self.take_while(|ch| ch.is_ascii_digit());

        if let Some((_, '.')) = self.lookahead {
            // Check if it's a decimal or a field access
            if let Some((_, next_ch)) = self.lookahead2 {
                if next_ch.is_ascii_digit() {
                    self.bump();
                    end = self.take_while(|ch| ch.is_ascii_digit());
                }
            }
        }

        let end = end.unwrap_or_else(|| self.source.len());
        Ok((Token {
            tokenType: TokenType::Number(
                self.source[start_index..end]
                    .parse()
                    .expect("unparseable number"),
            ),
            location,
        }))
    }

    fn read_identifier(&mut self, pos: usize) -> SpanResult<'input> {
        let start_location = self.location.clone();
        let end = self
            .take_while(|ch| is_id_start(ch) || is_id_body(ch))
            .unwrap_or_else(|| self.source.len());
        match &self.source[pos..end] {
            "else" => Ok(Token {
                tokenType: TokenType::Else,
                location: start_location,
            }),
            "false" => Ok(Token {
                tokenType: TokenType::False,
                location: start_location,
            }),
            "fun" => Ok(Token {
                tokenType: TokenType::Fun,
                location: start_location,
            }),
            "for" => Ok(Token {
                tokenType: TokenType::For,
                location: start_location,
            }),
            "if" => Ok(Token {
                tokenType: TokenType::If,
                location: start_location,
            }),
            "print" => Ok(Token {
                tokenType: TokenType::Print,
                location: start_location,
            }),
            "return" => Ok(Token {
                tokenType: TokenType::Return,
                location: start_location,
            }),
            "this" => Ok(Token {
                tokenType: TokenType::This,
                location: start_location,
            }),
            "true" => Ok(Token {
                tokenType: TokenType::True,
                location: start_location,
            }),
            "let" => Ok(Token {
                tokenType: TokenType::Let,
                location: start_location,
            }),
            "while" => Ok(Token {
                tokenType: TokenType::While,
                location: start_location,
            }),
            id => Ok(Token {
                tokenType: TokenType::Ident(id.to_string()),
                location: start_location,
            }),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = SpanResult<'input>;
    fn next(&mut self) -> Option<SpanResult<'input>> {
        self.skip_whitespace();
        if let Some((i, ch)) = self.bump() {
            match ch {
                '{' => Some(Ok(Token {
                    tokenType: TokenType::LBrace,
                    location: self.location,
                })),
                '}' => Some(Ok(Token {
                    tokenType: TokenType::RBrace,
                    location: self.location,
                })),
                '(' => Some(Ok(Token {
                    tokenType: TokenType::LParen,
                    location: self.location,
                })),
                ')' => Some(Ok(Token {
                    tokenType: TokenType::RParen,
                    location: self.location,
                })),
                '[' => Some(Ok(Token {
                    tokenType: TokenType::LBracket,
                    location: self.location,
                })),
                ']' => Some(Ok(Token {
                    tokenType: TokenType::RBracket,
                    location: self.location,
                })),
                ';' => Some(Ok(Token {
                    tokenType: TokenType::Semicolon,
                    location: self.location,
                })),
                ',' => Some(Ok(Token {
                    tokenType: TokenType::Comma,
                    location: self.location,
                })),
                '.' => Some(Ok(Token {
                    tokenType: TokenType::Dot,
                    location: self.location,
                })),
                '+' => Some(Ok(self.lookahead_match(
                    TokenType::PlusEqual,
                    TokenType::Plus,
                    '=',
                ))),
                '-' => Some(Ok(self.lookahead_match(
                    TokenType::MinusEqual,
                    TokenType::Minus,
                    '=',
                ))),
                '*' => Some(Ok(self.lookahead_match(
                    TokenType::StarEqual,
                    TokenType::StarEqual,
                    '=',
                ))),
                '/' => match self.lookahead {
                    Some((_, '/')) => {
                        self.skip_to_line_end();
                        self.next()
                    }
                    Some((_, '=')) => {
                        self.bump();
                        Some(Ok(Token {
                            tokenType: TokenType::SlashEqual,
                            location: self.location,
                        }))
                    }
                    _ => Some(Ok(Token {
                        tokenType: TokenType::Slash,
                        location: self.location,
                    })),
                },
                '!' => Some(Ok(self.lookahead_match(
                    TokenType::BangEqual,
                    TokenType::Bang,
                    '=',
                ))),
                '=' => Some(Ok(self.lookahead_match(
                    TokenType::EqualEqual,
                    TokenType::Equal,
                    '=',
                ))),
                '>' => Some(Ok(self.lookahead_match(
                    TokenType::GreaterEqual,
                    TokenType::Greater,
                    '=',
                ))),
                '<' => Some(Ok(self.lookahead_match(
                    TokenType::LessEqual,
                    TokenType::Less,
                    '=',
                ))),
                '&' => Some(Ok(self.lookahead_match(
                    TokenType::AmpAmp,
                    TokenType::Amp,
                    '&',
                ))),
                '|' => Some(Ok(self.lookahead_match(
                    TokenType::PipePipe,
                    TokenType::Pipe,
                    '|',
                ))),
                '"' => Some(self.read_string(i)),
                ch if is_id_start(ch) => Some(self.read_identifier(i)),
                ch if ch.is_ascii_digit() => Some(self.read_number(i)),
                ch => Some(Err(LexicalError::InvalidCharacter {
                    ch,
                    row: self.location.0,
                    col: self.location.1,
                })),
            }
        } else {
            None
        }
    }
}

pub fn lex(filename: &String) -> Result<(), Error> {
    let mut f = File::open(filename)?;
    let mut source = String::new();
    f.read_to_string(&mut source);
    let lexer = Lexer::new(&source);
    for res in lexer {
        if let Ok(token) = res {
            println!("{:?}", token);
        }
    }
    Ok(())
}
