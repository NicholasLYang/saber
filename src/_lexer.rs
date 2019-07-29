use failure::Error;

pub type Spanned<'a, Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

#[derive(Debug, Fail, PartialEq)]
pub enum LexicalError {
    #[fail(display = "Invalid character '{}' found at {}", ch, location)]
    InvalidCharacter { ch: char, location: usize },

    #[fail(display = "String starting at {} was not terminated", location)]
    UnterminatedString { location: usize },
}
