use crate::ast::{Loc, Type, TypeId};
use crate::lexer::Token;
use crate::parser::ParseError;
use crate::typechecker::TypeError;
use crate::utils::NameTable;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use id_arena::Arena;
use itertools::Itertools;

pub fn type_to_string(name_table: &NameTable, type_table: &Arena<Type>, type_id: TypeId) -> String {
    match &type_table[type_id] {
        Type::Unit => "()".to_string(),
        Type::Integer => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Var(id) => format!("var({})", id),
        Type::Array(type_id) => format!("[{}]", type_to_string(name_table, type_table, *type_id)),
        Type::Record(name, fields) => {
            let name_str = name_table.get_str(name);
            let fields_str = fields
                .iter()
                .map(|(name, type_id)| {
                    let type_str = type_to_string(name_table, type_table, *type_id);
                    let name_str = name_table.get_str(name);
                    format!("{}: {}", name_str, type_str)
                })
                .join(", ");
            format!("{} {{ {} }}", name_str, fields_str)
        }
        Type::Tuple(types) => {
            let elem_str = types
                .iter()
                .map(|type_id| type_to_string(name_table, type_table, *type_id))
                .join(", ");
            format!("({})", elem_str)
        }
        Type::Arrow(params_type, return_type) => {
            let params_str = type_to_string(name_table, type_table, *params_type);
            let return_str = type_to_string(name_table, type_table, *return_type);
            format!("{} => {}", params_str, return_str)
        }
        Type::Solved(type_id) => format!("{}", type_to_string(name_table, type_table, *type_id)),
    }
}

pub fn token_to_string(name_table: &NameTable, token: &Token) -> String {
    match token {
        Token::Break => "break".to_string(),
        Token::False => "false".to_string(),
        Token::True => "true".to_string(),
        Token::Else => "else".to_string(),
        Token::Export => "export".to_string(),
        Token::For => "for".to_string(),
        Token::If => "if".to_string(),
        Token::Loop => "loop".to_string(),
        Token::Return => "return".to_string(),
        Token::Struct => "struct".to_string(),
        Token::Let => "let".to_string(),
        Token::While => "while".to_string(),
        Token::Ident(i) => format!("<{}>", name_table.get_str(i)),
        Token::Float(f) => f.to_string(),
        Token::Integer(i) => i.to_string(),
        Token::LBrace => "{".to_string(),
        Token::RBrace => "}".to_string(),
        Token::LBracket => "[".to_string(),
        Token::RBracket => "]".to_string(),
        Token::LParen => "(".to_string(),
        Token::RParen => ")".to_string(),
        Token::Semicolon => ";".to_string(),
        Token::Colon => ":".to_string(),
        Token::Comma => ",".to_string(),
        Token::Dot => ".".to_string(),
        Token::Amp => "&".to_string(),
        Token::AmpAmp => "&&".to_string(),
        Token::Pipe => "|".to_string(),
        Token::PipePipe => "||".to_string(),
        Token::Greater => ">".to_string(),
        Token::GreaterEqual => ">=".to_string(),
        Token::Less => "<".to_string(),
        Token::LessEqual => "<=".to_string(),
        Token::Bang => "!".to_string(),
        Token::BangEqual => "!=".to_string(),
        Token::Equal => "=".to_string(),
        Token::EqualEqual => "==".to_string(),
        Token::Plus => "+".to_string(),
        Token::PlusEqual => "+=".to_string(),
        Token::Minus => "-".to_string(),
        Token::MinusEqual => "-=".to_string(),
        Token::Div => "/".to_string(),
        Token::DivEqual => "/=".to_string(),
        Token::Times => "*".to_string(),
        Token::TimesEqual => "*=".to_string(),
        Token::FatArrow => "=>".to_string(),
        Token::Arrow => "->".to_string(),
        Token::Slash => "\\".to_string(),
        Token::String(s) => format!("\"{}\"", s),
    }
}

impl From<&Loc<TypeError>> for Diagnostic<()> {
    fn from(type_error: &Loc<TypeError>) -> Self {
        let range: std::ops::Range<usize> = type_error.location.into();

        Diagnostic::error()
            .with_message("Type Error")
            .with_labels(vec![
                Label::primary((), range).with_message(type_error.inner.to_string())
            ])
    }
}

impl From<&Loc<ParseError>> for Diagnostic<()> {
    fn from(parse_error: &Loc<ParseError>) -> Self {
        let range: std::ops::Range<usize> = parse_error.location.into();

        Diagnostic::error()
            .with_message("Parse Error")
            .with_labels(vec![
                Label::primary((), range).with_message(parse_error.inner.to_string())
            ])
    }
}
