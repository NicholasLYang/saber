use crate::ast::{Loc, Type, TypeId};
use crate::lexer::SyntaxToken;
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

pub fn token_to_string(name_table: &NameTable, token: &SyntaxToken) -> String {
    match token {
        SyntaxToken::Break => "break".to_string(),
        SyntaxToken::False => "false".to_string(),
        SyntaxToken::True => "true".to_string(),
        SyntaxToken::Else => "else".to_string(),
        SyntaxToken::Export => "export".to_string(),
        SyntaxToken::For => "for".to_string(),
        SyntaxToken::If => "if".to_string(),
        SyntaxToken::Loop => "loop".to_string(),
        SyntaxToken::Return => "return".to_string(),
        SyntaxToken::Struct => "struct".to_string(),
        SyntaxToken::Let => "let".to_string(),
        SyntaxToken::While => "while".to_string(),
        SyntaxToken::Ident(i) => format!("<{}>", name_table.get_str(i)),
        SyntaxToken::Float(f) => f.to_string(),
        SyntaxToken::Integer(i) => i.to_string(),
        SyntaxToken::LBrace => "{".to_string(),
        SyntaxToken::RBrace => "}".to_string(),
        SyntaxToken::LBracket => "[".to_string(),
        SyntaxToken::RBracket => "]".to_string(),
        SyntaxToken::LParen => "(".to_string(),
        SyntaxToken::RParen => ")".to_string(),
        SyntaxToken::Semicolon => ";".to_string(),
        SyntaxToken::Colon => ":".to_string(),
        SyntaxToken::Comma => ",".to_string(),
        SyntaxToken::Dot => ".".to_string(),
        SyntaxToken::Amp => "&".to_string(),
        SyntaxToken::AmpAmp => "&&".to_string(),
        SyntaxToken::Pipe => "|".to_string(),
        SyntaxToken::PipePipe => "||".to_string(),
        SyntaxToken::Greater => ">".to_string(),
        SyntaxToken::GreaterEqual => ">=".to_string(),
        SyntaxToken::Less => "<".to_string(),
        SyntaxToken::LessEqual => "<=".to_string(),
        SyntaxToken::Bang => "!".to_string(),
        SyntaxToken::BangEqual => "!=".to_string(),
        SyntaxToken::Equal => "=".to_string(),
        SyntaxToken::EqualEqual => "==".to_string(),
        SyntaxToken::Plus => "+".to_string(),
        SyntaxToken::PlusEqual => "+=".to_string(),
        SyntaxToken::Minus => "-".to_string(),
        SyntaxToken::MinusEqual => "-=".to_string(),
        SyntaxToken::Div => "/".to_string(),
        SyntaxToken::DivEqual => "/=".to_string(),
        SyntaxToken::Times => "*".to_string(),
        SyntaxToken::TimesEqual => "*=".to_string(),
        SyntaxToken::FatArrow => "=>".to_string(),
        SyntaxToken::Arrow => "->".to_string(),
        SyntaxToken::Slash => "\\".to_string(),
        SyntaxToken::String(s) => format!("\"{}\"", s),
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
