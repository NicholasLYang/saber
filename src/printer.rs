use ast::{Type, TypeId};
use itertools::Itertools;
use lexer::Token;
use utils::{NameTable, TypeTable};

pub fn type_to_string(name_table: &NameTable, type_table: &TypeTable, type_id: TypeId) -> String {
    match type_table.get_type(type_id) {
        Type::Unit => "()".to_string(),
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Var(id) => format!("var({})", id),
        Type::Array(type_id) => format!("[{}]", type_to_string(name_table, type_table, *type_id)),
        Type::Record(fields) => {
            let fields_str = fields
                .iter()
                .map(|(name, type_id)| {
                    let type_str = type_to_string(name_table, type_table, *type_id);
                    let name_str = name_table.get_str(name);
                    format!("{}: {}", name_str, type_str)
                })
                .join(", ");
            format!("{{ {} }}", fields_str)
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
        Type::Solved(type_id) => type_to_string(name_table, type_table, *type_id),
    }
}

pub fn token_to_string(name_table: &NameTable, token: &Token) -> String {
    match token {
        Token::False => "false".to_string(),
        Token::True => "true".to_string(),
        Token::Else => "else".to_string(),
        Token::Export => "export".to_string(),
        Token::For => "for".to_string(),
        Token::If => "if".to_string(),
        Token::Return => "return".to_string(),
        Token::Struct => "struct".to_string(),
        Token::Let => "let".to_string(),
        Token::While => "while".to_string(),
        Token::Ident(i) => format!("<{}>", name_table.get_str(i)),
        Token::Float(f) => format!("{}", f),
        Token::Integer(i) => format!("{}", i),
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
