extern crate failure;

#[macro_use]
extern crate failure_derive;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);
use crate::ast::Type;
use std::collections::HashMap;
use std::io;
use std::io::Write;

mod ast;
mod code_generator;
mod lexer;
mod typechecker;

fn main() -> std::io::Result<()> {
    print!("> ");
    io::stdout().flush().unwrap();
    let mut input = String::new();
    if input == "exit" {
        return Ok(());
    }
    io::stdin()
        .read_line(&mut input)
        .ok()
        .expect("Couldn't read line");
    let lexer = lexer::Lexer::new(&input);
    let mut parser_out = parser::ProgramParser::new().parse(lexer);
    let mut ctx = HashMap::new();
    let mut type_names = HashMap::new();
    type_names.insert("integer".to_string(), Type::Int);
    type_names.insert("float".to_string(), Type::Float);
    type_names.insert("char".to_string(), Type::Char);
    if let Ok(out) = &mut parser_out {
        println!("{:#?}", out);
        if let Some(stmt) = out.pop() {
            let typed_stmt = typechecker::infer_stmt(&mut ctx, &type_names, stmt);
            println!("{:#?}", typed_stmt);
        }
    }
    Ok(())
}
