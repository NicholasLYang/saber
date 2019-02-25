extern crate failure;

#[macro_use]
extern crate failure_derive;
extern crate itertools;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::parser::Parser;
use crate::typechecker::TypeChecker;
use std::io;
use std::io::Write;

mod ast;
mod code_generator;
mod lexer;
mod parser;
mod typechecker;
mod types;

fn main() -> std::io::Result<()> {
    loop {
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
        let mut parser = Parser::new(lexer);
        let parser_out = parser.parse_statement();
        println!("PARSER OUT: {:#?}", parser_out);
        let mut typechecker = TypeChecker::new();

        if let Ok(stmt) = parser_out {
            let typed_stmt = typechecker.infer_stmt(stmt);
            println!("{:#?}", typed_stmt);
        }
    }
}
