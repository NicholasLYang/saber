extern crate failure;

#[macro_use]
extern crate failure_derive;
extern crate byteorder;
extern crate itertools;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::emitter::Emitter;
use crate::opcodes::OpCode;
use crate::parser::Parser;
use crate::typechecker::TypeChecker;
use crate::types::Result;
use ast::Type;
use std::fs::File;
use std::io;
use std::io::Write;

mod ast;
mod emitter;
mod ir;
mod lexer;
mod opcodes;
mod parser;
mod typechecker;
mod types;

fn main() -> Result<()> {
    run_repl()?;
    Ok(())
}

fn run_repl() -> Result<()> {
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
        let parser_out = parser.parse_statement()?;
        let mut typechecker = TypeChecker::new();
        let typed_stmt = typechecker.infer_stmt(parser_out)?;
        println!("{:#?}", typed_stmt);
    }
}
