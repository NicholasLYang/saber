extern crate base64;
extern crate bimap;
extern crate byteorder;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate im;
extern crate itertools;
extern crate leb128;
extern crate strum;
#[macro_use]
extern crate strum_macros;
extern crate serde;
extern crate serde_json;

use crate::emitter::Emitter;
use crate::parser::Parser;
use crate::typechecker::TypeChecker;
use crate::types::Result;
use code_generator::CodeGenerator;
use std::env;
use std::fs::{self, File};
use std::io;
use std::io::Write;

mod ast;
mod code_generator;
mod emitter;
mod lexer;
mod parser;
mod printer;
mod symbol_table;
mod typechecker;
mod types;
mod utils;
mod wasm;

fn main() {
    let args: Vec<String> = env::args().collect();

    let res = if args.len() < 2 {
        run_repl()
    } else {
        read_file(&args[1])
    };
    match res {
        Ok(_) => (),
        Err(err) => {
            println!("{}", err);
        }
    }
}

fn read_file(file_name: &str) -> Result<()> {
    let contents = fs::read_to_string(file_name)?;
    let lexer = lexer::Lexer::new(&contents);
    let mut parser = Parser::new(lexer);
    let program = parser.program()?;
    for error in &program.errors {
        println!("{}", error);
    }
    let mut typechecker = TypeChecker::new(parser.get_name_table());
    let typed_program = typechecker.check_program(program)?;
    let code_generator = CodeGenerator::new(typechecker);
    let program = code_generator.generate_program(typed_program)?;
    let mut emitter = Emitter::new();
    emitter.emit_program(program)?;
    let js_str = format!(
        "const code = \"{}\"; module.exports = code",
        emitter.output_base64()
    );
    let mut js_file = File::create("build/code.js")?;
    js_file.write_all(js_str.as_bytes())?;
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
            .expect("Couldn't read line");
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        if let Some(parser_out) = parser.stmt()? {
            let name_table = parser.get_name_table();
            let mut typechecker = TypeChecker::new(name_table);
            let typed_stmt = typechecker.stmt(parser_out)?;
            println!("{:#?}", typed_stmt);
        }
    }
}
