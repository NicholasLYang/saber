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

use crate::emitter::Emitter;
use crate::parser::Parser;
use crate::typechecker::TypeChecker;
use crate::types::Result;
use code_generator::CodeGenerator;
use std::env;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Write;

mod ast;
mod code_generator;
mod emitter;
mod lexer;
mod parser;
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
            ()
        }
    }
}

fn read_file(file_name: &String) -> Result<()> {
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let lexer = lexer::Lexer::new(&contents);
    let mut parser = Parser::new(lexer);
    let parser_out = parser.stmts()?;
    let mut typechecker = TypeChecker::new(parser.get_name_table());
    let typed_program = typechecker.check_program(parser_out)?;
    let mut code_generator = CodeGenerator::new(typechecker.get_name_table());
    let program = code_generator.generate_program(typed_program)?;
    let out_file = File::create("build/out.wasm")?;
    let mut emitter = Emitter::new(out_file);
    emitter.emit_prelude()?;
    emitter.emit_types_section(program.type_section)?;
    emitter.emit_functions_section(program.function_section)?;
    emitter.emit_exports_section(program.exports_section)?;
    emitter.emit_code_section(program.code_section)?;
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
        let parser_out = parser.stmt()?;
        let name_table = parser.get_name_table();
        let mut typechecker = TypeChecker::new(name_table);
        let typed_stmt = typechecker.stmt(parser_out)?;
        println!("{:#?}", typed_stmt);
    }
}
