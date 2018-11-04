#[macro_use]
extern crate failure;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use std::fs::File;
use std::io;
use std::io::Write;

mod ast;
mod code_generator;
mod lexer;

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
    if let Ok(out) = &mut parser_out {
        let mut file = File::create("build/test.wat")?;
        let code = code_generator::gen_program(out);
        file.write_all(code.as_bytes())?;
    }
    Ok(())
}
