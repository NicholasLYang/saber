#[macro_use]
extern crate failure;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use std::io;
use std::io::Write;

mod ast;
mod lexer;

fn main() -> std::io::Result<()> {
    print!("> ");
    io::stdout().flush().unwrap();
    let mut input = String::new();

    io::stdin()
        .read_line(&mut input)
        .ok()
        .expect("Couldn't read line");

    let lexer = lexer::Lexer::new(&input);
    let parser_out = parser::ProgramParser::new().parse(lexer);

    println!("{:#?}", parser_out);
    Ok(())
}
