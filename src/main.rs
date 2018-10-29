#[macro_use]
extern crate failure;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use std::fs::File;
use std::io::Read;

mod ast;
mod lexer;

fn main() -> std::io::Result<()> {
    let mut f = File::open("test.sbr")?;
    let mut source = String::new();
    f.read_to_string(&mut source)?;
    let lexer = lexer::Lexer::new(&source);
    let parser_out = parser::ExpressionParser::new().parse(lexer);

    println!("{:?}", parser_out);
    Ok(())
}
