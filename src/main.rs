#[macro_use]
extern crate failure;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use ast::Stmt;
use std::io;
use std::io::Write;

mod ast;
mod code_generator;
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
    let mut out = Vec::new();
    if let Ok(stmts) = parser_out {
        for s in stmts {
            let code = match s {
                Stmt::Expr(e) => code_generator::gen_expr(&e),
                _ => "Not implemented yet!".to_string(),
            };
            out.push(code);
        }
    }
    println!("{:#?}", out);
    Ok(())
}
