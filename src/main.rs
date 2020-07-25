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
use std::io::Write;
use utils::STR_INDEX;

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

    if args.len() < 2 {
        println!("Usage: saber <file>");
    } else {
        let res = read_file(&args[1]);
        match res {
            Ok(_) => (),
            Err(err) => {
                println!("{}", err);
            }
        }
    };
}

fn format_bool_vec(vec: &Vec<bool>) -> String {
    format!(
        "[{}]",
        vec.iter()
            .map(|b| if *b {
                "true".to_string()
            } else {
                "false".to_string()
            })
            .collect::<Vec<String>>()
            .join(",")
    )
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
    let (typed_program, named_types) = typechecker.check_program(program)?;
    let type_info_str = named_types
        .iter()
        .map(|(name, type_info)| format!("{}:{}", name, format_bool_vec(&type_info)))
        .collect::<Vec<String>>()
        .join(",");
    let mut type_info_file = File::create("build/type_info.ts")?;
    write!(
        type_info_file,
        "export const typeInfo = {{{}}};export const STR_INDEX = {};",
        type_info_str, STR_INDEX
    )?;
    let code_generator = CodeGenerator::new(typechecker);
    let program = code_generator.generate_program(typed_program)?;
    let mut emitter = Emitter::new();
    emitter.emit_program(program)?;
    let js_str = format!("export const code =\"{}\"", emitter.output_base64());
    let mut js_file = File::create("build/code.ts")?;
    js_file.write_all(js_str.as_bytes())?;
    Ok(())
}
