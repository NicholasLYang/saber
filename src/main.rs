extern crate base64;
extern crate bimap;
extern crate byteorder;
extern crate clap;
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
use clap::{App, AppSettings, Arg};
use code_generator::{CodeGenerator, ARRAY_ID, BOX_ARRAY_ID};
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs::{self, File};
use std::io;
use std::io::Write;
use std::process::Command;
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

fn main() -> Result<()> {
    let matches = App::new("saber")
        .version("0.1.0")
        .author("Nicholas Yang")
        .about("Saber Programming Language")
        .arg(
            Arg::with_name("debug")
                .long("debug")
                .short('d')
                .about("Debugging the compiler")
                .value_name("DEBUG_FILE"),
        )
        .arg(Arg::with_name("out-file").long("out-file"))
        .subcommand(
            App::new("build")
                .about("Build file")
                .arg(Arg::with_name("file")),
        )
        .subcommand(
            App::new("run")
                .about("Run file")
                .arg(Arg::with_name("file")),
        )
        .setting(AppSettings::ArgRequiredElseHelp)
        .setting(AppSettings::ColoredHelp)
        .get_matches();
    let debug_file = if let Some(debug_file_path) = matches.value_of("debug") {
        Some(File::create(debug_file_path)?)
    } else {
        None
    };
    if let Some(build_matches) = matches.subcommand_matches("build") {
        let file = build_matches.value_of("file").unwrap();
        compile_saber_file(file, debug_file)
    } else if let Some(run_matches) = matches.subcommand_matches("run") {
        let file = run_matches.value_of("file").unwrap();
        compile_saber_file(file, debug_file)?;
        let output = Command::new("node")
            .args(&["build/load.js"])
            .output()
            .expect("Failed to run code");
        io::stderr().write_all(&output.stderr).unwrap();
        io::stdout().write_all(&output.stdout).unwrap();
        Ok(())
    } else {
        Ok(())
    }
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

// TODO: Add some more general format for flags/build config
fn compile_saber_file<T: Write>(file_name: &str, debug_output: Option<T>) -> Result<()> {
    let contents = fs::read_to_string(file_name)?;
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let file = SimpleFile::new(file_name, contents.as_str());
    let lexer = lexer::Lexer::new(&contents);
    let mut parser = Parser::new(lexer);
    let program = parser.program().expect("Error parsing");
    for error in &program.errors {
        let diagnostic: Diagnostic<()> = error.into();
        term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
    }
    let mut typechecker = TypeChecker::new(parser.get_name_table());
    let program_t = typechecker.check_program(program);
    let runtime_type_info = typechecker.generate_runtime_type_info(&program_t.named_types);
    for error in &program_t.errors {
        let diagnostic: Diagnostic<()> = error.into();
        term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
    }

    let type_info_str = runtime_type_info
        .iter()
        .map(|(name, type_info)| format!("{}:{}", name, format_bool_vec(&type_info)))
        .collect::<Vec<String>>()
        .join(",");
    let mut type_info_file = File::create("runtime/type_info.ts")?;
    write!(
        type_info_file,
        "export const typeInfo: {{ [n: number]: boolean[] }} = {{{}}};export const STR_INDEX = {};\
        export const ARRAY_ID = {}; export const BOX_ARRAY_ID ={}",
        type_info_str, STR_INDEX, ARRAY_ID, BOX_ARRAY_ID
    )?;
    let code_generator = CodeGenerator::new(typechecker);
    let program = code_generator.generate_program(program_t)?;
    let mut emitter = Emitter::new();
    emitter.emit_program(program)?;
    if let Some(debug_output) = debug_output {
        emitter.output(debug_output)?;
    }
    let js_str = format!("export const code =\"{}\"", emitter.output_base64());
    let mut js_file = File::create("runtime/code.ts")?;
    js_file.write_all(js_str.as_bytes())?;
    let output = Command::new("npm")
        .args(&["--prefix", "runtime", "run", "build"])
        .output()
        .expect("Failed to build code");
    io::stderr().write_all(&output.stderr).unwrap();
    Ok(())
}
