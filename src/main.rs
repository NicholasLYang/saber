#[macro_use]
extern crate strum_macros;

use crate::mir::MirCompiler;
use crate::parser::Parser;
use crate::runtime::run_code;
use crate::typechecker::TypeChecker;
use crate::utils::SaberProgram;
use crate::wasm_backend::WasmBackend;
use anyhow::Result;
use clap::{App, AppSettings, Arg};
//use code_generator::CodeGenerator;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs::{self, File};
use std::io::Write;

mod ast;
mod lexer;
mod mir;
mod parser;
mod printer;
mod runtime;
mod symbol_table;
mod typechecker;
mod utils;
mod wasm_backend;

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
        let saber_program = compile_saber_file(file, debug_file)?;
        fs::write("out.wasm", saber_program.wasm_bytes)?;
    } else if let Some(run_matches) = matches.subcommand_matches("run") {
        let file = run_matches.value_of("file").unwrap();
        let saber_program = compile_saber_file(file, debug_file)?;
        run_code(saber_program)?;
    }
    Ok(())
}

// TODO: Add some more general format for flags/build config
fn compile_saber_file<T: Write>(file_name: &str, debug_output: Option<T>) -> Result<SaberProgram> {
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
    let runtime_types = typechecker.generate_runtime_type_info();

    for error in &program_t.errors {
        let diagnostic: Diagnostic<()> = error.into();
        term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
    }

    let mut mir_compiler = MirCompiler::new(typechecker);
    let program = mir_compiler.compile_program(program_t);

    mir_compiler.print_functions();
    let wasm_backend = WasmBackend::new(mir_compiler);
    let wasm_bytes = wasm_backend.generate_program(program);

    Ok(SaberProgram {
        wasm_bytes,
        runtime_types,
    })
}
