extern crate failure;

#[macro_use]
extern crate failure_derive;
extern crate byteorder;
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
use crate::wasm::{ExportEntry, ExternalKind, FunctionBody, FunctionType, OpCode, WasmType};
use ast::{Op, Pat, Type, TypedExpr, TypedStmt, Value};
use code_generator::CodeGenerator;
use im::hashmap::HashMap;
use std::env;
use std::fs::File;
use std::io;
use std::io::Write;
use std::sync::Arc;

mod ast;
mod code_generator;
mod emitter;
mod lexer;
mod parser;
mod typechecker;
mod types;
mod wasm;

fn make_types_section() -> Vec<FunctionType> {
    vec![FunctionType {
        param_types: vec![],
        return_type: Some(WasmType::f32),
    }]
}

fn make_exports_section() -> Vec<ExportEntry> {
    vec![ExportEntry {
        field_str: "main".to_string().into_bytes(),
        kind: ExternalKind::Function,
        index: 0,
    }]
}

fn make_code_section() -> Vec<FunctionBody> {
    vec![FunctionBody {
        locals: Vec::new(),
        code: vec![OpCode::F32Const(1.25)],
    }]
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        run_repl()
    } else {
        read_file()
    }
}

#[test]
fn test_emitter(
    types: Vec<FunctionType>,
    bodies: Vec<FunctionBody>,
    entries: Vec<ExportEntry>,
) -> Result<()> {
    let file = File::create("build/out.wasm")?;
    let mut emitter = Emitter::new(file);
    emitter.emit_prelude()?;
    emitter.emit_types_section(types)?;
    emitter.emit_function_section(vec![0])?;
    emitter.emit_exports_section(entries)?;
    emitter.emit_code_section(bodies)?;
    Ok(())
}

#[test]
fn test_code_generator() -> Result<(FunctionType, FunctionBody, ExportEntry)> {
    let mut generator = CodeGenerator::new();
    let lhs = Box::new(TypedExpr::Primary {
        value: Value::Integer(10),
        type_: Arc::new(Type::Int),
    });
    let rhs = Box::new(TypedExpr::Primary {
        value: Value::Integer(15),
        type_: Arc::new(Type::Int),
    });
    let body = TypedStmt::Return(TypedExpr::BinOp {
        lhs,
        rhs,
        op: Op::Minus,
        type_: Arc::new(Type::Int),
    });
    let type_ = Type::Arrow(Arc::new(Type::Unit), Arc::new(Type::Int));
    let func_expr = TypedExpr::Function {
        env: HashMap::new(),
        params: Pat::Id("a".into(), None),
        body: Box::new(body),
        type_: Arc::new(type_),
    };
    let binding = TypedStmt::Asgn(Pat::Id("main".into(), None), func_expr);
    generator.generate_top_level_stmt(&binding)
}

fn read_file() -> Result<()> {
    println!("READ FILE");
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
