use crate::types::Result;
use ast::Type;
use byteorder::{LittleEndian, WriteBytesExt};
use opcodes::{OpCode, WasmType};
use std::fs::File;
use std::io::prelude::*;
use wasm::FunctionType;

pub struct Emitter {
    file: File,
}

#[derive(Debug, Fail, PartialEq)]
pub enum EmitError {
    #[fail(
        display = "INTERNAL: Invalid function type. Type {:?} was not an Arrow",
        type_
    )]
    InvalidFunctionType { type_: Type },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
}

static MAGIC_NUM: u32 = 0x6d736100;
static VERSION: u32 = 0x1;

impl Emitter {
    pub fn new(file: File) -> Emitter {
        Emitter { file }
    }

    pub fn emit_code(&mut self, op_code: OpCode) -> Result<()> {
        let bytecode = match op_code {
            OpCode::MagicNum => self.file.write_u32::<LittleEndian>(MAGIC_NUM),
            OpCode::Version => self.file.write_u32::<LittleEndian>(VERSION),
            OpCode::SectionId(id) => self.file.write_u8(id),
            OpCode::SectionPayloadLength(len) => self.file.write_u32::<LittleEndian>(len),
            OpCode::SectionNameLength(len) => self.file.write_u32::<LittleEndian>(len),
            OpCode::ValueType(type_) => {
                let code = match type_ {
                    WasmType::i32 => 0x7f,
                    WasmType::i64 => 0x7e,
                    WasmType::f32 => 0x7d,
                    WasmType::f64 => 0x7c,
                    WasmType::AnyFunction => 0x70,
                    WasmType::Function => 0x60,
                };
                self.file.write_u8(code)
            }
            OpCode::SectionName(name) => {
                self.file.write(&name)?;
                Ok(())
            }
            OpCode::ParamCount(count) => self.file.write_u32::<LittleEndian>(count),
            _ => Ok(()),
        }?;
        Ok(())
    }

    pub fn emit_preamble(&mut self) -> Result<()> {
        self.emit_code(OpCode::MagicNum)?;
        self.emit_code(OpCode::Version)?;
        Ok(())
    }

    pub fn emit_function_section(&mut self) -> Result<()> {
        self.emit_code(OpCode::SectionId(1))
    }

    pub fn emit_function_types(&mut self, types: Vec<FunctionType>) -> Result<()> {
        for type_ in types {
            self.emit_code(OpCode::ValueType(WasmType::Function))?;
            self.emit_code(OpCode::ParamCount(type_.param_types.len() as u32))?;
        }
        Ok(())
    }
}
