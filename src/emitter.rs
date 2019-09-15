use crate::types::Result;
use ast::Type;
use byteorder::{LittleEndian, WriteBytesExt};
use opcodes::{OpCode, WasmType};
use std::convert::TryInto;
use std::fs::File;
use std::io::prelude::*;
use wasm::FunctionType;

pub struct Emitter {
    file: File,
    buffer: Vec<u8>,
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
        Emitter {
            file,
            buffer: Vec::new(),
        }
    }

    pub fn emit_code(&mut self, op_code: OpCode) -> Result<()> {
        let bytecode = match op_code {
            OpCode::MagicNumber => self.buffer.write_u32::<LittleEndian>(MAGIC_NUM),
            OpCode::Version => self.buffer.write_u32::<LittleEndian>(VERSION),
            OpCode::SectionId(code) => {
                leb128::write::unsigned(&mut self.buffer, code.into())?;
                Ok(())
            }
            OpCode::Count(i) | OpCode::Index(i) => {
                leb128::write::unsigned(&mut self.buffer, i.into())?;
                Ok(())
            }
            OpCode::Type(type_) => {
                leb128::write::unsigned(&mut self.buffer, type_.into())?;
                Ok(())
            }
            OpCode::Name(bytes) | OpCode::Code(bytes) => self.buffer.write_all(&bytes),
            OpCode::Kind(kind) => self.buffer.write_u8(kind),
            OpCode::I32Const(val) => {
                self.buffer.write_u8(0x41);
                self.buffer.write_i32::<LittleEndian>(val)
            }
        }?;
        Ok(())
    }

    pub fn flush_buffer(&mut self) -> Result<()> {
        // Flush buffer
        self.file.write_all(&self.buffer);
        self.buffer = Vec::new();
        Ok(())
    }

    pub fn write_section(&mut self, opcodes: Vec<OpCode>) -> Result<()> {
        self.flush_buffer()?;
        for opcode in opcodes {
            self.emit_code(opcode)?;
        }
        leb128::write::unsigned(&mut self.file, self.buffer.len().try_into().unwrap())?;
        self.flush_buffer()
    }

    pub fn emit_prelude(&mut self) -> Result<()> {
        self.emit_code(OpCode::MagicNumber)?;
        self.emit_code(OpCode::Version)
    }

    pub fn emit_types_section(&mut self, _types: Vec<FunctionType>) -> Result<()> {
        self.emit_code(OpCode::SectionId(1))?;
        // Number of type entries to come
        self.write_section(vec![
            OpCode::Count(1),
            OpCode::Type(WasmType::Function),
            // Param counts
            OpCode::Count(0),
            // Return counts
            OpCode::Count(1),
            OpCode::Type(WasmType::i32),
        ])
    }

    pub fn emit_function_section(&mut self) -> Result<()> {
        self.emit_code(OpCode::SectionId(3))?;
        // Num of bytes
        self.write_section(vec![OpCode::Count(1), OpCode::Index(0)])
    }

    pub fn emit_exports_section(&mut self) -> Result<()> {
        let bytes = "main".to_string().into_bytes();
        self.emit_code(OpCode::SectionId(7))?;
        // Num of bytes
        self.write_section(vec![
            // Num of exports
            OpCode::Count(1),
            OpCode::Count(bytes.len().try_into().unwrap()),
            OpCode::Name(bytes),
            OpCode::Kind(0),
            OpCode::Index(0),
        ])
    }

    pub fn emit_code_section(&mut self) -> Result<()> {
        self.emit_code(OpCode::SectionId(10))?;
        self.flush_buffer()?;
        self.emit_code(OpCode::Count(1))?;
        let mut code_body = Vec::new();
        leb128::write::unsigned(&mut code_body, 0)?;
        code_body.write_u8(0x41)?;
        leb128::write::signed(&mut code_body, 125)?;
        code_body.write_u8(0x0b)?;
        self.emit_code(OpCode::Count(code_body.len().try_into().unwrap()))?;
        self.emit_code(OpCode::Code(code_body));
        leb128::write::unsigned(&mut self.file, self.buffer.len().try_into().unwrap())?;
        self.flush_buffer()
    }
}
