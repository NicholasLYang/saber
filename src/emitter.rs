use crate::types::Result;
use ast::Type;
use byteorder::{LittleEndian, WriteBytesExt};
use std::convert::TryInto;
use std::fs::File;
use std::io::prelude::*;
use wasm::{ExportEntry, FunctionBody, FunctionType, OpCode, WasmType};

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
    #[fail(
        display = "INTERNAL: Index is larger than 32 bit integer. Should have been caught earlier"
    )]
    IndexTooLarge,
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
        leb128::write::unsigned(&mut self.file, usize_to_u64(self.buffer.len())?)?;
        self.flush_buffer()
    }

    pub fn emit_prelude(&mut self) -> Result<()> {
        self.emit_code(OpCode::MagicNumber)?;
        self.emit_code(OpCode::Version)
    }

    pub fn emit_types_section(&mut self, types: Vec<FunctionType>) -> Result<()> {
        self.emit_code(OpCode::SectionId(1))?;
        // Start with a count for number of type definitions
        let mut opcodes = vec![OpCode::Count(types.len().try_into().unwrap())];
        for type_ in types {
            opcodes.push(OpCode::Type(WasmType::Function));
            opcodes.push(OpCode::Count(usize_to_u32(type_.param_types.len())?));
            for param in &type_.param_types {
                opcodes.push(OpCode::Type(param.clone()));
            }
            match type_.return_type {
                Some(t) => {
                    opcodes.push(OpCode::Count(1));
                    opcodes.push(OpCode::Type(t));
                }
                None => {
                    opcodes.push(OpCode::Count(0));
                }
            }
        }
        self.write_section(opcodes)
    }

    pub fn emit_function_section(&mut self, function_type_indices: Vec<u32>) -> Result<()> {
        self.emit_code(OpCode::SectionId(3))?;
        let mut opcodes = vec![OpCode::Count(usize_to_u32(function_type_indices.len())?)];
        for index in function_type_indices {
            opcodes.push(OpCode::Index(index));
        }
        self.write_section(opcodes)
    }

    pub fn emit_exports_section(&mut self, exports: Vec<ExportEntry>) -> Result<()> {
        self.emit_code(OpCode::SectionId(7))?;
        let mut opcodes = vec![OpCode::Count(usize_to_u32(exports.len())?)];
        for entry in exports {
            opcodes.push(OpCode::Count(entry.field_str.len().try_into().unwrap()));
            opcodes.push(OpCode::Name(entry.field_str));
            opcodes.push(OpCode::Kind(entry.kind.into()));
            opcodes.push(OpCode::Index(entry.index));
        }
        self.write_section(opcodes)
    }

    pub fn emit_code_section(&mut self, _bodies: Vec<FunctionBody>) -> Result<()> {
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

// Dumb but it works
fn usize_to_u64(i: usize) -> Result<u64> {
    match i.try_into() {
        Ok(i) => Ok(i),
        Err(_) => Err(EmitError::IndexTooLarge)?,
    }
}

fn usize_to_u32(i: usize) -> Result<u32> {
    match i.try_into() {
        Ok(i) => Ok(i),
        Err(_) => Err(EmitError::IndexTooLarge)?,
    }
}
