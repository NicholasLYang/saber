use crate::types::Result;
use crate::wasm::{
    ElemSegment, ExportEntry, FunctionBody, FunctionType, GlobalType, ImportEntry, ImportKind,
    OpCode, ProgramData, WasmType,
};
use byteorder::{LittleEndian, WriteBytesExt};
use std::convert::TryInto;
use std::io::prelude::*;

pub struct Emitter {
    output: Vec<u8>,
    buffer: Vec<u8>,
}

#[derive(Debug, Fail, PartialEq)]
pub enum EmitError {
    #[fail(
        display = "INTERNAL: Index is larger than 32 bit integer. Should have been caught earlier"
    )]
    IndexTooLarge,
}

static MAGIC_NUM: u32 = 0x6d73_6100;
static VERSION: u32 = 0x1;

pub fn emit_code<T: Write>(mut dest: T, op_code: OpCode) -> Result<()> {
    match op_code {
        OpCode::MagicNumber => dest.write_u32::<LittleEndian>(MAGIC_NUM),
        OpCode::Version => dest.write_u32::<LittleEndian>(VERSION),
        OpCode::SectionId(code) => {
            leb128::write::unsigned(&mut dest, code.into())?;
            Ok(())
        }
        OpCode::Count(i) | OpCode::Index(i) => {
            leb128::write::unsigned(&mut dest, i.into())?;
            Ok(())
        }
        OpCode::Type(type_) => {
            leb128::write::unsigned(&mut dest, type_.into())?;
            Ok(())
        }
        OpCode::Name(bytes) | OpCode::Code(bytes) => dest.write_all(&bytes),
        OpCode::Kind(kind) => dest.write_u8(kind),
        OpCode::Bool(b) => dest.write_u8(b as u8),
        OpCode::I32Const(val) => {
            dest.write_u8(0x41)?;
            leb128::write::signed(&mut dest, val.into())?;
            Ok(())
        }
        OpCode::F32Const(val) => {
            dest.write_u8(0x43)?;
            dest.write_u32::<LittleEndian>(val.to_bits())?;
            Ok(())
        }
        OpCode::I32Add => dest.write_u8(0x6a),
        OpCode::I32Sub => dest.write_u8(0x6b),
        OpCode::I32Mul => dest.write_u8(0x6c),
        OpCode::I32Div => dest.write_u8(0x6d),
        OpCode::I32Xor => dest.write_u8(0x73),
        OpCode::I32Eq => dest.write_u8(0x46),
        OpCode::I32GreaterSigned => dest.write_u8(0x4a),
        //OpCode::I32GreaterUnsigned => dest.write_u8(0x4b),
        OpCode::F32Add => dest.write_u8(0x92),
        OpCode::F32Sub => dest.write_u8(0x93),
        OpCode::F32Mul => dest.write_u8(0x94),
        OpCode::F32Div => dest.write_u8(0x95),
        OpCode::F32ConvertI32 => dest.write_u8(0xb2),
        OpCode::GetLocal(i) => {
            dest.write_u8(0x20)?;
            leb128::write::unsigned(&mut dest, i.into())?;
            Ok(())
        }
        OpCode::SetLocal(i) => {
            dest.write_u8(0x21)?;
            leb128::write::unsigned(&mut dest, i.into())?;
            Ok(())
        }
        OpCode::GetGlobal(i) => {
            dest.write_u8(0x23)?;
            leb128::write::unsigned(&mut dest, i.into())?;
            Ok(())
        }
        OpCode::SetGlobal(i) => {
            dest.write_u8(0x24)?;
            leb128::write::unsigned(&mut dest, i.into())?;
            Ok(())
        }
        OpCode::I32Load(alignment, offset) => {
            dest.write_u8(0x28)?;
            leb128::write::unsigned(&mut dest, alignment.into())?;
            leb128::write::unsigned(&mut dest, offset.into())?;
            Ok(())
        }
        OpCode::F32Load(alignment, offset) => {
            dest.write_u8(0x2a)?;
            leb128::write::unsigned(&mut dest, alignment.into())?;
            leb128::write::unsigned(&mut dest, offset.into())?;
            Ok(())
        }
        OpCode::I32Store(alignment, offset) => {
            dest.write_u8(0x36)?;
            leb128::write::unsigned(&mut dest, alignment.into())?;
            leb128::write::unsigned(&mut dest, offset.into())?;
            Ok(())
        }
        OpCode::F32Store(alignment, offset) => {
            dest.write_u8(0x38)?;
            leb128::write::unsigned(&mut dest, alignment.into())?;
            leb128::write::unsigned(&mut dest, offset.into())?;
            Ok(())
        }
        OpCode::If => dest.write_u8(0x04),
        OpCode::Else => dest.write_u8(0x05),
        OpCode::Return => dest.write_u8(0x0f),
        OpCode::Call(i) => {
            dest.write_u8(0x10)?;
            leb128::write::unsigned(&mut dest, i.into())?;
            Ok(())
        }
        OpCode::CallIndirect(i) => {
            dest.write_u8(0x11)?;
            leb128::write::unsigned(&mut dest, i.into())?;
            dest.write_u8(0x00)
        }
        OpCode::Unreachable => dest.write_u8(0x00),
        OpCode::Drop => dest.write_u8(0x1a),
        OpCode::End => dest.write_u8(0x0b),
        /*        OpCode::GrowMemory => {
            dest.write_u8(0x40)?;
            leb128::write::unsigned(&mut dest, 0)?;
            Ok(())
        }
        OpCode::CurrentMemory => {
            dest.write_u8(0x3f)?;
            leb128::write::unsigned(&mut dest, 0)?;
            Ok(())
        }*/
    }?;
    Ok(())
}

impl Emitter {
    pub fn new() -> Emitter {
        Emitter {
            output: Vec::new(),
            buffer: Vec::new(),
        }
    }

    pub fn emit_program(&mut self, program: ProgramData) -> Result<()> {
        self.emit_prelude()?;
        self.emit_types_section(program.type_section)?;
        self.emit_imports_section(program.import_section)?;
        self.emit_functions_section(program.function_section)?;
        self.emit_table_section(program.elements_section.elems.len())?;
        self.emit_global_section(program.global_section)?;
        self.emit_exports_section(program.exports_section)?;
        self.emit_elements_section(program.elements_section)?;
        self.emit_code_section(program.code_section.into_iter().filter_map(|t| t).collect())?;
        Ok(())
    }

    pub fn emit_code(&mut self, op_code: OpCode) -> Result<()> {
        emit_code(&mut self.buffer, op_code)
    }

    pub fn output_base64(&mut self) -> String {
        base64::encode(&self.output)
    }

    pub fn flush_buffer(&mut self) -> Result<()> {
        // Flush buffer
        self.output.write_all(&self.buffer)?;
        self.buffer = Vec::new();
        Ok(())
    }

    pub fn write_section(&mut self, opcodes: Vec<OpCode>) -> Result<()> {
        self.flush_buffer()?;
        for opcode in opcodes {
            self.emit_code(opcode)?;
        }
        leb128::write::unsigned(&mut self.output, usize_to_u64(self.buffer.len())?)?;
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

    fn emit_imports_section(&mut self, imports: Vec<ImportEntry>) -> Result<()> {
        self.emit_code(OpCode::SectionId(2))?;
        let mut opcodes = vec![OpCode::Count(imports.len().try_into().unwrap())];
        for import in imports {
            opcodes.push(OpCode::Count(usize_to_u32(import.module_str.len())?));
            opcodes.push(OpCode::Name(import.module_str));
            opcodes.push(OpCode::Count(usize_to_u32(import.field_str.len())?));
            opcodes.push(OpCode::Name(import.field_str));
            match import.kind {
                ImportKind::Function { type_ } => {
                    opcodes.push(OpCode::Kind(0));
                    opcodes.push(OpCode::Index(usize_to_u32(type_)?));
                }
                ImportKind::Memory(memory_type) => {
                    opcodes.push(OpCode::Kind(2));
                    let is_max_present = memory_type.limits.1.is_some();
                    opcodes.push(OpCode::Bool(is_max_present));
                    opcodes.push(OpCode::Count(memory_type.limits.0));
                    if let Some(max) = memory_type.limits.1 {
                        opcodes.push(OpCode::Count(max));
                    }
                }
            };
        }
        self.write_section(opcodes)
    }

    pub fn emit_functions_section(
        &mut self,
        function_type_indices: Vec<Option<usize>>,
    ) -> Result<()> {
        let function_type_indices: Vec<_> = function_type_indices
            .iter()
            .filter_map(|t| t.as_ref())
            .collect();
        self.emit_code(OpCode::SectionId(3))?;
        let mut opcodes = vec![OpCode::Count(usize_to_u32(function_type_indices.len())?)];
        for index in function_type_indices {
            opcodes.push(OpCode::Index(usize_to_u32(*index)?));
        }
        self.write_section(opcodes)
    }

    fn emit_table_section(&mut self, initial_length: usize) -> Result<()> {
        self.emit_code(OpCode::SectionId(4))?;
        let opcodes = vec![
            OpCode::Count(1),
            OpCode::Type(WasmType::AnyFunction),
            OpCode::Bool(false),
            OpCode::Count(usize_to_u32(initial_length)?),
        ];
        self.write_section(opcodes)
    }

    fn emit_global_section(&mut self, globals: Vec<(GlobalType, Vec<OpCode>)>) -> Result<()> {
        self.emit_code(OpCode::SectionId(6))?;
        let mut opcodes = vec![OpCode::Count(usize_to_u32(globals.len())?)];
        for (global_type, mut global_opcodes) in globals {
            opcodes.push(OpCode::Type(global_type.content_type));
            opcodes.push(OpCode::Bool(global_type.mutability));
            opcodes.append(&mut global_opcodes);
            opcodes.push(OpCode::End);
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

    fn emit_elements_section(&mut self, mut segment: ElemSegment) -> Result<()> {
        self.emit_code(OpCode::SectionId(9))?;
        let mut opcodes = vec![OpCode::Count(1)]; // Only have one element in section
        opcodes.push(OpCode::Index(0));
        opcodes.append(&mut segment.offset);
        opcodes.push(OpCode::Count(usize_to_u32(segment.elems.len())?));
        for index in segment.elems {
            opcodes.push(OpCode::Index(usize_to_u32(index)?));
        }
        self.write_section(opcodes)
    }

    pub fn emit_code_section(&mut self, bodies: Vec<FunctionBody>) -> Result<()> {
        self.emit_code(OpCode::SectionId(10))?;
        self.flush_buffer()?;
        let mut opcodes = vec![OpCode::Count(usize_to_u32(bodies.len())?)];
        for body in bodies {
            let mut code_body = Vec::new();
            emit_code(
                &mut code_body,
                OpCode::Count(usize_to_u32(body.locals.len())?),
            )?;
            for local in body.locals {
                emit_code(&mut code_body, OpCode::Count(local.count))?;
                emit_code(&mut code_body, OpCode::Type(local.type_))?;
            }
            for opcode in body.code {
                emit_code(&mut code_body, opcode)?;
            }
            emit_code(&mut code_body, OpCode::End)?;
            opcodes.push(OpCode::Count(code_body.len().try_into().unwrap()));
            opcodes.push(OpCode::Code(code_body));
        }
        self.write_section(opcodes)
    }
}

// Dumb but it works
fn usize_to_u64(i: usize) -> Result<u64> {
    match i.try_into() {
        Ok(i) => Ok(i),
        Err(_) => Err(EmitError::IndexTooLarge.into()),
    }
}

fn usize_to_u32(i: usize) -> Result<u32> {
    match i.try_into() {
        Ok(i) => Ok(i),
        Err(_) => Err(EmitError::IndexTooLarge.into()),
    }
}
