use crate::types::Result;
use byteorder::{LittleEndian, WriteBytesExt};
use opcodes::OpCode;
use std::fs::File;
use std::io::prelude::*;

pub struct Emitter {
    file: File,
}

static MAGIC_NUM: u32 = 0x6d736100;
static VERSION: u32 = 0x1;

impl Emitter {
    pub fn new(file: File) -> Emitter {
        Emitter { file }
    }

    pub fn emit_code(&mut self, opCode: OpCode) -> Result<()> {
        let bytecode = match opCode {
            OpCode::MagicNum => self.file.write_u32::<LittleEndian>(MAGIC_NUM),
            OpCode::Version => self.file.write_u32::<LittleEndian>(VERSION),
            OpCode::SectionId(id) => self.file.write_u8(id),
            OpCode::SectionPayloadLength(len) => self.file.write_u32::<LittleEndian>(len),
            OpCode::SectionNameLength(len) => self.file.write_u32::<LittleEndian>(len),
            OpCode::SectionName(name) => {
                self.file.write(&name)?;
                Ok(())
            }
            _ => Ok(()),
        }?;

        Ok(())
    }
}
