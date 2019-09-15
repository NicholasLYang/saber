pub enum OpCode {
    MagicNumber,
    Version,
    SectionId(u8),
    Count(u32),
    Index(u32),
    Type(WasmType),
    Name(Vec<u8>),
    Kind(u8),
    Code(Vec<u8>),
    // Block,
    // Loop,
    // If,
    // Else,p
    // End,
    // Break,
    // BreakIf,
    // BreakTable,
    // Return,
    // Call,
    // CallIndirect,
    I32Const(i32),
}

pub enum WasmType {
    #[allow(non_camel_case_types)]
    i32,
    #[allow(non_camel_case_types)]
    i64,
    #[allow(non_camel_case_types)]
    f32,
    #[allow(non_camel_case_types)]
    f64,
    AnyFunction,
    Function,
}

impl From<WasmType> for u64 {
    fn from(wasm_type: WasmType) -> Self {
        match wasm_type {
            WasmType::i32 => 0x7f,
            WasmType::i64 => 0x7e,
            WasmType::f32 => 0x7d,
            WasmType::f64 => 0x7c,
            WasmType::AnyFunction => 0x70,
            WasmType::Function => 0x60,
        }
    }
}
