pub enum OpCode {
    // Preamble codes
    MagicNum,
    Version,
    // Section codes
    SectionId(u8),
    SectionPayloadLength(u32),
    SectionNameLength(u32),
    SectionName(Vec<u8>),
    // Type section
    TypesCount(u32),
    // Function Type Signatures
    FunctionForm(u8),
    ParamCount(u32),
    ValueType(WasmValue),

    // Function section
    FunctionCount(u32),
    Types(Vec<u32>),
}

pub enum WasmValue {
    i32,
    i64,
    f32,
    f64,
    AnyFunction,
    Function,
}
