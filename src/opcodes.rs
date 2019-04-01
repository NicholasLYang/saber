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
    ValueType(WasmType),

    // Function section
    FunctionCount(u32),
    Types(Vec<u32>),
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
