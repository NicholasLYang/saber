use std::fmt;

#[derive(Debug)]
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
    End,
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    I32Const(i32),
    F32Const(f32),
    // Convert signed 32 bit integer to 32 bit float
    F32ConvertI32,
    SetLocal(u32),
    GetLocal(u32),
    Return,
}

#[derive(Debug, Clone, PartialEq)]
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

impl fmt::Display for WasmType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                WasmType::i32 => "Wasm i32",
                WasmType::i64 => "Wasm i64",
                WasmType::f32 => "Wasm f32",
                WasmType::f64 => "Wasm f64",
                WasmType::AnyFunction => "Wasm AnyFunction",
                WasmType::Function => "Wasm Function",
            }
        )
    }
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

#[derive(Debug)]
pub struct FunctionType {
    pub param_types: Vec<WasmType>,
    pub return_type: Option<WasmType>,
}

#[derive(Debug)]
pub struct ImportEntry {
    module_len: u32,
    module_str: String,
    field_len: u32,
    field_str: String,
    kind: ImportKind,
}

#[derive(Debug)]
pub enum ImportKind {
    Function { type_: u32 },
    Table { type_: TableType },
    Memory { type_: MemoryType },
    Global { type_: GlobalType },
}

#[derive(Debug)]
pub struct TableType {
    // element_type is always any
    limits: ResizableLimits,
}

#[derive(Debug)]
pub struct MemoryType {
    limits: ResizableLimits,
}

#[derive(Debug)]
pub struct ResizableLimits(bool, u32, Option<u32>);

#[derive(Debug)]
pub struct GlobalType {
    content_type: WasmType,
    mutability: bool,
}

#[derive(Debug)]
pub struct ProgramData {
    pub type_section: Vec<FunctionType>,
    pub import_section: Vec<ImportEntry>,
    pub function_section: Vec<u32>,
    pub table_section: Vec<TableType>,
    pub memory_section: Vec<MemoryType>,
    pub global_section: Vec<(GlobalType, Vec<OpCode>)>,
    pub exports_section: Vec<ExportEntry>,
    pub code_section: Vec<FunctionBody>,
    pub data_section: Vec<DataSegment>,
}

impl ProgramData {
    pub fn new() -> Self {
        ProgramData {
            type_section: Vec::new(),
            import_section: Vec::new(),
            function_section: Vec::new(),
            table_section: Vec::new(),
            memory_section: Vec::new(),
            global_section: Vec::new(),
            exports_section: Vec::new(),
            code_section: Vec::new(),
            data_section: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct DataSegment {
    index: u32,
    offset: Vec<OpCode>,
    size: u32,
    data: Vec<u8>,
}

#[derive(Debug)]
pub enum ExternalKind {
    Function,
    Table,
    Memory,
    Global,
}

impl From<ExternalKind> for u8 {
    fn from(kind: ExternalKind) -> u8 {
        match kind {
            ExternalKind::Function => 0,
            ExternalKind::Table => 1,
            ExternalKind::Memory => 2,
            ExternalKind::Global => 3,
        }
    }
}

#[derive(Debug)]
pub struct ExportEntry {
    pub field_str: Vec<u8>,
    pub kind: ExternalKind,
    pub index: u32,
}

#[derive(Debug)]
pub struct FunctionBody {
    pub locals: Vec<LocalEntry>,
    pub code: Vec<OpCode>,
}

#[derive(Debug)]
pub struct LocalEntry {
    pub count: u32,
    pub type_: WasmType,
}
