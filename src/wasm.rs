use std::fmt;

#[derive(Clone, Debug)]
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
    Bool(bool),
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Eq,
    I32GreaterSigned,
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
    If,
    Else,
    Return,
    Call(u32),
    CallIndirect(u32),
    Unreachable,
    Drop,
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
    Empty,
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
                WasmType::Empty => "Wasm Empty",
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
            WasmType::Empty => 0x40,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub param_types: Vec<WasmType>,
    pub return_type: Option<WasmType>,
}

#[derive(Debug)]
pub struct ImportEntry {
    pub module_str: Vec<u8>,
    pub field_str: Vec<u8>,
    pub kind: ImportKind,
}

#[derive(Debug)]
pub enum ImportKind {
    Function { type_: usize },
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
    pub function_section: Vec<Option<usize>>,
    pub memory_section: Vec<MemoryType>,
    pub global_section: Vec<(GlobalType, Vec<OpCode>)>,
    pub exports_section: Vec<ExportEntry>,
    // Right now we only have one elem segment
    pub elements_section: ElemSegment,
    pub code_section: Vec<Option<FunctionBody>>,
    pub data_section: Vec<DataSegment>,
}

impl ProgramData {
    pub fn new(func_count: usize) -> Self {
        ProgramData {
            type_section: Vec::new(),
            import_section: Vec::new(),
            function_section: vec![None; func_count],
            memory_section: Vec::new(),
            global_section: Vec::new(),
            exports_section: Vec::new(),
            elements_section: ElemSegment {
                offset: vec![OpCode::I32Const(0), OpCode::End],
                elems: Vec::new(),
            },
            code_section: vec![None; func_count],
            data_section: Vec::new(),
        }
    }

    pub fn insert_type(&mut self, func_type: FunctionType) -> usize {
        for (i, type_) in self.type_section.iter().enumerate() {
            if &func_type == type_ {
                return i;
            }
        }
        self.type_section.push(func_type);
        self.type_section.len() - 1
    }
}

#[derive(Debug)]
pub struct ElemSegment {
    pub offset: Vec<OpCode>,
    // List of function indices
    pub elems: Vec<usize>,
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

#[derive(Clone, Debug)]
pub struct FunctionBody {
    pub locals: Vec<LocalEntry>,
    pub code: Vec<OpCode>,
}

#[derive(Clone, Debug)]
pub struct LocalEntry {
    pub count: u32,
    pub type_: WasmType,
}
