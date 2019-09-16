use crate::ast::{Type, TypedExpr};
use crate::types::Result;
use std::convert::TryInto;
use std::sync::Arc;

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

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub struct Section {
    id: u8,
    payload_len: u32,
    name_len: Option<u32>,
    name: Option<String>,
    payload_data: PayloadData,
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
pub enum PayloadData {
    TypeSection(Vec<FunctionType>),
    ImportSection(Vec<ImportEntry>),
    FunctionSection(Vec<u32>),
    TableSection(Vec<TableType>),
    MemorySection(Vec<MemoryType>),
    GlobalSection(Vec<(GlobalType, Vec<ByteCode>)>),
    ExportSection(Vec<ExportEntry>),
    CodeSection(Vec<FunctionBody>),
    DataSection(Vec<DataSegment>),
}

#[derive(Debug)]
pub struct DataSegment {
    index: u32,
    offset: Vec<ByteCode>,
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
    locals: Vec<LocalEntry>,
    code: Vec<ByteCode>,
}

#[derive(Debug)]
pub struct LocalEntry {
    count: u32,
    type_: WasmType,
}

#[derive(Debug)]
pub enum ByteCode {
    Const { type_: WasmType },
}

#[derive(Debug, Fail)]
pub enum WasmError {
    #[fail(
        display = "INTERNAL: Invalid function type. Type {:?} was not an Arrow",
        type_
    )]
    InvalidFunctionType { type_: Arc<Type> },
    #[fail(display = "Could not infer type var {:?}", type_)]
    CouldNotInfer { type_: Arc<Type> },
}

fn type_to_value_type(type_: &Arc<Type>) -> Result<Option<WasmType>> {
    match &**type_ {
        Type::Unit => Ok(None),
        Type::Int | Type::Bool | Type::Char => Ok(Some(WasmType::i32)),
        Type::Float => Ok(Some(WasmType::f32)),
        Type::String | Type::Array(_) | Type::Arrow(_, _) | Type::Record(_) | Type::Tuple(_) => {
            Ok(Some(WasmType::i32))
        }
        Type::Var(_) => Err(WasmError::CouldNotInfer {
            type_: type_.clone(),
        })?,
    }
}

pub fn generate_function_type(func_type: &Arc<Type>) -> Result<FunctionType> {
    if let Type::Arrow(params_type, return_type) = &**func_type {
        let param_types = convert_params_type(params_type);
        let return_type: Result<Option<WasmType>> = type_to_value_type(&return_type);
        Ok(FunctionType {
            param_types,
            return_type: return_type?,
        })
    } else {
        Err(WasmError::InvalidFunctionType {
            type_: func_type.clone(),
        })?
    }
}

fn convert_params_type(params_type: &Type) -> Vec<WasmType> {
    match params_type {
        Type::Unit => Vec::new(),
        Type::Int | Type::Bool | Type::Char => vec![WasmType::i32],
        Type::Float => vec![WasmType::f32],
        // Boxed types get converted to pointers
        Type::String | Type::Var(_) | Type::Array(_) | Type::Arrow(_, _) => vec![WasmType::i32],
        Type::Record(entries) => entries
            .iter()
            .flat_map(|(_name, type_)| convert_params_type(type_))
            .collect::<Vec<WasmType>>(),
        Type::Tuple(params) => params
            .iter()
            .flat_map(|param| convert_params_type(param))
            .collect::<Vec<WasmType>>(),
    }
}
