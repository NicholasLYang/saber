use crate::ast::{Type, TypedExpr};
use crate::types::Result;

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
    pub param_types: Vec<ValueType>,
    pub return_type: Option<ValueType>,
}

#[derive(Debug)]
pub enum ValueType {
    // Also a pointer
    #[allow(non_camel_case_types)]
    i32,
    #[allow(non_camel_case_types)]
    i64,
    #[allow(non_camel_case_types)]
    f32,
    #[allow(non_camel_case_types)]
    f64,
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
    content_type: ValueType,
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
    CodeSection(Vec<FunctionBody>),
}

#[derive(Debug)]
pub struct FunctionBody {
    body_size: u32,
    locals: Vec<LocalEntry>,
    code: Vec<ByteCode>,
}

#[derive(Debug)]
pub struct LocalEntry {
    count: u32,
    type_: ValueType,
}

#[derive(Debug)]
pub enum ByteCode {
    Const { type_: ValueType },
}

#[derive(Debug, Fail)]
pub enum IrError {
    #[fail(
        display = "INTERNAL: Invalid function type. Type {:?} was not an Arrow",
        type_
    )]
    InvalidFunctionType { type_: Type },
    #[fail(display = "Could not infer type var {:?}", type_)]
    CouldNotInfer { type_: Type },
}

impl Type {
    pub fn into_value_type(&self) -> Result<Option<ValueType>> {
        match self {
            Type::Unit => Ok(None),
            Type::Int | Type::Bool | Type::Char => Ok(Some(ValueType::i32)),
            Type::Float => Ok(Some(ValueType::f32)),
            Type::String
            | Type::Array(_)
            | Type::Arrow(_, _)
            | Type::Record(_)
            | Type::Tuple(_) => Ok(Some(ValueType::i32)),
            Type::Var(_) => Err(IrError::CouldNotInfer {
                type_: self.clone(),
            })?,
        }
    }
}
pub fn generate_function_type(func_type: &Type) -> Result<FunctionType> {
    if let Type::Arrow(params_type, return_type) = func_type {
        let param_types = convert_params_type(params_type);
        let return_type = Type::into_value_type(return_type)?;
        Ok(FunctionType {
            param_types,
            return_type,
        })
    } else {
        Err(IrError::InvalidFunctionType {
            type_: func_type.clone(),
        })?
    }
}

fn convert_params_type(params_type: &Type) -> Vec<ValueType> {
    match params_type {
        Type::Unit => Vec::new(),
        Type::Int | Type::Bool | Type::Char => vec![ValueType::i32],
        Type::Float => vec![ValueType::f32],
        // Boxed types get converted to pointers
        Type::String | Type::Var(_) | Type::Array(_) | Type::Arrow(_, _) => vec![ValueType::i32],
        Type::Record(entries) => entries
            .iter()
            .flat_map(|(_name, type_)| convert_params_type(type_))
            .collect::<Vec<ValueType>>(),
        Type::Tuple(params) => params
            .iter()
            .flat_map(|param| convert_params_type(param))
            .collect::<Vec<ValueType>>(),
    }
}
