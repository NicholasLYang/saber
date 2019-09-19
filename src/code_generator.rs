use ast::{Op, Type, TypedExpr, TypedStmt, Value};
use std::sync::Arc;
use types::Result;
use wasm::{FunctionBody, FunctionType, OpCode, WasmType};

#[derive(Debug, Fail, PartialEq)]
pub enum GenerationError {
    #[fail(display = "Operator '{}' is not valid for type {}", op, type_)]
    InvalidOperator { op: Op, type_: Type },
    #[fail(display = "Unsupported value, probably string")]
    UnsupportedValue,
    #[fail(
        display = "INTERNAL: Invalid function type. Type {:?} was not an Arrow",
        type_
    )]
    InvalidFunctionType { type_: Arc<Type> },
    #[fail(display = "Could not infer type var {:?}", type_)]
    CouldNotInfer { type_: Arc<Type> },
    #[fail(display = "Not implemented yet!")]
    NotImplemented,
}

fn generate_return_type(return_type: &Arc<Type>) -> Result<Option<WasmType>> {
    match &**return_type {
        Type::Unit => Ok(None),
        Type::Int | Type::Bool | Type::Char => Ok(Some(WasmType::i32)),
        Type::Float => Ok(Some(WasmType::f32)),
        Type::String | Type::Array(_) | Type::Arrow(_, _) | Type::Record(_) | Type::Tuple(_) => {
            Ok(Some(WasmType::i32))
        }
        Type::Var(_) => Err(GenerationError::CouldNotInfer {
            type_: return_type.clone(),
        })?,
    }
}

pub fn generate_function_type(func_type: &Arc<Type>) -> Result<FunctionType> {
    if let Type::Arrow(params_type, return_type) = &**func_type {
        let param_types = convert_params_type(params_type);
        let return_type = generate_return_type(&return_type)?;
        Ok(FunctionType {
            param_types,
            return_type,
        })
    } else {
        Err(GenerationError::InvalidFunctionType {
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

pub fn generate_function(
    type_: &Arc<Type>,
    body: &TypedStmt,
) -> Result<(FunctionType, FunctionBody)> {
    let function_type = generate_function_type(type_)?;
    let function_body = generate_function_body(body)?;
    Ok((function_type, function_body))
}

/// parent_function is the function inside which the current function
/// is being defined
pub fn generate_function_body(body: &TypedStmt) -> Result<FunctionBody> {
    let code = match body {
        TypedStmt::Return(expr) => generate_expr(expr)?,
        _ => Vec::new(),
    };
    Ok(FunctionBody {
        locals: Vec::new(),
        code,
    })
}

fn generate_expr(expr: &TypedExpr) -> Result<Vec<OpCode>> {
    match expr {
        TypedExpr::Primary { value, type_ } => Ok(vec![generate_primary(value)?]),
        TypedExpr::BinOp {
            op,
            lhs,
            rhs,
            type_,
        } => {
            let mut ops = generate_expr(lhs)?;
            let mut rhs_ops = generate_expr(rhs)?;
            ops.append(&mut rhs_ops);
            ops.push(generate_operator(&op, &type_)?);
            Ok(ops)
        }
        _ => Err(GenerationError::NotImplemented)?,
    }
}

fn generate_primary(value: &Value) -> Result<OpCode> {
    match value {
        Value::Float(f) => Ok(OpCode::F32Const(*f)),
        Value::Integer(i) => Ok(OpCode::I32Const(*i)),
        _ => Err(GenerationError::UnsupportedValue)?,
    }
}

fn generate_operator(op: &Op, type_: &Type) -> Result<OpCode> {
    match (op, type_) {
        (Op::Plus, Type::Int) => Ok(OpCode::I32Add),
        (Op::Minus, Type::Int) => Ok(OpCode::I32Sub),
        (Op::Plus, Type::Float) => Ok(OpCode::F32Add),
        (Op::Minus, Type::Float) => Ok(OpCode::F32Sub),
        _ => Err(GenerationError::InvalidOperator {
            op: op.clone(),
            type_: type_.clone(),
        })?,
    }
}
