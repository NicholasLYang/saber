use ast::{Op, Pat, Type, TypedExpr, TypedStmt, Value};
use std::sync::Arc;
use types::Result;
use wasm::{ExportEntry, ExternalKind, FunctionBody, FunctionType, OpCode, WasmType};

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
    #[fail(display = "Cannot return at top level")]
    TopLevelReturn,
    #[fail(display = "Cannot destructure function binding")]
    DestructureFunctionBinding,
}

pub struct CodeGenerator {
    /// Counter for function generation
    function_index: u32,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator { function_index: 0 }
    }

    pub fn generate_top_level_stmt(
        &mut self,
        stmt: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, ExportEntry)> {
        match stmt {
            TypedStmt::Asgn(pat, expr) => {
                // If the rhs is a function, we want to generate a
                // function binding (FunctionType, FunctionBody, and
                // ExportEntry)
                if let TypedExpr::Function {
                    env,
                    params,
                    body,
                    type_,
                } = expr
                {
                    return self.generate_function_binding(pat, type_, body);
                }
                Err(GenerationError::NotImplemented)?
            }
            TypedStmt::Return(_) => Err(GenerationError::TopLevelReturn)?,
            _ => Err(GenerationError::NotImplemented)?,
        }
    }

    pub fn generate_function_binding(
        &mut self,
        pat: &Pat,
        type_: &Arc<Type>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, ExportEntry)> {
        let (type_, body, index) = self.generate_function(type_, body)?;
        if let Pat::Id(name, _) = pat {
            let entry = ExportEntry {
                field_str: name.as_bytes().to_vec(),
                kind: ExternalKind::Function,
                index,
            };
            Ok((type_, body, entry))
        } else {
            Err(GenerationError::DestructureFunctionBinding)?
        }
    }

    pub fn generate_function(
        &mut self,
        type_: &Arc<Type>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, u32)> {
        let function_type = self.generate_function_type(type_)?;
        let function_body = self.generate_function_body(body)?;
        let function_index = self.function_index;
        self.function_index += 1;
        Ok((function_type, function_body, function_index))
    }

    fn generate_function_type(&self, func_type: &Arc<Type>) -> Result<FunctionType> {
        if let Type::Arrow(params_type, return_type) = &**func_type {
            let param_types = self.convert_params_type(params_type);
            let return_type = self.generate_return_type(&return_type)?;
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

    fn generate_return_type(&self, return_type: &Arc<Type>) -> Result<Option<WasmType>> {
        match &**return_type {
            Type::Unit => Ok(None),
            Type::Int | Type::Bool | Type::Char => Ok(Some(WasmType::i32)),
            Type::Float => Ok(Some(WasmType::f32)),
            Type::String
            | Type::Array(_)
            | Type::Arrow(_, _)
            | Type::Record(_)
            | Type::Tuple(_) => Ok(Some(WasmType::i32)),
            Type::Var(_) => Err(GenerationError::CouldNotInfer {
                type_: return_type.clone(),
            })?,
        }
    }

    fn convert_params_type(&self, params_type: &Type) -> Vec<WasmType> {
        match params_type {
            Type::Unit => Vec::new(),
            Type::Int | Type::Bool | Type::Char => vec![WasmType::i32],
            Type::Float => vec![WasmType::f32],
            // Boxed types get converted to pointers
            Type::String | Type::Var(_) | Type::Array(_) | Type::Arrow(_, _) => vec![WasmType::i32],
            Type::Record(entries) => entries
                .iter()
                .flat_map(|(_name, type_)| self.convert_params_type(type_))
                .collect::<Vec<WasmType>>(),
            Type::Tuple(params) => params
                .iter()
                .flat_map(|param| self.convert_params_type(param))
                .collect::<Vec<WasmType>>(),
        }
    }

    fn generate_function_body(&self, body: &TypedStmt) -> Result<FunctionBody> {
        let code = match body {
            TypedStmt::Return(expr) => self.generate_expr(expr)?,
            _ => Vec::new(),
        };
        Ok(FunctionBody {
            locals: Vec::new(),
            code,
        })
    }

    fn generate_expr(&self, expr: &TypedExpr) -> Result<Vec<OpCode>> {
        match expr {
            TypedExpr::Primary { value, type_ } => Ok(vec![self.generate_primary(value)?]),
            TypedExpr::BinOp {
                op,
                lhs,
                rhs,
                type_,
            } => {
                let mut ops = self.generate_expr(lhs)?;
                let mut rhs_ops = self.generate_expr(rhs)?;
                ops.append(&mut rhs_ops);
                ops.push(self.generate_operator(&op, &type_)?);
                Ok(ops)
            }
            _ => Err(GenerationError::NotImplemented)?,
        }
    }

    fn generate_primary(&self, value: &Value) -> Result<OpCode> {
        match value {
            Value::Float(f) => Ok(OpCode::F32Const(*f)),
            Value::Integer(i) => Ok(OpCode::I32Const(*i)),
            _ => Err(GenerationError::UnsupportedValue)?,
        }
    }

    fn generate_operator(&self, op: &Op, type_: &Type) -> Result<OpCode> {
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
}
