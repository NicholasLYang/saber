use ast::{Name, Op, Pat, Type, TypedExpr, TypedStmt, Value};
use std::sync::Arc;
use types::Result;
use wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, LocalEntry, OpCode, ProgramData,
    WasmType,
};

#[derive(Debug, Fail, PartialEq)]
pub enum GenerationError {
    #[fail(display = "Operator '{}' is not valid for type {}", op, type_)]
    InvalidOperator { op: Op, type_: Type },
    #[fail(display = "Local variable cannot have type: {}", type_)]
    InvalidLocal { type_: WasmType },
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
    #[fail(display = "Cannot find variable with name '{}'", name)]
    UndefinedVar { name: Name },
}

pub struct CodeGenerator {
    /// Counter for function generation
    function_index: u32,
    function_param: Option<Name>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            function_index: 0,
            function_param: None,
        }
    }

    pub fn generate_program(&mut self, program: Vec<TypedStmt>) -> Result<ProgramData> {
        let mut program_data = ProgramData::new();
        for stmt in program {
            let (type_, body, entry, index) = self.generate_top_level_stmt(&stmt)?;
            program_data.type_section.push(type_);
            program_data.code_section.push(body);
            program_data.exports_section.push(entry);
            program_data.function_section.push(index);
        }
        Ok(program_data)
    }

    pub fn generate_top_level_stmt(
        &mut self,
        stmt: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, ExportEntry, u32)> {
        match stmt {
            TypedStmt::Asgn(name, expr) => {
                // If the rhs is a function, we want to generate a
                // function binding (FunctionType, FunctionBody, and
                // ExportEntry)
                if let TypedExpr::Function {
                    env: _,
                    param,
                    body,
                    param_type,
                    return_type,
                } = expr
                {
                    return self.generate_function_binding(
                        name,
                        param,
                        param_type,
                        return_type,
                        body,
                    );
                }
                Err(GenerationError::NotImplemented)?
            }
            TypedStmt::Return(_) => Err(GenerationError::TopLevelReturn)?,
            _ => Err(GenerationError::NotImplemented)?,
        }
    }

    pub fn generate_function_binding(
        &mut self,
        name: &str,
        param: &Name,
        return_type: &Arc<Type>,
        param_type: &Arc<Type>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, ExportEntry, u32)> {
        self.function_param = Some(param.to_string());
        let (type_, body, index) = self.generate_function(return_type, param_type, body)?;
        let entry = ExportEntry {
            field_str: name.as_bytes().to_vec(),
            kind: ExternalKind::Function,
            index,
        };
        Ok((type_, body, entry, index))
    }

    pub fn generate_function(
        &mut self,
        return_type: &Arc<Type>,
        param_type: &Arc<Type>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, u32)> {
        let function_type = self.generate_function_type(return_type, param_type)?;
        let function_body = self.generate_function_body(body, &function_type)?;
        let function_index = self.function_index;
        self.function_index += 1;
        Ok((function_type, function_body, function_index))
    }

    fn generate_function_type(
        &self,
        return_type: &Arc<Type>,
        param_type: &Arc<Type>,
    ) -> Result<FunctionType> {
        let wasm_param_types = self.convert_params_type(param_type);
        let return_type = self.generate_return_type(&return_type)?;
        Ok(FunctionType {
            param_types: wasm_param_types,
            return_type,
        })
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

    fn get_params_index(&mut self, var: &Name) -> Result<Option<u32>> {
        Ok(Some(0))
    }

    fn generate_function_body(
        &mut self,
        body: &TypedStmt,
        func_type: &FunctionType,
    ) -> Result<FunctionBody> {
        let code = match body {
            TypedStmt::Return(expr) => self.generate_expr(expr)?,
            _ => Vec::new(),
        };
        let mut locals = vec![
            LocalEntry {
                count: 0,
                type_: WasmType::i32,
            },
            LocalEntry {
                count: 0,
                type_: WasmType::i64,
            },
            LocalEntry {
                count: 0,
                type_: WasmType::f32,
            },
            LocalEntry {
                count: 0,
                type_: WasmType::f64,
            },
        ];
        for param in &func_type.param_types {
            let index = match param {
                WasmType::i32 => 0,
                WasmType::i64 => 1,
                WasmType::f32 => 2,
                WasmType::f64 => 3,
                type_ => {
                    return Err((GenerationError::InvalidLocal {
                        type_: type_.clone(),
                    })
                    .into())
                }
            };
            locals[index].count += 1;
        }
        Ok(FunctionBody { locals, code })
    }

    fn generate_expr(&mut self, expr: &TypedExpr) -> Result<Vec<OpCode>> {
        match expr {
            TypedExpr::Primary { value, type_: _ } => Ok(vec![self.generate_primary(value)?]),
            TypedExpr::Var { name, type_: _ } => {
                let index = self.get_params_index(&name)?;
                match index {
                    Some(i) => Ok(vec![OpCode::GetLocal(i)]),
                    None => Err((GenerationError::UndefinedVar { name: name.clone() }).into()),
                }
            }
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
            (Op::Times, Type::Int) => Ok(OpCode::I32Mul),
            (Op::Div, Type::Int) => Ok(OpCode::I32Div),
            (Op::Times, Type::Float) => Ok(OpCode::F32Mul),
            (Op::Div, Type::Float) => Ok(OpCode::F32Div),
            _ => Err(GenerationError::InvalidOperator {
                op: op.clone(),
                type_: type_.clone(),
            })?,
        }
    }
}
