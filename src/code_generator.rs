use ast::{Name, Op, Type, TypedExpr, TypedStmt, Value};
use im::hashmap::HashMap;
use std::convert::TryInto;
use std::sync::Arc;
use utils::SymbolTable;
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
    #[fail(display = "Cannot have () as param type")]
    EmptyParamType,
    #[fail(display = "Could not infer type var {:?}", type_)]
    CouldNotInfer { type_: Arc<Type> },
    #[fail(display = "Not implemented yet!")]
    NotImplemented,
    #[fail(display = "Cannot return at top level")]
    TopLevelReturn,
    #[fail(display = "Cannot destructure function binding")]
    DestructureFunctionBinding,
    #[fail(display = "Cannot find variable with name '{}'", name)]
    UndefinedVar { name: String },
    #[fail(display = "Somehow you have 2^32 args...")]
    TooManyArgs,
    #[fail(display = "Cannot convert type {} to type {}", t1, t2)]
    CannotConvert { t1: Type, t2: Type },
}

pub struct CodeGenerator {
    /// Counter for function generation
    function_index: u32,
    symbol_table: SymbolTable,
    // Map from var name to index in local_variables
    var_indices: HashMap<Name, usize>,
    // The local variables for current function.
    local_variables: Vec<WasmType>,
}

type Result<T> = std::result::Result<T, GenerationError>;

impl CodeGenerator {
    pub fn new(symbol_table: SymbolTable) -> Self {
        CodeGenerator {
            function_index: 0,
            symbol_table,
            var_indices: HashMap::new(),
            local_variables: Vec::new(),
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
                    scope_index: _,
                    params,
                    body,
                    param_type,
                    return_type,
                } = expr
                {
                    return self.generate_function_binding(
                        name,
                        params,
                        return_type,
                        param_type,
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
        name: &Name,
        params: &Vec<(Name, Arc<Type>)>,
        return_type: &Arc<Type>,
        param_type: &Arc<Type>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, ExportEntry, u32)> {
        let (type_, body, index) = self.generate_function(return_type, params, body)?;
        let name = self.symbol_table.get_str(name);
        let entry = ExportEntry {
            field_str: name.as_bytes().to_vec(),
            kind: ExternalKind::Function,
            index,
        };
        self.var_indices = HashMap::new();
        self.local_variables = Vec::new();
        Ok((type_, body, entry, index))
    }

    pub fn generate_function(
        &mut self,
        return_type: &Arc<Type>,
        params: &Vec<(Name, Arc<Type>)>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody, u32)> {
        let function_type = self.generate_function_type(return_type, params)?;
        let function_body = self.generate_function_body(body, &function_type)?;
        let function_index = self.function_index;
        self.function_index += 1;
        Ok((function_type, function_body, function_index))
    }

    // Generates function type. Also inserts params into local_variables.
    // Not sure it should do both but w/e it's here.
    fn generate_function_type(
        &mut self,
        return_type: &Arc<Type>,
        params: &Vec<(Name, Arc<Type>)>,
    ) -> Result<FunctionType> {
        let mut wasm_param_types = Vec::new();
        for (param_name, param_type) in params {
            let wasm_type = self
                .generate_wasm_type(param_type)?
                .ok_or(GenerationError::EmptyParamType)?;
            self.local_variables.push(wasm_type.clone());
            self.var_indices
                .insert(*param_name, self.local_variables.len() - 1);
            wasm_param_types.push(wasm_type);
        }
        println!("RETURN: {:?}", return_type);
        let return_type = self.generate_wasm_type(&return_type)?;
        println!("WASM RETURN: {:?}", return_type);
        Ok(FunctionType {
            param_types: wasm_param_types,
            return_type,
        })
    }

    fn generate_wasm_type(&self, sbr_type: &Arc<Type>) -> Result<Option<WasmType>> {
        match &**sbr_type {
            Type::Unit => Ok(None),
            Type::Int | Type::Bool | Type::Char => Ok(Some(WasmType::i32)),
            Type::Float => Ok(Some(WasmType::f32)),
            Type::String
            | Type::Array(_)
            | Type::Arrow(_, _)
            | Type::Record(_)
            | Type::Tuple(_) => Ok(Some(WasmType::i32)),
            Type::Var(_) => Err(GenerationError::CouldNotInfer {
                type_: sbr_type.clone(),
            })?,
        }
    }

    fn get_params_index(&mut self, var: &Name) -> Result<usize> {
        Ok(*self
            .var_indices
            .get(var)
            .ok_or(GenerationError::UndefinedVar {
                name: self.symbol_table.get_str(var).to_string(),
            })?)
    }

    fn generate_stmt(&mut self, stmt: &TypedStmt) -> Result<Vec<OpCode>> {
        match stmt {
            TypedStmt::Return(expr) => self.generate_expr(expr),
            TypedStmt::Block(stmts) => {
                let mut opcodes = Vec::new();
                for stmt in stmts {
                    opcodes.append(&mut self.generate_stmt(stmt)?);
                }
                Ok(opcodes)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn generate_function_body(
        &mut self,
        body: &TypedStmt,
        func_type: &FunctionType,
    ) -> Result<FunctionBody> {
        let code = self.generate_stmt(body)?;
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
        for local_type in &self.local_variables {
            let index = match local_type {
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

    fn get_conversion(&self, from_type: &Arc<Type>, to_type: &Arc<Type>) -> Result<OpCode> {
        match (&**from_type, &**to_type) {
            (Type::Int, Type::Float) => Ok(OpCode::F32ConvertI32),
            (t1, t2) => Err(GenerationError::CannotConvert {
                t1: t1.clone(),
                t2: t2.clone(),
            }),
        }
    }

    fn generate_expr(&mut self, expr: &TypedExpr) -> Result<Vec<OpCode>> {
        match expr {
            TypedExpr::Primary { value, type_: _ } => Ok(vec![self.generate_primary(value)?]),
            TypedExpr::Var { name, type_: _ } => {
                let index = self.get_params_index(&name)?;
                Ok(vec![OpCode::GetLocal(index.try_into().unwrap())])
            }
            TypedExpr::BinOp {
                op,
                lhs,
                rhs,
                type_,
            } => {
                let mut lhs_ops = self.generate_expr(lhs)?;
                let lhs_type = lhs.get_type();
                if &lhs_type != type_ {
                    lhs_ops.push(self.get_conversion(&lhs_type, type_)?)
                }
                let mut rhs_ops = self.generate_expr(rhs)?;
                let rhs_type = rhs.get_type();
                if &rhs_type != type_ {
                    rhs_ops.push(self.get_conversion(&rhs_type, type_)?)
                }
                lhs_ops.append(&mut rhs_ops);
                lhs_ops.push(self.generate_operator(&op, &type_)?);
                Ok(lhs_ops)
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
