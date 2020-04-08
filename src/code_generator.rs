use ast::{Name, Op, Type, TypedExpr, TypedStmt, Value};
use im::hashmap::HashMap;
use std::convert::TryInto;
use std::sync::Arc;
use symbol_table::{SymbolTable, SymbolTableEntry};
use utils::NameTable;
use wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, LocalEntry, OpCode, ProgramData,
    WasmType,
};

#[derive(Debug, Fail, PartialEq)]
pub enum GenerationError {
    #[fail(
        display = "Operator '{}' from {} to {} does not exist",
        op, input_type, result_type
    )]
    InvalidOperator {
        op: Op,
        input_type: Type,
        result_type: Type,
    },
    #[fail(display = "Function '{}' not defined", name)]
    FunctionNotDefined { name: String },
    #[fail(display = "Unsupported value, probably string")]
    UnsupportedValue,
    #[fail(display = "Cannot have () as type")]
    EmptyType,
    #[fail(display = "Could not infer type var {:?}", type_)]
    CouldNotInfer { type_: Arc<Type> },
    #[fail(display = "Code Generator: Not implemented yet!")]
    NotImplemented,
    #[fail(display = "Not reachable")]
    NotReachable,
    #[fail(display = "Cannot return at top level")]
    TopLevelReturn,
    #[fail(display = "Cannot find variable with name '{}'", name)]
    UndefinedVar { name: String },
    #[fail(display = "Cannot convert type {} to type {}", t1, t2)]
    CannotConvert { t1: Type, t2: Type },
    #[fail(display = "Cannot export value")]
    ExportValue,
}

pub struct CodeGenerator {
    symbol_table: SymbolTable,
    name_table: NameTable,
    var_indices: HashMap<Name, usize>,
    // The local variables for current function.
    local_variables: Vec<WasmType>,
    // Parameter count. Important for computing offsets and generating
    // local_entries
    param_count: usize,
    // All the generated code
    program_data: ProgramData,
}

type Result<T> = std::result::Result<T, GenerationError>;

impl CodeGenerator {
    pub fn new(name_table: NameTable, symbol_table: SymbolTable) -> Self {
        let func_count = symbol_table.get_function_index();
        CodeGenerator {
            symbol_table,
            name_table,
            var_indices: HashMap::new(),
            local_variables: Vec::new(),
            param_count: 0,
            program_data: ProgramData::new(func_count),
        }
    }

    pub fn generate_program(mut self, program: Vec<TypedStmt>) -> Result<ProgramData> {
        for stmt in program {
            (&mut self).generate_top_level_stmt(&stmt)?;
        }
        Ok(self.program_data)
    }

    pub fn generate_top_level_stmt(&mut self, stmt: &TypedStmt) -> Result<()> {
        match stmt {
            TypedStmt::Function {
                name,
                params,
                params_type: _,
                return_type,
                body,
                scope,
            } => {
                self.param_count = 0;
                self.local_variables = Vec::new();
                self.generate_function_binding(*name, *scope, params, return_type, body)
            },
            TypedStmt::Return(_) => {
                Err(GenerationError::TopLevelReturn)?
            },
            TypedStmt::Export(func_name) => {
                let name_str = self.name_table.get_str(func_name);
                let sym_entry = self
                    .symbol_table
                    .lookup_name(func_name)
                    .ok_or(GenerationError::FunctionNotDefined {
                        name: name_str.to_string(),
                    })?;
                if let SymbolTableEntry::Function {
                    index,
                    params_type: _,
                    return_type: _,
                } = sym_entry
                {
                    let entry = ExportEntry {
                        field_str: name_str.as_bytes().to_vec(),
                        kind: ExternalKind::Function,
                        index: (*index).try_into().unwrap(),
                    };
                    self.program_data.exports_section.push(entry);
                    Ok(())
                } else {
                    Err(GenerationError::ExportValue)
                }
            }
            _ => Err(GenerationError::NotImplemented)?,
        }
    }

    pub fn generate_function_binding(
        &mut self,
        name: Name,
        scope: usize,
        params: &Vec<(Name, Arc<Type>)>,
        return_type: &Arc<Type>,
        body: &TypedStmt,
    ) -> Result<()> {
        let entry = self
            .symbol_table
            .lookup_name(&name)
            .unwrap();
        let index = if let SymbolTableEntry::Function {
            index,
            params_type: _,
            return_type: _,
        } = entry
        {
            *index
        } else {
            return Err(GenerationError::NotReachable)
        };
        let old_scope = self.symbol_table.set_scope(scope);
        let (type_, body) = self.generate_function(return_type, params, body)?;
        self.program_data.type_section[index] = Some(type_);
        self.program_data.code_section.push(body);
        self.program_data.function_section[index] = index;
        self.symbol_table.set_scope(old_scope);
        Ok(())
    }

    pub fn generate_function(
        &mut self,
        return_type: &Arc<Type>,
        params: &Vec<(Name, Arc<Type>)>,
        body: &TypedStmt,
    ) -> Result<(FunctionType, FunctionBody)> {
        let return_type = self.generate_wasm_type(&return_type)?;
        let function_type = self.generate_function_type(&return_type, params)?;
        let function_body = self.generate_function_body(body, return_type)?;
        Ok((function_type, function_body))
    }

    // Generates function type. Also inserts params into local_variables.
    // Not sure it should do both but w/e it's here.
    fn generate_function_type(
        &mut self,
        return_type: &Option<WasmType>,
        params: &Vec<(Name, Arc<Type>)>,
    ) -> Result<FunctionType> {
        let mut wasm_param_types = Vec::new();
        for (param_name, param_type) in params {
            let wasm_type = self
                .generate_wasm_type(param_type)?
                .ok_or(GenerationError::EmptyType)?;
            self.local_variables.push(wasm_type.clone());
            self.var_indices
                .insert(*param_name, self.local_variables.len() - 1);
            self.param_count += 1;
            wasm_param_types.push(wasm_type);
        }
        Ok(FunctionType {
            param_types: wasm_param_types,
            return_type: return_type.clone(),
        })
    }

    fn generate_function_body(
        &mut self,
        body: &TypedStmt,
        return_type: Option<WasmType>,
    ) -> Result<FunctionBody> {
        let code = self.generate_stmt(body, true, &return_type)?;
        let mut locals = Vec::new();
        // We want to generate only the locals not params so we skip
        // to after the params
        for local_type in &self.local_variables[self.param_count..] {
            locals.push(LocalEntry {
                count: 1,
                type_: local_type.clone(),
            })
        }
        Ok(FunctionBody { locals, code })
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
                name: self.name_table.get_str(var).to_string(),
            })?)
    }

    fn generate_stmt(
        &mut self,
        stmt: &TypedStmt,
        is_last: bool,
        return_type: &Option<WasmType>,
    ) -> Result<Vec<OpCode>> {
        match stmt {
            TypedStmt::Function {
                name,
                params,
                params_type: _,
                return_type,
                body,
                scope,
            } => {
                let mut old_param_count = 0;
                let mut old_local_variables = Vec::new();
                let mut old_var_indices = HashMap::new();
                std::mem::swap(&mut old_param_count, &mut self.param_count);
                std::mem::swap(&mut old_local_variables, &mut self.local_variables);
                std::mem::swap(&mut old_var_indices, &mut self.var_indices);
                self.generate_function_binding(*name, *scope, params, return_type, body)?;
                std::mem::swap(&mut old_param_count, &mut self.param_count);
                std::mem::swap(&mut old_local_variables, &mut self.local_variables);
                std::mem::swap(&mut old_var_indices, &mut self.var_indices);
                Ok(Vec::new())
            },
            TypedStmt::Return(expr) => {
                if is_last {
                    self.generate_expr(expr)
                } else {
                    let mut opcodes = self.generate_expr(expr)?;
                    opcodes.push(OpCode::Return);
                    Ok(opcodes)
                }
            }
            TypedStmt::If(cond, then_block, else_block) => {
                // Start with the cond opcodes
                let mut opcodes = self.generate_expr(cond)?;
                opcodes.push(OpCode::If);
                opcodes.push(OpCode::Type(WasmType::Empty));
                // For now we say false because allowing the blocks to
                // implicitly return by leaving on the stack means we have to
                // change the block return type.
                opcodes.append(&mut self.generate_stmt(then_block, false, return_type)?);
                if let Some(else_block) = else_block {
                    opcodes.push(OpCode::Else);
                    opcodes.append(&mut self.generate_stmt(else_block, false, return_type)?);
                }
                opcodes.push(OpCode::End);
                // If this is the last instruction and the function is
                // supposed to return a value, push on an unreachable
                if let Some(_) = return_type {
                    if is_last {
                        opcodes.push(OpCode::Unreachable)
                    }
                }
                Ok(opcodes)
            }
            TypedStmt::Asgn(name, expr) => {
                let wasm_type = self
                    .generate_wasm_type(&expr.get_type())?
                    .ok_or(GenerationError::EmptyType)?;
                self.local_variables.push(wasm_type);
                let local_index = self.local_variables.len() - 1;
                self.var_indices.insert(*name, local_index);
                let mut opcodes = self.generate_expr(expr)?;
                opcodes.push(OpCode::SetLocal(local_index.try_into().unwrap()));
                Ok(opcodes)
            }
            TypedStmt::Block(stmts) => {
                let mut opcodes = Vec::new();
                for (i, stmt) in stmts.iter().enumerate() {
                    opcodes.append(&mut self.generate_stmt(
                        stmt,
                        is_last && i == stmts.len() - 1,
                        return_type,
                    )?);
                }
                Ok(opcodes)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn promote_types(
        &self,
        type1: &Arc<Type>,
        type2: &Arc<Type>,
        ops1: &mut Vec<OpCode>,
        ops2: &mut Vec<OpCode>,
    ) -> Result<Arc<Type>> {
        if type1 == type2 {
            return Ok(type1.clone());
        }
        match (&**type1, &**type2) {
            (Type::Int, Type::Float) => {
                ops1.push(OpCode::F32ConvertI32);
                Ok(Arc::new(Type::Float))
            }
            (Type::Float, Type::Int) => {
                ops2.push(OpCode::F32ConvertI32);
                Ok(Arc::new(Type::Float))
            }
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
            TypedExpr::Call {
                callee,
                args,
                type_: _,
            } => {
                if let TypedExpr::Var { name, type_: _ } = &**callee {
                    let mut opcodes = Vec::new();
                    let entry = self.symbol_table.lookup_name(name).ok_or(
                        GenerationError::FunctionNotDefined {
                            name: self.name_table.get_str(name).to_string(),
                        },
                    )?;
                    let index = if let SymbolTableEntry::Function { index, params_type: _, return_type: _} = entry {
                        *index
                    }  else {
                        // Should not be reachable since this shouldn't typecheck in the first place
                        return Err(GenerationError::NotReachable)
                    };
                    opcodes.append(&mut self.generate_expr(args)?);
                    opcodes.push(OpCode::Call((index).try_into().unwrap()));
                    Ok(opcodes)
                } else {
                    Err(GenerationError::NotImplemented)
                }
            }
            TypedExpr::BinOp {
                op,
                lhs,
                rhs,
                type_,
            } => {
                let mut lhs_ops = self.generate_expr(lhs)?;
                let lhs_type = lhs.get_type();
                let mut rhs_ops = self.generate_expr(rhs)?;
                let rhs_type = rhs.get_type();

                let promoted_type =
                    self.promote_types(&lhs_type, &rhs_type, &mut lhs_ops, &mut rhs_ops)?;
                lhs_ops.append(&mut rhs_ops);
                lhs_ops.push(self.generate_operator(&op, &type_, &promoted_type)?);
                Ok(lhs_ops)
            }
            _ => Err(GenerationError::NotImplemented)?,
        }
    }

    fn generate_primary(&self, value: &Value) -> Result<OpCode> {
        match value {
            Value::Float(f) => Ok(OpCode::F32Const(*f)),
            Value::Integer(i) => Ok(OpCode::I32Const(*i)),
            Value::Bool(b) => {
                if *b {
                    Ok(OpCode::I32Const(1))
                } else {
                    Ok(OpCode::I32Const(0))
                }
            }
            _ => Err(GenerationError::UnsupportedValue)?,
        }
    }

    fn generate_operator(
        &self,
        op: &Op,
        result_type: &Type,
        input_type: &Arc<Type>,
    ) -> Result<OpCode> {
        match (op, &**input_type, result_type) {
            (Op::Plus, Type::Int, Type::Int) => Ok(OpCode::I32Add),
            (Op::Minus, Type::Int, Type::Int) => Ok(OpCode::I32Sub),
            (Op::Plus, Type::Float, Type::Float) => Ok(OpCode::F32Add),
            (Op::Minus, Type::Float, Type::Float) => Ok(OpCode::F32Sub),
            (Op::Times, Type::Int, Type::Int) => Ok(OpCode::I32Mul),
            (Op::Div, Type::Int, Type::Int) => Ok(OpCode::I32Div),
            (Op::Times, Type::Float, Type::Float) => Ok(OpCode::F32Mul),
            (Op::Div, Type::Float, Type::Float) => Ok(OpCode::F32Div),
            (Op::Greater, Type::Int, Type::Bool) => Ok(OpCode::I32GreaterSigned),
            (Op::EqualEqual, Type::Int, Type::Bool) => Ok(OpCode::I32Eq),
            _ => Err(GenerationError::InvalidOperator {
                op: op.clone(),
                input_type: (&**input_type).clone(),
                result_type: result_type.clone(),
            })?,
        }
    }
}
