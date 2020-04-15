use ast::{ExprT, Loc, Name, Op, StmtT, Type, Value};
use im::hashmap::HashMap;
use std::convert::TryInto;
use std::sync::Arc;
use symbol_table::{SymbolTable, SymbolTableEntry};
use utils::NameTable;
use wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, ImportEntry, ImportKind, LocalEntry,
    OpCode, ProgramData, TableType, WasmType,
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

    fn generate_default_imports(&mut self) -> Result<()> {
        let print_type = FunctionType {
            param_types: vec![WasmType::i32],
            return_type: None,
        };
        let print_id = self
            .name_table
            .get_id(&"print".to_string())
            .expect("Print must be defined");
        let print_entry = self
            .symbol_table
            .lookup_name_in_scope(*print_id, 0)
            .expect("Print is not in symbol table");
        match print_entry {
            SymbolTableEntry::Function {
                index,
                params_type: _,
                return_type: _,
            } => {
                self.program_data.insert_type(print_type);
                self.program_data.import_section.push(ImportEntry {
                    module_str: "std".into(),
                    field_str: "print".into(),
                    kind: ImportKind::Function { type_: *index },
                });
                Ok(())
            }
            _ => Err(GenerationError::NotReachable),
        }
    }

    pub fn generate_program(mut self, program: Vec<Loc<StmtT>>) -> Result<ProgramData> {
        for stmt in program {
            (&mut self).generate_top_level_stmt(&stmt.inner)?;
        }
        self.generate_default_imports()?;
        Ok(self.program_data)
    }

    pub fn generate_top_level_stmt(&mut self, stmt: &StmtT) -> Result<()> {
        match stmt {
            StmtT::Function {
                name,
                params,
                params_type: _,
                return_type,
                body,
                scope,
            } => {
                self.param_count = 0;
                self.local_variables = Vec::new();
                self.generate_function_binding(*name, *scope, params, return_type, &(*body).inner)?;
                Ok(())
            }
            StmtT::Return(_) => Err(GenerationError::TopLevelReturn),
            StmtT::Export(func_name) => {
                let name_str = self.name_table.get_str(func_name);
                let sym_entry = self.symbol_table.lookup_name(*func_name).ok_or(
                    GenerationError::FunctionNotDefined {
                        name: name_str.to_string(),
                    },
                )?;
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
            s => {
                println!("{:?}", s);
                Err(GenerationError::NotImplemented)
            }
        }
    }

    pub fn generate_function_binding(
        &mut self,
        name: Name,
        scope: usize,
        params: &[(Name, Arc<Type>)],
        return_type: &Arc<Type>,
        body: &StmtT,
    ) -> Result<usize> {
        let entry = self.symbol_table.lookup_name_in_scope(name, scope).unwrap();
        let index = if let SymbolTableEntry::Function {
            index,
            params_type: _,
            return_type: _,
        } = entry
        {
            *index
        } else {
            return Err(GenerationError::NotReachable);
        };
        let old_scope = self.symbol_table.set_scope(scope);
        let (type_, body) = self.generate_function(return_type, params, body)?;
        self.program_data.insert_type(type_);
        self.program_data.code_section.push(body);
        self.program_data.function_section[index] = Some(index);
        self.symbol_table.set_scope(old_scope);
        Ok(index)
    }

    pub fn generate_function(
        &mut self,
        return_type: &Arc<Type>,
        params: &[(Name, Arc<Type>)],
        body: &StmtT,
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
        params: &[(Name, Arc<Type>)],
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
        body: &StmtT,
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

    fn get_args_type(&self, arg_type: &Arc<Type>) -> Result<Vec<WasmType>> {
        if let Type::Tuple(elems) = &**arg_type {
            let mut wasm_types = Vec::new();
            for elem in elems {
                wasm_types.append(&mut self.get_args_type(elem)?);
            }
            Ok(wasm_types)
        } else {
            let wasm_type = match self.generate_wasm_type(arg_type)? {
                Some(t) => vec![t],
                None => Vec::new(),
            };
            Ok(wasm_type)
        }
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
            }),
        }
    }

    fn get_params_index(&mut self, var: Name) -> Result<usize> {
        Ok(*self
            .var_indices
            .get(&var)
            .ok_or(GenerationError::UndefinedVar {
                name: self.name_table.get_str(&var).to_string(),
            })?)
    }

    fn generate_stmt(
        &mut self,
        stmt: &StmtT,
        is_last: bool,
        return_type: &Option<WasmType>,
    ) -> Result<Vec<OpCode>> {
        match stmt {
            StmtT::Function {
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
                self.generate_function_binding(*name, *scope, params, return_type, &(*body).inner)?;
                std::mem::swap(&mut old_param_count, &mut self.param_count);
                std::mem::swap(&mut old_local_variables, &mut self.local_variables);
                std::mem::swap(&mut old_var_indices, &mut self.var_indices);
                Ok(Vec::new())
            }
            StmtT::Expr(expr) => {
                let mut opcodes = self.generate_expr(&expr.inner)?;
                if expr.inner.get_type() != Arc::new(Type::Unit) {
                    opcodes.push(OpCode::Drop);
                }
                Ok(opcodes)
            }
            StmtT::Return(expr) => {
                if is_last {
                    self.generate_expr(&expr.inner)
                } else {
                    let mut opcodes = self.generate_expr(&expr.inner)?;
                    opcodes.push(OpCode::Return);
                    Ok(opcodes)
                }
            }
            StmtT::If(cond, then_block, else_block) => {
                // Start with the cond opcodes
                let mut opcodes = self.generate_expr(&cond.inner)?;
                opcodes.push(OpCode::If);
                opcodes.push(OpCode::Type(WasmType::Empty));
                // For now we say false because allowing the blocks to
                // implicitly return by leaving on the stack means we have to
                // change the block return type.
                opcodes.append(&mut self.generate_stmt(&then_block.inner, false, return_type)?);
                if let Some(else_block) = else_block {
                    opcodes.push(OpCode::Else);
                    opcodes.append(&mut self.generate_stmt(
                        &else_block.inner,
                        false,
                        return_type,
                    )?);
                }
                opcodes.push(OpCode::End);
                // If this is the last instruction and the function is
                // supposed to return a value, push on an unreachable
                if return_type.is_some() && is_last {
                    opcodes.push(OpCode::Unreachable)
                }
                Ok(opcodes)
            }
            StmtT::Asgn(name, expr) => {
                let wasm_type = self
                    .generate_wasm_type(&expr.inner.get_type())?
                    .ok_or(GenerationError::EmptyType)?;
                self.local_variables.push(wasm_type);
                let local_index = self.local_variables.len() - 1;
                self.var_indices.insert(*name, local_index);
                let mut opcodes = self.generate_expr(&expr.inner)?;
                opcodes.push(OpCode::SetLocal(local_index.try_into().unwrap()));
                Ok(opcodes)
            }
            StmtT::Block(stmts) => {
                let mut opcodes = Vec::new();
                for (i, stmt) in stmts.iter().enumerate() {
                    opcodes.append(&mut self.generate_stmt(
                        &stmt.inner,
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

    fn generate_expr(&mut self, expr: &ExprT) -> Result<Vec<OpCode>> {
        match expr {
            ExprT::Primary { value, type_: _ } => Ok(vec![self.generate_primary(value)?]),
            ExprT::Var { name, type_: _ } => {
                let index = self.get_params_index(*name)?;
                Ok(vec![OpCode::GetLocal(index.try_into().unwrap())])
            }
            ExprT::Call {
                callee,
                args,
                type_,
            } => {
                if let ExprT::Var { name, type_: _ } = &callee.inner {
                    let mut opcodes = Vec::new();
                    let entry = self.symbol_table.lookup_name(*name).ok_or(
                        GenerationError::FunctionNotDefined {
                            name: self.name_table.get_str(&name).to_string(),
                        },
                    )?;
                    let index = if let SymbolTableEntry::Function {
                        index,
                        params_type: _,
                        return_type: _,
                    } = entry
                    {
                        *index
                    } else {
                        // Should not be reachable since this shouldn't typecheck in the first place
                        return Err(GenerationError::NotReachable);
                    };
                    opcodes.append(&mut self.generate_expr(&args.inner)?);
                    opcodes.push(OpCode::Call((index).try_into().unwrap()));
                    Ok(opcodes)
                } else {
                    println!("args: {:?}", args);
                    println!("callee: {:?}", callee);
                    println!("type_: {:?}", type_);
                    let args_wasm_type = self.get_args_type(&args.inner.get_type())?;
                    let return_wasm_type = self.generate_wasm_type(&type_)?;
                    let func_type = FunctionType {
                        param_types: args_wasm_type,
                        return_type: return_wasm_type,
                    };
                    let type_sig_index = self.program_data.insert_type(func_type);
                    let mut opcodes = self.generate_expr(&args.inner)?;
                    opcodes.append(&mut self.generate_expr(&callee.inner)?);
                    opcodes.push(OpCode::CallIndirect(type_sig_index.try_into().unwrap()));
                    Ok(opcodes)
                }
            }
            ExprT::Function {
                params,
                params_type,
                return_type,
                body,
                name,
                scope_index,
            } => {
                let mut old_param_count = 0;
                let mut old_local_variables = Vec::new();
                let mut old_var_indices = HashMap::new();
                std::mem::swap(&mut old_param_count, &mut self.param_count);
                std::mem::swap(&mut old_local_variables, &mut self.local_variables);
                std::mem::swap(&mut old_var_indices, &mut self.var_indices);
                let index = self.generate_function_binding(
                    *name,
                    *scope_index,
                    params,
                    return_type,
                    &(*body).inner,
                )?;
                std::mem::swap(&mut old_param_count, &mut self.param_count);
                std::mem::swap(&mut old_local_variables, &mut self.local_variables);
                std::mem::swap(&mut old_var_indices, &mut self.var_indices);
                self.program_data.elements_section.elems.push(index);
                let table_index = self.program_data.elements_section.elems.len() - 1;
                Ok(vec![OpCode::I32Const(table_index.try_into().unwrap())])
            }
            ExprT::Tuple(elems, _) => {
                if elems.len() == 0 {
                    Ok(Vec::new())
                } else {
                    Err(GenerationError::NotImplemented)
                }
            }
            ExprT::BinOp {
                op,
                lhs,
                rhs,
                type_,
            } => {
                let mut lhs_ops = self.generate_expr(&lhs.inner)?;
                let lhs_type = lhs.inner.get_type();
                let mut rhs_ops = self.generate_expr(&rhs.inner)?;
                let rhs_type = rhs.inner.get_type();

                let promoted_type =
                    self.promote_types(&lhs_type, &rhs_type, &mut lhs_ops, &mut rhs_ops)?;
                lhs_ops.append(&mut rhs_ops);
                lhs_ops.push(self.generate_operator(&op, &type_, &promoted_type)?);
                Ok(lhs_ops)
            }
            _ => Err(GenerationError::NotImplemented),
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
            _ => Err(GenerationError::UnsupportedValue),
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
            }),
        }
    }
}
