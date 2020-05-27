use ast::{ExprT, Loc, Name, Op, StmtT, Type, UnaryOp, Value};
use std::convert::TryInto;
use std::sync::Arc;
use symbol_table::{EntryType, SymbolTable};
use utils::NameTable;
use wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, ImportEntry, ImportKind, LocalEntry,
    OpCode, ProgramData, WasmType,
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
    #[fail(display = "Code Generator: Could not infer type var {:?}", type_)]
    CouldNotInfer { type_: Arc<Type> },
    #[fail(display = "Code Generator: Not implemented yet!")]
    NotImplemented,
    #[fail(display = "Code Generator: Not reachable")]
    NotReachable,
    #[fail(display = "Cannot return at top level")]
    TopLevelReturn,
    #[fail(display = "Cannot convert type {} to type {}", t1, t2)]
    CannotConvert { t1: Type, t2: Type },
    #[fail(display = "Cannot export value")]
    ExportValue,
}

pub struct CodeGenerator {
    symbol_table: SymbolTable,
    name_table: NameTable,
    // All the generated code
    program_data: ProgramData,
    return_type: Option<WasmType>,
}

type Result<T> = std::result::Result<T, GenerationError>;

impl CodeGenerator {
    pub fn new(name_table: NameTable, symbol_table: SymbolTable) -> Self {
        let func_count = symbol_table.get_function_index();
        CodeGenerator {
            symbol_table,
            name_table,
            program_data: ProgramData::new(func_count),
            return_type: None,
        }
    }

    fn generate_default_imports(&mut self) -> Result<()> {
        let print_type = FunctionType {
            param_types: vec![WasmType::i32],
            return_type: None,
        };
        let type_index = self.program_data.insert_type(print_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "print".into(),
            kind: ImportKind::Function { type_: type_index },
        });
        Ok(())
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
                local_variables,
                body,
                scope,
            } => {
                self.generate_function_binding(
                    *name,
                    *scope,
                    params,
                    return_type,
                    local_variables,
                    &(*body).inner,
                )?;
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
                if let EntryType::Function {
                    index,
                    params_type: _,
                    return_type: _,
                } = &sym_entry.entry_type
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
            _ => Err(GenerationError::NotImplemented),
        }
    }

    pub fn generate_function_binding(
        &mut self,
        name: Name,
        scope: usize,
        params: &[(Name, Arc<Type>)],
        return_type: &Arc<Type>,
        local_variables: &Vec<Arc<Type>>,
        body: &ExprT,
    ) -> Result<usize> {
        let entry = self.symbol_table.lookup_name_in_scope(name, scope).unwrap();
        let index = if let EntryType::Function {
            index,
            params_type: _,
            return_type: _,
        } = &entry.entry_type
        {
            *index
        } else {
            return Err(GenerationError::NotReachable);
        };
        let old_scope = self.symbol_table.restore_scope(scope);
        let (type_, body) = self.generate_function(return_type, params, local_variables, body)?;
        let type_index = self.program_data.insert_type(type_);
        self.program_data.code_section[index] = Some(body);
        self.program_data.function_section[index] = Some(type_index);
        self.symbol_table.restore_scope(old_scope);
        Ok(index)
    }

    pub fn generate_function(
        &mut self,
        return_type: &Arc<Type>,
        params: &[(Name, Arc<Type>)],
        local_variables: &Vec<Arc<Type>>,
        body: &ExprT,
    ) -> Result<(FunctionType, FunctionBody)> {
        let return_type = self.generate_wasm_type(&return_type)?;
        self.return_type = return_type.clone();
        let function_type = self.generate_function_type(&return_type, params)?;
        let function_body = self.generate_function_body(body, local_variables, params.len())?;
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
        for (_, param_type) in params {
            let wasm_type = self
                .generate_wasm_type(param_type)?
                .ok_or(GenerationError::EmptyType)?;
            wasm_param_types.push(wasm_type);
        }
        Ok(FunctionType {
            param_types: wasm_param_types,
            return_type: return_type.clone(),
        })
    }

    fn generate_function_body(
        &mut self,
        body: &ExprT,
        local_variables: &[Arc<Type>],
        param_count: usize,
    ) -> Result<FunctionBody> {
        let code = self.generate_expr(body)?;
        let mut local_entries = Vec::new();
        // We want to generate only the locals not params so we skip
        // to after the params
        for local_type in &local_variables[param_count..] {
            let wasm_type = self
                .generate_wasm_type(local_type)?
                .ok_or(GenerationError::EmptyType)?;
            local_entries.push(LocalEntry {
                count: 1,
                type_: wasm_type,
            })
        }
        Ok(FunctionBody {
            locals: local_entries,
            code,
        })
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

    fn get_var_index(&mut self, var: Name) -> Result<usize> {
        match self
            .symbol_table
            .lookup_name(var)
            .expect("Variable must be in scope")
            .entry_type
        {
            EntryType::Function {
                index: _,
                params_type: _,
                return_type: _,
            } => Err(GenerationError::NotReachable),
            EntryType::Var { index, var_type: _ } => Ok(index),
        }
    }

    fn generate_stmt(&mut self, stmt: &StmtT, is_last: bool) -> Result<Vec<OpCode>> {
        match stmt {
            StmtT::Function {
                name,
                params,
                params_type: _,
                return_type,
                local_variables,
                body,
                scope,
            } => {
                self.symbol_table.restore_scope(*scope);
                self.generate_function_binding(
                    *name,
                    *scope,
                    params,
                    return_type,
                    local_variables,
                    &(*body).inner,
                )?;
                Ok(Vec::new())
            }
            StmtT::Expr(expr) => {
                let mut opcodes = self.generate_expr(&expr.inner)?;
                if expr.inner.get_type() != Arc::new(Type::Unit) {
                    opcodes.push(OpCode::Drop);
                }
                if is_last {
                    // If the return type is empty, we can safely return
                    if let Some(WasmType::Empty) = self.return_type {
                        Ok(opcodes)
                    } else {
                        // But otherwise, we need to push on an unreachable to assure
                        // wasm that we're not going to just end without returning
                        // anything.
                        // TODO: Actually check if we're returning nothing
                        opcodes.push(OpCode::Unreachable);
                        Ok(opcodes)
                    }
                } else {
                    Ok(opcodes)
                }
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
            StmtT::Asgn(name, expr) => {
                let index = self.get_var_index(*name)?;
                let mut opcodes = self.generate_expr(&expr.inner)?;
                opcodes.push(OpCode::SetLocal(index.try_into().unwrap()));
                Ok(opcodes)
            }
            StmtT::Block(stmts) => {
                let mut opcodes = Vec::new();
                for (i, stmt) in stmts.iter().enumerate() {
                    opcodes.append(
                        &mut self.generate_stmt(&stmt.inner, is_last && i == stmts.len() - 1)?,
                    );
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
                let index = self.get_var_index(*name)?;
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
                    if let EntryType::Function {
                        index,
                        params_type: _,
                        return_type: _,
                    } = &entry.entry_type
                    {
                        let index = *index;
                        opcodes.append(&mut self.generate_expr(&args.inner)?);
                        opcodes.push(OpCode::Call((index).try_into().unwrap()));
                        return Ok(opcodes);
                    };
                }
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
            ExprT::Function {
                params,
                params_type: _,
                return_type,
                body,
                name,
                local_variables,
                scope_index,
            } => {
                self.symbol_table.restore_scope(*scope_index);
                let index = self.generate_function_binding(
                    *name,
                    *scope_index,
                    params,
                    return_type,
                    local_variables,
                    &(*body).inner,
                )?;
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
            ExprT::Block {
                stmts,
                end_expr,
                type_: _,
                scope_index,
            } => {
                self.symbol_table.restore_scope(*scope_index);
                let mut opcodes = Vec::new();
                for stmt in stmts {
                    opcodes.append(&mut self.generate_stmt(&stmt.inner, false)?);
                }
                if let Some(expr) = end_expr {
                    opcodes.append(&mut self.generate_expr(&expr.inner)?);
                }
                Ok(opcodes)
            }
            ExprT::If(cond, then_block, else_block, type_) => {
                // Start with the cond opcodes
                let mut opcodes = self.generate_expr(&cond.inner)?;
                opcodes.push(OpCode::If);
                opcodes.push(OpCode::Type(
                    self.generate_wasm_type(type_)?.unwrap_or(WasmType::Empty),
                ));
                opcodes.append(&mut self.generate_expr(&then_block.inner)?);
                if let Some(else_block) = else_block {
                    opcodes.push(OpCode::Else);
                    opcodes.append(&mut self.generate_expr(&else_block.inner)?);
                }
                opcodes.push(OpCode::End);
                Ok(opcodes)
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
            ExprT::UnaryOp { op, rhs, type_: _ } => match op {
                UnaryOp::Minus => {
                    let mut opcodes = vec![OpCode::I32Const(0)];
                    opcodes.append(&mut self.generate_expr(&rhs.inner)?);
                    opcodes.push(OpCode::I32Sub);
                    Ok(opcodes)
                }
                UnaryOp::Not => {
                    let mut opcodes = self.generate_expr(&rhs.inner)?;
                    opcodes.push(OpCode::I32Const(-1));
                    opcodes.push(OpCode::I32Xor);
                    Ok(opcodes)
                }
            },
            ExprT::Record { fields, type_ } => {
                // Since all types are either pointers or 32 bit ints/floats,
                // we can do one field == 4 bytes
                let mut opcodes = Vec::new();
                opcodes.push(OpCode::CurrentMemory);
                let print_id = self
                    .name_table
                    .get_id(&"print".to_string())
                    .expect("Print must be defined");
                let print_entry = self
                    .symbol_table
                    .lookup_name_in_scope(*print_id, 0)
                    .expect("Print is not in symbol table");
                match print_entry.entry_type {
                    EntryType::Function {
                        index,
                        params_type: _,
                        return_type: _,
                    } => opcodes.push(OpCode::Call(index.try_into().unwrap())),
                    _ => return Err(GenerationError::NotReachable),
                }
                Ok(opcodes)
            }
            e => {
                println!("{:?}", e);
                Err(GenerationError::NotImplemented)
            }
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
