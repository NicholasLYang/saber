use crate::ast::{ExprT, Function, Loc, Name, Op, ProgramT, StmtT, Type, TypeId, UnaryOp, Value};
use crate::lexer::LocationRange;
use crate::printer::type_to_string;
use crate::symbol_table::{
    EntryType, SymbolTable, ALLOC_INDEX, CLONE_INDEX, DEALLOC_INDEX, STREQ_INDEX,
};
use crate::typechecker::{is_ref_type, TypeChecker};
use crate::utils::{NameTable, TypeTable, FLOAT_INDEX, STR_INDEX};
use crate::wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, ImportEntry, ImportKind, LocalEntry,
    OpCode, ProgramData, WasmType,
};
use std::convert::TryInto;

#[derive(Debug, Fail, PartialEq)]
pub enum GenerationError {
    #[fail(display = "Operator '{}' for type {} does not exist", op, input_type)]
    InvalidOperator { op: Op, input_type: String },
    #[fail(display = "Function '{}' not defined", name)]
    FunctionNotDefined { name: String },
    #[fail(display = "Cannot have () as type")]
    EmptyType,
    #[fail(display = "{}: Could not infer type var {:?}", location, type_)]
    CouldNotInfer {
        location: LocationRange,
        type_: Type,
    },
    #[fail(display = "Code Generator: Not implemented yet! {}", reason)]
    NotImplemented { reason: &'static str },
    #[fail(display = "Code Generator: Not reachable")]
    NotReachable,
    #[fail(display = "Cannot return at top level")]
    TopLevelReturn,
    #[fail(display = "{}: Cannot convert type {} to type {}", location, t1, t2)]
    CannotConvert {
        location: LocationRange,
        t1: String,
        t2: String,
    },
    #[fail(display = "{}: Record cannot have more than 64 fields", location)]
    RecordTooLarge { location: LocationRange },
    #[fail(display = "Cannot export value")]
    ExportValue,
    #[fail(display = "{}: Value is not a function", location)]
    NotAFunction { location: LocationRange },
}

pub struct CodeGenerator {
    symbol_table: SymbolTable,
    name_table: NameTable,
    type_table: TypeTable,
    // All the generated code
    program_data: ProgramData,
    return_type: Option<WasmType>,
}

type Result<T> = std::result::Result<T, GenerationError>;

impl CodeGenerator {
    pub fn new(typechecker: TypeChecker) -> Self {
        let (symbol_table, name_table, type_table) = typechecker.get_tables();
        let func_count = symbol_table.get_function_index();
        CodeGenerator {
            symbol_table,
            name_table,
            type_table,
            program_data: ProgramData::new(func_count),
            return_type: None,
        }
    }

    fn generate_default_imports(&mut self) -> Result<()> {
        let alloc_type = FunctionType {
            param_types: vec![WasmType::i32],
            return_type: Some(WasmType::i32),
        };
        let alloc_type_index = self.program_data.insert_type(alloc_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "alloc".into(),
            kind: ImportKind::Function {
                type_: alloc_type_index,
            },
        });
        let dealloc_type = FunctionType {
            param_types: vec![WasmType::i32],
            return_type: None,
        };
        let dealloc_type_index = self.program_data.insert_type(dealloc_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "dealloc".into(),
            kind: ImportKind::Function {
                type_: dealloc_type_index,
            },
        });
        let clone_type = FunctionType {
            param_types: vec![WasmType::i32],
            return_type: None,
        };
        let clone_type_index = self.program_data.insert_type(clone_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "clone".into(),
            kind: ImportKind::Function {
                type_: clone_type_index,
            },
        });
        let streq_type = FunctionType {
            param_types: vec![WasmType::i32, WasmType::i32],
            return_type: Some(WasmType::i32),
        };
        let streq_type_index = self.program_data.insert_type(streq_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "streq".into(),
            kind: ImportKind::Function {
                type_: streq_type_index,
            },
        });
        let print_int_type = FunctionType {
            param_types: vec![WasmType::i32],
            return_type: None,
        };
        let print_int_index = self.program_data.insert_type(print_int_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "printInt".into(),
            kind: ImportKind::Function {
                type_: print_int_index,
            },
        });
        let print_float_type = FunctionType {
            param_types: vec![WasmType::f32],
            return_type: None,
        };
        let print_float_index = self.program_data.insert_type(print_float_type);
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "printFloat".into(),
            kind: ImportKind::Function {
                type_: print_float_index,
            },
        });
        self.program_data.import_section.push(ImportEntry {
            module_str: "std".into(),
            field_str: "printString".into(),
            kind: ImportKind::Function {
                type_: print_int_index,
            },
        });
        Ok(())
    }

    pub fn generate_program(mut self, program: ProgramT) -> Result<ProgramData> {
        for stmt in program.stmts {
            (&mut self).generate_top_level_stmt(&stmt)?;
        }
        self.generate_default_imports()?;
        Ok(self.program_data)
    }

    pub fn generate_top_level_stmt(&mut self, stmt: &Loc<StmtT>) -> Result<()> {
        match &stmt.inner {
            StmtT::Function {
                name,
                params_type: _,
                return_type,
                function:
                    Function {
                        params,
                        local_variables,
                        body,
                        scope_index,
                    },
            } => {
                self.generate_function_binding(
                    *name,
                    *scope_index,
                    params,
                    *return_type,
                    local_variables,
                    body,
                    stmt.location,
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
                    type_: _,
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
            _ => Err(GenerationError::NotImplemented {
                reason: "Top level statements aren't done yet",
            }),
        }
    }

    // Generates a Stmt::Function
    pub fn generate_function_binding(
        &mut self,
        name: Name,
        scope: usize,
        params: &[(Name, TypeId)],
        return_type: TypeId,
        local_variables: &Vec<TypeId>,
        body: &Loc<ExprT>,
        location: LocationRange,
    ) -> Result<usize> {
        let entry = self.symbol_table.lookup_name_in_scope(name, scope).unwrap();
        let index = if let EntryType::Function {
            index,
            params_type: _,
            return_type: _,
            type_: _,
        } = &entry.entry_type
        {
            *index
        } else {
            return Err(GenerationError::NotReachable);
        };
        let old_scope = self.symbol_table.restore_scope(scope);
        let (type_, body) =
            self.generate_function(return_type, params, local_variables, body, location)?;
        let type_index = self.program_data.insert_type(type_);
        self.program_data.code_section[index] = Some(body);
        self.program_data.function_section[index] = Some(type_index);
        self.symbol_table.restore_scope(old_scope);
        Ok(index)
    }

    pub fn generate_function(
        &mut self,
        return_type: TypeId,
        params: &[(Name, TypeId)],
        local_variables: &Vec<TypeId>,
        body: &Loc<ExprT>,
        location: LocationRange,
    ) -> Result<(FunctionType, FunctionBody)> {
        let return_type = self.generate_wasm_type(return_type, location)?;
        self.return_type = return_type.clone();
        let function_type = self.generate_function_type(&return_type, params, location)?;
        let function_body = self.generate_function_body(body, local_variables, params.len())?;
        Ok((function_type, function_body))
    }

    // Generates function type. Also inserts params into local_variables.
    // Not sure it should do both but w/e it's here.
    fn generate_function_type(
        &mut self,
        return_type: &Option<WasmType>,
        params: &[(Name, TypeId)],
        location: LocationRange,
    ) -> Result<FunctionType> {
        let mut wasm_param_types = Vec::new();
        for (_, param_type) in params {
            let wasm_type = self
                .generate_wasm_type(*param_type, location)?
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
        body: &Loc<ExprT>,
        local_variables: &[TypeId],
        param_count: usize,
    ) -> Result<FunctionBody> {
        let code = self.generate_expr(body)?;
        let mut local_entries = Vec::new();
        // We want to generate only the locals not params so we skip
        // to after the params
        for local_type in &local_variables[param_count..] {
            let wasm_type = self
                .generate_wasm_type(*local_type, body.location)?
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

    fn get_args_type(&self, arg_type: TypeId, location: LocationRange) -> Result<Vec<WasmType>> {
        if let Type::Tuple(elems) = self.type_table.get_type(arg_type) {
            let mut wasm_types = Vec::new();
            for elem in elems {
                wasm_types.append(&mut self.get_args_type(*elem, location)?);
            }
            Ok(wasm_types)
        } else {
            let wasm_type = match self.generate_wasm_type(arg_type, location)? {
                Some(t) => vec![t],
                None => Vec::new(),
            };
            Ok(wasm_type)
        }
    }

    fn generate_wasm_type(
        &self,
        sbr_type: TypeId,
        location: LocationRange,
    ) -> Result<Option<WasmType>> {
        match self.type_table.get_type(sbr_type) {
            Type::Unit => Ok(None),
            Type::Int | Type::Bool | Type::Char => Ok(Some(WasmType::i32)),
            Type::Float => Ok(Some(WasmType::f32)),
            Type::String
            | Type::Array(_)
            | Type::Arrow(_, _)
            | Type::Record(_)
            | Type::Tuple(_) => Ok(Some(WasmType::i32)),
            Type::Solved(type_id) => self.generate_wasm_type(*type_id, location),
            Type::Var(id) => Err(GenerationError::CouldNotInfer {
                location,
                type_: Type::Var(*id),
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
                type_: _,
            } => Err(GenerationError::NotReachable),
            EntryType::Var { index, var_type: _ } => Ok(index),
        }
    }

    fn generate_stmt(&mut self, stmt: &Loc<StmtT>, is_last: bool) -> Result<Vec<OpCode>> {
        match &stmt.inner {
            StmtT::Function {
                name,
                params_type: _,
                return_type,
                function:
                    Function {
                        params,
                        local_variables,
                        body,
                        scope_index,
                    },
            } => {
                self.symbol_table.restore_scope(*scope_index);
                self.generate_function_binding(
                    *name,
                    *scope_index,
                    params,
                    *return_type,
                    local_variables,
                    body,
                    stmt.location,
                )?;
                Ok(Vec::new())
            }
            StmtT::Expr(expr) => {
                let mut opcodes = self.generate_expr(expr)?;
                match self.type_table.get_type(expr.inner.get_type()) {
                    Type::Unit => {}
                    _ => opcodes.push(OpCode::Drop),
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
                    self.generate_expr(expr)
                } else {
                    let mut opcodes = self.generate_expr(expr)?;
                    opcodes.push(OpCode::Return);
                    Ok(opcodes)
                }
            }
            StmtT::Asgn(name, expr) => {
                let index = self.get_var_index(*name)?;
                let mut opcodes = self.generate_expr(&expr)?;
                opcodes.push(OpCode::SetLocal(index.try_into().unwrap()));
                /*
                This works, except it gives an extra refcount when you initialize a value
                if is_ref_type(expr.inner.get_type()) {
                    opcodes.push(OpCode::GetLocal(index.try_into().unwrap()));
                    opcodes.push(OpCode::Call(CLONE_INDEX));
                }*/
                Ok(opcodes)
            }
            StmtT::Block(stmts) => {
                let mut opcodes = Vec::new();
                for (i, stmt) in stmts.iter().enumerate() {
                    opcodes.append(&mut self.generate_stmt(stmt, is_last && i == stmts.len() - 1)?);
                }
                Ok(opcodes)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn promote_types(
        &self,
        type1: TypeId,
        type2: TypeId,
        ops1: &mut Vec<OpCode>,
        ops2: &mut Vec<OpCode>,
        location: LocationRange,
    ) -> Result<TypeId> {
        if type1 == type2 {
            return Ok(type1);
        }
        match (
            self.type_table.get_type(type1),
            self.type_table.get_type(type2),
        ) {
            (Type::Int, Type::Float) => {
                ops1.push(OpCode::F32ConvertI32);
                Ok(FLOAT_INDEX)
            }
            (Type::Float, Type::Int) => {
                ops2.push(OpCode::F32ConvertI32);
                Ok(FLOAT_INDEX)
            }
            (t1, t2) => {
                if t1 == t2 {
                    Ok(type1)
                } else {
                    Err(GenerationError::CannotConvert {
                        t1: type_to_string(&self.name_table, &self.type_table, type1),
                        t2: type_to_string(&self.name_table, &self.type_table, type2),
                        location,
                    })
                }
            }
        }
    }

    fn generate_expr(&mut self, expr: &Loc<ExprT>) -> Result<Vec<OpCode>> {
        match &expr.inner {
            ExprT::Primary { value, type_: _ } => self.generate_primary(value),
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
                        type_: _,
                    } = &entry.entry_type
                    {
                        let index = *index;
                        opcodes.append(&mut self.generate_expr(args)?);
                        opcodes.push(OpCode::Call((index).try_into().unwrap()));
                        return Ok(opcodes);
                    };
                }
                let args_wasm_type = self.get_args_type(args.inner.get_type(), args.location)?;
                let return_wasm_type = self.generate_wasm_type(*type_, expr.location)?;
                let func_type = FunctionType {
                    param_types: args_wasm_type,
                    return_type: return_wasm_type,
                };
                let type_sig_index = self.program_data.insert_type(func_type);
                let mut opcodes = self.generate_expr(args)?;
                opcodes.append(&mut self.generate_expr(callee)?);
                opcodes.push(OpCode::CallIndirect(type_sig_index.try_into().unwrap()));
                Ok(opcodes)
            }
            ExprT::Function {
                name,
                type_,
                function:
                    Function {
                        params,
                        body,
                        local_variables,
                        scope_index,
                    },
            } => {
                self.symbol_table.restore_scope(*scope_index);
                let (_, return_type) = if let Type::Arrow(params_type, return_type) =
                    self.type_table.get_type(*type_)
                {
                    (*params_type, *return_type)
                } else {
                    return Err(GenerationError::NotAFunction {
                        location: expr.location,
                    });
                };
                let index = self.generate_function_binding(
                    *name,
                    *scope_index,
                    params,
                    return_type,
                    local_variables,
                    body,
                    expr.location,
                )?;
                self.program_data.elements_section.elems.push(index);
                let table_index = self.program_data.elements_section.elems.len() - 1;
                Ok(vec![OpCode::I32Const(table_index.try_into().unwrap())])
            }
            ExprT::Tuple(elems, type_id) => {
                if elems.len() == 0 {
                    Ok(Vec::new())
                } else {
                    self.generate_record_literal(elems.iter(), elems.len(), *type_id, expr.location)
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
                    opcodes.append(&mut self.generate_stmt(stmt, false)?);
                }
                if let Some(expr) = end_expr {
                    opcodes.append(&mut self.generate_expr(expr)?);
                }
                let entries = self.symbol_table.get_scope_entries(*scope_index);
                for (_, entry) in entries {
                    if let EntryType::Var { var_type, index } = &entry.entry_type {
                        if is_ref_type(*var_type) {
                            opcodes.push(OpCode::GetLocal((*index).try_into().unwrap()));
                            opcodes.push(OpCode::Call(DEALLOC_INDEX));
                        }
                    }
                }
                Ok(opcodes)
            }
            ExprT::If(cond, then_block, else_block, type_) => {
                // Start with the cond opcodes
                let mut opcodes = self.generate_expr(cond)?;
                opcodes.push(OpCode::If);
                opcodes.push(OpCode::Type(
                    self.generate_wasm_type(*type_, expr.location)?
                        .unwrap_or(WasmType::Empty),
                ));
                opcodes.append(&mut self.generate_expr(then_block)?);
                if let Some(else_block) = else_block {
                    opcodes.push(OpCode::Else);
                    opcodes.append(&mut self.generate_expr(else_block)?);
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
                let mut lhs_ops = self.generate_expr(lhs)?;
                let lhs_type = lhs.inner.get_type();
                let mut rhs_ops = self.generate_expr(rhs)?;
                let rhs_type = rhs.inner.get_type();

                let promoted_type = self.promote_types(
                    lhs_type,
                    rhs_type,
                    &mut lhs_ops,
                    &mut rhs_ops,
                    expr.location,
                )?;
                lhs_ops.append(&mut rhs_ops);
                lhs_ops.push(self.generate_operator(
                    &op,
                    self.type_table.get_type(*type_),
                    promoted_type,
                )?);
                Ok(lhs_ops)
            }
            ExprT::UnaryOp { op, rhs, type_: _ } => match op {
                UnaryOp::Minus => {
                    let mut opcodes = vec![OpCode::I32Const(0)];
                    opcodes.append(&mut self.generate_expr(rhs)?);
                    opcodes.push(OpCode::I32Sub);
                    Ok(opcodes)
                }
                UnaryOp::Not => {
                    let mut opcodes = self.generate_expr(rhs)?;
                    opcodes.push(OpCode::I32Const(-1));
                    opcodes.push(OpCode::I32Xor);
                    Ok(opcodes)
                }
            },
            ExprT::Field(lhs, name, _) => {
                let mut opcodes = self.generate_expr(&**lhs)?;
                let lhs_type = lhs.inner.get_type();
                if let Type::Record(fields) = self.type_table.get_type(lhs_type) {
                    let field_position = fields
                        .iter()
                        .position(|(field_name, _)| *field_name == *name)
                        .ok_or(GenerationError::NotImplemented {
                            reason: "Field doesn't exist: Should be caught by typechecker",
                        })?;
                    let field_wasm_type = self
                        .generate_wasm_type(fields[field_position].1, expr.location)?
                        .unwrap_or(WasmType::Empty);
                    // Account for type_id
                    let field_loc = field_position + 1;
                    let offset: u32 = (4 * field_loc).try_into().unwrap();
                    let load_code = match field_wasm_type {
                        WasmType::i32 => OpCode::I32Load(2, offset),
                        WasmType::f32 => OpCode::F32Load(2, offset),
                        WasmType::Empty => OpCode::Drop,
                        _ => {
                            return Err(GenerationError::NotReachable);
                        }
                    };
                    opcodes.push(load_code);
                    Ok(opcodes)
                } else {
                    Err(GenerationError::NotReachable)
                }
            }
            ExprT::Record {
                name: _,
                fields,
                type_,
            } => self.generate_record_literal(
                fields.iter().map(|(_, expr)| expr),
                fields.len(),
                *type_,
                expr.location,
            ),
        }
    }

    fn generate_record_literal<'a, I>(
        &mut self,
        fields: I,
        length: usize,
        type_id: TypeId,
        location: LocationRange,
    ) -> Result<Vec<OpCode>>
    where
        I: Iterator<Item = &'a Loc<ExprT>>,
    {
        // We add 1 field for type id
        let length = length + 1;
        // Since all types are either pointers or 32 bit ints/floats,
        // we can do one field == 4 bytes
        if length >= 64 {
            return Err(GenerationError::RecordTooLarge { location });
        }
        // Size of record in bytes
        let record_size: i32 = (4 * length).try_into().unwrap();
        let mut opcodes = vec![OpCode::I32Const(record_size), OpCode::Call(ALLOC_INDEX)];
        // Save pointer to block at global 0
        opcodes.push(OpCode::SetGlobal(0));

        // Set type_id
        // Get ptr to record
        opcodes.push(OpCode::GetGlobal(0));
        // TODO: Add proper error handling for too many types.
        opcodes.push(OpCode::I32Const(type_id.try_into().unwrap()));
        // *ptr = type_id
        opcodes.push(OpCode::I32Store(2, 0));
        opcodes.push(OpCode::GetGlobal(0));
        opcodes.push(OpCode::I32Const(4));
        opcodes.push(OpCode::I32Add);
        opcodes.push(OpCode::SetGlobal(0));

        // We need to write the struct to memory. This consists of
        // looping through entries, generating the opcodes and
        // storing them in the heap
        for expr in fields {
            // No, you're not seeing double. We push
            // the value of Global 0 to "save" it
            // in case it gets mutated when we generate
            // the field.
            opcodes.push(OpCode::GetGlobal(0));
            // We push it again as an address for store
            opcodes.push(OpCode::GetGlobal(0));
            opcodes.append(&mut self.generate_expr(expr)?);
            let wasm_type = self
                .generate_wasm_type(expr.inner.get_type(), expr.location)?
                .unwrap_or(WasmType::Empty);
            let store_code = match wasm_type {
                // Alignment of 2, offset of 0
                WasmType::i32 => OpCode::I32Store(2, 0),
                WasmType::f32 => OpCode::F32Store(2, 0),
                _ => {
                    return Err(GenerationError::NotImplemented {
                        reason: "Cannot generate code to store types other than i32 and f32",
                    })
                }
            };
            opcodes.push(store_code);
            // We restore here. g1 is already on the stack
            // so we add 4 to it and save
            opcodes.push(OpCode::I32Const(4));
            opcodes.push(OpCode::I32Add);
            opcodes.push(OpCode::SetGlobal(0));
        }
        // return heap_ptr - sizeof(record)
        opcodes.push(OpCode::GetGlobal(0));
        opcodes.push(OpCode::I32Const(record_size));
        opcodes.push(OpCode::I32Sub);
        Ok(opcodes)
    }

    fn generate_primary(&self, value: &Value) -> Result<Vec<OpCode>> {
        match value {
            Value::Float(f) => Ok(vec![OpCode::F32Const(*f)]),
            Value::Integer(i) => Ok(vec![OpCode::I32Const(*i)]),
            Value::Bool(b) => {
                if *b {
                    Ok(vec![OpCode::I32Const(1)])
                } else {
                    Ok(vec![OpCode::I32Const(0)])
                }
            }
            Value::String(s) => {
                let mut bytes = s.as_bytes().to_vec();
                let raw_str_length: i32 = bytes.len().try_into().expect("String is too long");
                // Make bytes a multiple of 4 for ease of use;
                while bytes.len() % 4 != 0 {
                    bytes.push(0);
                }
                let buffer_length: i32 = bytes.len().try_into().expect("String is too long");
                // We tack on 8 more bytes for the length and type_id
                let mut opcodes = vec![
                    OpCode::I32Const(buffer_length + 8),
                    OpCode::Call(ALLOC_INDEX),
                ];
                opcodes.push(OpCode::SetGlobal(0));

                // Set type_id
                // Get ptr to record
                opcodes.push(OpCode::GetGlobal(0));
                opcodes.push(OpCode::I32Const(STR_INDEX.try_into().unwrap()));
                // *ptr = type_id
                opcodes.push(OpCode::I32Store(2, 0));

                // Pop on global 0 as return value (ptr to type id)
                // This may seem weird, as intuitively we'd want the
                // pointer to be to the start of the string
                // but for GC reasons we want it to point to type id
                opcodes.push(OpCode::GetGlobal(0));

                opcodes.push(OpCode::GetGlobal(0));
                opcodes.push(OpCode::I32Const(4));
                opcodes.push(OpCode::I32Add);
                opcodes.push(OpCode::SetGlobal(0));

                opcodes.push(OpCode::GetGlobal(0));
                opcodes.push(OpCode::I32Const(raw_str_length));
                opcodes.push(OpCode::I32Store(2, 0));
                for b in bytes.chunks_exact(4) {
                    // Increment global 1 by 4 bytes
                    opcodes.push(OpCode::GetGlobal(0));
                    opcodes.push(OpCode::I32Const(4));
                    opcodes.push(OpCode::I32Add);
                    opcodes.push(OpCode::SetGlobal(0));
                    let packed_bytes = ((b[0] as u32) << 0)
                        + ((b[1] as u32) << 8)
                        + ((b[2] as u32) << 16)
                        + ((b[3] as u32) << 24);
                    opcodes.push(OpCode::GetGlobal(0));
                    opcodes.push(OpCode::I32Const(packed_bytes as i32));
                    opcodes.push(OpCode::I32Store(2, 0))
                }
                Ok(opcodes)
            }
        }
    }

    fn generate_operator(&self, op: &Op, result_type: &Type, input_type: TypeId) -> Result<OpCode> {
        match (op, self.type_table.get_type(input_type), result_type) {
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
            (Op::EqualEqual, Type::String, Type::Bool) => Ok(OpCode::Call(STREQ_INDEX)),
            (op, _, _) => Err(GenerationError::InvalidOperator {
                op: op.clone(),
                input_type: type_to_string(&self.name_table, &self.type_table, input_type),
            }),
        }
    }
}
