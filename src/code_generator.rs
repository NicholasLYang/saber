use crate::ast::{
    BuiltInTypes, ExprT, Function, FunctionId, Loc, Name, Op, ProgramT, StmtT, Type, TypeId,
    UnaryOp, Value,
};
use crate::lexer::LocationRange;
use crate::printer::type_to_string;
use crate::symbol_table::{
    FunctionInfo, SymbolTable, VarIndex, ALLOC_INDEX, DEALLOC_INDEX, PRINT_CHAR_INDEX,
    PRINT_HEAP_INDEX, STREQ_INDEX,
};
use crate::typechecker::TypeChecker;
use crate::utils::{get_final_type, NameTable};
use crate::wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, ImportEntry, ImportKind, LocalEntry,
    OpCode, ProgramData, WasmType,
};
use id_arena::Arena;
use std::convert::TryInto;
use thiserror::Error;
use walrus::{FunctionId, Module, ModuleConfig, ValType};

// Indicates value is an array of primitives
pub static ARRAY_ID: i32 = -1;
// Indicates value is an array of boxed values
pub static BOX_ARRAY_ID: i32 = -2;

#[derive(Debug, Error, PartialEq)]
pub enum GenerationError {
    #[error("Function '{name}' not defined")]
    FunctionNotDefined { name: String },
    #[error("Operator '{op}' for type {input_type} does not exist")]
    InvalidOperator { op: Op, input_type: String },
    #[error("Cannot have () as type")]
    EmptyType,
    #[error("Code Generator: Not implemented yet! {reason}")]
    NotImplemented { reason: &'static str },
    #[error("Code Generator: Not reachable")]
    NotReachable,
    #[error("Cannot return at top level")]
    TopLevelReturn,
    #[error("{location}: Cannot convert type {t1} to type {t2}")]
    CannotConvert {
        location: LocationRange,
        t1: String,
        t2: String,
    },
    #[error("{location}: Record cannot have more than 64 fields")]
    RecordTooLarge { location: LocationRange },
    #[error("Cannot export value")]
    ExportValue,
    #[error("{location}: Value is not a function")]
    NotAFunction { location: LocationRange },
}

pub struct CodeGenerator {
    symbol_table: SymbolTable,
    name_table: NameTable,
    builtin_types: BuiltInTypes,
    type_arena: Arena<Type>,
    module: Module,
    // All the generated code
    program_data: ProgramData,
    return_type: Option<WasmType>,
    current_function: Option<usize>,
    default_functions: DefaultFunctions,
}

pub struct DefaultFunctions {
    alloc: FunctionId,
    dealloc: FunctionId,
    clone: FunctionId,
    streq: FunctionId,
    print_heap: FunctionId,
    print_int: FunctionId,
    print_float: FunctionId,
    print_string: FunctionId,
    print_char: FunctionId,
}

type Result<T> = std::result::Result<T, GenerationError>;

impl CodeGenerator {
    pub fn new(typechecker: TypeChecker) -> Self {
        let expr_func_count = typechecker.get_expr_func_index();
        let (symbol_table, name_table, type_table, builtin_types) = typechecker.get_tables();
        let func_count = symbol_table.get_function_index();
        let config = ModuleConfig::new();
        let mut module = Module::with_config(config);
        let default_functions = Self::create_default_functions(&mut module);

        CodeGenerator {
            symbol_table,
            name_table,
            builtin_types,
            type_arena: type_table,
            module,
            default_functions,
            program_data: ProgramData::new(func_count, expr_func_count),
            return_type: None,
            current_function: None,
        }
    }

    fn create_default_functions(module: &mut Module) -> DefaultFunctions {
        let alloc_type = module.types.add(&[ValType::I32], &[ValType::I32]);
        let (alloc, _) = module.add_import_func("std", "alloc", alloc_type);

        let dealloc_type = module.types.add(&[ValType::I32], &[]);
        let (dealloc, _) = module.add_import_func("std", "dealloc", dealloc_type);

        let clone_type = module.types.add(&[ValType::I32], &[]);
        let (clone, _) = module.add_import_func("std", "clone", clone_type);

        let streq_type = module
            .types
            .add(&[ValType::I32, ValType::I32], &[ValType::I32]);
        let (streq, _) = module.add_import_func("std", "streq", streq_type);

        let print_heap_type = module.types.add(&[], &[]);
        let (print_heap, _) = module.add_import_func("std", "printHeap", print_heap_type);

        let print_int_type = module.types.add(&[ValType::I32], &[]);
        let (print_int, _) = module.add_import_func("std", "printInt", print_int_type);

        let print_float_type = module.types.add(&[ValType::F32], &[]);
        let (print_float, _) = module.add_import_func("std", "printFloat", print_float_type);

        let print_string_type = module.types.add(&[ValType::I32], &[]);
        let (print_string, _) = module.add_import_func("std", "printString", print_string_type);

        let print_char_type = module.types.add(&[ValType::I32], &[]);
        let (print_char, _) = module.add_import_func("std", "printChar", print_char_type);

        DefaultFunctions {
            alloc,
            dealloc,
            clone,
            streq,
            print_heap,
            print_int,
            print_float,
            print_string,
            print_char,
        }
    }

    pub fn generate_program(mut self, mut program: ProgramT) -> Result<ProgramData> {
        for local_func_index in 0..program.functions.len() {
            let func_index = (1 + PRINT_CHAR_INDEX as usize) + local_func_index;
            let function = program.functions.remove(&func_index).unwrap();
            self.generate_function(func_index, function.inner, function.location)?;
        }

        for stmt in program.stmts {
            self.generate_top_level_stmt(&stmt)?;
        }
        self.generate_default_imports()?;
        Ok(self.program_data)
    }

    fn generate_top_level_stmt(&mut self, stmt: &Loc<StmtT>) -> Result<()> {
        match &stmt.inner {
            StmtT::Return(_) => Err(GenerationError::TopLevelReturn),
            StmtT::Export(func_name) => {
                let name_str = self.name_table.get_str(func_name);
                let sym_entry = self.symbol_table.lookup_name(*func_name).ok_or(
                    GenerationError::FunctionNotDefined {
                        name: name_str.to_string(),
                    },
                )?;

                if let Some(FunctionInfo {
                    func_index,
                    func_scope: _,
                    params_type: _,
                    return_type: _,
                    is_top_level: _,
                }) = &sym_entry.function_info
                {
                    let entry = ExportEntry {
                        field_str: name_str.as_bytes().to_vec(),
                        kind: ExternalKind::Function,
                        index: (*func_index).try_into().unwrap(),
                    };
                    self.program_data.exports_section.push(entry);
                    Ok(())
                } else {
                    Err(GenerationError::ExportValue)
                }
            }
            stmt => {
                println!("{:?}", stmt);

                Err(GenerationError::NotImplemented {
                    reason: "Top level statements aren't done yet",
                })
            }
        }
    }

    pub fn generate_function(
        &mut self,
        func_index: FunctionId,
        function: Function,
        location: LocationRange,
    ) -> Result<()> {
        let old_func = self.current_function;
        self.current_function = Some(func_index);
        let old_scope = self.symbol_table.swap_scope(function.scope_index);
        let return_type = self.generate_wasm_type(function.return_type, location)?;
        self.return_type = return_type;
        let function_type =
            self.generate_function_type(&return_type, &function.params, location)?;
        let function_body = self.generate_function_body(
            &function.body,
            &function.local_variables,
            function.params.len(),
        )?;

        let type_index = self.program_data.insert_type(function_type);
        let local_func_index = func_index - (PRINT_CHAR_INDEX as usize + 1);

        self.program_data.code_section[local_func_index] = Some(function_body);
        self.program_data.function_section[local_func_index] = Some(type_index);
        self.symbol_table.swap_scope(old_scope);
        self.current_function = old_func;

        Ok(())
    }

    fn generate_function_type(
        &mut self,
        return_type: &Option<WasmType>,
        params: &[(Name, TypeId)],
        location: LocationRange,
    ) -> Result<FunctionType> {
        let mut wasm_param_types = vec![WasmType::i32];
        for (_, param_type) in params {
            let wasm_type = self
                .generate_wasm_type(*param_type, location)?
                .ok_or(GenerationError::EmptyType)?;
            wasm_param_types.push(wasm_type);
        }
        Ok(FunctionType {
            param_types: wasm_param_types,
            return_type: *return_type,
        })
    }

    fn generate_function_body(
        &mut self,
        body: &Loc<ExprT>,
        local_variables: &[TypeId],
        param_count: usize,
    ) -> Result<FunctionBody> {
        let mut code = self.generate_expr(body)?;
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

        // In the future we gotta do control flow checking to ensure a function evaluates
        // to a value
        if self.return_type.is_some() && body.inner.get_type() == self.builtin_types.unit {
            code.push(OpCode::Unreachable);
        }

        Ok(FunctionBody {
            locals: local_entries,
            code,
        })
    }

    fn get_args_type(
        &self,
        arg_type: TypeId,
        location: LocationRange,
        mut initial_args: Vec<WasmType>,
    ) -> Result<Vec<WasmType>> {
        if let Type::Tuple(elems) = &self.type_arena[arg_type] {
            let mut wasm_types = initial_args;
            for elem in elems {
                wasm_types = self.get_args_type(*elem, location, wasm_types)?;
            }
            Ok(wasm_types)
        } else {
            let wasm_type = match self.generate_wasm_type(arg_type, location)? {
                Some(t) => {
                    initial_args.push(t);
                    initial_args
                }
                None => initial_args,
            };
            Ok(wasm_type)
        }
    }

    fn generate_wasm_type(
        &self,
        sbr_type: TypeId,
        location: LocationRange,
    ) -> Result<Option<WasmType>> {
        match &self.type_arena[sbr_type] {
            Type::Unit => Ok(None),
            Type::Int | Type::Bool | Type::Char => Ok(Some(WasmType::i32)),
            Type::Float => Ok(Some(WasmType::f32)),
            Type::String
            | Type::Array(_)
            | Type::Arrow(_, _)
            | Type::Record(_, _)
            | Type::Tuple(_)
            | Type::Var(_) => Ok(Some(WasmType::i32)),
            Type::Solved(type_id) => self.generate_wasm_type(*type_id, location),
        }
    }

    fn generate_stmt(&mut self, stmt: &Loc<StmtT>, is_last: bool) -> Result<Vec<OpCode>> {
        match &stmt.inner {
            StmtT::Expr(expr) => {
                let mut opcodes = self.generate_expr(expr)?;
                match &self.type_arena[expr.inner.get_type()] {
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
            StmtT::Let(name, expr) => {
                let entry = self.symbol_table.lookup_name(*name).unwrap();
                let var_index = entry.var_index;
                let mut opcodes = self.generate_expr(expr)?;
                opcodes.push(OpCode::SetLocal(var_index.try_into().unwrap()));
                Ok(opcodes)
            }
            StmtT::Break => Ok(vec![OpCode::Br(1)]),
            StmtT::Loop(block) => {
                let mut opcodes = vec![OpCode::Loop, OpCode::Type(WasmType::Empty)];
                opcodes.append(&mut self.generate_expr(block)?);
                // Break to loop
                opcodes.push(OpCode::Br(0));
                opcodes.push(OpCode::End);
                Ok(opcodes)
            }
            StmtT::If {
                cond,
                then_block,
                else_block,
            } => {
                let mut opcodes = self.generate_expr(cond)?;
                opcodes.push(OpCode::If);
                opcodes.push(OpCode::Type(WasmType::Empty));
                opcodes.append(&mut self.generate_expr(then_block)?);
                if let Some(else_block) = else_block {
                    opcodes.push(OpCode::Else);
                    opcodes.append(&mut self.generate_expr(else_block)?);
                }
                opcodes.push(OpCode::End);
                Ok(opcodes)
            }
            _ => todo!(),
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
        match (&self.type_arena[type1], &self.type_arena[type2]) {
            (Type::Int, Type::Float) => {
                ops1.push(OpCode::F32ConvertI32);
                Ok(self.builtin_types.float)
            }
            (Type::Float, Type::Int) => {
                ops2.push(OpCode::F32ConvertI32);
                Ok(self.builtin_types.float)
            }
            (Type::Solved(t1), _) => self.promote_types(*t1, type2, ops1, ops2, location),
            (_, Type::Solved(t2)) => self.promote_types(type1, *t2, ops1, ops2, location),
            (t1, t2) => {
                if t1 == t2 {
                    Ok(type1)
                } else {
                    Err(GenerationError::CannotConvert {
                        t1: type_to_string(&self.name_table, &self.type_arena, type1),
                        t2: type_to_string(&self.name_table, &self.type_arena, type2),
                        location,
                    })
                }
            }
        }
    }

    fn generate_func_args(&mut self, args: &Loc<ExprT>) -> Result<Vec<OpCode>> {
        match &args.inner {
            ExprT::Tuple(entries, _) => {
                let mut opcodes = Vec::new();
                for entry in entries {
                    opcodes.append(&mut self.generate_expr(entry)?)
                }
                Ok(opcodes)
            }
            _ => self.generate_expr(args),
        }
    }

    fn generate_expr(&mut self, expr: &Loc<ExprT>) -> Result<Vec<OpCode>> {
        match &expr.inner {
            ExprT::Asgn { lhs, rhs, type_: _ } => {
                let mut opcodes = self.generate_expr(rhs)?;
                opcodes.push(OpCode::SetGlobal(0));
                let index = self.symbol_table.codegen_lookup(lhs.inner.ident).unwrap();
                if lhs.inner.accessors.is_empty() {
                    match index {
                        VarIndex::Local(index) => {
                            opcodes.push(OpCode::GetGlobal(0));
                            opcodes.push(OpCode::TeeLocal(index.try_into().unwrap()));
                        }
                        VarIndex::Capture(index) => {
                            let index = index + 1;
                            opcodes.push(OpCode::GetLocal(0));
                            opcodes.push(OpCode::GetGlobal(0));
                            opcodes.push(OpCode::I32Store(2, (index * 4).try_into().unwrap()));
                            opcodes.push(OpCode::GetGlobal(0));
                        }
                    }
                }
                Ok(opcodes)
            }
            ExprT::Primary { value, type_: _ } => self.generate_primary(value),
            ExprT::Var { name, type_: _ } => {
                let index = self.symbol_table.codegen_lookup(*name).unwrap();
                let opcodes = match index {
                    VarIndex::Local(index) => vec![OpCode::GetLocal(index.try_into().unwrap())],
                    VarIndex::Capture(index) => {
                        let index = index + 1;
                        vec![
                            OpCode::GetLocal(0),
                            OpCode::I32Load(2, (index * 4).try_into().unwrap()),
                        ]
                    }
                };
                Ok(opcodes)
            }
            ExprT::DirectCall {
                callee,
                captures_var_index,
                args,
                type_: _,
            } => {
                // NOTE: This is super brittle again, as if we add another runtime function,
                // we'll need to change this comparison
                let mut opcodes = if let Some(cf) = self.current_function {
                    if *callee <= PRINT_CHAR_INDEX.try_into().unwrap() {
                        Vec::new()
                    } else if cf == *callee
                    // Basically if we're in a recursive function situation
                    {
                        vec![OpCode::GetLocal(0)]
                    } else if let Some(var_index) = captures_var_index {
                        vec![OpCode::GetLocal(*var_index as u32)]
                    } else {
                        vec![OpCode::I32Const(0)]
                    }
                } else {
                    Vec::new()
                };
                opcodes.append(&mut self.generate_func_args(args)?);
                opcodes.push(OpCode::Call((*callee).try_into().unwrap()));
                Ok(opcodes)
            }
            ExprT::IndirectCall {
                callee,
                args,
                type_,
            } => {
                let args_wasm_type =
                    self.get_args_type(args.inner.get_type(), args.location, vec![WasmType::i32])?;
                let return_wasm_type = self.generate_wasm_type(*type_, expr.location)?;
                let func_type = FunctionType {
                    param_types: args_wasm_type,
                    return_type: return_wasm_type,
                };
                let type_sig_index = self.program_data.insert_type(func_type);
                // We have a GetGlobal here because global #1 always needs to
                // be restored to its original value after CallIndirect
                //
                // Why global #1? Well we don't have that same discipline with global #0
                // And we need to use it across generate_expr which is tricky
                let mut opcodes = vec![OpCode::GetGlobal(1)];
                opcodes.append(&mut self.generate_expr(callee)?);
                opcodes.push(OpCode::SetGlobal(1));
                opcodes.push(OpCode::GetGlobal(1));
                opcodes.push(OpCode::I32Load(2, 8));
                opcodes.append(&mut self.generate_expr(args)?);
                opcodes.push(OpCode::GetGlobal(1));
                opcodes.push(OpCode::I32Load(2, 4));
                opcodes.push(OpCode::CallIndirect(type_sig_index.try_into().unwrap()));
                opcodes.push(OpCode::SetGlobal(1));
                Ok(opcodes)
            }
            ExprT::Index {
                lhs,
                index,
                type_: _,
            } => {
                // Push global 1 on stack to save it
                let mut opcodes = vec![OpCode::GetGlobal(1)];
                opcodes.append(&mut self.generate_expr(lhs)?);
                opcodes.push(OpCode::SetGlobal(1));
                opcodes.append(&mut self.generate_expr(index)?);
                // Save index in global 0
                opcodes.push(OpCode::SetGlobal(0));
                // Get index
                opcodes.push(OpCode::GetGlobal(0));
                // Get lhs
                opcodes.push(OpCode::GetGlobal(1));
                // Load size
                opcodes.push(OpCode::I32Load(0, 4));
                // Check if index < size
                opcodes.push(OpCode::I32LessUnsigned);
                opcodes.push(OpCode::If);
                opcodes.push(OpCode::Type(WasmType::Empty));
                // Push on index
                opcodes.push(OpCode::GetGlobal(0));
                if !matches!(self.type_arena[lhs.inner.get_type()], Type::String) {
                    opcodes.push(OpCode::I32Const(4));
                    opcodes.push(OpCode::I32Mul);
                }
                // Push on lhs
                opcodes.push(OpCode::GetGlobal(1));
                opcodes.push(OpCode::I32Add);
                opcodes.push(OpCode::SetGlobal(0));
                opcodes.push(OpCode::Else);
                opcodes.push(OpCode::Unreachable);
                opcodes.push(OpCode::End);
                opcodes.push(OpCode::SetGlobal(1));
                opcodes.push(OpCode::GetGlobal(0));
                if matches!(self.type_arena[lhs.inner.get_type()], Type::String) {
                    opcodes.push(OpCode::I32Load8U(0, 8));
                } else {
                    opcodes.push(OpCode::I32Load(0, 8));
                }
                Ok(opcodes)
            }
            ExprT::Array {
                entries,
                entry_type,
                type_: _,
            } => self.generate_array(entries, *entry_type, expr.location),
            ExprT::Tuple(elems, type_id) => {
                if elems.is_empty() {
                    Ok(Vec::new())
                } else {
                    self.generate_tuple(elems.iter(), elems.len(), *type_id, expr.location)
                }
            }
            ExprT::Block {
                stmts,
                end_expr,
                type_: _,
                scope_index,
            } => {
                let old_scope = self.symbol_table.swap_scope(*scope_index);
                let mut opcodes = Vec::new();
                for stmt in stmts {
                    opcodes.append(&mut self.generate_stmt(stmt, false)?);
                }
                if let Some(expr) = end_expr {
                    opcodes.append(&mut self.generate_expr(expr)?);
                }
                let entries = self.symbol_table.get_scope_entries(*scope_index);
                for (_, entry) in entries {
                    if self.type_arena[entry.var_type].is_ref_type() {
                        opcodes.push(OpCode::GetLocal((entry.var_index).try_into().unwrap()));
                        opcodes.push(OpCode::Call(DEALLOC_INDEX));
                    }
                }
                self.symbol_table.swap_scope(old_scope);
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
                let opcode = self.generate_operator(op, &self.type_arena[*type_], promoted_type)?;
                lhs_ops.push(opcode);
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
            ExprT::TupleField(lhs, index, type_) => {
                self.generate_tuple_field(lhs, *index as u32, type_)
            }
            ExprT::Record {
                name: _,
                fields,
                type_,
            } => self.generate_tuple(
                fields.iter().map(|(_, expr)| expr),
                fields.len(),
                *type_,
                expr.location,
            ),
        }
    }

    fn generate_tuple_field(
        &mut self,
        lhs: &Loc<ExprT>,
        index: u32,
        type_: &TypeId,
    ) -> Result<Vec<OpCode>> {
        let mut opcodes = self.generate_expr(&*lhs)?;
        let field_wasm_type = self
            .generate_wasm_type(*type_, lhs.location)?
            .unwrap_or(WasmType::Empty);
        // Account for type_id
        let field_loc = index + 1;
        let offset = 4 * field_loc;
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
    }

    fn generate_array(
        &mut self,
        entries: &[Loc<ExprT>],
        entry_type: TypeId,
        location: LocationRange,
    ) -> Result<Vec<OpCode>> {
        // We add 2 fields for type id and length
        let length = entries.len() + 2;
        // Since all types are either pointers or 32 bit ints/floats,
        // we can do one field == 4 bytes
        if length >= 64 {
            return Err(GenerationError::RecordTooLarge { location });
        }
        let mut offset = 0;
        let record_size: i32 = (4 * length).try_into().unwrap();
        let mut opcodes = vec![OpCode::I32Const(record_size), OpCode::Call(ALLOC_INDEX)];
        // Save pointer to block at global 0
        opcodes.push(OpCode::SetGlobal(0));
        // Set type_id
        // Get ptr to record
        opcodes.push(OpCode::GetGlobal(0));
        if self.type_arena[entry_type].is_ref_type() {
            opcodes.push(OpCode::I32Const(BOX_ARRAY_ID));
        } else {
            opcodes.push(OpCode::I32Const(ARRAY_ID));
        }
        opcodes.push(OpCode::I32Store(2, offset));
        opcodes.push(OpCode::GetGlobal(0));
        offset += 4;
        opcodes.push(OpCode::GetGlobal(0));
        opcodes.push(OpCode::I32Const(entries.len().try_into().unwrap()));
        opcodes.push(OpCode::I32Store(2, offset));
        offset += 4;
        for entry in entries {
            opcodes.append(&mut self.generate_allocated_expr(entry, offset)?);
            offset += 4;
        }
        Ok(opcodes)
    }

    // Generate allocated expression for array or tuple
    fn generate_allocated_expr(&mut self, expr: &Loc<ExprT>, offset: u32) -> Result<Vec<OpCode>> {
        // No, you're not seeing double. We push
        // the value of Global 0 to "save" it
        // in case it gets mutated when we generate
        // the struct.
        // We push it again as an address for store
        let mut opcodes = vec![OpCode::GetGlobal(0), OpCode::GetGlobal(0)];
        opcodes.append(&mut self.generate_expr(expr)?);
        let wasm_type = self
            .generate_wasm_type(expr.inner.get_type(), expr.location)?
            .unwrap_or(WasmType::Empty);
        let store_code = match wasm_type {
            // Alignment of 2, offset of 0
            WasmType::i32 => OpCode::I32Store(2, offset),
            WasmType::f32 => OpCode::F32Store(2, offset),
            _ => {
                return Err(GenerationError::NotImplemented {
                    reason: "Cannot generate code to store types other than i32 and f32",
                })
            }
        };
        opcodes.push(store_code);
        opcodes.push(OpCode::SetGlobal(0));
        Ok(opcodes)
    }

    fn generate_tuple<'a, I>(
        &mut self,
        fields: I,
        length: usize,
        type_id: TypeId,
        location: LocationRange,
    ) -> Result<Vec<OpCode>>
    where
        I: Iterator<Item = &'a Loc<ExprT>>,
    {
        let mut offset = 0;
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
        let final_type_id = get_final_type(&self.type_arena, type_id);
        opcodes.push(OpCode::I32Const(final_type_id.index().try_into().unwrap()));
        // *ptr = type_id
        opcodes.push(OpCode::I32Store(2, offset));
        offset += 4;
        // We need to write the struct to memory. This consists of
        // looping through entries, generating the opcodes and
        // storing them in the heap
        for expr in fields {
            opcodes.append(&mut self.generate_allocated_expr(expr, offset)?);
            offset += 4;
        }
        // return heap_ptr - sizeof(record)
        opcodes.push(OpCode::GetGlobal(0));
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

                let mut offset = 0;
                // We tack on 8 more bytes for the length and type_id
                let mut opcodes = vec![
                    OpCode::I32Const(buffer_length + 8),
                    OpCode::Call(ALLOC_INDEX),
                ];
                opcodes.push(OpCode::SetGlobal(0));

                // Set type_id
                // Get ptr to record
                opcodes.push(OpCode::GetGlobal(0));
                opcodes.push(OpCode::I32Const(
                    self.builtin_types.string.index().try_into().unwrap(),
                ));
                // *ptr = type_id
                opcodes.push(OpCode::I32Store(2, offset));

                // Pop on global 0 as return value (ptr to type id)
                // This may seem weird, as intuitively we'd want the
                // pointer to be to the start of the string
                // but for GC reasons we want it to point to type id
                opcodes.push(OpCode::GetGlobal(0));

                offset += 4;

                opcodes.push(OpCode::GetGlobal(0));
                opcodes.push(OpCode::I32Const(raw_str_length));
                opcodes.push(OpCode::I32Store(2, offset));
                for b in bytes.chunks_exact(4) {
                    offset += 4;
                    let packed_bytes = (b[0] as u32)
                        + ((b[1] as u32) << 8)
                        + ((b[2] as u32) << 16)
                        + ((b[3] as u32) << 24);
                    opcodes.push(OpCode::GetGlobal(0));
                    opcodes.push(OpCode::I32Const(packed_bytes as i32));
                    opcodes.push(OpCode::I32Store(2, offset))
                }
                Ok(opcodes)
            }
        }
    }

    fn generate_operator(&self, op: &Op, result_type: &Type, input_type: TypeId) -> Result<OpCode> {
        match (op, &self.type_arena[input_type], result_type) {
            (Op::Plus, Type::Int, Type::Int) => Ok(OpCode::I32Add),
            (Op::Minus, Type::Int, Type::Int) => Ok(OpCode::I32Sub),
            (Op::Plus, Type::Float, Type::Float) => Ok(OpCode::F32Add),
            (Op::Minus, Type::Float, Type::Float) => Ok(OpCode::F32Sub),
            (Op::Times, Type::Int, Type::Int) => Ok(OpCode::I32Mul),
            (Op::Div, Type::Int, Type::Int) => Ok(OpCode::I32Div),
            (Op::Times, Type::Float, Type::Float) => Ok(OpCode::F32Mul),
            (Op::Div, Type::Float, Type::Float) => Ok(OpCode::F32Div),
            (Op::Less, Type::Int, Type::Bool) => Ok(OpCode::I32LessSigned),
            (Op::Greater, Type::Int, Type::Bool) => Ok(OpCode::I32GreaterSigned),
            (Op::GreaterEqual, Type::Int, Type::Bool) => Ok(OpCode::I32GreaterEqSigned),
            (Op::EqualEqual, Type::Int, Type::Bool) => Ok(OpCode::I32Eq),
            (Op::EqualEqual, Type::String, Type::Bool) => Ok(OpCode::Call(STREQ_INDEX)),
            (op, _, _) => Err(GenerationError::InvalidOperator {
                op: op.clone(),
                input_type: type_to_string(&self.name_table, &self.type_arena, input_type),
            }),
        }
    }
}
