use crate::ast::{
    BuiltInTypes, ExprT, Function, Loc, Name, Op, ProgramT, StmtT, Type, TypeId, UnaryOp, Value,
};
use crate::lexer::LocationRange;
use crate::printer::type_to_string;
use crate::symbol_table::{
    FunctionInfo, SymbolTable, VarIndex, PRINT_CHAR_INDEX, PRINT_HEAP_INDEX, STREQ_INDEX,
};

use crate::typechecker::TypeChecker;
use crate::utils::{get_final_type, NameTable};
use crate::wasm::{
    ExportEntry, ExternalKind, FunctionBody, FunctionType, LocalEntry, OpCode, ProgramData,
    WasmType,
};
use id_arena::Arena;
use im::HashMap;
use std::convert::TryInto;
use thiserror::Error;
use walrus::ir::{BinaryOp, ExtendedLoad, LoadKind, MemArg, StoreKind};
use walrus::{
    FunctionBuilder, FunctionId, GlobalId, InitExpr, InstrSeqBuilder, LocalFunction, LocalId,
    MemoryId, Module, ModuleConfig, TableId, ValType,
};
use wasmtime::Val;

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

#[derive(Clone)]
struct CurrentFunctionContext {
    params: HashMap<Name, LocalId>,
    env_pointer: LocalId,
    return_type: Option<ValType>,
    id: usize,
}

pub struct CodeGenerator {
    symbol_table: SymbolTable,
    name_table: NameTable,
    builtin_types: BuiltInTypes,
    type_arena: Arena<Type>,
    module: Module,
    memory_id: MemoryId,
    functions: Vec<FunctionId>,
    current_function: Option<CurrentFunctionContext>,
    function_table: TableId,
    default_functions: DefaultFunctions,
    global_variables: GlobalVariables,
}

// Global variables that we use almost as registers.
// Probably not ideal or necessary, so TODO: remove
struct GlobalVariables {
    g_i32: GlobalId,
    g_f32: GlobalId,
}

impl GlobalVariables {
    pub fn new(module: &mut Module) -> Self {
        let g_i32 = module.globals.add_local(
            ValType::I32,
            true,
            InitExpr::Value(walrus::ir::Value::I32(0)),
        );
        let g_f32 = module.globals.add_local(
            ValType::F32,
            true,
            InitExpr::Value(walrus::ir::Value::F32(0.0)),
        );

        Self { g_i32, g_f32 }
    }
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

impl DefaultFunctions {
    pub fn new(module: &mut Module) -> Self {
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

        Self {
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
}

type Result<T> = std::result::Result<T, GenerationError>;

impl CodeGenerator {
    pub fn new(typechecker: TypeChecker) -> Self {
        //let expr_func_count = typechecker.get_expr_func_index();
        let (symbol_table, name_table, type_table, builtin_types) = typechecker.get_tables();
        let config = ModuleConfig::new();
        let mut module = Module::with_config(config);
        let memory_id = module.memories.add_local(false, 1, None);
        let default_functions = DefaultFunctions::new(&mut module);
        let function_table = module.tables.insert(1, None, ValType::Funcref);

        CodeGenerator {
            symbol_table,
            name_table,
            builtin_types,
            type_arena: type_table,
            module,
            memory_id,
            function_table,
            default_functions,
            current_function: None,
            global_variables: GlobalVariables::new(&mut module),
        }
    }

    pub fn generate_program(mut self, mut program: ProgramT) -> Result<Module> {
        for local_func_index in 0..program.functions.len() {
            let func_index = (1 + PRINT_CHAR_INDEX as usize) + local_func_index;
            let function = program.functions.remove(&func_index).unwrap();
            self.generate_function(func_index, function.inner, function.location)?;
        }

        for stmt in program.stmts {
            self.generate_top_level_stmt(&stmt)?;
        }
        self.generate_default_imports()?;
        Ok(self.module)
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
                self.module.exports.add(name_str)
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
        let entry = self.symbol_table.lookup_name_in_scope(name, scope).unwrap();
        let id = entry.func_index.unwrap();
        let params = self.generate_function_params(params);
        let old_func = self.current_function;
        self.current_function = Some(CurrentFunctionContext {
            id,
            params,
            env_pointer: self.module.locals.add(ValType::I32),
            return_type: self.generate_wasm_type(self.symbol_table.functions[id].return_type),
        });
        let old_scope = self.symbol_table.swap_scope(function.scope_index);

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

    fn generate_function_params(&mut self, params: &[(Name, TypeId)]) -> HashMap<Name, LocalId> {
        let mut wasm_params = HashMap::new();
        for (param_name, param_type) in params {
            if let Some(wasm_type) = self.generate_wasm_type(*param_type) {
                let id = self.module.locals.add(wasm_type);
                wasm_params.insert(*param_name, id);
            }
        }

        wasm_params
    }

    // pub fn generate_function(
    //     &mut self,
    //     return_type: TypeId,
    //     params: &[(Name, TypeId)],
    //     local_variables: &[TypeId],
    //     body: &Loc<ExprT>,
    //     location: LocationRange,
    // ) -> Result<()> {
    //     let return_type = self.generate_wasm_type(return_type, location)?;
    //     let function_builder =
    //         self.generate_function_builder(&vec![return_type], params, location)?;
    //     self.generate_function_body(function_builder, body, local_variables, params.len())?;
    //     Ok(())
    // }

    fn generate_function_builder(
        &mut self,
        return_type: &[ValType],
        params: &[(Name, TypeId)],
        location: LocationRange,
    ) -> Result<FunctionBuilder> {
        // First type is always i32 for pointer to closure
        let mut wasm_param_types = vec![ValType::I32];

        for (_, param_type) in params {
            let wasm_type = self
                .generate_wasm_type(*param_type, location)?
                .ok_or(GenerationError::EmptyType)?;
            wasm_param_types.push(wasm_type);
        }

        Ok(FunctionBuilder::new(
            &mut self.module.types,
            &wasm_param_types,
            return_type,
        ))
    }

    fn generate_function_body(
        &mut self,
        fn_builder: &mut FunctionBuilder,
        body: &Loc<ExprT>,
        local_variables: &[TypeId],
        param_count: usize,
    ) -> Result<FunctionBody> {
        self.generate_expr(fn_builder.func_body(), body)?;

        let mut local_entries = Vec::new();
        // We want to generate only the locals not params so we skip
        // to after the params
        for local_type in &local_variables[param_count..] {
            let wasm_type = self
                .generate_wasm_type(*local_type)
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
        mut initial_args: Vec<ValType>,
    ) -> Result<Vec<ValType>> {
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

    fn generate_wasm_type(&self, sbr_type: TypeId) -> Option<ValType> {
        match &self.type_arena[sbr_type] {
            Type::Unit => None,
            Type::Int | Type::Bool | Type::Char => Some(ValType::I32),
            Type::Float => Some(ValType::F32),
            Type::String
            | Type::Array(_)
            | Type::Arrow(_, _)
            | Type::Record(_, _)
            | Type::Tuple(_)
            | Type::Var(_) => Some(ValType::I32),
            Type::Solved(type_id) => self.generate_wasm_type(*type_id),
        }
    }

    fn generate_stmt(
        &mut self,
        fn_body: &mut InstrSeqBuilder,
        stmt: &Loc<StmtT>,
        is_last: bool,
    ) -> Result<Vec<OpCode>> {
        match &stmt.inner {
            StmtT::Expr(expr) => {
                let mut opcodes = self.generate_expr(expr)?;
                match &self.type_arena[expr.inner.get_type()] {
                    Type::Unit => {}
                    _ => opcodes.push(OpCode::Drop),
                }
                if is_last {
                    // If the return type is empty, we can safely return
                    if self.return_type.is_none() {
                    } else {
                        // But otherwise, we need to push on an unreachable to assure
                        // wasm that we're not going to just end without returning
                        // anything.
                        // TODO: Actually check if we're returning nothing
                        opcodes.push(OpCode::Unreachable);
                    }
                } else {
                }
            }
            StmtT::Return(expr) => {
                if is_last {
                    self.generate_expr(expr)
                } else {
                    let mut opcodes = self.generate_expr(expr)?;
                    opcodes.push(OpCode::Return);
                }
            }
            StmtT::Let(name, expr) => {
                let entry = self.symbol_table.lookup_name(*name).unwrap();
                let var_index = entry.var_index;
                let mut opcodes = self.generate_expr(expr)?;
                opcodes.push(OpCode::SetLocal(var_index.try_into().unwrap()));
            }
            StmtT::Break => Ok(vec![OpCode::Br(1)]),
            StmtT::Loop(block) => {
                let mut opcodes = vec![OpCode::Loop, OpCode::Type(WasmType::Empty)];
                opcodes.append(&mut self.generate_expr(block)?);
                // Break to loop
                opcodes.push(OpCode::Br(0));
                opcodes.push(OpCode::End);
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
            }
            _ => todo!(),
        }
    }

    fn generate_func_args(
        &mut self,
        fn_body: &mut InstrSeqBuilder,
        args: &Loc<ExprT>,
    ) -> Result<()> {
        match &args.inner {
            ExprT::Tuple(entries, _) => {
                let mut opcodes = Vec::new();
                for entry in entries {
                    opcodes.append(&mut self.generate_expr(fn_body, entry)?)
                }
            }
            _ => self.generate_expr(fn_body, args),
        }
    }

    fn generate_expr(&mut self, fn_body: &mut InstrSeqBuilder, expr: &Loc<ExprT>) -> Result<()> {
        match &expr.inner {
            ExprT::Asgn { lhs, rhs, type_: _ } => {
                self.generate_expr(fn_body, rhs)?;
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
            ExprT::Primary { value, type_: _ } => self.generate_primary(fn_builder, value),
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
                captures_var_index,
                func_id,
                args,
                type_: _,
            } => {
                // NOTE: This is super brittle again, as if we add another runtime function,
                // we'll need to change this comparison
                if let Some(cf) = &self.current_function {
                    if cf.id == *func_id
                    // Basically if we're in a recursive function situation
                    {
                        fn_body.local_get()
                        vec![OpCode::GetLocal(0)]
                    } else if let Some(var_index) = captures_var_index {
                        fn_body.local_get(cf.env_pointer);
                    } else {
                        fn_body.i32_const(0);
                    }
                } else {
                    Vec::new()
                };
                self.generate_func_args(fn_body, args)?;
                fn_body.call(self.functions[*func_id]);
            }
            ExprT::IndirectCall {
                callee,
                args,
                type_,
            } => {
                let arg_types =
                    self.get_args_type(args.inner.get_type(), args.location, vec![ValType::I32])?;

                let return_types = if let Some(ty) = self.generate_wasm_type(*type_) {
                    vec![ty]
                } else {
                    Vec::new()
                };

                let type_sig_id = self.module.types.add(&arg_types, &return_types);
                // We have a GetGlobal here because global #1 always needs to
                // be restored to its original value after CallIndirect
                //
                // Why global #1? Well we don't have that same discipline with global #0
                // And we need to use it across generate_expr which is tricky
                let callee_id = self.module.locals.add(ValType::I32);
                self.generate_expr(fn_builder, callee)?;
                fn_body
                    .local_set(callee_id)
                    // Loading pointer to closure as first argument
                    .load(
                        self.memory_id,
                        LoadKind::I32 { atomic: false },
                        MemArg {
                            align: 2,
                            offset: 8,
                        },
                    );
                self.generate_expr(fn_builder, args)?;
                // Loading function id as second argument
                fn_body
                    .load(
                        self.memory_id,
                        LoadKind::I32 { atomic: false },
                        MemArg {
                            align: 2,
                            offset: 4,
                        },
                    )
                    .call_indirect(type_sig_id, self.function_table);
            }
            ExprT::Index {
                lhs,
                index,
                type_: _,
            } => {
                let lhs_id = self.module.locals.add(ValType::I32);
                let index_id = self.module.locals.add(ValType::I32);
                self.generate_expr(fn_body, lhs)?;
                fn_body.local_set(lhs_id);
                self.generate_expr(fn_body, index)?;
                fn_body.local_set(index_id);

                // Bounds check
                fn_body
                    .local_get(index_id)
                    .local_get(lhs_id)
                    .load(
                        self.memory_id,
                        LoadKind::I32,
                        MemArg {
                            align: 2,
                            offset: 4,
                        },
                    )
                    .binop(BinaryOp::I32LtU)
                    .if_else(
                        None.into(),
                        |then_builder| {
                            then_builder.local_get(index_id);
                            if !matches!(self.type_arena[lhs.inner.get_type()], Type::String) {
                                then_builder.i32_const(4).binop(BinaryOp::I32Mul);
                            }
                            then_builder
                                .local_get(lhs_id)
                                .binop(BinaryOp::I32Add)
                                .local_set(lhs_id);
                        },
                        |else_builder| {
                            // TODO: Handle out of bounds error
                            else_builder.unreachable();
                        },
                    );

                let load_kind = if matches!(self.type_arena[lhs.inner.get_type()], Type::String) {
                    LoadKind::I32_8 {
                        kind: ExtendedLoad::ZeroExtend,
                    }
                } else {
                    LoadKind::I32 { atomic: false }
                };

                fn_body.local_get(lhs_id).load(
                    self.memory_id,
                    load_kind,
                    MemArg {
                        align: 2,
                        offset: 0,
                    },
                );
            }
            ExprT::Array {
                entries,
                entry_type,
                type_: _,
            } => self.generate_array(fn_body, entries, *entry_type, expr.location),
            ExprT::Tuple(elems, type_id) => {
                if !elems.is_empty() {
                    self.generate_tuple(
                        fn_body,
                        elems.iter(),
                        elems.len(),
                        *type_id,
                        expr.location,
                    );
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
                    self.generate_stmt(fn_body, stmt, false)?;
                }
                if let Some(expr) = end_expr {
                    self.generate_expr(fn_body, expr)?;
                }
                let entries = self.symbol_table.get_scope_entries(*scope_index);
                for (_, entry) in entries {
                    if self.type_arena[entry.var_type].is_ref_type() {
                        opcodes.push(OpCode::GetLocal((entry.var_index).try_into().unwrap()));
                        opcodes.push(OpCode::Call(DEALLOC_INDEX));
                    }
                }
                self.symbol_table.swap_scope(old_scope);
            }
            ExprT::If(cond, then_block, else_block, type_) => {
                // Start with the cond opcodes
                self.generate_expr(fn_body, cond)?;
                let if_type = self.generate_wasm_type(*type_);
                func_body().if_else(
                    if_type.into(),
                    |then_builder| self.generate_expr(then_builder, then_block).unwrap(),
                    |else_builder| {
                        if let Some(else_block) = else_block {
                            self.generate_expr(else_builder, else_block).unwrap();
                        }
                    },
                );
            }
            ExprT::BinOp {
                op,
                lhs,
                rhs,
                type_,
            } => {
                self.generate_expr(fn_builder, lhs)?;
                self.generate_expr(fn_builder, rhs)?;
                self.generate_operator(fn_builder, op, lhs.inner.get_type())?;
            }
            ExprT::UnaryOp { op, rhs, type_: _ } => match op {
                UnaryOp::Minus => {
                    fn_body.i32_const(0);
                    self.generate_expr(fn_builder, rhs)?;
                    fn_body.binop(BinaryOp::I32Sub);
                }
                UnaryOp::Not => {
                    self.generate_expr(fn_builder, rhs)?;
                    fn_body.i32_const(-1).binop(BinaryOp::I32Xor);
                }
            },
            ExprT::TupleField(lhs, index, type_) => {
                self.generate_tuple_field(fn_builder, lhs, *index as u32, type_);
            }
            ExprT::Record {
                name: _,
                fields,
                type_,
            } => self.generate_tuple(
                fn_body,
                fields.iter().map(|(_, expr)| expr),
                fields.len(),
                *type_,
                expr.location,
            ),
        }

        Ok(())
    }

    fn generate_tuple_field(
        &mut self,
        fn_body: &mut InstrSeqBuilder,
        lhs: &Loc<ExprT>,
        index: u32,
        type_: &TypeId,
    ) -> Result<()> {
        self.generate_expr(fn_body, &*lhs)?;
        let mut ty = self.generate_wasm_type(*type_);

        // In the future we'll have tuples expand to more than one type
        if let Some(field_wasm_type) = ty {
            // Account for type_id
            let field_loc = index + 1;
            let offset = 4 * field_loc;
            let load_kind = match field_wasm_type {
                ValType::I32 => LoadKind::I32 { atomic: false },
                ValType::F32 => LoadKind::F32,
                _ => {
                    return Err(GenerationError::NotReachable);
                }
            };

            fn_body
                .func_body()
                .load(self.memory_id, load_kind, MemArg { offset, align: 2 });
        } else {
            fn_body.drop();
        }

        Ok(())
    }

    fn generate_array(
        &mut self,
        fn_body: &mut InstrSeqBuilder,
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
            self.generate_allocated_expr(fn_body, entry, offset)?;
            offset += 4;
        }
        Ok(opcodes)
    }

    // Generate allocated expression for array or tuple
    fn generate_allocated_expr(
        &mut self,
        fn_body: &mut InstrSeqBuilder,
        expr: &Loc<ExprT>,
        offset: u32,
    ) -> Result<()> {
        self.generate_expr(fn_body, expr)?;

        if let Some(ty) = self.generate_wasm_type(expr.inner.get_type()) {
            let store_kind = match ty {
                // Alignment of 2, offset of 0
                ValType::I32 => StoreKind::I32 { atomic: false },
                ValType::F32 => StoreKind::F32,
                _ => {
                    return Err(GenerationError::NotImplemented {
                        reason: "Cannot generate code to store types other than i32 and f32",
                    })
                }
            };

            fn_body.store(
                self.memory_id,
                store_kind,
                MemArg {
                    align: 5,
                    offset: (offset * 4) as u32,
                },
            );
        } else {
            fn_body.drop();
        }

        Ok(())
    }

    fn generate_tuple<'a, I>(
        &mut self,
        fn_body: &mut InstrSeqBuilder,
        fields: I,
        length: usize,
        type_id: TypeId,
        location: LocationRange,
    ) -> Result<()>
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

        let block_ptr = self.module.locals.add(ValType::I32);

        fn_body
            .i32_const(record_size)
            .call(self.default_functions.alloc)
            .local_set(block_ptr);

        // TODO: Add proper error handling for too many types.
        let final_type_id = get_final_type(&self.type_arena, type_id);

        fn_body
            .i32_const(final_type_id.index().try_into().unwrap())
            .store(
                self.memory_id,
                StoreKind::I32 { atomic: false },
                MemArg { align: 2, offset },
            );

        offset += 4;
        // We need to write the struct to memory. This consists of
        // looping through entries, generating the opcodes and
        // storing them in the heap
        for expr in fields {
            self.generate_allocated_expr(fn_builder, expr, offset)?;
            offset += 4;
        }

        fn_body.local_get(block_ptr);

        Ok(())
    }

    fn generate_primary(&mut self, fn_builder: &mut InstrSeqBuilder, value: &Value) -> Result<()> {
        let mut fn_body = fn_builder.func_body();
        match value {
            Value::Float(f) => {
                fn_body.f32_const(*f);
            }
            Value::Integer(i) => {
                fn_body.i32_const(*i);
            }
            Value::Bool(b) => {
                fn_body.i32_const(*b as i32);
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

                let str_ptr = self.module.locals.add(ValType::I32);

                fn_body
                    // We tack on 8 more bytes for the length and type_id
                    .i32_const(buffer_length + 8)
                    .call(self.default_functions.alloc)
                    .local_set(str_ptr);

                // Set type_id
                // Get ptr to record
                fn_body
                    .local_get(str_ptr)
                    .i32_const(self.builtin_types.string.index().try_into().unwrap())
                    .store(
                        self.memory_id,
                        StoreKind::I32 { atomic: false },
                        MemArg { align: 5, offset },
                    );

                offset += 4;

                fn_body.local_get(str_ptr).i32_const(raw_str_length).store(
                    self.memory_id,
                    StoreKind::I32 { atomic: false },
                    MemArg { align: 5, offset },
                );

                for b in bytes.chunks_exact(4) {
                    offset += 4;
                    let packed_bytes = (b[0] as u32)
                        + ((b[1] as u32) << 8)
                        + ((b[2] as u32) << 16)
                        + ((b[3] as u32) << 24);

                    fn_body
                        .local_get(str_ptr)
                        .i32_const(packed_bytes as i32)
                        .store(
                            self.memory_id,
                            StoreKind::I32 { atomic: false },
                            MemArg { align: 5, offset },
                        );
                }
            }
        }

        Ok(())
    }

    fn generate_operator(
        &self,
        fn_builder: &mut InstrSeqBuilder,
        op: &Op,
        input_type: TypeId,
    ) -> Result<()> {
        let op = match (op, &self.type_arena[input_type]) {
            (Op::Plus, Type::Int) => BinaryOp::I32Add,
            (Op::Minus, Type::Int) => BinaryOp::I32Sub,
            (Op::Plus, Type::Float) => BinaryOp::F32Add,
            (Op::Minus, Type::Float) => BinaryOp::F32Sub,
            (Op::Times, Type::Int) => BinaryOp::I32Mul,
            (Op::Div, Type::Int) => BinaryOp::I32DivS,
            (Op::Times, Type::Float) => BinaryOp::F32Mul,
            (Op::Div, Type::Float) => BinaryOp::F32Div,
            (Op::Less, Type::Int) => BinaryOp::I32LtS,
            (Op::Greater, Type::Int) => BinaryOp::I32GtS,
            (Op::GreaterEqual, Type::Int) => BinaryOp::I32GeS,
            (Op::EqualEqual, Type::Int) => BinaryOp::I32Eq,
            (Op::EqualEqual, Type::String) => {
                fn_builder.func_body().call(self.default_functions.streq);
                return Ok(());
            }
            (op, _) => {
                return Err(GenerationError::InvalidOperator {
                    op: op.clone(),
                    input_type: type_to_string(&self.name_table, &self.type_arena, input_type),
                })
            }
        };

        fn_builder.func_body().binop(op);

        Ok(())
    }
}
