use crate::ast;
use crate::ast::{Export, ExportKind, ExprT, ProgramT, StmtT, Value};
use crate::mir::{
    BinaryOp, Block, BlockId, CaptureBlock, Function, FunctionId, InstrId, Instruction,
    InstructionKind, Primary, Program, Type, UnaryOp,
};
use crate::symbol_table::SymbolTable;
use crate::typechecker::TypeChecker;
use crate::utils::{get_final_type, NameTable};
use id_arena::Arena;
use indexmap::set::IndexSet;
use std::mem::{replace, swap};

pub struct MirCompiler {
    pub string_literals: Vec<String>,
    pub name_table: NameTable,
    symbol_table: SymbolTable,
    blocks: Vec<Block>,
    current_block: Vec<Instruction>,
    type_arena: Arena<ast::Type>,
    functions: Vec<Function>,
    // Contains local var indices
    function_captures: Vec<IndexSet<(usize, Type)>>,
    capture_blocks: Vec<CaptureBlock>,
}

impl MirCompiler {
    pub fn new(type_checker: TypeChecker) -> Self {
        let TypeChecker {
            name_table,
            symbol_table,
            type_arena,
            ..
        } = type_checker;

        MirCompiler {
            string_literals: Vec::new(),
            blocks: Vec::new(),
            current_block: Vec::new(),
            name_table,
            symbol_table,
            type_arena,
            functions: Vec::new(),
            function_captures: Vec::new(),
            capture_blocks: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn print_functions(&self) {
        for (idx, func) in self.functions.iter().enumerate() {
            println!();
            println!("#{}", idx);
            println!("params: {:?}", func.params);
            for block in &func.body {
                Self::print_block(&block.0)
            }
        }
    }

    #[allow(dead_code)]
    pub fn print_current_function(&self) {
        for block in &self.blocks {
            Self::print_block(&block.0);
        }
        Self::print_block(&self.current_block)
    }

    #[allow(dead_code)]
    pub fn print_block(block: &[Instruction]) {
        println!("-----------------");
        for instr in block {
            println!("{:?}", instr);
        }
        println!("-----------------");
    }

    pub fn compile_program(&mut self, program: ProgramT) -> Program {
        for function in program.functions {
            self.compile_function(function.inner);
        }

        self.compile_capture_blocks();

        for Export { kind, idx, name } in program.exports {
            match kind {
                ExportKind::Var => {
                    todo!()
                }
                ExportKind::Function => {
                    self.functions[idx].export_name = Some(name);
                }
            }
        }

        Program {
            functions: replace(&mut self.functions, Vec::new()),
            string_literals: replace(&mut self.string_literals, Vec::new()),
        }
    }

    fn add_instruction(&mut self, kind: InstructionKind, ty: Option<Type>) -> InstrId {
        self.current_block.push(Instruction { kind, ty });
        InstrId(self.current_block.len() - 1)
    }

    fn compile_capture_blocks(&mut self) {
        let capture_blocks = replace(&mut self.capture_blocks, Vec::new());
        let function_captures = replace(&mut self.function_captures, Vec::new());
        for CaptureBlock {
            enclosing_function,
            defined_function,
            block_id,
        } in capture_blocks
        {
            // Set the capture block to be current block;
            swap(
                &mut self.current_block,
                &mut self.functions[enclosing_function.0].body[block_id.0].0,
            );

            let captures = &function_captures[defined_function.0];
            let alloc_size = captures.len() * 4;
            let mut ptr =
                self.add_instruction(InstructionKind::Alloc(alloc_size), Some(Type::Pointer));
            let offset =
                self.add_instruction(InstructionKind::Primary(Primary::I32(4)), Some(Type::I32));
            let original_ptr = ptr;

            for (var_index, var_type) in captures {
                let op = match var_type {
                    Type::Pointer => BinaryOp::PointerStore,
                    Type::I32 => BinaryOp::I32Store,
                    Type::F32 => BinaryOp::F32Store,
                };
                let rhs_id =
                    self.add_instruction(InstructionKind::VarGet(*var_index), Some(*var_type));
                self.add_instruction(InstructionKind::Binary(op, ptr, rhs_id), None);
                ptr = self.add_instruction(
                    InstructionKind::Binary(BinaryOp::PointerAdd, ptr, offset),
                    Some(Type::Pointer),
                )
            }

            self.add_instruction(InstructionKind::Return(original_ptr), Some(Type::Pointer));

            swap(
                &mut self.current_block,
                &mut self.functions[enclosing_function.0].body[block_id.0].0,
            );
        }
    }

    fn compile_function(&mut self, function: ast::Function) {
        self.function_captures.push(IndexSet::new());
        let old_scope = self.symbol_table.swap_scope(function.scope_index);

        for stmt in function.body {
            self.compile_stmt(stmt.inner);
        }

        let mut body = replace(&mut self.blocks, Vec::new());
        body.push(Block(replace(&mut self.current_block, Vec::new())));
        let mut params = Vec::new();
        for (_, param_type) in function.params {
            if let Some(ty) = self.ast_type_to_mir_type(param_type) {
                params.push(ty)
            }
        }

        self.functions.push(Function {
            params,
            body,
            returns: self
                .ast_type_to_mir_type(function.return_type)
                .into_iter()
                .collect(),
            export_name: None,
        });

        self.symbol_table.swap_scope(old_scope);
    }

    fn ast_type_to_mir_type(&self, type_id: ast::TypeId) -> Option<Type> {
        match self.type_arena[type_id] {
            ast::Type::Integer => Some(Type::I32),
            ast::Type::Float => Some(Type::F32),
            ast::Type::Bool => Some(Type::I32),
            ast::Type::Char => Some(Type::I32),
            ast::Type::String
            | ast::Type::Var(_)
            | ast::Type::Array(_)
            | ast::Type::Record(_, _)
            | ast::Type::Tuple(_)
            | ast::Type::Arrow(_, _) => Some(Type::Pointer),
            ast::Type::Solved(id) => self.ast_type_to_mir_type(id),
            ast::Type::Unit => None,
        }
    }

    fn ast_type_to_load_op(&self, ty: Option<Type>) -> Option<UnaryOp> {
        ty.map(|t| match t {
            Type::I32 => UnaryOp::I32Load,
            Type::F32 => UnaryOp::F32Load,
            Type::Pointer => UnaryOp::PointerLoad,
        })
    }

    fn ast_type_to_store_op(&self, ty: Option<Type>) -> Option<BinaryOp> {
        ty.map(|t| match t {
            Type::I32 => BinaryOp::I32Store,
            Type::F32 => BinaryOp::F32Store,
            Type::Pointer => BinaryOp::PointerStore,
        })
    }

    // Compiles enclosed variable by going up scope chain
    fn compile_enclosed_var(
        &mut self,
        var_type: Type,
        var_index: usize,
        function_scopes: Vec<usize>,
    ) -> InstrId {
        let mut env_ptr = self.add_instruction(InstructionKind::VarGet(0), Some(Type::Pointer));
        let len = function_scopes.len();
        for (idx, func_index) in function_scopes.into_iter().enumerate() {
            // If we're at the last function scope, we need to get the specific field
            if idx == len - 1 {
                let (captures_index, _) =
                    self.function_captures[func_index].insert_full((var_index, var_type));

                let offset = self.add_instruction(
                    InstructionKind::Primary(Primary::I32((captures_index * 4) as i32)),
                    Some(Type::I32),
                );

                env_ptr = self.add_instruction(
                    InstructionKind::Binary(BinaryOp::PointerAdd, env_ptr, offset),
                    Some(Type::Pointer),
                );
            } else {
                env_ptr = self.add_instruction(
                    InstructionKind::Unary(UnaryOp::PointerLoad, env_ptr),
                    Some(var_type),
                );
            }
        }

        env_ptr
    }

    pub fn compile_stmt(&mut self, stmt: StmtT) -> InstrId {
        match stmt {
            StmtT::Expr(expr) => {
                let ty = get_final_type(&self.type_arena, expr.inner.get_type());
                let id = self.compile_expr(expr.inner);

                if !matches!(self.type_arena[ty], ast::Type::Unit) {
                    self.add_instruction(InstructionKind::Unary(UnaryOp::Drop, id), None)
                } else {
                    id
                }
            }
            StmtT::Return(expr) => {
                let id = self.compile_expr(expr.inner);
                self.add_instruction(InstructionKind::Return(id), None)
            }
            StmtT::Let(name, rhs) => {
                let entry = self.symbol_table.lookup_name(name).unwrap();
                let var_index = entry.var_index;
                let id = self.compile_expr(rhs.inner);

                self.add_instruction(InstructionKind::VarSet(var_index, id), None)
            }
            s => todo!("{:?}", s),
        }
    }

    pub fn compile_expr(&mut self, expr: ExprT) -> InstrId {
        let ty = self.ast_type_to_mir_type(expr.get_type());
        match expr {
            ExprT::Asgn { lhs, rhs, type_: _ } => {
                if !lhs.inner.accessors.is_empty() {
                    todo!();
                }

                let rhs_id = self.compile_expr(rhs.inner);

                let (entry, function_scopes) = self
                    .symbol_table
                    .lookup_name_with_function_hierarchy(lhs.inner.ident)
                    .unwrap();

                if function_scopes.is_empty() {
                    let var_index = entry.var_index;
                    self.add_instruction(InstructionKind::VarSet(var_index, rhs_id), None)
                } else {
                    let ty = ty.expect("Must be assignable");
                    let var_index = entry.var_index;
                    let var_ptr = self.compile_enclosed_var(ty, var_index, function_scopes);
                    let op = match ty {
                        Type::I32 => BinaryOp::I32Store,
                        Type::F32 => BinaryOp::F32Store,
                        Type::Pointer => BinaryOp::PointerStore,
                    };
                    self.add_instruction(InstructionKind::Binary(op, var_ptr, rhs_id), None)
                }
            }
            ExprT::Block {
                stmts,
                end_expr,
                scope_index,
                type_: _,
            } => {
                let old_scope = self.symbol_table.swap_scope(scope_index);

                let mut instr_id = None;
                for stmt in stmts {
                    instr_id = Some(self.compile_stmt(stmt.inner));
                }

                if let Some(end_expr) = end_expr {
                    instr_id = Some(self.compile_expr(end_expr.inner));
                }

                self.symbol_table.swap_scope(old_scope);

                instr_id.unwrap() // If instr_id is None, we've messed up in the type checker
            }
            ExprT::If(cond, then_expr, else_expr, _) => {
                let current_block = replace(&mut self.current_block, Vec::new());

                self.compile_expr(then_expr.inner);
                let then_block = replace(&mut self.current_block, Vec::new());
                self.blocks.push(Block(then_block));
                let then_block_id = self.blocks.len() - 1;

                self.compile_expr(else_expr.inner);
                let else_block = replace(&mut self.current_block, current_block);
                self.blocks.push(Block(else_block));
                let else_block_id = self.blocks.len() - 1;

                let cond_id = self.compile_expr(cond.inner);

                self.add_instruction(
                    InstructionKind::If(cond_id, BlockId(then_block_id), BlockId(else_block_id)),
                    ty,
                )
            }
            ExprT::Primary { value, type_: _ } => {
                let instruction = InstructionKind::Primary(self.compile_value(value));
                self.add_instruction(instruction, ty)
            }
            ExprT::Var { name, type_: _ } => {
                let (entry, function_scopes) = self
                    .symbol_table
                    .lookup_name_with_function_hierarchy(name)
                    .unwrap();

                if function_scopes.is_empty() {
                    let var_index = entry.var_index;
                    self.add_instruction(InstructionKind::VarGet(var_index), ty)
                } else {
                    if let Some(ty) = ty {
                        let var_index = entry.var_index;
                        let var_ptr = self.compile_enclosed_var(ty, var_index, function_scopes);
                        let op = match ty {
                            Type::I32 => UnaryOp::I32Load,
                            Type::F32 => UnaryOp::F32Load,
                            Type::Pointer => UnaryOp::PointerLoad,
                        };
                        self.add_instruction(InstructionKind::Unary(op, var_ptr), Some(ty))
                    } else {
                        self.add_instruction(InstructionKind::Noop, None)
                    }
                }
            }
            ExprT::BinOp {
                op,
                lhs,
                rhs,
                type_: _,
            } => {
                let lhs_id = self.compile_expr(lhs.inner);
                let rhs_id = self.compile_expr(rhs.inner);

                self.add_instruction(InstructionKind::Binary(op.into(), lhs_id, rhs_id), ty)
            }
            ExprT::UnaryOp { op, rhs, type_: _ } => {
                let rhs_id = self.compile_expr(rhs.inner);

                self.add_instruction(InstructionKind::Unary(op.into(), rhs_id), ty)
            }
            ExprT::Function {
                func_index,
                type_: _,
            } => {
                self.blocks.push(Block(Vec::new()));
                let block_id = BlockId(self.blocks.len() - 1);

                self.capture_blocks.push(CaptureBlock {
                    enclosing_function: FunctionId(self.functions.len()),
                    defined_function: FunctionId(func_index),
                    block_id,
                });

                self.add_instruction(InstructionKind::Br(block_id), Some(Type::Pointer))
            }
            ExprT::Record {
                name: _,
                fields,
                type_,
            } => {
                // We have to store the fields because we need to know how many
                // fields are actually reified versus just empty fields
                // TODO: Figure out if reification should be its own pass
                let mut field_ids_and_ops = Vec::new();

                for (_, field) in fields {
                    let field_ty = self.ast_type_to_mir_type(field.inner.get_type());
                    let field_id = self.compile_expr(field.inner);

                    let op = match self.ast_type_to_store_op(field_ty) {
                        Some(op) => op,
                        // If there's no MIR type, this is a unit expression
                        // and doesn't need to be stored
                        None => continue,
                    };
                    field_ids_and_ops.push((field_id, op));
                }

                let record_len_in_words = field_ids_and_ops.len() * 4 + 1;
                let original_ptr = self.add_instruction(
                    InstructionKind::Alloc(record_len_in_words),
                    Some(Type::Pointer),
                );

                let mut ptr = original_ptr;
                let word = self
                    .add_instruction(InstructionKind::Primary(Primary::I32(32)), Some(Type::I32));
                let final_ty = get_final_type(&self.type_arena, type_);
                let final_ty_id = self.add_instruction(
                    InstructionKind::Primary(Primary::I32(final_ty.index() as i32)),
                    Some(Type::I32),
                );

                self.add_instruction(
                    InstructionKind::Binary(BinaryOp::I32Store, ptr, final_ty_id),
                    Some(Type::I32),
                );
                ptr = self.add_instruction(
                    InstructionKind::Binary(BinaryOp::I32Add, ptr, word),
                    Some(Type::I32),
                );

                for (id, op) in field_ids_and_ops {
                    self.add_instruction(InstructionKind::Binary(op, ptr, id), None);

                    ptr = self.add_instruction(
                        InstructionKind::Binary(BinaryOp::PointerAdd, ptr, word),
                        Some(Type::Pointer),
                    );
                }

                original_ptr
            }
            ExprT::Index { lhs, index, type_ } => {
                let ty = self.ast_type_to_mir_type(type_);
                let op = match self.ast_type_to_load_op(ty) {
                    None => {
                        return self.add_instruction(InstructionKind::Noop, None);
                    }
                    Some(op) => op,
                };

                let lhs_id = self.compile_expr(lhs.inner);
                let index_id = self.compile_expr(index.inner);
                // TODO: Define stuff like word once
                let word = self
                    .add_instruction(InstructionKind::Primary(Primary::I32(32)), Some(Type::I32));
                let index_in_bytes = self.add_instruction(
                    InstructionKind::Binary(BinaryOp::I32Mul, index_id, word),
                    Some(Type::I32),
                );

                let ptr = self.add_instruction(
                    InstructionKind::Binary(BinaryOp::PointerAdd, lhs_id, index_in_bytes),
                    Some(Type::Pointer),
                );

                self.add_instruction(InstructionKind::Unary(op, ptr), ty)
            }
            ExprT::TupleField(tuple, index, _) => {
                let op = match self.ast_type_to_load_op(ty) {
                    None => {
                        return self.add_instruction(InstructionKind::Noop, None);
                    }
                    Some(op) => op,
                };

                let index = index + 1; // Gotta account for type tag
                let tuple_id = self.compile_expr(tuple.inner);
                let index_id = self.add_instruction(
                    InstructionKind::Primary(Primary::I32((index * 32) as i32)),
                    Some(Type::I32),
                );

                let ptr = self.add_instruction(
                    InstructionKind::Binary(BinaryOp::PointerAdd, tuple_id, index_id),
                    Some(Type::Pointer),
                );

                self.add_instruction(InstructionKind::Unary(op, ptr), ty)
            }
            ExprT::Print { args, type_ } => {
                let op = match self.type_arena[args.inner.get_type()] {
                    ast::Type::Integer => UnaryOp::PrintInt,
                    ast::Type::Float => UnaryOp::PrintFloat,
                    _ => UnaryOp::PrintPointer,
                };
                let args_id = self.compile_expr(args.inner);
                self.add_instruction(InstructionKind::Unary(op, args_id), None)
            }
            ExprT::DirectCall {
                callee,
                args,
                type_: _,
            } => {
                let args = match args.inner {
                    ExprT::Tuple(fields, _) => fields
                        .into_iter()
                        .map(|field| self.compile_expr(field.inner))
                        .collect(),
                    e => {
                        let expr_type = e.get_type();
                        let expr_id = self.compile_expr(e);

                        let fields_types = if let ast::Type::Tuple(fields_types) =
                            &self.type_arena[get_final_type(&self.type_arena, expr_type)]
                        {
                            // TODO: Remove this clone
                            fields_types.clone()
                        } else {
                            todo!("Cannot have a non-tuple type here. Can we catch this in the type checker?")
                        };

                        let mut ids = Vec::new();
                        for ty in fields_types {
                            let ty = self.ast_type_to_mir_type(ty);
                            let op = self
                                .ast_type_to_load_op(ty)
                                .expect("Should be assignable type here");

                            ids.push(self.add_instruction(InstructionKind::Unary(op, expr_id), ty));
                        }

                        ids
                    }
                };

                self.add_instruction(InstructionKind::Call(FunctionId(callee), args), ty)
            }
            e => todo!("{:?}", e),
        }
    }

    fn compile_value(&mut self, value: Value) -> Primary {
        match value {
            Value::Float(f) => Primary::F32(f),
            Value::Integer(i) => Primary::I32(i),
            Value::Bool(b) => Primary::I32(b as i32),
            Value::String(s) => {
                self.string_literals.push(s);
                Primary::String(self.string_literals.len() - 1)
            }
        }
    }
}
