/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

use crate::ast;
use crate::ast::{BinaryOpT, Export, ExportKind, ExprT, ProgramT, StmtT, Value};
use crate::symbol_table::SymbolTable;
use crate::typechecker::TypeChecker;
use crate::utils::{get_final_type, NameTable};
use id_arena::Arena;
use indexmap::set::IndexSet;
use std::fmt;
use std::fmt::Formatter;
use std::mem::{replace, swap};

pub type Name = usize;
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct InstrId(pub usize);
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BlockId(pub usize);
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FunctionId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    I32,
    F32,
    Pointer,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Pointer => write!(f, "pointer"),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub string_literals: Vec<String>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }

        writeln!(f, "Strings:")?;
        for (i, string) in self.string_literals.iter().enumerate() {
            writeln!(f, "${}: {}", i, string)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Type>,
    pub returns: Vec<Type>,
    pub body: Vec<Block>,
    pub export_name: Option<Name>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<_>>()
            .join(", ");
        let returns = self
            .returns
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<_>>()
            .join(", ");

        writeln!(f, "({}) -> ({})", params, returns)?;

        let body = self
            .body
            .iter()
            .enumerate()
            .map(|(i, b)| format!("@{}\n{}", i, b))
            .collect::<Vec<_>>()
            .join("\n");

        writeln!(f, "{}", body)
    }
}

struct CaptureBlock {
    // Function in which the function is defined
    enclosing_function: FunctionId,
    // Block id for enclosing_function
    block_id: BlockId,
    // Function that is being defined
    defined_function: FunctionId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Instruction>);

impl fmt::Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "------------------")?;
        let instructions = self
            .0
            .iter()
            .enumerate()
            .map(|(i, instr)| format!("#{}: {}", i, instr))
            .collect::<Vec<_>>()
            .join("\n");
        writeln!(f, "{}", instructions)?;
        writeln!(f, "------------------")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub ty: Option<Type>,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}",
            self.kind,
            self.ty.map(|t| t.to_string()).unwrap_or("None".to_string())
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    // This could be too specific. I might want
    // blocks to have outgoing edges and have a
    // break with index as the instruction
    If(InstrId, BlockId, BlockId),
    Binary(BinaryOp, InstrId, InstrId),
    Unary(UnaryOp, InstrId),
    Primary(Primary),
    VarSet(usize, InstrId),
    VarGet(usize),
    Return(InstrId),
    Call(FunctionId, Vec<InstrId>),
    Noop,
    // We use this for captures. Basically we need to compile the captures struct
    // AFTER the compilation finishes. We do this by having an unconditional branch
    // to a block and then compiling the struct in that block after
    Br(BlockId),
    Alloc(usize),
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InstructionKind::If(cond_id, then_block_id, else_block_id) => {
                write!(
                    f,
                    "if #{} @{} @{}",
                    cond_id.0, then_block_id.0, else_block_id.0
                )
            }
            InstructionKind::Binary(op, lhs_id, rhs_id) => {
                write!(f, "{} #{} #{}", op, lhs_id.0, rhs_id.0)
            }
            InstructionKind::Unary(op, rhs_id) => write!(f, "{} #{}", op, rhs_id.0),
            InstructionKind::Primary(primary) => write!(f, "{}", primary),
            InstructionKind::VarSet(idx, rhs_id) => write!(f, "var.set {} #{}", idx, rhs_id.0),
            InstructionKind::VarGet(idx) => write!(f, "var.get {}", idx),
            InstructionKind::Return(id) => write!(f, "return #{}", id.0),
            InstructionKind::Call(func_idx, args) => {
                write!(f, "call {}", func_idx.0)?;
                for arg in args {
                    write!(f, " #{}", arg.0)?;
                }
                Ok(())
            }
            InstructionKind::Noop => write!(f, "noop"),
            InstructionKind::Br(block_id) => write!(f, "br @{}", block_id.0),
            InstructionKind::Alloc(size) => write!(f, "alloc {}", size),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    I32(i32),
    // TODO: Make this into a general purpose pointer type
    //  that indexes into a vec of heap allocated values
    String(usize),
    F32(f32),
}

impl fmt::Display for Primary {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Primary::I32(i) => write!(f, "i32.const {}", i),
            Primary::String(i) => write!(f, "str.const ${}", i),
            Primary::F32(i) => write!(f, "f32.const {}", i),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32NotEqual,
    I32Equal,
    I32Greater,
    I32GreaterEqual,
    I32Less,
    I32LessEqual,
    I32And,
    I32Or,
    I32Store,
    I32Tee,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32NotEqual,
    F32Equal,
    F32Greater,
    F32GreaterEqual,
    F32Less,
    F32LessEqual,
    F32Store,
    BoolAnd,
    BoolOr,
    StringEqual,
    StringConcat,
    PointerStore,
    PointerAdd,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::I32Add => "i32.add",
                BinaryOp::I32Sub => "i32.sub",
                BinaryOp::I32Mul => "i32.mul",
                BinaryOp::I32Div => "i32.div",
                BinaryOp::I32NotEqual => "i32.not_eq",
                BinaryOp::I32Equal => "i32.eq",
                BinaryOp::I32Greater => "i32.gt",
                BinaryOp::I32GreaterEqual => "i32.gt_eq",
                BinaryOp::I32Less => "i32.lt",
                BinaryOp::I32LessEqual => "i32.lt_eq",
                BinaryOp::I32And => "i32.and",
                BinaryOp::I32Or => "i32.or",
                BinaryOp::I32Store => "i32.store",
                BinaryOp::I32Tee => "i32.tee",
                BinaryOp::F32Add => "f32.add",
                BinaryOp::F32Sub => "f32.sub",
                BinaryOp::F32Mul => "f32.mul",
                BinaryOp::F32Div => "f32.div",
                BinaryOp::F32NotEqual => "f32.not_eq",
                BinaryOp::F32Equal => "f32.eq",
                BinaryOp::F32Greater => "f32.gt",
                BinaryOp::F32GreaterEqual => "f32.gt_eq",
                BinaryOp::F32Less => "f32.lt",
                BinaryOp::F32LessEqual => "f32.lt_eq",
                BinaryOp::F32Store => "f32.store",
                BinaryOp::BoolAnd => "bool.and",
                BinaryOp::BoolOr => "bool.or",
                BinaryOp::StringEqual => "str.eq",
                BinaryOp::StringConcat => "str.concat",
                BinaryOp::PointerStore => "ptr.store",
                BinaryOp::PointerAdd => "ptr.add",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    PointerLoad,
    F32Load,
    I32Load,
    F32Negate,
    I32Negate,
    BoolNegate,
    Drop,
    // Eventually would like to have print be just a function but that requires generics
    PrintFloat,
    PrintInt,
    PrintPointer,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::PointerLoad => "ptr.load",
                UnaryOp::F32Load => "f32.load",
                UnaryOp::I32Load => "i32.load",
                UnaryOp::F32Negate => "f32.neg",
                UnaryOp::I32Negate => "i32.neg",
                UnaryOp::BoolNegate => "bool.not",
                UnaryOp::Drop => "drop",
                UnaryOp::PrintFloat => "print_float",
                UnaryOp::PrintInt => "print_int",
                UnaryOp::PrintPointer => "print_pointer",
            }
        )
    }
}

impl From<ast::UnaryOpT> for UnaryOp {
    fn from(op: ast::UnaryOpT) -> Self {
        match op {
            ast::UnaryOpT::I32Minus => UnaryOp::I32Negate,
            ast::UnaryOpT::F32Minus => UnaryOp::F32Negate,
            ast::UnaryOpT::BoolNot => UnaryOp::BoolNegate,
        }
    }
}

impl From<BinaryOpT> for BinaryOp {
    fn from(op: BinaryOpT) -> Self {
        match op {
            BinaryOpT::I32Add => BinaryOp::I32Add,
            BinaryOpT::I32Sub => BinaryOp::I32Sub,
            BinaryOpT::I32Mul => BinaryOp::I32Mul,
            BinaryOpT::I32Div => BinaryOp::I32Div,
            BinaryOpT::I32NotEqual => BinaryOp::I32NotEqual,
            BinaryOpT::I32Equal => BinaryOp::I32Equal,
            BinaryOpT::I32Greater => BinaryOp::I32Greater,
            BinaryOpT::I32GreaterEqual => BinaryOp::I32GreaterEqual,
            BinaryOpT::I32Less => BinaryOp::I32Less,
            BinaryOpT::I32LessEqual => BinaryOp::I32LessEqual,
            BinaryOpT::I32And => BinaryOp::I32And,
            BinaryOpT::I32Or => BinaryOp::I32Or,
            BinaryOpT::F32Add => BinaryOp::F32Add,
            BinaryOpT::F32Sub => BinaryOp::F32Sub,
            BinaryOpT::F32Mul => BinaryOp::F32Mul,
            BinaryOpT::F32Div => BinaryOp::F32Div,
            BinaryOpT::F32NotEqual => BinaryOp::F32NotEqual,
            BinaryOpT::F32Equal => BinaryOp::F32Equal,
            BinaryOpT::F32Greater => BinaryOp::F32Greater,
            BinaryOpT::F32GreaterEqual => BinaryOp::F32GreaterEqual,
            BinaryOpT::F32Less => BinaryOp::F32Less,
            BinaryOpT::F32LessEqual => BinaryOp::F32LessEqual,
            BinaryOpT::BoolAnd => BinaryOp::BoolAnd,
            BinaryOpT::BoolOr => BinaryOp::BoolOr,
            BinaryOpT::StringEqual => BinaryOp::StringEqual,
            BinaryOpT::StringConcat => BinaryOp::StringConcat,
        }
    }
}

pub struct MirCompiler {
    pub string_literals: Vec<String>,
    pub name_table: NameTable,
    parameters: Vec<Type>,
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
            parameters: Vec::new(),
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
        })
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

                    let op = match field_ty {
                        Some(Type::F32) => BinaryOp::F32Store,
                        Some(Type::I32) => BinaryOp::F32Store,
                        Some(Type::Pointer) => BinaryOp::PointerStore,
                        // If there's no MIR type, this is a unit expression
                        // and doesn't need to be stored
                        None => continue,
                    };
                    field_ids_and_ops.push((field_id, op));
                }

                let record_len_in_words = field_ids_and_ops.len() * 4 + 1;
                let mut ptr = self.add_instruction(
                    InstructionKind::Alloc(record_len_in_words),
                    Some(Type::Pointer),
                );
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

                ptr
            }
            ExprT::Print { args, type_ } => {
                let op = match self.type_arena[type_] {
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
                            // TODO: Deduplicate code
                            let op = match ty.expect("Should be assignable type here") {
                                Type::I32 => UnaryOp::I32Load,
                                Type::F32 => UnaryOp::F32Load,
                                Type::Pointer => UnaryOp::PointerLoad,
                            };
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

#[cfg(test)]
mod test {
    use crate::ast::{BinaryOpT, ExprT, Loc, Type};
    use crate::lexer::LocationRange;
    use crate::loc;
    use crate::mir::{BinaryOp, Block, BlockId, InstrId, InstructionKind, Value};
    use crate::mir::{MirCompiler, Primary};
    use crate::symbol_table::SymbolTable;
    use id_arena::Arena;

    #[test]
    fn test_compile_value() {
        let mut compiler = MirCompiler::new(SymbolTable::new());

        assert_eq!(
            compiler.compile_value(Value::Integer(120)),
            Primary::I32(120)
        );

        assert_eq!(
            compiler.compile_value(Value::String("Sam Rayburn".to_string())),
            Primary::String(0)
        );

        assert_eq!(compiler.compile_value(Value::Bool(true)), Primary::I32(1));

        assert_eq!(compiler.compile_value(Value::Bool(false)), Primary::I32(0));

        assert_eq!(compiler.compile_value(Value::Float(2.3)), Primary::F32(2.3));
    }

    #[test]
    fn test_compile_bin_op() {
        let mut compiler = MirCompiler::new(SymbolTable::new());
        let mut type_arena: Arena<Type> = Arena::new();
        let int_id = type_arena.alloc(Type::Integer);

        compiler.compile_expr(ExprT::BinOp {
            op: BinaryOpT::I32Add,
            lhs: Box::new(loc!(
                ExprT::Primary {
                    value: Value::Integer(20),
                    type_: int_id
                },
                LocationRange(0, 4)
            )),
            rhs: Box::new(loc!(
                ExprT::Primary {
                    value: Value::Integer(40),
                    type_: int_id
                },
                LocationRange(0, 6)
            )),
            type_: int_id,
        });

        let instructions = compiler.get_current_block();
        assert_eq!(
            instructions,
            &vec![
                InstructionKind::Primary(Primary::I32(20)),
                InstructionKind::Primary(Primary::I32(40)),
                InstructionKind::Binary(BinaryOp::I32Add, InstrId(0), InstrId(1))
            ]
        );
    }

    #[test]
    fn test_compile_if_expr() {
        let mut type_arena: Arena<Type> = Arena::new();
        let int_id = type_arena.alloc(Type::Integer);
        let bool_id = type_arena.alloc(Type::Bool);
        let mut compiler = MirCompiler::new(SymbolTable::new(), type_arena);

        compiler.compile_expr(ExprT::If(
            Box::new(loc!(
                ExprT::Primary {
                    value: Value::Bool(true),
                    type_: bool_id
                },
                LocationRange(0, 6)
            )),
            Box::new(loc!(
                ExprT::Primary {
                    value: Value::Integer(20),
                    type_: int_id
                },
                LocationRange(0, 4)
            )),
            Box::new(loc!(
                ExprT::Primary {
                    value: Value::Integer(40),
                    type_: int_id
                },
                LocationRange(0, 6)
            )),
            int_id,
        ));

        let blocks = compiler.get_blocks();
        assert_eq!(
            blocks,
            &vec![
                Block(vec![InstructionKind::Primary(Primary::I32(20))]),
                Block(vec![InstructionKind::Primary(Primary::I32(40))]),
            ]
        );

        let instructions = compiler.get_current_block();
        assert_eq!(
            instructions,
            &vec![
                InstructionKind::Primary(Primary::I32(1)),
                InstructionKind::If(InstrId(0), BlockId(0), BlockId(1))
            ]
        );
    }
}
