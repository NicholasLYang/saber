/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

use crate::ast;
use crate::ast::{BinaryOpT, ExprT, Loc, ProgramT, StmtT, Target, Value};
use crate::symbol_table::SymbolTable;
use crate::utils::get_final_type;
use id_arena::Arena;
use indexmap::set::IndexSet;
use std::mem::replace;

pub type Name = usize;
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct InstrId(usize);
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BlockId(usize);
#[derive(Debug, Copy, Clone, PartialEq)]
struct FunctionId(usize);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    I32,
    F32,
    Pointer,
}

pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub params: Vec<Type>,
    pub returns: Vec<Type>,
    pub body: Vec<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Instruction>);

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    // This could be too specific. I might want
    // blocks to have outgoing edges and have a
    // break with index as the instruction
    If(InstrId, (BlockId, InstrId), (BlockId, InstrId)),
    Binary(BinaryOp, InstrId, InstrId),
    Unary(UnaryOp, InstrId),
    Primary(Primary),
    VarSet(usize),
    VarGet(usize),
    Return(InstrId),
    Call(FunctionId, Vec<InstrId>),
    Noop,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    I32(i32),
    // TODO: Make this into a general purpose pointer type
    //  that indexes into a vec of heap allocated values
    String(usize),
    F32(f32),
}

#[derive(Debug, Clone, PartialEq)]
enum BinaryOp {
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
    PointerMul,
}

#[derive(Debug, Clone, PartialEq)]
enum UnaryOp {
    PointerLoad,
    F32Load,
    I32Load,
    F32Negate,
    I32Negate,
    BoolNegate,
    Drop,
    // Eventually would like to have print be just a function but that requires generics
    Print,
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
    string_literals: Vec<String>,
    parameters: Vec<Type>,
    symbol_table: SymbolTable,
    blocks: Vec<Block>,
    current_block: Vec<Instruction>,
    type_arena: Arena<ast::Type>,
    functions: Vec<Function>,
    function_captures: Vec<IndexSet<Name>>,
}

impl MirCompiler {
    pub fn new(symbol_table: SymbolTable, type_arena: Arena<ast::Type>) -> Self {
        MirCompiler {
            string_literals: Vec::new(),
            parameters: Vec::new(),
            blocks: Vec::new(),
            current_block: Vec::new(),
            symbol_table,
            type_arena,
            functions: Vec::new(),
            function_captures: Vec::new(),
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

        Program {
            functions: replace(&mut self.functions, Vec::new()),
        }
    }

    fn add_instruction(&mut self, kind: InstructionKind, ty: Option<Type>) -> InstrId {
        self.current_block.push(Instruction { kind, ty });
        InstrId(self.current_block.len() - 1)
    }

    fn compile_function(&mut self, function: ast::Function) {
        self.compile_expr(function.body.inner);
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
            _ => None,
        }
    }

    // Compiles enclosed variable by going up scope chain
    fn compile_enclosed_var(
        &mut self,
        var_name: Name,
        var_type: Type,
        function_scopes: Vec<usize>,
    ) -> InstrId {
        let mut env_ptr = self.add_instruction(InstructionKind::VarGet(0), Some(Type::Pointer));
        let len = function_scopes.len();
        for (idx, func_index) in function_scopes.into_iter().enumerate() {
            // If we're at the last function scope, we need to get the specific field
            if idx == len - 1 {
                let (captures_index, _) = self.function_captures[func_index].insert_full(var_name);

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
                let id = self.compile_expr(expr.inner);
                self.add_instruction(InstructionKind::Unary(UnaryOp::Drop, id), None)
            }
            StmtT::Return(expr) => {
                let id = self.compile_expr(expr.inner);
                self.add_instruction(InstructionKind::Return(id), None)
            }
            s => todo!("{:?}", s),
        }
    }

    pub fn compile_expr(&mut self, expr: ExprT) -> InstrId {
        let ty = self.ast_type_to_mir_type(expr.get_type());

        match expr {
            ExprT::Asgn { lhs, rhs, type_ } => {
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
                    self.add_instruction(InstructionKind::VarSet(var_index), None)
                } else {
                    let ty = ty.expect("Must be assignable");
                    let var_ptr = self.compile_enclosed_var(lhs.inner.ident, ty, function_scopes);
                    let op = match ty {
                        Type::I32 => BinaryOp::I32Store,
                        Type::F32 => BinaryOp::F32Store,
                        Type::Pointer => BinaryOp::PointerStore,
                    };
                    self.add_instruction(InstructionKind::Binary(op, var_ptr, rhs_id), None)
                }
            }
            ExprT::If(cond, then_expr, else_expr, type_) => {
                let current_block = replace(&mut self.current_block, Vec::new());

                let then_instr_id = self.compile_expr(then_expr.inner);
                let then_block = replace(&mut self.current_block, Vec::new());
                self.blocks.push(Block(then_block));
                let then_block_id = self.blocks.len() - 1;

                let else_instr_id = self.compile_expr(else_expr.inner);
                let else_block = replace(&mut self.current_block, current_block);
                self.blocks.push(Block(else_block));
                let else_block_id = self.blocks.len() - 1;

                let cond_id = self.compile_expr(cond.inner);

                self.add_instruction(
                    InstructionKind::If(
                        cond_id,
                        (BlockId(then_block_id), then_instr_id),
                        (BlockId(else_block_id), else_instr_id),
                    ),
                    ty,
                )
            }
            ExprT::Var { name, type_ } => {
                let (entry, function_scopes) = self
                    .symbol_table
                    .lookup_name_with_function_hierarchy(name)
                    .unwrap();

                if function_scopes.is_empty() {
                    let var_index = entry.var_index;
                    self.add_instruction(InstructionKind::VarGet(var_index), ty)
                } else {
                    if let Some(ty) = ty {
                        let var_ptr = self.compile_enclosed_var(name, ty, function_scopes);
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
                type_,
            } => {
                let lhs_id = self.compile_expr(lhs.inner);
                let rhs_id = self.compile_expr(rhs.inner);

                self.add_instruction(InstructionKind::Binary(op.into(), lhs_id, rhs_id), ty)
            }
            ExprT::UnaryOp { op, rhs, type_: _ } => {
                let rhs_id = self.compile_expr(rhs.inner);

                self.add_instruction(InstructionKind::Unary(op.into(), rhs_id), ty)
            }
            ExprT::Primary { value, type_: _ } => {
                let instruction = InstructionKind::Primary(self.compile_value(value));
                self.add_instruction(instruction, ty)
            }
            ExprT::Print { args, type_: _ } => {
                let args_id = self.compile_expr(args.inner);
                self.add_instruction(InstructionKind::Unary(UnaryOp::Print, args_id), None)
            }
            ExprT::DirectCall {
                callee,
                args,
                type_: _,
            } => {
                let args = match args.inner {
                    ExprT::Tuple(fields, ty) => fields
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

    fn get_current_block(&self) -> &Vec<Instruction> {
        &self.current_block
    }

    fn get_blocks(&self) -> &Vec<Block> {
        &self.blocks
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
