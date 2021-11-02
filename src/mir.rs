/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

use crate::ast;
use crate::ast::{ExprT, Loc, OpT, ProgramT, StmtT, Target, Value};
use crate::symbol_table::SymbolTable;
use id_arena::Arena;
use indexmap::set::IndexSet;
use std::mem::replace;

pub type Name = usize;
#[derive(Debug, Clone, PartialEq)]
pub struct InstrId(usize);
#[derive(Debug, Clone, PartialEq)]
pub struct BlockId(usize);

enum Type {
    I32,
    F32,
    Pointer,
}

struct Function {
    name: Name,
    params: Vec<Type>,
    body: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(Vec<Instruction>);

#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    // This could be too specific. I might want
    // blocks to have outgoing edges and have a
    // break with index as the instruction
    If(InstrId, BlockId, BlockId),
    Binary(BinaryOp, InstrId, InstrId),
    Unary(UnaryOp, InstrId),
    Primary(Primary),
    VarSet(usize),
    VarGet(usize),
    Return(InstrId),
}

#[derive(Debug, Clone, PartialEq)]
enum Primary {
    I32(i32),
    // TODO: Make this into a general purpose pointer type
    //  that indexes into a vec of heap allocated values
    String(usize),
    F32(f32),
    USize(usize),
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
    Drop,
}

impl From<crate::ast::OpT> for BinaryOp {
    fn from(op: OpT) -> Self {
        match op {
            OpT::I32Add => BinaryOp::I32Add,
            OpT::I32Sub => BinaryOp::I32Sub,
            OpT::I32Mul => BinaryOp::I32Mul,
            OpT::I32Div => BinaryOp::I32Div,
            OpT::I32NotEqual => BinaryOp::I32NotEqual,
            OpT::I32Equal => BinaryOp::I32Equal,
            OpT::I32Greater => BinaryOp::I32Greater,
            OpT::I32GreaterEqual => BinaryOp::I32GreaterEqual,
            OpT::I32Less => BinaryOp::I32Less,
            OpT::I32LessEqual => BinaryOp::I32LessEqual,
            OpT::I32And => BinaryOp::I32And,
            OpT::I32Or => BinaryOp::I32Or,
            OpT::F32Add => BinaryOp::F32Add,
            OpT::F32Sub => BinaryOp::F32Sub,
            OpT::F32Mul => BinaryOp::F32Mul,
            OpT::F32Div => BinaryOp::F32Div,
            OpT::F32NotEqual => BinaryOp::F32NotEqual,
            OpT::F32Equal => BinaryOp::F32Equal,
            OpT::F32Greater => BinaryOp::F32Greater,
            OpT::F32GreaterEqual => BinaryOp::F32GreaterEqual,
            OpT::F32Less => BinaryOp::F32Less,
            OpT::F32LessEqual => BinaryOp::F32LessEqual,
            OpT::BoolAnd => BinaryOp::BoolAnd,
            OpT::BoolOr => BinaryOp::BoolOr,
            OpT::StringEqual => BinaryOp::StringEqual,
            OpT::StringConcat => BinaryOp::StringConcat,
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
            function_captures: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn print_instructions(&self) {
        for block in &self.blocks {
            println!("-----------------");
            for instr in &block.0 {
                println!("{:?}", instr);
            }
            println!("-----------------");
        }
        println!("-----------------");
        for instr in &self.current_block {
            println!("{:?}", instr);
        }
        println!("-----------------");
    }

    pub fn compile_program(&mut self, program: ProgramT) {
        for function in program.functions {
            self.compile_function(function.inner);
        }
    }

    fn add_instruction(&mut self, instruction: Instruction) -> InstrId {
        self.current_block.push(instruction);
        InstrId(self.current_block.len() - 1)
    }

    fn compile_function(&mut self, function: ast::Function) {
        self.compile_expr(function.body.inner);
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
    fn compile_enclosed_var(&mut self, var_name: Name, function_scopes: Vec<usize>) -> InstrId {
        let mut env_ptr = self.add_instruction(Instruction::VarGet(0));

        for (idx, func_index) in function_scopes.into_iter().enumerate() {
            // If we're at the last function scope, we need to get the specific field
            if idx == 0 {
                let (captures_index, _) = self.function_captures[func_index].insert_full(var_name);

                let offset =
                    self.add_instruction(Instruction::Primary(Primary::USize(captures_index * 4)));

                env_ptr = self.add_instruction(Instruction::Binary(
                    BinaryOp::PointerAdd,
                    env_ptr,
                    offset,
                ));
            } else {
                env_ptr = self.add_instruction(Instruction::Unary(UnaryOp::PointerLoad, env_ptr));
            }
        }

        env_ptr
    }

    pub fn compile_stmt(&mut self, stmt: StmtT) -> InstrId {
        match stmt {
            StmtT::Expr(expr) => {
                let id = self.compile_expr(expr.inner);
                self.add_instruction(Instruction::Unary(UnaryOp::Drop, id))
            }
            s => todo!("{:?}", s),
        }
    }

    pub fn compile_expr(&mut self, expr: ExprT) -> InstrId {
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
                    self.add_instruction(Instruction::VarSet(var_index))
                } else {
                    let var_ptr = self.compile_enclosed_var(lhs.inner.ident, function_scopes);
                    let op = match self
                        .ast_type_to_mir_type(type_)
                        .expect("Should be assignable type here")
                    {
                        Type::I32 => BinaryOp::I32Store,
                        Type::F32 => BinaryOp::F32Store,
                        Type::Pointer => BinaryOp::PointerStore,
                    };
                    self.add_instruction(Instruction::Binary(op, var_ptr, rhs_id))
                }
            }
            ExprT::Var { name, type_ } => {
                let (entry, function_scopes) = self
                    .symbol_table
                    .lookup_name_with_function_hierarchy(name)
                    .unwrap();

                if function_scopes.is_empty() {
                    let var_index = entry.var_index;
                    self.add_instruction(Instruction::VarGet(var_index))
                } else {
                    let var_ptr = self.compile_enclosed_var(name, function_scopes);
                    let op = match self
                        .ast_type_to_mir_type(type_)
                        .expect("Should be assignable type here")
                    {
                        Type::I32 => UnaryOp::I32Load,
                        Type::F32 => UnaryOp::F32Load,
                        Type::Pointer => UnaryOp::PointerLoad,
                    };
                    self.add_instruction(Instruction::Unary(op, var_ptr))
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
                self.add_instruction(Instruction::Binary(op.into(), lhs_id, rhs_id))
            }
            ExprT::If(cond, then_expr, else_expr, type_) => {
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

                self.add_instruction(Instruction::If(
                    cond_id,
                    BlockId(then_block_id),
                    BlockId(else_block_id),
                ))
            }
            ExprT::Primary { value, type_: _ } => {
                let instruction = Instruction::Primary(self.compile_value(value));
                self.add_instruction(instruction)
            }
            ExprT::Block {
                mut stmts,
                end_expr,
                scope_index,
                type_,
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

                instr_id.unwrap()
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
    use crate::ast::{ExprT, Loc, OpT, Type};
    use crate::lexer::LocationRange;
    use crate::loc;
    use crate::mir::{BinaryOp, Block, BlockId, InstrId, Instruction, Value};
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
            op: OpT::I32Add,
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
                Instruction::Primary(Primary::I32(20)),
                Instruction::Primary(Primary::I32(40)),
                Instruction::Binary(BinaryOp::I32Add, InstrId(0), InstrId(1))
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
                Block(vec![Instruction::Primary(Primary::I32(20))]),
                Block(vec![Instruction::Primary(Primary::I32(40))]),
            ]
        );

        let instructions = compiler.get_current_block();
        assert_eq!(
            instructions,
            &vec![
                Instruction::Primary(Primary::I32(1)),
                Instruction::If(InstrId(0), BlockId(0), BlockId(1))
            ]
        );
    }
}
