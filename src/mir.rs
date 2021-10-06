/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

use crate::ast::{ExprT, Loc, OpT, Value};
use crate::symbol_table::SymbolTable;
use std::mem::replace;

pub type Name = usize;
#[derive(Debug, Clone, PartialEq)]
pub struct InstrId(usize);
#[derive(Debug, Clone, PartialEq)]
pub struct BlockId(usize);

enum Type {
    Int,
    Float,
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
    // Probably shouldn't reuse this from typed_ast pass
    // but I don't see why I should make another op enum
    Binary(OpT, InstrId, InstrId),
    Unary(UnaryOp, InstrId),
    Primary(Primary),
    Return(InstrId),
}

#[derive(Debug, Clone, PartialEq)]
enum Primary {
    Var(usize),
    I32(i32),
    // TODO: Make this into a general purpose pointer type
    //  that indexes into a vec of heap allocated values
    String(usize),
    F32(f32),
}

#[derive(Debug, Clone, PartialEq)]
enum UnaryOp {}

struct MirCompiler {
    string_literals: Vec<String>,
    parameters: Vec<Type>,
    symbol_table: SymbolTable,
    blocks: Vec<Block>,
    current_block: Vec<Instruction>,
}

impl MirCompiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        MirCompiler {
            string_literals: Vec::new(),
            parameters: Vec::new(),
            blocks: Vec::new(),
            current_block: Vec::new(),
            symbol_table,
        }
    }

    pub fn compile_expr(&mut self, expr: Loc<ExprT>) -> InstrId {
        match expr.inner {
            ExprT::BinOp {
                op,
                lhs,
                rhs,
                type_: _,
            } => {
                let lhs_id = self.compile_expr(*lhs);
                let rhs_id = self.compile_expr(*rhs);
                self.current_block
                    .push(Instruction::Binary(op, lhs_id, rhs_id));

                InstrId(self.current_block.len() - 1)
            }
            ExprT::If(cond, then_expr, else_expr, type_) => {
                let mut current_block = replace(&mut self.current_block, Vec::new());

                self.compile_expr(*then_expr);
                let then_block = replace(&mut self.current_block, Vec::new());
                self.blocks.push(Block(then_block));
                let then_block_id = self.blocks.len() - 1;

                self.compile_expr(*else_expr);
                let else_block = replace(&mut self.current_block, current_block);
                self.blocks.push(Block(else_block));
                let else_block_id = self.blocks.len() - 1;

                let cond_id = self.compile_expr(*cond);

                self.current_block.push(Instruction::If(
                    cond_id,
                    BlockId(then_block_id),
                    BlockId(else_block_id),
                ));

                InstrId(self.current_block.len() - 1)
            }
            ExprT::Primary { value, type_: _ } => {
                let instr = Instruction::Primary(self.compile_value(value));
                self.current_block.push(instr);

                InstrId(self.current_block.len() - 1)
            }
            _ => todo!(),
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

    fn compile_var(&self, name: Name) {
        todo!()
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
    use crate::mir::{Block, BlockId, InstrId, Instruction, Value};
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

        compiler.compile_expr(loc!(
            ExprT::BinOp {
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
                type_: int_id
            },
            LocationRange(0, 2)
        ));

        let instructions = compiler.get_current_block();
        assert_eq!(
            instructions,
            &vec![
                Instruction::Primary(Primary::I32(20)),
                Instruction::Primary(Primary::I32(40)),
                Instruction::Binary(OpT::I32Add, InstrId(0), InstrId(1))
            ]
        );
    }

    #[test]
    fn test_compile_if_expr() {
        let mut compiler = MirCompiler::new(SymbolTable::new());
        let mut type_arena: Arena<Type> = Arena::new();
        let int_id = type_arena.alloc(Type::Integer);
        let bool_id = type_arena.alloc(Type::Bool);

        compiler.compile_expr(loc!(
            ExprT::If(
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
                int_id
            ),
            LocationRange(0, 2)
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
