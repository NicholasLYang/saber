/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

use crate::ast;
use crate::ast::{ExprT, Loc, OpT, ProgramT, Target, Value};
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
    I32Set,
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
    F32Set,
    BoolAnd,
    BoolOr,
    StringEqual,
    StringConcat,
    PointerSet,
    PointerAdd,
    PointerMul,
}

#[derive(Debug, Clone, PartialEq)]
enum UnaryOp {
    PointerLoad,
    I32Load,
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

struct MirCompiler {
    string_literals: Vec<String>,
    parameters: Vec<Type>,
    symbol_table: SymbolTable,
    blocks: Vec<Block>,
    current_block: Vec<Instruction>,
    type_arena: Arena<ast::Type>,
    function_captures: Vec<IndexSet<Name>>,
    function_local_variables: Vec<Vec<Type>>,
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
            function_local_variables: Vec::new(),
        }
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

    fn compile_target(&self, target: Loc<Target>) -> InstrId {
        todo!()
    }

    pub fn compile_expr(&mut self, expr: Loc<ExprT>) -> InstrId {
        match expr.inner {
            ExprT::Asgn { lhs, rhs, type_ } => {
                if !lhs.inner.accessors.is_empty() {
                    todo!();
                }
                // Uhh this probably doesn't work
                // need to either do a store or a set
                // depending on target
                let lhs_id = self.compile_target(lhs);
                let rhs_id = self.compile_expr(*rhs);
                let op = match self
                    .ast_type_to_mir_type(type_)
                    .expect("Should be assignable type here")
                {
                    Type::I32 => BinaryOp::I32Set,
                    Type::F32 => BinaryOp::F32Set,
                    Type::Pointer => BinaryOp::PointerSet,
                };

                self.current_block
                    .push(Instruction::Binary(op, lhs_id, rhs_id));
            }
            ExprT::Var { name, type_ } => {
                let (entry, functions) = self
                    .symbol_table
                    .lookup_name_with_function_hierarchy(name)
                    .unwrap();
                if functions.is_empty() {
                    self.current_block
                        .push(Instruction::Primary(Primary::Var(entry.var_index)))
                } else {
                    self.current_block
                        .push(Instruction::Primary(Primary::Var(0)));

                    let mut env_ptr = InstrId(self.current_block.len() - 1);
                    for (idx, func_index) in functions.into_iter().enumerate() {
                        if idx == 0 {
                            let (captures_index, _) =
                                self.function_captures[func_index].insert_full(name);
                            self.current_block
                                .push(Instruction::Primary(Primary::USize(captures_index * 4)));
                            let offset = InstrId(self.current_block.len() - 1);
                            self.current_block.push(Instruction::Binary(
                                BinaryOp::PointerAdd,
                                env_ptr,
                                offset,
                            ));

                            env_ptr = InstrId(self.current_block.len() - 1);
                        } else {
                            self.current_block
                                .push(Instruction::Unary(UnaryOp::PointerLoad, env_ptr));
                            env_ptr = InstrId(self.current_block.len() - 1)
                        }
                    }
                }
            }
            ExprT::BinOp {
                op,
                lhs,
                rhs,
                type_: _,
            } => {
                let lhs_id = self.compile_expr(*lhs);
                let rhs_id = self.compile_expr(*rhs);
                self.current_block
                    .push(Instruction::Binary(op.into(), lhs_id, rhs_id));
            }
            ExprT::If(cond, then_expr, else_expr, type_) => {
                let current_block = replace(&mut self.current_block, Vec::new());

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
            }
            ExprT::Primary { value, type_: _ } => {
                let instr = Instruction::Primary(self.compile_value(value));
                self.current_block.push(instr);
            }
            _ => todo!(),
        }

        InstrId(self.current_block.len() - 1)
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
    use crate::ast::{ExprT, Loc, Type};
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

        compiler.compile_expr(loc!(
            ExprT::BinOp {
                op: BinaryOp::I32Add,
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
                Instruction::Binary(BinaryOp::I32Add, InstrId(0), InstrId(1))
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
