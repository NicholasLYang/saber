/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

pub mod analyzer;
pub mod compiler;

use crate::ast;
use crate::ast::BinaryOpT;
use std::fmt;
use std::fmt::Formatter;

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

#[cfg(test)]
mod test {
    use crate::ast::{BinaryOpT, ExprT, Loc, Type};
    use crate::lexer::LocationRange;
    use crate::loc;
    use crate::mir::compiler::MirCompiler;
    use crate::mir::Primary;
    use crate::mir::{BinaryOp, Block, BlockId, InstrId, InstructionKind, Value};
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
