/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

use crate::ast::{ExprT, Loc, OpT, Value};
use crate::symbol_table::SymbolTable;

pub type Name = usize;
#[derive(Debug, Clone, PartialEq)]
pub struct InstrId(usize);

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
enum Instruction {
    // Probably shouldn't reuse this from typed_ast pass
    // but I don't see why I should make another op enum
    Binary(OpT, InstrId, InstrId),
    Unary(UnaryOp, InstrId),
    Primary(Primary),
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
    instructions: Vec<Instruction>,
}

impl MirCompiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        MirCompiler {
            string_literals: Vec::new(),
            parameters: Vec::new(),
            instructions: Vec::new(),
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
                self.instructions
                    .push(Instruction::Binary(op, lhs_id, rhs_id));

                InstrId(self.instructions.len() - 1)
            }
            ExprT::Primary { value, type_: _ } => {
                let instr = Instruction::Primary(self.compile_value(value));
                self.instructions.push(instr);

                InstrId(self.instructions.len() - 1)
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
}

#[cfg(test)]
mod test {
    use crate::mir::Value;
    use crate::mir::{MirCompiler, Primary};
    use crate::symbol_table::SymbolTable;

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
}
