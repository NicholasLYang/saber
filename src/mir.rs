/*
Mid-Level Intermediate Representation is a flat representation with no nested control flow,
no nested scope.
*/

pub type Name = usize;

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

enum Instruction {
    Binary(BinaryOp, usize, usize),
    Unary(UnaryOp, usize),
    Primary(Primary),
}

enum Primary {
    Var(usize),
    Int(i32),
    String(usize),
    Float(f32),
}

enum BinaryOp {}

enum UnaryOp {}

struct MirCompiler {
    string_literals: Vec<String>,
}

impl MirCompiler {
    fn compile_value(&self, value: Value) {
        match value {
            Value::Float(f) => Primary::Float(f),
            Value::Int(i) => Primary::Int(i),
            Value::String(s) => {
                self.string_literals.push(s);
                Primary::String(self.string_literals.len() - 1)
            }
        }
    }
}
