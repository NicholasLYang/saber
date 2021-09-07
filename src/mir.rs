pub type Name = usize;

enum Type {
    Int,
    Float,
    String,
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
    Float(f32),
}
