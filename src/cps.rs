use crate::ast::Name;

enum Value {
    Var(Name),
    Int(i32),
    Float(f32),
    String(String),
}

enum Op {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
}

enum CExpr {
    PrimitiveOp {
        op: Op,
        args: Vec<usize>,
        next: Box<CExpr>,
    },
    DirectCall {
        func_id: usize,
        args: Vec<usize>,
        next: Box<CExpr>,
    },
    IndirectCall {
        caller_id: usize,
    },
}
