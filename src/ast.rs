#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Primary(Value),
    BinOp(Op, Box<Expr>, Box<Expr>),
    UnaryOp(Unary, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Num(f32),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Div,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Unary {
    Bang,
    Minus,
}
