#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Value(f32),
    BinOp(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Div,
}
