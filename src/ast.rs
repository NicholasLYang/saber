pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Var(Pat, Expr),
    Expr(Expr),
    Return(Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    Var(Pat, TypedExpr),
    Expr(TypedExpr),
    Return(TypedExpr),
    If(Expr, Vec<TypedStmt>, Option<Vec<TypedStmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Primary(Value),
    Var(Name),
    BinOp(Op, Box<Expr>, Box<Expr>),
    UnaryOp(Unary, Box<Expr>),
    Function(Pat, Vec<Stmt>),
    Call(Name, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpr {
    Primary(Value, Type),
    Var(Name, Type),
    BinOp(Op, Box<Expr>, Box<Expr>, Type),
    UnaryOp(Unary, Box<Expr>, Type),
    Function(Pat, Vec<Stmt>, Type),
    Call(Name, Box<Expr>, Type),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Num(f32),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Comma,
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

// Yeah this is hilariously basic rn.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Primitive(Primitive),
    // Pair. Not sure how to do larger than two arguments. Nesting?
    // Idk
    Tuple(Vec<Type>),
    // Function from n types to one type. Might extend to n types
    // sometime
    Arrow(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Primitive {
    Float,
    Bool,
    Char,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Id(Name),
    Record(Vec<Name>),
    Tuple(Vec<Pat>),
}
