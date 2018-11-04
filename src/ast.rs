pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Var(Asgn, Expr),
    Expr(Expr),
    Return(Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Asgn {
    // Single assignment, versus multi assignment (TODO)
    Single(Name),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Primary(Value),
    Var(Name),
    BinOp(Op, Box<Expr>, Box<Expr>),
    UnaryOp(Unary, Box<Expr>),
    // TODO: Allow different argument styles like ({ a, b}) or
    // (:named_arg, :other_named_arg)
    Function(Vec<Name>, Vec<Stmt>),
    Call(Name, Vec<Expr>),
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

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr {
    pub expr: Expr,
    pub _type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedStmt {
    pub expr: Expr,
    pub _type: Type,
}

// Yeah this is hilariously basic rn.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Literal(Literal),
    // Pair. Not sure how to do larger than two arguments. Nesting?
    // Idk
    Tuple(Vec<Type>),
    // Function from n types to one type. Might extend to n types
    // sometime
    Arrow(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float,
    Char,
}
