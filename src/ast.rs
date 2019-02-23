pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Asgn(Pat, Option<Expr>),
    Expr(Expr),
    Return(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    Asgn(Pat, Option<TypedExpr>),
    Expr(TypedExpr),
    Return(TypedExpr),
    Block(Vec<TypedStmt>),
    If(TypedExpr, Box<TypedStmt>, Option<Box<TypedStmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Primary {
        value: Value,
    },
    Var {
        name: Name,
    },
    BinOp {
        op: Op,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnaryOp {
        unary: Unary,
        rhs: Box<Expr>,
    },
    Function {
        params: Pat,
        body: Box<Stmt>,
    },
    Call {
        callee: Box<Expr>,
        arg: Box<Expr>,
    },
    Tuple(Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpr {
    Primary {
        value: Value,
        type_: Type,
    },
    Var {
        name: Name,
        type_: Type,
    },
    BinOp {
        op: Op,
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
        type_: Type,
    },
    UnaryOp {
        unary: Unary,
        rhs: Box<TypedExpr>,
        type_: Type,
    },
    Function {
        params: Pat,
        body: Box<TypedStmt>,
        type_: Type,
    },
    Call {
        callee: Box<TypedExpr>,
        arg: Box<TypedExpr>,
        type_: Type,
    },
    Tuple(Vec<TypedExpr>, Type),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Float(f32),
    Integer(i32),
    Bool(bool),
    String(String),
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
    Float,
    Int,
    Bool,
    Char,
    String,
    Var(Name),
    Array(Box<Type>),
    // Pair. Not sure how to do larger than two arguments. Nesting?
    // Idk
    Tuple(Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeSig {
    Array(Box<TypeSig>),
    Name(Name),
}
#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Id(Name, Option<TypeSig>),
    Record(Vec<(Name, Option<TypeSig>)>),
    Tuple(Vec<Pat>),
}

// Oy vey, cause Rust doesn't allow enum field access
impl TypedExpr {
    pub fn get_type(&self) -> &Type {
        match &self {
            TypedExpr::Primary { value: _, type_ } => type_,
            TypedExpr::Var { name: _, type_ } => type_,
            TypedExpr::Tuple(_elems, type_) => type_,
            TypedExpr::BinOp {
                op: _,
                lhs: _,
                rhs: _,
                type_,
            } => type_,
            TypedExpr::UnaryOp {
                unary: _,
                rhs: _,
                type_,
            } => type_,
            TypedExpr::Function {
                params: _,
                body: _,
                type_,
            } => type_,
            TypedExpr::Call {
                callee: _,
                arg: _,
                type_,
            } => type_,
        }
    }
}
