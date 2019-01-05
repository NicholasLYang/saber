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
        body: Vec<Stmt>,
    },
    Call {
        callee: Box<Expr>,
        arg: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpr {
    Primary {
        value: Value,
        _type: Type,
    },
    Var {
        name: Name,
        _type: Type,
    },
    BinOp {
        op: Op,
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
        _type: Type,
    },
    UnaryOp {
        unary: Unary,
        rhs: Box<TypedExpr>,
        _type: Type,
    },
    Function {
        params: Pat,
        body: Vec<TypedStmt>,
        _type: Type,
    },
    Call {
        callee: Box<TypedExpr>,
        arg: Box<TypedExpr>,
        _type: Type,
    },
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
    Float,
    Bool,
    Char,
    // Pair. Not sure how to do larger than two arguments. Nesting?
    // Idk
    Tuple(Vec<Type>),
    // Function from n types to one type. Might extend to n types
    // sometime
    Arrow(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Id(Name),
    Record(Vec<Name>),
    Tuple(Vec<Pat>),
}

// Oy vey, cause Rust doesn't allow enum field access
impl TypedExpr {
    pub fn get_type(&self) -> &Type {
        match &self {
            TypedExpr::Primary { value, _type } => _type,
            TypedExpr::Var { name, _type } => _type,
            TypedExpr::BinOp {
                op,
                lhs,
                rhs,
                _type,
            } => _type,
            TypedExpr::UnaryOp { unary, rhs, _type } => _type,
            TypedExpr::Function {
                params,
                body,
                _type,
            } => _type,
            TypedExpr::Call { callee, arg, _type } => _type,
        }
    }
}
