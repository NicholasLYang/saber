pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Asgn(Pat, Option<TypeAnnotation>, Expr),
    Expr(Expr),
    Return(Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    Asgn(Pat, TypedExpr),
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
    Tuple {
        fst: Box<Expr>,
        snd: Box<Expr>,
    },
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
        body: Vec<TypedStmt>,
        type_: Type,
    },
    Call {
        callee: Box<TypedExpr>,
        arg: Box<TypedExpr>,
        type_: Type,
    },
    Tuple {
        fst: Box<TypedExpr>,
        snd: Box<TypedExpr>,
        type_: Type,
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
    Int,
    Bool,
    Char,
    Array(Box<Type>),
    // Pair. Not sure how to do larger than two arguments. Nesting?
    // Idk
    Tuple(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation {
    Array(Box<TypeAnnotation>),
    Name(Name),
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
            TypedExpr::Primary { value: _, type_ } => type_,
            TypedExpr::Var { name: _, type_ } => type_,
            TypedExpr::Tuple {
                fst: _,
                snd: _,
                type_,
            } => type_,
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
