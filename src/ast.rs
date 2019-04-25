use std::sync::Arc;
pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Asgn(Pat, Expr),
    Expr(Expr),
    Return(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    Asgn(Pat, TypedExpr),
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
        op: Op,
        rhs: Box<Expr>,
    },
    Function {
        params: Pat,
        return_type: Option<TypeSig>,
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
        type_: Arc<Type>,
    },
    Var {
        name: Name,
        type_: Arc<Type>,
    },
    BinOp {
        op: Op,
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
        type_: Arc<Type>,
    },
    UnaryOp {
        op: Op,
        rhs: Box<TypedExpr>,
        type_: Arc<Type>,
    },
    Function {
        params: Pat,
        body: Box<TypedStmt>,
        type_: Arc<Type>,
    },
    Call {
        callee: Box<TypedExpr>,
        arg: Box<TypedExpr>,
        type_: Arc<Type>,
    },
    Tuple(Vec<TypedExpr>, Arc<Type>),
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

// Yeah this is hilariously basic rn.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Unit,
    Float,
    Int,
    Bool,
    Char,
    String,
    Var(Name),
    Array(Arc<Type>),
    Record(Vec<(Name, Arc<Type>)>),
    Tuple(Vec<Arc<Type>>),
    Arrow(Arc<Type>, Arc<Type>),
}

impl Type {
    pub fn get_total_count(&self) -> u32 {
        let mut count = 0;
        match self {
            Type::Tuple(elems) => {
                for elem in elems {
                    count += elem.get_total_count();
                }
            }
            Type::Record(elems) => {
                for (name, type_) in elems {
                    count += type_.get_total_count();
                }
            }
            _ => count += 1,
        }
        count
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeSig {
    Array(Box<TypeSig>),
    Name(Name),
}
#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Id(Name, Option<TypeSig>),
    Record(Vec<Pat>),
    Tuple(Vec<Pat>),
    Empty,
}

// Oy vey, cause Rust doesn't allow enum field access
impl TypedExpr {
    pub fn get_type(&self) -> Arc<Type> {
        match &self {
            TypedExpr::Primary { value: _, type_ } => type_.clone(),
            TypedExpr::Var { name: _, type_ } => type_.clone(),
            TypedExpr::Tuple(_elems, type_) => type_.clone(),
            TypedExpr::BinOp {
                op: _,
                lhs: _,
                rhs: _,
                type_,
            } => type_.clone(),
            TypedExpr::UnaryOp {
                op: _,
                rhs: _,
                type_,
            } => type_.clone(),
            TypedExpr::Function {
                params: _,
                body: _,
                type_,
            } => type_.clone(),
            TypedExpr::Call {
                callee: _,
                arg: _,
                type_,
            } => type_.clone(),
        }
    }
}
