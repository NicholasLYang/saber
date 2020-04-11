use lexer::LocationRange;
use std::fmt;
use std::sync::Arc;

pub type Name = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Stmt {
    pub location: LocationRange,
    pub kind: StmtKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtKind {
    Asgn(Pat, Expr),
    Expr(Expr),
    Return(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Function(Name, Pat, Option<TypeSig>, Box<Stmt>),
    Export(Name),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    Asgn(Name, TypedExpr),
    Expr(TypedExpr),
    Return(TypedExpr),
    Block(Vec<TypedStmt>),
    If(TypedExpr, Box<TypedStmt>, Option<Box<TypedStmt>>),
    Function {
        name: Name,
        params: Vec<(Name, Arc<Type>)>,
        params_type: Arc<Type>,
        return_type: Arc<Type>,
        body: Box<TypedStmt>,
        scope: usize,
    },
    Export(Name),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub location: LocationRange,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
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
        args: Box<Expr>,
    },
    Field(Box<Expr>, Name),
    Record {
        entries: Vec<(Name, Expr)>,
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
    // Note: only used for anonymous functions. Functions that are
    // bound with let are TypedStmt::Function
    Function {
        params: Vec<(Name, Arc<Type>)>,
        params_type: Arc<Type>,
        return_type: Arc<Type>,
        body: Box<TypedStmt>,
        scope_index: usize,
    },
    Field(Box<TypedExpr>, Name, Arc<Type>),
    Call {
        callee: Box<TypedExpr>,
        args: Box<TypedExpr>,
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Float(f) => format!("float: {}", f),
                Value::Integer(i) => format!("int: {}", i),
                Value::Bool(b) => format!("bool: {}", b),
                // TODO: Have this truncate the string
                Value::String(s) => format!("string: {}", s),
            }
        )
    }
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

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Plus => "+",
                Op::Minus => "-",
                Op::Times => "*",
                Op::Div => "/",
                Op::BangEqual => "!=",
                Op::EqualEqual => "==",
                Op::Greater => ">",
                Op::GreaterEqual => ">=",
                Op::Less => "<",
                Op::LessEqual => "<=",
            }
        )
    }
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Unit => "()".into(),
                Type::Float => "float".into(),
                Type::Int => "int".into(),
                Type::Bool => "bool".into(),
                Type::Char => "char".into(),
                Type::String => "string".into(),
                Type::Var(name) => format!("var({})", name),
                Type::Array(t) => format!("[{}]", t),
                Type::Record(_) => "{ Record }".into(),
                Type::Tuple(_) => "(Tuple)".into(),
                Type::Arrow(t1, t2) => format!("{} => {}", t1, t2),
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeSig {
    Array(Box<TypeSig>),
    Name(Name),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Id(Name, Option<TypeSig>, LocationRange),
    Record(Vec<Name>, Option<TypeSig>, LocationRange),
    Tuple(Vec<Pat>, LocationRange),
    Empty(LocationRange),
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
                scope_index: _,
                params_type,
                return_type,
            } => Arc::new(Type::Arrow(params_type.clone(), return_type.clone())),
            TypedExpr::Field(_, _, type_) => type_.clone(),
            TypedExpr::Call {
                callee: _,
                args: _,
                type_,
            } => type_.clone(),
        }
    }
}
