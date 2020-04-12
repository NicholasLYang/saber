use lexer::LocationRange;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::sync::Arc;

pub type Name = usize;

// Wrapper to provide location to AST nodes
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Loc<T> {
    pub location: LocationRange,
    pub inner: T,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Asgn(Pat, Loc<Expr>),
    Expr(Loc<Expr>),
    Return(Loc<Expr>),
    Block(Vec<Loc<Stmt>>),
    If(Loc<Expr>, Box<Loc<Stmt>>, Option<Box<Loc<Stmt>>>),
    Function(Name, Pat, Option<TypeSig>, Box<Loc<Stmt>>),
    Export(Name),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtT {
    Asgn(Name, Loc<ExprT>),
    Expr(Loc<ExprT>),
    Return(Loc<ExprT>),
    Block(Vec<Loc<StmtT>>),
    If(Loc<ExprT>, Box<Loc<StmtT>>, Option<Box<Loc<StmtT>>>),
    Function {
        name: Name,
        params: Vec<(Name, Arc<Type>)>,
        params_type: Arc<Type>,
        return_type: Arc<Type>,
        body: Box<Loc<StmtT>>,
        scope: usize,
    },
    Export(Name),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Expr {
    Primary {
        value: Value,
    },
    Var {
        name: Name,
    },
    BinOp {
        op: Op,
        lhs: Box<Loc<Expr>>,
        rhs: Box<Loc<Expr>>,
    },
    UnaryOp {
        op: Op,
        rhs: Box<Loc<Expr>>,
    },
    Function {
        params: Pat,
        return_type: Option<TypeSig>,
        body: Box<Loc<Stmt>>,
    },
    Call {
        callee: Box<Loc<Expr>>,
        args: Box<Loc<Expr>>,
    },
    Field(Box<Loc<Expr>>, Name),
    Record {
        entries: Vec<(Name, Loc<Expr>)>,
    },
    Tuple(Vec<Loc<Expr>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprT {
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
        lhs: Box<Loc<ExprT>>,
        rhs: Box<Loc<ExprT>>,
        type_: Arc<Type>,
    },
    UnaryOp {
        op: Op,
        rhs: Box<Loc<ExprT>>,
        type_: Arc<Type>,
    },
    // Note: only used for anonymous functions. Functions that are
    // bound with let are TypedStmt::Function
    Function {
        params: Vec<(Name, Arc<Type>)>,
        params_type: Arc<Type>,
        return_type: Arc<Type>,
        body: Box<Loc<StmtT>>,
        scope_index: usize,
    },
    Field(Box<ExprT>, Name, Arc<Type>),
    Call {
        callee: Box<Loc<ExprT>>,
        args: Box<Loc<ExprT>>,
        type_: Arc<Type>,
    },
    Tuple(Vec<Loc<ExprT>>, Arc<Type>),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TypeSig {
    Array(Box<TypeSig>),
    Name(Name),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Pat {
    Id(Name, Option<TypeSig>, LocationRange),
    Record(Vec<Name>, Option<TypeSig>, LocationRange),
    Tuple(Vec<Pat>, LocationRange),
    Empty(LocationRange),
}

// Oy vey, cause Rust doesn't allow enum field access
impl ExprT {
    pub fn get_type(&self) -> Arc<Type> {
        match &self {
            ExprT::Primary { value: _, type_ } => type_.clone(),
            ExprT::Var { name: _, type_ } => type_.clone(),
            ExprT::Tuple(_elems, type_) => type_.clone(),
            ExprT::BinOp {
                op: _,
                lhs: _,
                rhs: _,
                type_,
            } => type_.clone(),
            ExprT::UnaryOp {
                op: _,
                rhs: _,
                type_,
            } => type_.clone(),
            ExprT::Function {
                params: _,
                body: _,
                scope_index: _,
                params_type,
                return_type,
            } => Arc::new(Type::Arrow(params_type.clone(), return_type.clone())),
            ExprT::Field(_, _, type_) => type_.clone(),
            ExprT::Call {
                callee: _,
                args: _,
                type_,
            } => type_.clone(),
        }
    }
}
