use lexer::LocationRange;
use parser::ParseError;
use serde::{Deserialize, Serialize};
use std::fmt;
use typechecker::TypeError;

pub type Name = usize;
pub type TypeId = usize;

// Wrapper to provide location to AST nodes
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Loc<T> {
    pub location: LocationRange,
    pub inner: T,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Program {
    pub stmts: Vec<Loc<Stmt>>,
    pub type_defs: Vec<Loc<TypeDef>>,
    pub errors: Vec<ParseError>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ProgramT {
    pub stmts: Vec<Loc<StmtT>>,
    pub named_types: Vec<(Name, TypeId)>,
    pub errors: Vec<TypeError>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Asgn(Pat, Loc<Expr>),
    Expr(Loc<Expr>),
    Return(Loc<Expr>),
    Block(Vec<Loc<Stmt>>),
    Function(Name, Pat, Option<Loc<TypeSig>>, Box<Loc<Expr>>),
    Export(Name),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum StmtT {
    Asgn(Name, Loc<ExprT>),
    Expr(Loc<ExprT>),
    Return(Loc<ExprT>),
    Block(Vec<Loc<StmtT>>),
    Function {
        name: Name,
        params_type: TypeId,
        return_type: TypeId,
        function: Function,
    },
    Export(Name),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Expr {
    Block(Vec<Loc<Stmt>>, Option<Box<Loc<Expr>>>),
    If(Box<Loc<Expr>>, Box<Loc<Expr>>, Option<Box<Loc<Expr>>>),
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
        op: UnaryOp,
        rhs: Box<Loc<Expr>>,
    },
    Function {
        params: Pat,
        return_type: Option<Loc<TypeSig>>,
        body: Box<Loc<Expr>>,
    },
    Call {
        callee: Box<Loc<Expr>>,
        args: Box<Loc<Expr>>,
    },
    Field(Box<Loc<Expr>>, Name),
    Record {
        name: Name,
        fields: Vec<(Name, Loc<Expr>)>,
    },
    Tuple(Vec<Loc<Expr>>),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum ExprT {
    Block {
        stmts: Vec<Loc<StmtT>>,
        end_expr: Option<Box<Loc<ExprT>>>,
        scope_index: usize,
        type_: TypeId,
    },
    If(
        Box<Loc<ExprT>>,
        Box<Loc<ExprT>>,
        Option<Box<Loc<ExprT>>>,
        TypeId,
    ),
    Primary {
        value: Value,
        type_: TypeId,
    },
    Var {
        name: Name,
        type_: TypeId,
    },
    BinOp {
        op: Op,
        lhs: Box<Loc<ExprT>>,
        rhs: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    UnaryOp {
        op: UnaryOp,
        rhs: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    // Note: only used for anonymous functions. Functions that are
    // bound with let are StmtT::Function
    Function {
        function: Function,
        type_: TypeId,
        name: Name,
    },
    Record {
        name: Name,
        fields: Vec<(Name, Loc<ExprT>)>,
        type_: TypeId,
    },
    Field(Box<Loc<ExprT>>, Name, TypeId),
    Call {
        callee: Box<Loc<ExprT>>,
        args: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    Tuple(Vec<Loc<ExprT>>, TypeId),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Value {
    Float(f32),
    Integer(i32),
    Bool(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Function {
    pub params: Vec<(Name, TypeId)>,
    pub body: Box<Loc<ExprT>>,
    pub local_variables: Vec<TypeId>,
    pub scope_index: usize,
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
pub enum UnaryOp {
    Minus,
    Not,
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Type {
    Unit,
    Float,
    Int,
    Bool,
    Char,
    String,
    Var(Name),
    Array(TypeId),
    Record(Vec<(Name, TypeId)>),
    Tuple(Vec<TypeId>),
    Arrow(TypeId, TypeId),
    // Points to a type that is solved further
    // Not the greatest solution but meh
    Solved(TypeId),
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
                Type::Record(fields) => {
                    let elems = fields
                        .iter()
                        .map(|(n, t)| format!("{}: {}", n, t))
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("({})", elems)
                }
                Type::Tuple(ts) => {
                    let elems = ts
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("({})", elems)
                }
                Type::Arrow(t1, t2) => format!("{} => {}", t1, t2),
                Type::Solved(t) => format!("solved({})", t),
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TypeSig {
    Array(Box<Loc<TypeSig>>),
    Name(Name),
    Empty,
    Arrow(Vec<Loc<TypeSig>>, Box<Loc<TypeSig>>),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TypeDef {
    Struct(Name, Vec<(Name, Loc<TypeSig>)>),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Pat {
    Id(Name, Option<Loc<TypeSig>>, LocationRange),
    Record(Vec<Name>, Option<Loc<TypeSig>>, LocationRange),
    Tuple(Vec<Pat>, LocationRange),
    Empty(LocationRange),
}

// Oy vey, cause Rust doesn't allow enum field access
impl ExprT {
    pub fn get_type(&self) -> TypeId {
        match &self {
            ExprT::Primary { value: _, type_ } => *type_,
            ExprT::Var { name: _, type_ } => *type_,
            ExprT::Tuple(_elems, type_) => *type_,
            ExprT::BinOp {
                op: _,
                lhs: _,
                rhs: _,
                type_,
            } => *type_,
            ExprT::UnaryOp {
                op: _,
                rhs: _,
                type_,
            } => *type_,
            ExprT::Function {
                function: _,
                name: _,
                type_,
            } => *type_,
            ExprT::Field(_, _, type_) => *type_,
            ExprT::Call {
                callee: _,
                args: _,
                type_,
            } => *type_,
            ExprT::Block {
                stmts: _,
                end_expr: _,
                scope_index: _,
                type_,
            } => *type_,
            ExprT::Record {
                name: _,
                fields: _,
                type_,
            } => *type_,
            ExprT::If(_, _, _, type_) => *type_,
        }
    }
}
