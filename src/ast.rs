use crate::lexer::LocationRange;
use crate::parser::ParseError;
use crate::typechecker::TypeError;
use id_arena::{Arena, Id};
use serde::{Deserialize, Serialize};
use std::fmt;

pub type Name = usize;
pub type FunctionId = usize;
pub type TypeId = Id<Type>;

// Wrapper to provide location to AST nodes
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Loc<T> {
    pub location: LocationRange,
    pub inner: T,
}

#[macro_export]
macro_rules! loc {
    ($inner:expr, $location:expr) => {
        Loc {
            inner: $inner,
            location: $location,
        }
    };
    ($inner:expr, $location:expr,) => {
        loc!($inner, $location)
    };
}

impl<T: fmt::Display> fmt::Display for Loc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
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
    LogicalAnd,
    LogicalOr,
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
                Op::LogicalAnd => "&&",
                Op::LogicalOr => "||",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Program {
    pub stmts: Vec<Loc<Stmt>>,
    pub type_defs: Vec<Loc<TypeDef>>,
    pub errors: Vec<Loc<ParseError>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Let(Pat, Loc<Expr>),
    Break,
    Expr(Loc<Expr>),
    Loop(Loc<Expr>),
    Return(Loc<Expr>),
    Function(Name, Pat, Option<Loc<TypeSig>>, Box<Loc<Expr>>),
    Export(Name),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Expr {
    Asgn {
        lhs: Box<Loc<Expr>>,
        rhs: Box<Loc<Expr>>,
    },
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
    TupleField(Box<Loc<Expr>>, u32),
    Index {
        lhs: Box<Loc<Expr>>,
        index: Box<Loc<Expr>>,
    },
    Record {
        name: Name,
        fields: Vec<(Name, Loc<Expr>)>,
    },
    Tuple(Vec<Loc<Expr>>),
    Array(Vec<Loc<Expr>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgramT {
    pub functions: Vec<Loc<Function>>,
    pub stmts: Vec<Loc<StmtT>>,
    pub named_types: Vec<(Name, TypeId)>,
    pub errors: Vec<Loc<TypeError>>,
    pub exports: Vec<Export>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportKind {
    Var,
    Function,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub kind: ExportKind,
    pub idx: usize,
    pub name: Name,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtT {
    Let(Name, Loc<ExprT>),
    Break,
    Expr(Loc<ExprT>),
    Return(Loc<ExprT>),
    Loop(Loc<ExprT>),
    Function {
        func_index: usize,
    },
    If {
        cond: Box<Loc<ExprT>>,
        then_block: Box<Loc<ExprT>>,
        else_block: Option<Box<Loc<ExprT>>>,
    },
}

// Field or index accesses on a struct/array
#[derive(Debug, PartialEq, Clone)]
pub enum Accessor {
    Field(usize),
    Index(Loc<ExprT>),
}
// Lvalue or target for assignment
// An identifier followed by any number of index or field accesses
// foo.bar[0].baz = 10
#[derive(Debug, PartialEq, Clone)]
pub struct Target {
    pub ident: Name,
    pub accessors: Vec<Loc<Accessor>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum UnaryOpT {
    F32Minus,
    I32Minus,
    BoolNot,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum BinaryOpT {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32NotEqual,
    I32Equal,
    I32Greater,
    I32GreaterEqual,
    I32Less,
    I32LessEqual,
    I32And,
    I32Or,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32NotEqual,
    F32Equal,
    F32Greater,
    F32GreaterEqual,
    F32Less,
    F32LessEqual,
    BoolAnd,
    BoolOr,
    StringEqual,
    StringConcat,
}

impl fmt::Display for BinaryOpT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOpT::I32Add => "i32.add",
                BinaryOpT::I32Sub => "i32.sub",
                BinaryOpT::I32Mul => "i32.mul",
                BinaryOpT::I32Div => "i32.div",
                BinaryOpT::I32NotEqual => "i32.not_eq",
                BinaryOpT::I32Equal => "i32.eq",
                BinaryOpT::I32Greater => "i32.greater",
                BinaryOpT::I32GreaterEqual => "i32.greater_eq",
                BinaryOpT::I32Less => "i32.less",
                BinaryOpT::I32LessEqual => "i32.less_eq",
                BinaryOpT::I32And => "i32.and",
                BinaryOpT::I32Or => "i32.or",
                BinaryOpT::F32Add => "f32.add",
                BinaryOpT::F32Sub => "f32.sub",
                BinaryOpT::F32Mul => "f32.mul",
                BinaryOpT::F32Div => "f32.div",
                BinaryOpT::F32NotEqual => "f32.not_eq",
                BinaryOpT::F32Equal => "f32.equal",
                BinaryOpT::F32Greater => "f32.greater",
                BinaryOpT::F32GreaterEqual => "f32.greater_eq",
                BinaryOpT::F32Less => "f32.less",
                BinaryOpT::F32LessEqual => "f32.less_eq",
                BinaryOpT::BoolAnd => "bool.and",
                BinaryOpT::BoolOr => "bool.or",
                BinaryOpT::StringEqual => "str.eq",
                BinaryOpT::StringConcat => "str.concat",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprT {
    Asgn {
        lhs: Loc<Target>,
        rhs: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    Block {
        stmts: Vec<Loc<StmtT>>,
        end_expr: Option<Box<Loc<ExprT>>>,
        scope_index: usize,
        type_: TypeId,
    },
    If(Box<Loc<ExprT>>, Box<Loc<ExprT>>, Box<Loc<ExprT>>, TypeId),
    Primary {
        value: Value,
        type_: TypeId,
    },
    Var {
        name: Name,
        type_: TypeId,
    },
    BinOp {
        op: BinaryOpT,
        lhs: Box<Loc<ExprT>>,
        rhs: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    UnaryOp {
        op: UnaryOpT,
        rhs: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    Function {
        func_index: usize,
        type_: TypeId,
    },
    Record {
        name: Name,
        fields: Vec<(Name, Loc<ExprT>)>,
        type_: TypeId,
    },
    Index {
        lhs: Box<Loc<ExprT>>,
        index: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    TupleField(Box<Loc<ExprT>>, u32, TypeId),
    Print {
        args: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    DirectCall {
        callee: FunctionId,
        args: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    IndirectCall {
        callee: Box<Loc<ExprT>>,
        args: Box<Loc<ExprT>>,
        type_: TypeId,
    },
    Tuple(Vec<Loc<ExprT>>, TypeId),
    Array {
        entries: Vec<Loc<ExprT>>,
        entry_type: TypeId,
        type_: TypeId,
    },
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Value {
    Float(f32),
    Integer(i32),
    Bool(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub params: Vec<(Name, TypeId)>,
    pub return_type: TypeId,
    pub body: Vec<Loc<StmtT>>,
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

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Float,
    Integer,
    Bool,
    Char,
    String,
    Var(Name),
    Array(TypeId),
    Record(Name, Vec<(Name, TypeId)>),
    Tuple(Vec<TypeId>),
    Arrow(TypeId, TypeId),
    // Points to a type that is solved further
    // Not the greatest solution but meh
    Solved(TypeId),
}

impl Type {
    pub fn is_ref_type(&self) -> bool {
        matches!(
            self,
            Type::Integer | Type::Bool | Type::Unit | Type::Char | Type::Float
        )
    }
}

// Struct for the builtin types to prevent reallocation
pub struct BuiltInTypes {
    pub unit: TypeId,
    pub float: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub char: TypeId,
    pub string: TypeId,
}

impl BuiltInTypes {
    pub fn new(type_arena: &mut Arena<Type>) -> Self {
        BuiltInTypes {
            unit: type_arena.alloc(Type::Unit),
            float: type_arena.alloc(Type::Float),
            int: type_arena.alloc(Type::Integer),
            bool: type_arena.alloc(Type::Bool),
            char: type_arena.alloc(Type::Char),
            string: type_arena.alloc(Type::String),
        }
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
            ExprT::Asgn {
                lhs: _,
                rhs: _,
                type_,
            } => *type_,
            ExprT::Primary { value: _, type_ } => *type_,
            ExprT::Var { name: _, type_ } => *type_,
            ExprT::Tuple(_elems, type_) => *type_,
            ExprT::Array {
                entries: _,
                entry_type: _,
                type_,
            } => *type_,
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
                func_index: _,
                type_,
            } => *type_,
            ExprT::Index {
                lhs: _,
                index: _,
                type_,
            } => *type_,
            ExprT::TupleField(_, _, type_) => *type_,
            ExprT::Print { args: _, type_ } => *type_,
            ExprT::DirectCall {
                callee: _,
                args: _,
                type_,
            } => *type_,
            ExprT::IndirectCall {
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
