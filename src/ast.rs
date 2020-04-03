use im::hashmap::HashMap;
use std::fmt;
use std::sync::Arc;
pub type Name = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Asgn(Pat, Expr),
    Expr(Expr),
    Return(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Export(Name),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    // We need to desugar complicated bindings like:
    //   let { a, b, c } = f();
    // into:
    //   let res = f();
    //   let a = res.a;
    //   let b = res.b;
    //   let c = res.c;
    // However we want to make sure res doesn't shadow anything
    // and don't want to do scope analysis before desugaring.
    // The hacky solution we're pulling out of our ass?
    // HiddenVars! Basically compiler internal vars that
    // don't shadow. We have HiddenAsgn to assign to
    // a hidden var and HiddenVar to use it.
    // HiddenAsgn(Name, TypedExpr),
    Asgn(Name, TypedExpr),
    Expr(TypedExpr),
    Return(TypedExpr),
    Block(Vec<TypedStmt>),
    If(TypedExpr, Box<TypedStmt>, Option<Box<TypedStmt>>),
    Export(Name),
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
        args: Vec<Expr>,
    },
    Field(Box<Expr>, Name),
    Record {
        entries: Vec<(Name, Expr)>,
    },
    Tuple(Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub names: HashMap<Name, Arc<Type>>,
    pub parent: Option<usize>,
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
    HiddenVar {
        name: u64,
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
        param: Name,
        param_type: Arc<Type>,
        return_type: Arc<Type>,
        body: Box<TypedStmt>,
        scope_index: usize,
    },
    Field(Box<TypedExpr>, Name, Arc<Type>),
    Call {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
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
                Type::Var(name) => format!("var({})", name).into(),
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
    Record(Vec<(Name, TypeSig)>),
    Tuple(Vec<TypeSig>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Id(Name, Option<TypeSig>),
    Record(Vec<Name>),
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
            TypedExpr::HiddenVar { name: _, type_ } => type_.clone(),
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
                param: _,
                body: _,
                scope_index: _,
                param_type,
                return_type,
            } => Arc::new(Type::Arrow(param_type.clone(), return_type.clone())),
            TypedExpr::Field(_, _, type_) => type_.clone(),
            TypedExpr::Call {
                callee: _,
                args: _,
                type_,
            } => type_.clone(),
        }
    }
}
