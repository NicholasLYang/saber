use ast::Value;
use ast::{Expr, Name, Op, Pat, Stmt, Type, TypeSig, TypedExpr, TypedStmt};
use im::hashmap::HashMap;
use std::sync::Arc;

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "Could not determine type of '{}'", name)]
    InferFailure { name: Name },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
    #[fail(
        display = "Could not find operation {} with arguments of type {} and {}",
        op, lhs_type, rhs_type
    )]
    OpFailure {
        op: Op,
        lhs_type: Arc<Type>,
        rhs_type: Arc<Type>,
    },
    #[fail(display = "Could not unify {} with {}", type1, type2)]
    UnificationFailure { type1: Arc<Type>, type2: Arc<Type> },
    #[fail(display = "Type {} does not exit", type_name)]
    TypeDoesNotExist { type_name: Name },
    #[fail(display = "Arity mismatch: Expected {} but got {}", arity1, arity2)]
    ArityMismatch { arity1: usize, arity2: usize },
    #[fail(display = "Record contains non indentifier patterns: {:?}", record)]
    RecordContainsNonIds { record: Pat },
    #[fail(display = "Invalid unary operator: {}", op)]
    InvalidUnaryOp { op: Op },
    #[fail(display = "Cannot apply unary operator to {:?}", expr)]
    InvalidUnaryExpr { expr: TypedExpr },
    #[fail(display = "Callee is not a function")]
    CalleeNotFunction,
}

pub struct TypeChecker {
    // A symbol table of sorts
    ctx: HashMap<Name, Arc<Type>>,
    // Type names. Right now just has the primitives like string,
    // integer, float, char
    type_names: HashMap<Name, Arc<Type>>,
    // The return type for the typing context
    return_type: Option<Arc<Type>>,
    variable_counter: usize,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let primitive_types = vec![
            ("int".to_string(), Arc::new(Type::Int)),
            ("float".to_string(), Arc::new(Type::Float)),
            ("char".to_string(), Arc::new(Type::Char)),
            ("string".to_string(), Arc::new(Type::String)),
        ];
        let ctx = HashMap::new();
        let mut type_names = HashMap::new();
        for (name, type_) in primitive_types {
            type_names.insert(name, type_);
        }
        TypeChecker {
            ctx,
            type_names,
            return_type: None,
            variable_counter: 0,
        }
    }

    pub fn check_program(&mut self, program: Vec<Stmt>) -> Result<Vec<TypedStmt>, TypeError> {
        let mut typed_stmts = Vec::new();
        for stmt in program {
            typed_stmts.push(self.stmt(stmt)?);
        }
        Ok(typed_stmts)
    }

    pub fn stmt(&mut self, stmt: Stmt) -> Result<TypedStmt, TypeError> {
        match stmt {
            Stmt::Expr(expr) => {
                let typed_expr = self.expr(expr)?;
                Ok(TypedStmt::Expr(typed_expr))
            }
            Stmt::HiddenAsgn(name, type_sig, expr) => {
                let typed_rhs = self.expr(expr)?;
                self.asgn(&name, &type_sig, typed_rhs.get_type())?;
                Ok(TypedStmt::HiddenAsgn(name, typed_rhs))
            }
            Stmt::Asgn(name, type_sig, expr) => {
                let typed_rhs = self.expr(expr)?;
                self.asgn(&name, &type_sig, typed_rhs.get_type())?;
                Ok(TypedStmt::Asgn(name, typed_rhs))
            }
            Stmt::If(cond, then_stmt, else_stmt) => {
                let typed_cond = self.expr(cond)?;
                if !self.unify(&typed_cond.get_type(), &Arc::new(Type::Bool)) {
                    return Err(TypeError::UnificationFailure {
                        type1: typed_cond.get_type(),
                        type2: Arc::new(Type::Bool),
                    });
                }
                let typed_then = self.stmt(*then_stmt)?;
                let typed_else = match else_stmt {
                    Some(else_stmt) => Some(Box::new(self.stmt(*else_stmt)?)),
                    None => None,
                };
                Ok(TypedStmt::If(typed_cond, Box::new(typed_then), typed_else))
            }
            Stmt::Return(expr) => {
                let typed_exp = self.expr(expr)?;
                match self.return_type.clone() {
                    Some(ref return_type) => {
                        if self.unify(&typed_exp.get_type(), return_type) {
                            Ok(TypedStmt::Return(typed_exp))
                        } else {
                            Err(TypeError::UnificationFailure {
                                type1: typed_exp.get_type().clone(),
                                type2: return_type.clone(),
                            })
                        }
                    }
                    None => {
                        self.return_type = Some(typed_exp.get_type().clone());
                        Ok(TypedStmt::Return(typed_exp))
                    }
                }
            }
            Stmt::Block(stmts) => {
                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    typed_stmts.push(self.stmt(stmt)?);
                }
                Ok(TypedStmt::Block(typed_stmts))
            }
            Stmt::Export(name) => {
                if !self.ctx.contains_key(&name) {
                    let type_ = self.get_fresh_type_var();
                    self.ctx.insert(name.clone(), type_);
                }
                Ok(TypedStmt::Export(name))
            }
        }
    }

    fn value(&self, value: Value) -> TypedExpr {
        match value {
            Value::Integer(_i) => TypedExpr::Primary {
                value,
                type_: Arc::new(Type::Int),
            },
            Value::Float(_f) => TypedExpr::Primary {
                value,
                type_: Arc::new(Type::Float),
            },
            Value::Bool(_b) => TypedExpr::Primary {
                value,
                type_: Arc::new(Type::Bool),
            },
            Value::String(s) => TypedExpr::Primary {
                value: Value::String(s),
                type_: Arc::new(Type::String),
            },
        }
    }

    fn lookup_type_sig(&self, sig: &TypeSig) -> Result<Arc<Type>, TypeError> {
        match sig {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(Arc::new(Type::Array(type_)))
            }
            TypeSig::Name(name) => {
                if let Some(type_) = self.type_names.get(name) {
                    Ok(type_.clone())
                } else {
                    Err(TypeError::TypeDoesNotExist {
                        type_name: name.clone(),
                    })
                }
            }
        }
    }

    fn get_fresh_type_var(&mut self) -> Arc<Type> {
        let type_var = Type::Var(self.variable_counter.to_string());
        self.variable_counter += 1;
        Arc::new(type_var)
    }

    fn pat(&mut self, pat: &Pat) -> Result<Arc<Type>, TypeError> {
        match pat {
            Pat::Id(name, Some(type_sig)) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                self.ctx.insert(name.to_string(), type_.clone());
                Ok(type_)
            }
            Pat::Id(name, None) => {
                let type_ = self.get_fresh_type_var();
                self.ctx.insert(name.to_string(), type_.clone());
                Ok(type_)
            }
            Pat::Tuple(pats) => {
                let types: Result<Vec<_>, _> = pats.iter().map(|pat| self.pat(pat)).collect();
                Ok(Arc::new(Type::Tuple(types?)))
            }
            Pat::Record(pats) => {
                let types: Result<Vec<_>, _> = pats
                    .iter()
                    .map(|name| Ok((name.clone(), self.pat(pat)?)))
                    .collect();
                Ok(Arc::new(Type::Record(types?)))
            }
            Pat::Empty => Ok(Arc::new(Type::Unit)),
        }
    }

    fn asgn(
        &mut self,
        name: &str,
        type_sig: &Option<TypeSig>,
        rhs_type: Arc<Type>,
    ) -> Result<Arc<Type>, TypeError> {
        let lhs_type = if let Some(type_sig) = type_sig {
            self.lookup_type_sig(type_sig)?
        } else {
            self.get_fresh_type_var()
        };
        if self.unify(&lhs_type, &rhs_type) {
            self.ctx.insert(name.to_string(), rhs_type.clone());
            Ok(rhs_type)
        } else {
            Err(TypeError::UnificationFailure {
                type1: lhs_type,
                type2: rhs_type,
            })
        }
    }

    fn expr(&mut self, expr: Expr) -> Result<TypedExpr, TypeError> {
        match expr {
            Expr::Primary { value } => Ok(self.value(value)),
            Expr::Var { name } => match self.ctx.get(&name) {
                Some(type_) => Ok(TypedExpr::Var {
                    name,
                    type_: type_.clone(),
                }),
                None => Err(TypeError::InferFailure {
                    name: name.to_string(),
                }),
            },
            Expr::BinOp { op, lhs, rhs } => {
                let typed_lhs = self.expr(*lhs)?;
                let typed_rhs = self.expr(*rhs)?;
                let lhs_type = typed_lhs.get_type();
                let rhs_type = typed_rhs.get_type();
                match self.op(&op, lhs_type, rhs_type) {
                    Some(op_type) => Ok(TypedExpr::BinOp {
                        op,
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                        type_: Arc::new(op_type),
                    }),
                    None => Err(TypeError::OpFailure {
                        op: op.clone(),
                        lhs_type: typed_lhs.get_type(),
                        rhs_type: typed_rhs.get_type(),
                    }),
                }
            }
            Expr::Tuple(elems) => {
                let mut typed_elems = Vec::new();
                let mut types = Vec::new();
                for elem in elems {
                    let typed_elem = self.expr(elem)?;
                    types.push(typed_elem.get_type());
                    typed_elems.push(typed_elem);
                }
                Ok(TypedExpr::Tuple(typed_elems, Arc::new(Type::Tuple(types))))
            }
            Expr::Function {
                params,
                body,
                return_type,
            } => {
                // Insert params into ctx
                self.pat(&params)?;
                // Insert return type into typechecker so that
                // typechecker can verify return statements.
                if let Some(return_type_sig) = return_type {
                    self.return_type = Some(self.lookup_type_sig(&return_type_sig)?);
                }
                // Check body
                let body = self.stmt(*body)?;

                let mut return_type = None;
                std::mem::swap(&mut return_type, &mut self.return_type);
                let params_type = self.retrieve_type_from_params(&params)?;
                let return_type = return_type.unwrap_or(Arc::new(Type::Unit));
                let function_type = Type::Arrow(params_type.clone(), return_type);
                Ok(TypedExpr::Function {
                    params,
                    body: Box::new(body),
                    type_: Arc::new(function_type),
                    env: HashMap::new(),
                })
            }
            Expr::UnaryOp { op, rhs } => match op {
                Op::Minus | Op::Plus => {
                    let typed_rhs = self.expr(*rhs)?;
                    if self.unify(&typed_rhs.get_type(), &Arc::new(Type::Int))
                        || self.unify(&typed_rhs.get_type(), &Arc::new(Type::Float))
                    {
                        let type_ = typed_rhs.get_type();
                        Ok(TypedExpr::UnaryOp {
                            op,
                            rhs: Box::new(typed_rhs),
                            type_,
                        })
                    } else {
                        Err(TypeError::InvalidUnaryExpr {
                            expr: typed_rhs.clone(),
                        })
                    }
                }
                op => Err(TypeError::InvalidUnaryOp { op }),
            },
            Expr::Call { callee, args } => {
                let typed_callee = self.expr(*callee)?;
                let mut typed_args = Vec::new();
                for arg in args {
                    typed_args.push(self.expr(arg)?);
                }
                let callee_type = typed_callee.get_type();
                if let Type::Arrow(args_type, return_type) = &(*callee_type) {
                    Ok(TypedExpr::Call {
                        callee: Box::new(typed_callee),
                        args: typed_args,
                        type_: args_type.clone(),
                    })
                } else {
                    Err(TypeError::CalleeNotFunction)
                }
            }
            _ => Err(TypeError::NotImplemented),
        }
    }

    /*
     Retrieves param type from context
    */
    fn retrieve_type_from_params(&mut self, params: &Pat) -> Result<Arc<Type>, TypeError> {
        match params {
            Pat::Id(name, _) => {
                {
                    let maybe_type = self.ctx.get(name);
                    if let Some(type_) = maybe_type {
                        return Ok(type_.clone());
                    }
                }
                let type_var = self.get_fresh_type_var();
                self.ctx = self.ctx.update(name.clone(), type_var.clone());
                Ok(type_var)
            }
            Pat::Tuple(pats) => {
                let mut param_types = Vec::new();
                for pat in pats {
                    let type_ = self.retrieve_type_from_params(pat)?;
                    param_types.push(type_);
                }
                Ok(Arc::new(Type::Tuple(param_types)))
            }
            Pat::Record(names) => {
                let mut param_types = Vec::new();
                for name in names {
                    let type_ = self.get_fresh_type_var();
                    param_types.push((name.clone(), type_));
                }
                Ok(Arc::new(Type::Record(param_types)))
            }
            Pat::Empty => Ok(Arc::new(Type::Unit)),
        }
    }

    fn op(&mut self, op: &Op, lhs_type: Arc<Type>, rhs_type: Arc<Type>) -> Option<Type> {
        match op {
            Op::Comma => Some(Type::Tuple(vec![lhs_type, rhs_type])),
            Op::Plus | Op::Minus | Op::Times | Op::Div => match (&*lhs_type, &*rhs_type) {
                (Type::Float, Type::Float)
                | (Type::Bool, Type::Float)
                | (Type::Float, Type::Bool) => Some(Type::Float),
                (Type::Int, Type::Int) => Some(Type::Int),
                _ => None,
            },
            Op::BangEqual | Op::EqualEqual => {
                if self.unify(&lhs_type, &rhs_type) {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
                // If we can unify lhs and rhs, and lhs with Int or Float then
                // by transitivity we can unify everything with float
                let is_num = self.unify(&lhs_type, &Arc::new(Type::Float))
                    || self.unify(&lhs_type, &Arc::new(Type::Int));
                if self.unify(&lhs_type, &rhs_type) && is_num {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
        }
    }

    fn unify_type_vectors(
        &mut self,
        type_vector1: &Vec<Arc<Type>>,
        type_vector2: &Vec<Arc<Type>>,
    ) -> bool {
        if type_vector1.len() != type_vector2.len() {
            return false;
        }
        for (t1, t2) in type_vector1.iter().zip(type_vector2.iter()) {
            if !self.unify(t1, t2) {
                return false;
            }
        }
        return true;
    }

    fn unify(&mut self, type1: &Arc<Type>, type2: &Arc<Type>) -> bool {
        if Arc::ptr_eq(type1, type2) || type1 == type2 {
            return true;
        }
        match (&**type1, &**type2) {
            (Type::Tuple(t1), Type::Tuple(t2)) => self.unify_type_vectors(&t1, &t2),
            (Type::Arrow(param_type1, return_type1), Type::Arrow(param_type2, return_type2)) => {
                self.unify(&param_type1, &param_type2) && self.unify(&return_type1, &return_type2)
            }
            (Type::Int, Type::Bool) | (Type::Bool, Type::Int) => true,
            (Type::Var(name), t2) | (t2, Type::Var(name)) => {
                let ctx = self.ctx.clone();
                let var_type = ctx.get(name);
                let is_unified = if let Some(t1) = var_type {
                    let is_unified = self.unify(&t1, &Arc::new(t2.clone()));
                    self.ctx = self.ctx.update(name.to_string(), t1.clone());
                    is_unified
                } else {
                    self.ctx = self.ctx.update(name.clone(), Arc::new(t2.clone()));
                    true
                };
                is_unified
            }
            _ => false,
        }
    }
}
