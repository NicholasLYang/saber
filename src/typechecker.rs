use ast::Value;
use ast::{Expr, Name, Op, Pat, Stmt, Type, TypeSig, TypedExpr, TypedStmt};
use itertools::Itertools;
use std::collections::HashMap;
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
}

pub struct TypeChecker {
    // Honestly I don't know. Probably just the types of the existing
    // variables?
    ctx: HashMap<Name, Arc<Type>>,
    // Type names. Right now just has the primitives like string,
    // integer, float, char
    type_names: HashMap<Name, Type>,
    // The return type for the typing context
    return_type: Option<Arc<Type>>,
    variable_counter: usize,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let primitive_types = vec![
            ("integer".to_string(), Type::Int),
            ("float".to_string(), Type::Float),
            ("char".to_string(), Type::Char),
            ("string".to_string(), Type::String),
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

    pub fn infer_stmt(&mut self, stmt: Stmt) -> Result<TypedStmt, TypeError> {
        match stmt {
            Stmt::Expr(expr) => {
                let typed_expr = self.infer_expr(expr)?;
                Ok(TypedStmt::Expr(typed_expr))
            }
            Stmt::Asgn(pat, expr) => {
                let typed_rhs = self.infer_expr(expr)?;
                self.infer_asgn(&pat, typed_rhs.get_type())?;
                Ok(TypedStmt::Asgn(pat, typed_rhs))
            }
            Stmt::If(cond, then_stmt, else_stmt) => {
                let typed_cond = self.infer_expr(cond)?;
                if !self.unify(&typed_cond.get_type(), &Arc::new(Type::Bool)) {
                    return Err(TypeError::UnificationFailure {
                        type1: typed_cond.get_type(),
                        type2: Arc::new(Type::Bool),
                    });
                }
                let typed_then = self.infer_stmt(*then_stmt)?;
                let typed_else = match else_stmt {
                    Some(else_stmt) => Some(Box::new(self.infer_stmt(*else_stmt)?)),
                    None => None,
                };
                Ok(TypedStmt::If(typed_cond, Box::new(typed_then), typed_else))
            }
            Stmt::Return(expr) => {
                let typed_exp = self.infer_expr(expr)?;
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
                    typed_stmts.push(self.infer_stmt(stmt)?);
                }
                Ok(TypedStmt::Block(typed_stmts))
            }
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn infer_value(&self, value: Value) -> TypedExpr {
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

    fn lookup_type_sig(&self, sig: &TypeSig) -> Result<Type, TypeError> {
        match sig {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(Type::Array(Arc::new(type_)))
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

    fn get_fresh_type_var(&mut self) -> Type {
        let type_var = Type::Var(self.variable_counter.to_string());
        self.variable_counter += 1;
        type_var
    }

    fn infer_pat(&mut self, pat: &Pat) -> Result<Type, TypeError> {
        match pat {
            Pat::Id(name, Some(type_sig)) => self.lookup_type_sig(&type_sig),
            Pat::Id(_name, None) => Ok(self.get_fresh_type_var()),
            Pat::Tuple(pats) => {
                let mut types = Vec::new();
                for pat in pats {
                    let type_ = self.infer_pat(pat)?;
                    types.push(Arc::new(type_));
                }
                Ok(Type::Tuple(types))
            }
            Pat::Empty => Ok(Type::Unit),
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn infer_asgn(&mut self, pat: &Pat, rhs_type: Arc<Type>) -> Result<Arc<Type>, TypeError> {
        let lhs_type = self.infer_pat(pat)?;
        let arc_lhs_type = Arc::new(lhs_type);
        if self.unify(&arc_lhs_type, &rhs_type) {
            Ok(rhs_type)
        } else {
            Err(TypeError::UnificationFailure {
                type1: arc_lhs_type,
                type2: rhs_type,
            })
        }
    }

    fn infer_expr(&mut self, expr: Expr) -> Result<TypedExpr, TypeError> {
        match expr {
            Expr::Primary { value } => Ok(self.infer_value(value)),
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
                let typed_lhs = self.infer_expr(*lhs)?;
                let typed_rhs = self.infer_expr(*rhs)?;
                let lhs_type = typed_lhs.get_type();
                let rhs_type = typed_rhs.get_type();
                match self.infer_op(&op, lhs_type, rhs_type) {
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
                    let typed_elem = self.infer_expr(elem)?;
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
                self.insert_params(&params)?;
                // Insert return type into typechecker so that
                // typechecker can verify return statements.
                if let Some(return_type_sig) = return_type {
                    self.return_type = Some(Arc::new(self.lookup_type_sig(&return_type_sig)?));
                }
                // Check body
                let body = self.infer_stmt(*body)?;

                let mut return_type = None;
                std::mem::swap(&mut return_type, &mut self.return_type);
                let params_type = self.retrieve_params(&params);
                let return_type = return_type.unwrap_or(Arc::new(Type::Unit));
                let function_type = Type::Arrow(Arc::new(Type::Unit), return_type);
                Ok(TypedExpr::Function {
                    params,
                    body: Box::new(body),
                    type_: Arc::new(function_type),
                })
            }
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn retrieve_params(&mut self, params: &Pat) -> Result<Arc<Type>, TypeError> {
        match params {
            Pat::Id(name, _) => {
                {
                    let maybe_type = self.ctx.get(name);
                    if let Some(type_) = maybe_type {
                        return Ok(type_.clone());
                    }
                }
                let type_var = Arc::new(self.get_fresh_type_var());
                self.ctx.insert(name.clone(), type_var.clone());
                Ok(type_var)
            }
            Pat::Tuple(pats) => {
                let mut param_types = Vec::new();
                for pat in pats {
                    let type_ = self.retrieve_params(pat)?;
                    param_types.push(type_);
                }
                Ok(Arc::new(Type::Tuple(param_types)))
            }
            Pat::Record(pats) => {
                let mut param_types = Vec::new();
                for pat in pats {
                    let type_ = self.retrieve_params(pat)?;
                    if let Pat::Id(name, _) = pat {
                        param_types.push((name.clone(), type_));
                    } else {
                        return Err(TypeError::RecordContainsNonIds {
                            record: params.clone(),
                        });
                    }
                }
                Ok(Arc::new(Type::Record(param_types)))
            }
            Pat::Empty => Ok(Arc::new(Type::Unit)),
        }
    }

    // Insert params into context
    fn insert_params(&mut self, params: &Pat) -> Result<(), TypeError> {
        match params {
            Pat::Id(name, Some(type_sig)) => {
                let type_ = self.lookup_type_sig(type_sig)?;
                self.ctx.insert(name.clone(), Arc::new(type_));
            }
            Pat::Id(name, None) => {
                let type_ = self.get_fresh_type_var();
                self.ctx.insert(name.clone(), Arc::new(type_));
            }
            Pat::Tuple(pats) => {
                for pat in pats {
                    self.insert_params(pat)?;
                }
            }
            Pat::Record(pats) => {
                for pat in pats {
                    self.insert_params(pat)?;
                }
            }
            _ => (),
        };
        Ok(())
    }

    fn infer_op(&mut self, op: &Op, lhs_type: Arc<Type>, rhs_type: Arc<Type>) -> Option<Type> {
        match op {
            Op::Comma => Some(Type::Tuple(vec![lhs_type, rhs_type])),
            Op::Plus | Op::Minus | Op::Times | Op::Div => match (&*lhs_type, &*rhs_type) {
                (Type::Float, Type::Float)
                | (Type::Int, Type::Int)
                | (Type::Bool, Type::Float)
                | (Type::Float, Type::Bool) => Some(Type::Float),
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
                let var_type = self.ctx.remove(name);
                if let Some(t1) = var_type {
                    let is_unified = self.unify(&t1, &Arc::new(t2.clone()));
                    self.ctx.insert(name.to_string(), t1);
                    is_unified
                } else {
                    self.ctx.insert(name.clone(), Arc::new(t2.clone()));
                    true
                }
            }
            _ => false,
        }
    }
}
