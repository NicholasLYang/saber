use ast::Value;
use ast::{Expr, Name, Op, Pat, Stmt, Type, TypeSig, TypedExpr, TypedStmt};
use std::collections::HashMap;

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "Could not determine type of '{}'", name)]
    InferFailure { name: Name },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
    #[fail(
        display = "Could not find operation {:?} with arguments of type {:?} and {:?}",
        op, lhs_type, rhs_type
    )]
    OpFailure {
        op: Op,
        lhs_type: Type,
        rhs_type: Type,
    },
    #[fail(display = "Could not unify {:?} with {:?}", type1, type2)]
    UnificationFailure { type1: Type, type2: Type },
    #[fail(display = "Type {:?} does not exit", type_name)]
    TypeDoesNotExist { type_name: Name },
    #[fail(display = "Arity mismatch: Expected {:?} but got {:?}", arity1, arity2)]
    ArityMismatch { arity1: usize, arity2: usize },
}

pub struct TypeChecker {
    ctx: HashMap<Name, Type>,
    type_names: HashMap<Name, Type>,
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
                self.infer_asgn(&pat, typed_rhs.get_type().clone())?;
                Ok(TypedStmt::Asgn(pat, typed_rhs))
            }
            Stmt::If(cond, then_stmt, else_stmt) => {
                let typed_cond = self.infer_expr(cond)?;
                if !self.unify(typed_cond.get_type(), &Type::Bool) {
                    return Err(TypeError::UnificationFailure {
                        type1: typed_cond.get_type().clone(),
                        type2: Type::Bool,
                    });
                }
                let typed_then = self.infer_stmt(*then_stmt)?;
                let typed_else = match else_stmt {
                    Some(else_stmt) => Some(Box::new(self.infer_stmt(*else_stmt)?)),
                    None => None,
                };
                Ok(TypedStmt::If(typed_cond, Box::new(typed_then), typed_else))
            }
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn infer_value(&self, value: Value) -> TypedExpr {
        match value {
            Value::Integer(_i) => TypedExpr::Primary {
                value,
                type_: Type::Int,
            },
            Value::Float(_f) => TypedExpr::Primary {
                value,
                type_: Type::Float,
            },
            Value::Bool(_b) => TypedExpr::Primary {
                value,
                type_: Type::Bool,
            },
            Value::String(s) => TypedExpr::Primary {
                value: Value::String(s),
                type_: Type::String,
            },
        }
    }

    fn lookup_type_sig(&self, sig: &TypeSig) -> Result<Type, TypeError> {
        match sig {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(Type::Array(Box::new(type_.clone())))
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

    fn infer_multiple_asgn(&mut self, asgns: &Vec<Pat>, rhs_type: Type) -> Result<Type, TypeError> {
        let pat_types = self.get_types_from_pat_vec(asgns)?;
        match rhs_type {
            Type::Tuple(types) => {
                if self.unify_type_vectors(&pat_types, &types) {
                    return Ok(Type::Tuple(types));
                } else {
                    return Err(TypeError::UnificationFailure {
                        type1: Type::Tuple(pat_types),
                        type2: Type::Tuple(types),
                    });
                }
            }
            _ => Err(TypeError::UnificationFailure {
                type1: rhs_type.clone(),
                type2: Type::Tuple(pat_types),
            }),
        }
    }

    fn get_types_from_pat_vec(&mut self, patterns: &Vec<Pat>) -> Result<Vec<Type>, TypeError> {
        let mut types = Vec::new();
        for pat in patterns.iter() {
            let type_ = match pat {
                Pat::Id(_name, Some(type_sig)) => self.lookup_type_sig(&type_sig)?,
                Pat::Id(_name, None) => self.get_fresh_type_var(),
                _ => self.get_fresh_type_var(),
            };
            types.push(type_);
        }
        Ok(types)
    }

    fn get_fresh_type_var(&mut self) -> Type {
        let type_var = Type::Var(self.variable_counter.to_string());
        self.variable_counter += 1;
        type_var
    }

    fn infer_asgn(&mut self, pat: &Pat, rhs_type: Type) -> Result<Type, TypeError> {
        match pat {
            Pat::Id(name, Some(type_sig)) => {
                let expected_type = self.lookup_type_sig(&type_sig)?;
                if self.unify(&expected_type, &rhs_type) {
                    self.ctx.insert(name.clone(), expected_type.clone());
                    Ok(rhs_type)
                } else {
                    Err(TypeError::UnificationFailure {
                        type1: expected_type,
                        type2: rhs_type.clone(),
                    })
                }
            }
            Pat::Id(_name, None) => Ok(self.get_fresh_type_var()),
            Pat::Tuple(pats) => self.infer_multiple_asgn(pats, rhs_type),
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn infer_expr(&self, expr: Expr) -> Result<TypedExpr, TypeError> {
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
                let lhs_type = typed_lhs.get_type().clone();
                let rhs_type = typed_rhs.get_type().clone();
                match self.infer_op(&op, lhs_type, rhs_type) {
                    Some(op_type) => Ok(TypedExpr::BinOp {
                        op,
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                        type_: op_type,
                    }),
                    None => Err(TypeError::OpFailure {
                        op: op.clone(),
                        lhs_type: typed_lhs.get_type().clone(),
                        rhs_type: typed_rhs.get_type().clone(),
                    }),
                }
            }
            Expr::Tuple(elems) => {
                let mut typed_elems = Vec::new();
                let mut types = Vec::new();
                for elem in elems {
                    let typed_elem = self.infer_expr(elem)?;
                    types.push(typed_elem.get_type().clone());
                    typed_elems.push(typed_elem);
                }
                Ok(TypedExpr::Tuple(typed_elems, Type::Tuple(types)))
            }
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn infer_op(&self, op: &Op, lhs_type: Type, rhs_type: Type) -> Option<Type> {
        match op {
            Op::Comma => Some(Type::Tuple(vec![lhs_type, rhs_type])),
            Op::Plus | Op::Minus | Op::Times | Op::Div => match (lhs_type, rhs_type) {
                (Type::Float, Type::Float)
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
                let is_num =
                    self.unify(&lhs_type, &Type::Float) || self.unify(&lhs_type, &Type::Int);
                if self.unify(&lhs_type, &rhs_type) && is_num {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
        }
    }

    fn unify_type_vectors(&self, type_vector1: &Vec<Type>, type_vector2: &Vec<Type>) -> bool {
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

    fn unify(&self, type1: &Type, type2: &Type) -> bool {
        if type1 == type2 {
            return true;
        }
        match (type1, type2) {
            (Type::Tuple(t1), Type::Tuple(t2)) => self.unify_type_vectors(t1, t2),
            (Type::Arrow(param_type1, return_type1), Type::Arrow(param_type2, return_type2)) => {
                self.unify(param_type1, param_type2) && self.unify(return_type1, return_type2)
            }
            (Type::Int, Type::Bool) | (Type::Bool, Type::Int) => true,
            _ => false,
        }
    }
}
