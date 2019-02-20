use ast::Value;
use ast::{Expr, Name, Op, Pat, Stmt, Type, TypeAnnotation, TypedExpr, TypedStmt};
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
}

pub fn infer_value(value: Value) -> TypedExpr {
    match value {
        Value::Num(_n) => TypedExpr::Primary {
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

pub fn infer_stmt(
    ctx: &mut HashMap<Name, Type>,
    type_names: &HashMap<Name, Type>,
    stmt: Stmt,
) -> Result<TypedStmt, TypeError> {
    match stmt {
        Stmt::Expr(expr) => {
            let typed_expr = infer_expr(ctx, expr)?;
            Ok(TypedStmt::Expr(typed_expr))
        }
        Stmt::Asgn(pat, type_annotation, expr) => {
            infer_asgn(ctx, type_names, pat, type_annotation, expr)
        }
        _ => Err(TypeError::NotImplemented),
    }
}

fn lookup_type_annotation(
    type_names: &HashMap<Name, Type>,
    annotation: TypeAnnotation,
) -> Option<Type> {
    match annotation {
        TypeAnnotation::Array(annotation) => {
            if let Some(type_) = lookup_type_annotation(type_names, *annotation) {
                Some(Type::Array(Box::new(type_)))
            } else {
                None
            }
        }
        TypeAnnotation::Name(name) => {
            if let Some(type_) = type_names.get(&name) {
                Some(type_.clone())
            } else {
                None
            }
        }
    }
}

pub fn infer_asgn(
    ctx: &mut HashMap<Name, Type>,
    type_names: &HashMap<Name, Type>,
    pat: Pat,
    type_annotation: Option<TypeAnnotation>,
    expr: Expr,
) -> Result<TypedStmt, TypeError> {
    let typed_rhs = infer_expr(ctx, expr)?;
    let expected_type = if let Some(annotation) = type_annotation {
        lookup_type_annotation(type_names, annotation)
    } else {
        None
    };

    match (expected_type, pat) {
        (Some(expected_type), Pat::Id(name)) => {
            if unify(ctx, &expected_type, typed_rhs.get_type()) {
                ctx.insert(name.clone(), expected_type.clone());
                Ok(TypedStmt::Asgn(Pat::Id(name), typed_rhs))
            } else {
                Err(TypeError::UnificationFailure {
                    type1: expected_type,
                    type2: typed_rhs.get_type().clone(),
                })
            }
        }
        (None, Pat::Id(name)) => Ok(TypedStmt::Asgn(Pat::Id(name), typed_rhs)),
        _ => Err(TypeError::NotImplemented),
    }
}

pub fn infer_expr(ctx: &HashMap<Name, Type>, expr: Expr) -> Result<TypedExpr, TypeError> {
    match expr {
        Expr::Primary { value } => Ok(infer_value(value)),
        Expr::Var { name } => match ctx.get(&name) {
            Some(type_) => Ok(TypedExpr::Var {
                name,
                type_: type_.clone(),
            }),
            None => Err(TypeError::InferFailure {
                name: name.to_string(),
            }),
        },
        Expr::BinOp { op, lhs, rhs } => {
            let typed_lhs = infer_expr(&ctx, *lhs)?;
            let typed_rhs = infer_expr(&ctx, *rhs)?;
            let lhs_type = typed_lhs.get_type().clone();
            let rhs_type = typed_rhs.get_type().clone();
            match infer_op(ctx, &op, lhs_type, rhs_type) {
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
        _ => Err(TypeError::NotImplemented),
    }
}

pub fn infer_op(
    ctx: &HashMap<Name, Type>,
    op: &Op,
    lhs_type: Type,
    rhs_type: Type,
) -> Option<Type> {
    match op {
        Op::Comma => Some(Type::Tuple(Box::new(lhs_type), Box::new(rhs_type))),
        Op::Plus | Op::Minus | Op::Times | Op::Div => match (lhs_type, rhs_type) {
            (Type::Float, Type::Float) | (Type::Bool, Type::Float) | (Type::Float, Type::Bool) => {
                Some(Type::Float)
            }
            _ => None,
        },
        Op::BangEqual | Op::EqualEqual => {
            if unify(ctx, &lhs_type, &rhs_type) {
                Some(Type::Bool)
            } else {
                None
            }
        }
        Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
            // If we can unify lhs and rhs, and lhs with Float then
            // by transitivity we can unify everything with float
            if unify(ctx, &lhs_type, &rhs_type) && unify(ctx, &lhs_type, &Type::Float) {
                Some(Type::Bool)
            } else {
                None
            }
        }
    }
}

pub fn unify(ctx: &HashMap<Name, Type>, type1: &Type, type2: &Type) -> bool {
    if type1 == type2 {
        return true;
    }
    match (type1, type2) {
        (Type::Tuple(fst1, snd1), Type::Tuple(fst2, snd2)) => {
            unify(ctx, fst1, fst2) && unify(ctx, snd1, snd2)
        }
        (Type::Arrow(param_type1, return_type1), Type::Arrow(param_type2, return_type2)) => {
            unify(ctx, param_type1, param_type2) && unify(ctx, return_type1, return_type2)
        }
        (Type::Float, Type::Bool) | (Type::Bool, Type::Float) => true,
        (Type::Float, Type::Int) | (Type::Int, Type::Float) => true,
        _ => false,
    }
}
