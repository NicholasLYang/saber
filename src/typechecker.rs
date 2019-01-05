use ast::Value;
use ast::{Expr, Name, Op, Type, TypedExpr};
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
}

pub fn infer_value(value: Value) -> TypedExpr {
    match value {
        Value::Num(_n) => TypedExpr::Primary {
            value,
            _type: Type::Float,
        },
        Value::Bool(_b) => TypedExpr::Primary {
            value,
            _type: Type::Bool,
        },
    }
}

pub fn infer_expr(ctx: HashMap<Name, Type>, expr: Expr) -> Result<TypedExpr, TypeError> {
    match expr {
        Expr::Primary { value } => Ok(infer_value(value)),
        Expr::Var { name } => match ctx.get(&name) {
            Some(_type) => Ok(TypedExpr::Var {
                name,
                _type: *_type,
            }),
            None => Err(TypeError::InferFailure {
                name: name.to_string(),
            }),
        },
        Expr::BinOp { op, lhs, rhs } => {
            let typed_lhs = infer_expr(ctx, *lhs)?;
            let typed_rhs = infer_expr(ctx, *rhs)?;
            match infer_op(&op, &typed_lhs, &typed_rhs) {
                Some(typedOp) => Ok(typedOp),
                None => Err(TypeError::OpFailure {
                    op,
                    lhs_type: typed_lhs.get_type().clone(),
                    rhs_type: typed_rhs.get_type().clone(),
                }),
            }
        }
        _ => Err(TypeError::NotImplemented),
    }
}

pub fn infer_op(op: &Op, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<TypedExpr> {
    None
}
