use ast::{Expr, Literal, Name, Type, TypedExpr};
use std::collections::HashMap;

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "Could not determine type of '{}'", name)]
    InferFailure { name: Name },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
}

pub fn infer_expr(ctx: HashMap<Name, Type>, expr: Expr) -> Result<TypedExpr, TypeError> {
    match expr {
        Expr::Primary(val) => Ok(TypedExpr {
            expr: Expr::Primary(val),
            _type: Type::Literal(Literal::Float),
        }),
        Expr::Var(name) => match ctx.get(&name) {
            Some(t) => Ok(TypedExpr {
                expr: Expr::Var(name),
                _type: t.clone(),
            }),
            None => Err(TypeError::InferFailure {
                name: name.to_string(),
            }),
        },
        _ => Err(TypeError::NotImplemented),
    }
}
