use ast::Primitive::*;
use ast::{Expr, Name, Op, Type, TypedExpr, TypedOp};
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

//////////////////////////////////////////////////////////////////////////////////////////////////////
// // Very dumb, just does it manually                                                              //
// pub fn infer_op(op: &Op, lhs_type: &Type, rhs_type: &Type) -> Option<TypedOp> {                  //
//     match (lhs_type, rhs_type) {                                                                 //
//         (Type::Primitive(lhs), Type::Primitive(rhs)) => match (lhs, rhs) {                       //
//             (Float, Float) => match &op {                                                        //
//                 op @ Op::Plus | op @ Op::Minus | op @ Op::Times | op @ Op::Div => Some(TypedOp { //
//                     op: **op,                                                                    //
//                     _type: Type::Arrow(                                                          //
//                         vec![Type::Primitive(Float), Type::Primitive(Float)],                    //
//                         Box::new(Type::Primitive(Float)),                                        //
//                     ),                                                                           //
//                 }),                                                                              //
//                 Op::BangEqual                                                                    //
//                 | Op::EqualEqual                                                                 //
//                 | Op::Greater                                                                    //
//                 | Op::GreaterEqual                                                               //
//                 | Op::Less                                                                       //
//                 | Op::LessEqual => Some(TypedOp {                                                //
//                     op: *op,                                                                     //
//                     _type: Type::Arrow(                                                          //
//                         vec![Type::Primitive(Float), Type::Primitive(Float)],                    //
//                         Box::new(Type::Primitive(Bool)),                                         //
//                     ),                                                                           //
//                 }),                                                                              //
//             },                                                                                   //
//             _ => None,                                                                           //
//         },                                                                                       //
//         _ => None,                                                                               //
//     }                                                                                            //
// }                                                                                                //
//////////////////////////////////////////////////////////////////////////////////////////////////////

pub fn infer_expr(ctx: HashMap<Name, Type>, expr: Expr) -> Result<TypedExpr, TypeError> {
    match expr {
        Expr::Primary(val) => Ok(TypedExpr {
            expr: Expr::Primary(val),
            _type: Type::Primitive(Float),
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
        // Expr::BinOp(op, lhs, rhs) => {
        //     let typed_lhs = infer_expr(ctx, *lhs)?;
        //     let typed_rhs = infer_expr(ctx, *rhs)?;
        //     match infer_op(&op, &typed_lhs._type, &typed_rhs._type) {
        //         Some(TypedOp { op, _type }) => Ok(TypedExpr {
        //             expr: Expr::BinOp(op, Box::new(typed_lhs.expr), Box::new(typed_rhs.expr)),
        //             _type,
        //         }),
        //         None => Err(TypeError::OpFailure {
        //             op,
        //             lhs_type: typed_lhs._type.clone(),
        //             rhs_type: typed_rhs._type.clone(),
        //         }),
        //     }
        // }
        _ => Err(TypeError::NotImplemented),
    }
}
