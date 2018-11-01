use ast::{Expr, Op, Value};

pub fn gen_expr(expr: &Expr) -> String {
    match expr {
        Expr::Primary(v) => gen_value(v),
        Expr::BinOp(op, lhs, rhs) => {
            format!("({} {} {})", gen_expr(rhs), gen_expr(lhs), gen_op(op))
        }
        _ => "".to_string(),
    }
}

fn gen_value(val: &Value) -> String {
    match val {
        Value::Num(n) => format!("(f32.const {})", n),
        Value::Bool(b) => format!("(i32.const {})", *b as i32),
    }
}

fn gen_op(op: &Op) -> String {
    match op {
        Op::Plus => "f32.add".to_string(),
        Op::Minus => "f32.sub".to_string(),
        _ => "Not implemented!".to_string(),
    }
}
