use ast::{Expr, Op, Unary, Value};

pub fn gen_expr(expr: &Expr) -> String {
    match expr {
        Expr::Primary(v) => gen_value(v),
        Expr::BinOp(op, lhs, rhs) => {
            format!("({} {} {})", gen_expr(rhs), gen_expr(lhs), gen_op(op))
        }
        Expr::UnaryOp(unary, expr) => match unary {
            Unary::Bang => format!("(i32.xor {} -1)", gen_expr(expr)),
            Unary::Minus => format!("(i32.sub 0 {})", gen_expr(expr)),
        },
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
        Op::Div => "f32.div".to_string(),
        Op::Times => "f32.mul".to_string(),
        Op::BangEqual => "f32.ne".to_string(),
        Op::EqualEqual => "f32.eq".to_string(),
        Op::Greater => "f32.gt".to_string(),
        Op::GreaterEqual => "f32.ge".to_string(),
        Op::Less => "f32.lt".to_string(),
        Op::LessEqual => "f32.le".to_string(),
    }
}
