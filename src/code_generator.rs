use ast::{Asgn, Expr, Name, Op, Stmt, Unary, Value};
use std::iter::Iterator;

pub fn gen_program(program: &mut Vec<Stmt>) -> String {
    let mut out = String::new();
    for stmt in program {
        let code = match stmt {
            Stmt::Var(asgn, expr) => gen_global_var(asgn, expr),
            _ => "".to_string(),
        };
        out = format!("{}{}", out, code);
    }
    out
}

fn gen_global_var(asgn: &Asgn, expr: &mut Expr) -> String {
    match (expr, asgn) {
        (Expr::Function(params, body), Asgn::Single(name)) => gen_function(params, body, name),
        _ => "".to_string(),
    }
}

fn gen_function(params: &Vec<Name>, body: &mut Vec<Stmt>, name: &Name) -> String {
    format!("(func {} (result f32) {})", name, gen_body(body))
}

fn gen_body_stmt(stmt: &mut Stmt) -> String {
    match stmt {
        Stmt::Var(Asgn::Single(name), expr) => format!("(local {} f32)", name),
        Stmt::Expr(expr) => gen_expr(expr),
        Stmt::Return(expr) => format!("(return {})", gen_expr(expr)),
        Stmt::If(cond, then, opt_else) => {
            if let Some(els) = opt_else {
                format!(
                    "{} (if (then {})(else {})",
                    gen_expr(cond),
                    gen_body(then),
                    gen_body(els)
                )
            } else {
                format!("{} (if (then {}))", gen_expr(cond), gen_body(then))
            }
        }
    }
}

fn gen_body(stmts: &mut Vec<Stmt>) -> String {
    let mut acc = String::new();
    for stmt in stmts {
        acc = format!("{}{}", acc, gen_body_stmt(stmt));
    }
    acc
}

fn gen_expr(expr: &Expr) -> String {
    match expr {
        Expr::Primary(v) => gen_value(v),
        Expr::BinOp(op, lhs, rhs) => format!("{} {} {}", gen_expr(lhs), gen_expr(rhs), gen_op(op)),
        Expr::UnaryOp(unary, expr) => match unary {
            Unary::Bang => format!("i32.xor {} -1", gen_expr(expr)),
            Unary::Minus => format!("i32.sub 0 {}", gen_expr(expr)),
        },
        _ => "".to_string(),
    }
}

fn gen_value(val: &Value) -> String {
    match val {
        Value::Num(n) => format!("f32.const {}", n),
        Value::Bool(b) => format!("i32.const {}", *b as i32),
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
