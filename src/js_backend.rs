use crate::ast;
use crate::ast::{BinaryOpT, Expr, ExprT, StmtT, UnaryOpT, Value};
use crate::mir::BinaryOp;
use crate::utils::NameTable;
use itertools::Itertools;
use walrus::ir::UnaryOp;

pub struct JsBackend {
    name_table: NameTable,
}

impl JsBackend {
    pub fn new(name_table: NameTable) -> JsBackend {
        Self { name_table }
    }

    pub fn generate_function(&mut self, function: &ast::Function) {
        let declaration = format!("function f{}", function.name);
        let params = format!(
            "({})",
            function
                .params
                .iter()
                .map(|(name, _)| format!("v{}", name))
                .join(", ")
        );

        let mut body = Vec::new();
        for stmt in function.body.iter() {
            body.push(self.generate_stmt(&stmt.inner))
        }

        println!(
            "{}{} {{{}}}",
            declaration,
            params,
            body.into_iter().join("\n")
        );
    }

    fn generate_stmt(&mut self, stmt: &ast::StmtT) -> String {
        match stmt {
            StmtT::Break => "break".to_string(),
            StmtT::Return(expr) => format!("return {}", self.generate_expr(&expr.inner)),
            StmtT::Expr(expr) => format!("{};", self.generate_expr(&expr.inner)),
            _ => todo!(),
        }
    }

    fn generate_expr(&mut self, expr: &ast::ExprT) -> String {
        match expr {
            ExprT::Primary { value, type_: _ } => self.generate_value(value),
            ExprT::BinOp {
                op,
                lhs,
                rhs,
                type_: _,
            } => {
                format!(
                    "({}) {} ({})",
                    self.generate_expr(&lhs.inner),
                    self.generate_op(op),
                    self.generate_expr(&rhs.inner)
                )
            }
            ExprT::UnaryOp { op, rhs, type_: _ } => match op {
                UnaryOpT::I32Minus => {
                    format!("({}[0] *= -1)", self.generate_expr(&rhs.inner))
                }
                UnaryOpT::F32Minus => {
                    format!("-({})", self.generate_expr(&rhs.inner))
                }
                UnaryOpT::BoolNot => {
                    format!("!({})", self.generate_expr(&rhs.inner))
                }
            },
            ExprT::Var { name, type_: _ } => {
                format!("v{}", name)
            }
            ExprT::Print { args, type_: _ } => {
                format!("console.log({})", self.generate_expr(&args.inner))
            }
            _ => todo!(),
        }
    }

    fn generate_op(&self, op: &BinaryOpT) -> &'static str {
        match op {
            BinaryOpT::I32Add | BinaryOpT::F32Add => "+",
            BinaryOpT::I32Sub | BinaryOpT::F32Sub => "-",
            BinaryOpT::I32Mul | BinaryOpT::F32Mul => "*",
            BinaryOpT::I32Div | BinaryOpT::F32Div => "/",
            BinaryOpT::I32NotEqual | BinaryOpT::F32NotEqual => "!=",
            BinaryOpT::I32Equal | BinaryOpT::F32Equal => "===",
            BinaryOpT::I32Greater | BinaryOpT::F32Greater => ">",
            BinaryOpT::I32GreaterEqual | BinaryOpT::F32GreaterEqual => ">=",
            BinaryOpT::I32Less | BinaryOpT::F32Less => "<",
            BinaryOpT::I32LessEqual | BinaryOpT::F32LessEqual => "<=",
            BinaryOpT::I32And => "&",
            BinaryOpT::I32Or => "|",
            BinaryOpT::BoolAnd => "&&",
            BinaryOpT::BoolOr => "||",
            BinaryOpT::StringEqual => "===",
            BinaryOpT::StringConcat => "+",
        }
    }

    fn generate_value(&mut self, value: &ast::Value) -> String {
        match value {
            Value::Float(f) => f.to_string(),
            Value::Bool(true) => "true".to_string(),
            Value::Bool(false) => "false".to_string(),
            // TODO: Output with escapes
            Value::String(s) => format!(
                "\"{}\"",
                s.chars()
                    .flat_map(|c| c.escape_default())
                    .collect::<String>()
            ),
            Value::Integer(i) => format!("new Int32Array([{}])", i),
        }
    }
}
