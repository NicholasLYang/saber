#[macro_use]
extern crate failure;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use std::fs::File;
use std::io::Read;

mod ast;
mod lexer;

fn main() -> std::io::Result<()> {
    // let mut f = File::create("test.wat")?;
    // let add = Op {
    //     name: String::from("add"),
    //     _type: String::from("i32"),
    // };
    // let lhs = Node::Value(Value {
    //     val: 10,
    //     _type: String::from("i32"),
    // });
    // let rhs = Node::Value(Value {
    //     val: 10,
    //     _type: String::from("i32"),
    // });
    // let bin = BinOp {
    //     op: add,
    //     lhs: Box::new(lhs),
    //     rhs: Box::new(rhs),
    // };
    // let sexpr = bin_op(&bin);
    // f.write(&sexpr.into_bytes())?;
    let mut f = File::open("test.sbr")?;
    let mut source = String::new();
    f.read_to_string(&mut source);
    let lexer = lexer::Lexer::new(&source);
    let parser_out = parser::ExprParser::new().parse(lexer);

    println!("{:?}", parser_out);
    Ok(())
}

// fn constant(v: &Value) -> String {
//     format!("({}.constant {})", v._type, v.val)
// }

// fn node(n: &Node) -> String {
//     match n {
//         Node::Value(v) => constant(v),
//         Node::BinOp(b) => bin_op(b),
//     }
// }

// fn bin_op(bin: &BinOp) -> String {
//     let BinOp { op, lhs, rhs } = bin;
//     format!("({}.{} {} {})", op._type, op.name, node(lhs), node(rhs))
// }
