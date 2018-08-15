use std::fs::File;
use std::io::Write;

struct Value {
    val: i32,
    _type: String,
}

struct BinOp {
    op: Op,
    lhs: Box<Node>,
    rhs: Box<Node>,
}

enum Node {
    Value(Value),
    BinOp(BinOp),
}

struct Op {
    name: String,
    _type: String,
}

fn main() -> std::io::Result<()> {
    let mut f = File::create("test.wat")?;
    let add = Op {
        name: String::from("add"),
        _type: String::from("i32"),
    };
    let lhs = Node::Value(Value {
        val: 10,
        _type: String::from("i32"),
    });
    let rhs = Node::Value(Value {
        val: 10,
        _type: String::from("i32"),
    });
    let bin = BinOp {
        op: add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    };
    let sexpr = bin_op(&bin);
    f.write(&sexpr.into_bytes())?;
    Ok(())
}

fn constant(v: &Value) -> String {
    format!("({}.constant {})", v._type, v.val)
}

fn node(n: &Node) -> String {
    match n {
        Node::Value(v) => constant(v),
        Node::BinOp(b) => bin_op(b),
    }
}

fn bin_op(bin: &BinOp) -> String {
    let BinOp { op, lhs, rhs } = bin;
    format!("({}.{} {} {})", op._type, op.name, node(lhs), node(rhs))
}
