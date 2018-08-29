pub struct Value {
    pub val: i32,
    pub _type: String,
}

pub struct BinOp {
    pub op: Op,
    pub lhs: Box<Node>,
    pub rhs: Box<Node>,
}

pub enum Node {
    Value(Value),
    BinOp(BinOp),
}

pub struct Op {
    pub name: String,
    pub _type: String,
}
