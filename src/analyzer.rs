
pub fn get_closure_from_block(block: &Vec<TypedStmt>) -> Vec<Closure> {
    let mut closures = Vec::new();
    for (stmt in body) {
        match stmt {
            TypedStmt::Asgn(pat, expr) => {

        }
    }
    }
}

fn get_closure_from_expr(expr: &TypedExpr, binding: &Pat) -> Vec<Closure> {
    match expr {
        TypedExpr::Function { params, return_type, body } => {
            
        }
    }
}


struct Closure {
    params: Pat,
    type_: Type,
    body: TypedExpr,
    binding: Option<Name>,
    env: HashMap<Name, Type>>
}
