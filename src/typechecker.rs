use ast::Value;
use ast::{Expr, Name, Op, Pat, Scope, Stmt, Type, TypeSig, TypedExpr, TypedStmt};
use im::hashmap::HashMap;
use std::sync::Arc;
use utils::SymbolTable;

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "Could not determine type of '{}'", name)]
    InferFailure { name: String },
    #[fail(display = "Not implemented yet")]
    NotImplemented,
    #[fail(
        display = "Could not find operation {} with arguments of type {} and {}",
        op, lhs_type, rhs_type
    )]
    OpFailure {
        op: Op,
        lhs_type: Arc<Type>,
        rhs_type: Arc<Type>,
    },
    #[fail(display = "Could not unify {} with {}", type1, type2)]
    UnificationFailure { type1: Arc<Type>, type2: Arc<Type> },
    #[fail(display = "Type {} does not exit", type_name)]
    TypeDoesNotExist { type_name: Name },
    #[fail(display = "Arity mismatch: Expected {} but got {}", arity1, arity2)]
    ArityMismatch { arity1: usize, arity2: usize },
    //    #[fail(display = "Record contains non indentifier patterns: {:?}", record)]
    //    RecordContainsNonIds { record: Pat },
    #[fail(display = "Field {} does not exist in record", name)]
    FieldDoesNotExist { name: String },
    #[fail(display = "Type {} is not a record", type_)]
    NotARecord { type_: Arc<Type> },
    #[fail(display = "Invalid unary operator: {}", op)]
    InvalidUnaryOp { op: Op },
    #[fail(display = "Cannot apply unary operator to {:?}", expr)]
    InvalidUnaryExpr { expr: TypedExpr },
    #[fail(display = "Callee is not a function")]
    CalleeNotFunction,
}

pub struct TypeChecker {
    // A symbol table of sorts
    scopes: Vec<Scope>,
    current_scope: usize,
    // Type names. Right now just has the primitives like string,
    // integer, float, char
    type_names: HashMap<Name, Arc<Type>>,
    // The return type for the typing context
    return_type: Option<Arc<Type>>,
    // Index for type variable names
    type_var_index: usize,
    // Symbol table
    symbol_table: SymbolTable,
}

fn build_type_names(symbol_table: &mut SymbolTable) -> HashMap<Name, Arc<Type>> {
    let primitive_types = vec![
        ("int", Arc::new(Type::Int)),
        ("float", Arc::new(Type::Float)),
        ("char", Arc::new(Type::Char)),
        ("string", Arc::new(Type::String)),
    ];
    let mut type_names = HashMap::new();
    for (name, type_) in primitive_types {
        let id = symbol_table.insert(name.to_string());
        type_names.insert(id, type_);
    }
    type_names
}

impl TypeChecker {
    pub fn new(mut symbol_table: SymbolTable) -> TypeChecker {
        let scopes = vec![Scope {
            names: HashMap::new(),
            parent: None,
        }];
        TypeChecker {
            scopes,
            current_scope: 0,
            type_names: build_type_names(&mut symbol_table),
            return_type: None,
            type_var_index: 0,
            symbol_table,
        }
    }

    pub fn get_symbol_table(self) -> SymbolTable {
        self.symbol_table
    }

    fn get_fresh_type_var(&mut self) -> Arc<Type> {
        let type_var = Type::Var(self.type_var_index);
        self.type_var_index += 1;
        Arc::new(type_var)
    }

    fn lookup_var(&self, name: &usize) -> Option<Arc<Type>> {
        let mut index = Some(self.current_scope);
        while let Some(i) = index {
            if let Some(type_) = self.scopes[i].names.get(name) {
                return Some(type_.clone());
            }
            index = self.scopes[i].parent;
        }
        None
    }

    fn insert_var(&mut self, name: usize, type_: Arc<Type>) {
        let current_scope = self.current_scope;
        self.scopes[current_scope].names.insert(name, type_);
    }

    pub fn check_program(&mut self, program: Vec<Stmt>) -> Result<Vec<TypedStmt>, TypeError> {
        let mut typed_stmts = Vec::new();
        for stmt in program {
            typed_stmts.push(self.stmt(stmt)?);
        }
        Ok(typed_stmts.into_iter().flatten().collect())
    }

    pub fn stmt(&mut self, stmt: Stmt) -> Result<Vec<TypedStmt>, TypeError> {
        match stmt {
            Stmt::Expr(expr) => {
                let typed_expr = self.expr(expr)?;
                Ok(vec![TypedStmt::Expr(typed_expr)])
            }
            Stmt::Function(func_name, params, return_type_sig, body) => {
                let params_type = self.pat(&params)?;
                let return_type = if let Some(type_sig) = &return_type_sig {
                    self.lookup_type_sig(type_sig)?
                } else {
                    self.get_fresh_type_var()
                };
                let func_scope = {
                    let mut names = HashMap::new();
                    names.insert(
                        func_name,
                        Arc::new(Type::Arrow(params_type.clone(), return_type)),
                    );
                    Scope {
                        names,
                        parent: Some(self.current_scope),
                    }
                };
                self.scopes.push(func_scope);
                let previous_scope = self.current_scope;
                self.current_scope = self.scopes.len() - 1;
                let rhs = self.func(params, body, return_type_sig, params_type)?;
                self.current_scope = previous_scope;
                Ok(vec![TypedStmt::Asgn(func_name, rhs)])
            }
            Stmt::Asgn(pat, rhs) => Ok(self.asgn(pat, rhs)?),
            Stmt::If(cond, then_stmt, else_stmt) => {
                let typed_cond = self.expr(cond)?;
                if !self.unify(&typed_cond.get_type(), &Arc::new(Type::Bool)) {
                    return Err(TypeError::UnificationFailure {
                        type1: typed_cond.get_type(),
                        type2: Arc::new(Type::Bool),
                    });
                }
                let typed_then = self.stmt(*then_stmt)?;
                let typed_else = match else_stmt {
                    Some(else_stmt) => Some(Box::new(TypedStmt::Block(self.stmt(*else_stmt)?))),
                    None => None,
                };
                Ok(vec![TypedStmt::If(
                    typed_cond,
                    Box::new(TypedStmt::Block(typed_then)),
                    typed_else,
                )])
            }
            Stmt::Return(expr) => {
                let typed_exp = self.expr(expr)?;
                match self.return_type.clone() {
                    Some(ref return_type) => {
                        if self.unify(&typed_exp.get_type(), return_type) {
                            Ok(vec![TypedStmt::Return(typed_exp)])
                        } else {
                            Err(TypeError::UnificationFailure {
                                type1: typed_exp.get_type().clone(),
                                type2: return_type.clone(),
                            })
                        }
                    }
                    None => {
                        self.return_type = Some(typed_exp.get_type().clone());
                        Ok(vec![TypedStmt::Return(typed_exp)])
                    }
                }
            }
            Stmt::Block(stmts) => {
                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    typed_stmts.push(self.stmt(stmt)?);
                }
                Ok(vec![TypedStmt::Block(
                    typed_stmts.into_iter().flatten().collect(),
                )])
            }
            Stmt::Export(name) => {
                if let None = self.lookup_var(&name) {
                    let type_ = self.get_fresh_type_var();
                    self.insert_var(name.clone(), type_);
                }
                Ok(vec![TypedStmt::Export(name)])
            }
        }
    }

    fn value(&self, value: Value) -> TypedExpr {
        match value {
            Value::Integer(_i) => TypedExpr::Primary {
                value,
                type_: Arc::new(Type::Int),
            },
            Value::Float(_f) => TypedExpr::Primary {
                value,
                type_: Arc::new(Type::Float),
            },
            Value::Bool(_b) => TypedExpr::Primary {
                value,
                type_: Arc::new(Type::Bool),
            },
            Value::String(s) => TypedExpr::Primary {
                value: Value::String(s),
                type_: Arc::new(Type::String),
            },
        }
    }

    fn lookup_type_sig(&self, sig: &TypeSig) -> Result<Arc<Type>, TypeError> {
        match sig {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(Arc::new(Type::Array(type_)))
            }
            TypeSig::Name(name) => {
                if let Some(type_) = self.type_names.get(name) {
                    Ok(type_.clone())
                } else {
                    Err(TypeError::TypeDoesNotExist {
                        type_name: name.clone(),
                    })
                }
            }
            TypeSig::Record(entries) => {
                let mut field_types = Vec::new();
                for (name, type_sig) in entries {
                    let type_ = self.lookup_type_sig(type_sig)?;
                    field_types.push((name.clone(), type_));
                }
                Ok(Arc::new(Type::Record(field_types)))
            }
            TypeSig::Tuple(entries) => {
                let mut types = Vec::new();
                for type_sig in entries {
                    let type_ = self.lookup_type_sig(type_sig)?;
                    types.push(type_);
                }
                Ok(Arc::new(Type::Tuple(types)))
            }
        }
    }

    fn pat(&mut self, pat: &Pat) -> Result<Arc<Type>, TypeError> {
        match pat {
            Pat::Id(_, Some(type_sig)) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                Ok(type_)
            }
            Pat::Id(name, None) => {
                let type_ = self.get_fresh_type_var();
                self.insert_var(*name, type_.clone());
                Ok(type_)
            }
            Pat::Tuple(pats) => {
                let types: Result<Vec<_>, _> = pats.iter().map(|pat| self.pat(pat)).collect();
                Ok(Arc::new(Type::Tuple(types?)))
            }
            Pat::Record(pats, type_sig) => {
                // In the future I should unify these two if they both exist.
                if let Some(type_sig) = type_sig {
                    self.lookup_type_sig(&type_sig)
                } else {
                    let types: Result<Vec<_>, _> = pats
                        .iter()
                        .map(|name| Ok((name.clone(), self.get_fresh_type_var())))
                        .collect();
                    Ok(Arc::new(Type::Record(types?)))
                }
            }
            Pat::Empty => Ok(Arc::new(Type::Unit)),
        }
    }

    /// Gets the function parameters as a list of names with
    /// types. Useful for WebAssembly code generation.
    fn get_func_params(&mut self, pat: &Pat) -> Result<Vec<(Name, Arc<Type>)>, TypeError> {
        match pat {
            Pat::Id(name, Some(type_sig)) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                Ok(vec![(*name, type_)])
            }
            Pat::Id(name, None) => {
                let type_ = self.get_fresh_type_var();
                self.insert_var(*name, type_.clone());
                Ok(vec![(*name, type_)])
            }
            Pat::Tuple(pats) => {
                let mut types = Vec::new();
                for pat in pats {
                    types.append(&mut self.get_func_params(pat)?)
                }
                Ok(types)
            }
            Pat::Record(names, type_sig) => {
                let type_ = if let Some(type_sig) = type_sig {
                    Some(self.lookup_type_sig(&type_sig)?)
                } else {
                    None
                };
                let mut params = Vec::new();
                for name in names {
                    let field_type = if let Some(t) = &type_ {
                        self.get_field_type(t, name)?
                    } else {
                        self.get_fresh_type_var()
                    };
                    params.push((name.clone(), field_type))
                }
                Ok(params)
            }
            Pat::Empty => Ok(Vec::new()),
        }
    }

    fn get_field_type(
        &mut self,
        record_type: &Arc<Type>,
        field_name: &usize,
    ) -> Result<Arc<Type>, TypeError> {
        if let Type::Record(fields) = &**record_type {
            if let Some((_, type_)) = fields.iter().find(|(name, _)| name == field_name) {
                Ok(type_.clone())
            } else {
                Err(TypeError::FieldDoesNotExist {
                    name: self.symbol_table.get_str(field_name).to_string(),
                })
            }
        } else {
            Err(TypeError::NotARecord {
                type_: record_type.clone(),
            })
        }
    }

    fn generate_pattern_bindings(
        &mut self,
        pat: &Pat,
        owner_name: &usize,
        rhs_type: &Arc<Type>,
    ) -> Result<Vec<TypedStmt>, TypeError> {
        let mut bindings = Vec::new();
        match pat {
            Pat::Id(_, _) | Pat::Empty => Ok(Vec::new()),
            Pat::Record(names, _) => {
                for name in names {
                    bindings.push(TypedStmt::Asgn(
                        name.clone(),
                        TypedExpr::Field(
                            Box::new(TypedExpr::Var {
                                name: owner_name.clone(),
                                type_: rhs_type.clone(),
                            }),
                            name.clone(),
                            self.get_field_type(rhs_type, &name)?,
                        ),
                    ));
                }
                Ok(bindings)
            }
            Pat::Tuple(pats) => {
                // TODO: Make this recursive so that we can
                // flatten bindings
                for (i, pat) in pats.iter().enumerate() {
                    let name = self.symbol_table.get_fresh_name();
                    bindings.push(TypedStmt::Asgn(
                        name.clone(),
                        TypedExpr::Field(
                            Box::new(TypedExpr::Var {
                                name: owner_name.clone(),
                                type_: rhs_type.clone(),
                            }),
                            i,
                            self.get_fresh_type_var(),
                        ),
                    ));
                    let type_ = self.get_fresh_type_var();
                    bindings.append(&mut self.generate_pattern_bindings(pat, &name, &type_)?)
                }
                Ok(bindings)
            }
        }
    }

    fn asgn(&mut self, pat: Pat, rhs: Expr) -> Result<Vec<TypedStmt>, TypeError> {
        let pat_type = self.pat(&pat)?;
        let typed_rhs = self.expr(rhs)?;
        if self.unify(&pat_type, &typed_rhs.get_type()) {
            let name = if let Pat::Id(name, _) = pat {
                name
            } else {
                self.symbol_table.get_fresh_name()
            };
            self.insert_var(name, typed_rhs.get_type());
            let mut pat_bindings =
                self.generate_pattern_bindings(&pat, &name, &typed_rhs.get_type())?;
            let mut bindings = vec![TypedStmt::Asgn(name.clone(), typed_rhs)];
            bindings.append(&mut pat_bindings);
            Ok(bindings)
        } else {
            Err(TypeError::UnificationFailure {
                type1: pat_type,
                type2: typed_rhs.get_type(),
            })
        }
    }

    fn func(
        &mut self,
        params: Pat,
        body: Box<Stmt>,
        return_type: Option<TypeSig>,
        param_type: Arc<Type>,
    ) -> Result<TypedExpr, TypeError> {
        let func_params = self.get_func_params(&params)?;
        for (name, type_) in &func_params {
            self.insert_var(name.clone(), type_.clone());
        }
        // Insert return type into typechecker so that
        // typechecker can verify return statements.
        if let Some(return_type_sig) = return_type {
            self.return_type = Some(self.lookup_type_sig(&return_type_sig)?);
        }
        // Check body
        let body = self.stmt(*body)?;
        let mut return_type = None;
        std::mem::swap(&mut return_type, &mut self.return_type);
        let return_type = return_type.unwrap_or(Arc::new(Type::Unit));
        Ok(TypedExpr::Function {
            params: func_params,
            body: Box::new(TypedStmt::Block(body)),
            param_type: param_type.clone(),
            return_type,
            scope_index: self.current_scope,
        })
    }

    fn expr(&mut self, expr: Expr) -> Result<TypedExpr, TypeError> {
        match expr {
            Expr::Primary { value } => Ok(self.value(value)),
            Expr::Var { name } => match self.lookup_var(&name) {
                Some(type_) => Ok(TypedExpr::Var {
                    name,
                    type_: type_.clone(),
                }),
                None => Err(TypeError::InferFailure {
                    name: self.symbol_table.get_str(&name).to_string(),
                }),
            },
            Expr::BinOp { op, lhs, rhs } => {
                let typed_lhs = self.expr(*lhs)?;
                let typed_rhs = self.expr(*rhs)?;
                let lhs_type = typed_lhs.get_type();
                let rhs_type = typed_rhs.get_type();
                match self.op(&op, lhs_type, rhs_type) {
                    Some(op_type) => Ok(TypedExpr::BinOp {
                        op,
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                        type_: Arc::new(op_type),
                    }),
                    None => Err(TypeError::OpFailure {
                        op: op.clone(),
                        lhs_type: typed_lhs.get_type(),
                        rhs_type: typed_rhs.get_type(),
                    }),
                }
            }
            Expr::Tuple(elems) => {
                let mut typed_elems = Vec::new();
                let mut types = Vec::new();
                for elem in elems {
                    let typed_elem = self.expr(elem)?;
                    types.push(typed_elem.get_type());
                    typed_elems.push(typed_elem);
                }
                Ok(TypedExpr::Tuple(typed_elems, Arc::new(Type::Tuple(types))))
            }
            Expr::Function {
                params,
                body,
                return_type,
            } => {
                let func_scope = Scope {
                    names: HashMap::new(),
                    parent: Some(self.current_scope),
                };
                let params_type = self.pat(&params)?;
                self.scopes.push(func_scope);
                let previous_scope = self.current_scope;
                self.current_scope = self.scopes.len() - 1;
                let func = self.func(params, body, return_type, params_type);
                self.current_scope = previous_scope;
                func
            }
            Expr::UnaryOp { op, rhs } => match op {
                Op::Minus | Op::Plus => {
                    let typed_rhs = self.expr(*rhs)?;
                    if self.unify(&typed_rhs.get_type(), &Arc::new(Type::Int))
                        || self.unify(&typed_rhs.get_type(), &Arc::new(Type::Float))
                    {
                        let type_ = typed_rhs.get_type();
                        Ok(TypedExpr::UnaryOp {
                            op,
                            rhs: Box::new(typed_rhs),
                            type_,
                        })
                    } else {
                        Err(TypeError::InvalidUnaryExpr {
                            expr: typed_rhs.clone(),
                        })
                    }
                }
                op => Err(TypeError::InvalidUnaryOp { op }),
            },
            Expr::Call { callee, args } => {
                let typed_callee = self.expr(*callee)?;
                let callee_type = typed_callee.get_type();
                let (params_type, return_type) = match &(*callee_type) {
                    Type::Arrow(params_type, return_type) => {
                        (params_type.clone(), return_type.clone())
                    }
                    Type::Var(_) => (self.get_fresh_type_var(), self.get_fresh_type_var()),
                    _ => return Err(TypeError::CalleeNotFunction),
                };
                let typed_args = self.expr(*args)?;
                let args_type = typed_args.get_type();
                if self.unify(&params_type, &args_type) {
                    Ok(TypedExpr::Call {
                        callee: Box::new(typed_callee),
                        args: Box::new(typed_args),
                        type_: return_type,
                    })
                } else {
                    Err(TypeError::UnificationFailure {
                        type1: params_type,
                        type2: args_type.clone(),
                    })
                }
            }
            _ => Err(TypeError::NotImplemented),
        }
    }

    fn op(&mut self, op: &Op, lhs_type: Arc<Type>, rhs_type: Arc<Type>) -> Option<Type> {
        match op {
            Op::Plus | Op::Minus | Op::Times | Op::Div => match (&*lhs_type, &*rhs_type) {
                (Type::Float, Type::Float)
                | (Type::Int, Type::Float)
                | (Type::Float, Type::Int) => Some(Type::Float),
                (Type::Int, Type::Int) => Some(Type::Int),
                _ => None,
            },
            Op::BangEqual | Op::EqualEqual => {
                if self.unify(&lhs_type, &rhs_type) {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
                // If we can unify lhs and rhs, and lhs with Int or Float then
                // by transitivity we can unify everything with float
                let is_num = self.unify(&lhs_type, &Arc::new(Type::Float))
                    || self.unify(&lhs_type, &Arc::new(Type::Int));
                if self.unify(&lhs_type, &rhs_type) && is_num {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
        }
    }

    fn unify_type_vectors(
        &mut self,
        type_vector1: &Vec<Arc<Type>>,
        type_vector2: &Vec<Arc<Type>>,
    ) -> bool {
        if type_vector1.len() != type_vector2.len() {
            return false;
        }
        for (t1, t2) in type_vector1.iter().zip(type_vector2.iter()) {
            if !self.unify(t1, t2) {
                return false;
            }
        }
        return true;
    }

    fn unify(&mut self, type1: &Arc<Type>, type2: &Arc<Type>) -> bool {
        if Arc::ptr_eq(type1, type2) || type1 == type2 {
            return true;
        }
        match (&**type1, &**type2) {
            (Type::Tuple(ts), Type::Unit) | (Type::Unit, Type::Tuple(ts)) => ts.len() == 0,
            (Type::Tuple(t1), Type::Tuple(t2)) => self.unify_type_vectors(&t1, &t2),
            (Type::Arrow(param_type1, return_type1), Type::Arrow(param_type2, return_type2)) => {
                self.unify(&param_type1, &param_type2) && self.unify(&return_type1, &return_type2)
            }
            (Type::Int, Type::Bool) | (Type::Bool, Type::Int) => true,
            // TODO: We need a way to look up usages of type vars and
            // convert them to the other type with unification
            (Type::Var(_), _) | (_, Type::Var(_)) => true,
            _ => false,
        }
    }
}
