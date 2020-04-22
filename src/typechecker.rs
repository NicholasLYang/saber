use ast::{Expr, ExprT, Loc, Name, Op, Pat, Stmt, StmtT, Type, TypeSig, UnaryOp, Value};
use im::hashmap::HashMap;
use lexer::LocationRange;
use std::sync::Arc;
use symbol_table::{EntryType, SymbolTable};
use utils::NameTable;

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "{}: Variable not defined: '{}'", location, name)]
    VarNotDefined {
        location: LocationRange,
        name: String,
    },
    #[fail(display = "{}: Not implemented yet", location)]
    NotImplemented { location: LocationRange },
    #[fail(
        display = "{}: Could not find operation {} with arguments of type {} and {}",
        location, op, lhs_type, rhs_type
    )]
    OpFailure {
        location: LocationRange,
        op: Op,
        lhs_type: Arc<Type>,
        rhs_type: Arc<Type>,
    },
    #[fail(display = "{}: Could not unify {} with {}", location, type1, type2)]
    UnificationFailure {
        location: LocationRange,
        type1: Arc<Type>,
        type2: Arc<Type>,
    },
    #[fail(display = "{}: Type {} does not exist", location, type_name)]
    TypeDoesNotExist {
        location: LocationRange,
        type_name: String,
    },
    #[fail(display = "Field {} does not exist in record", name)]
    FieldDoesNotExist { name: String },
    #[fail(display = "Type {} is not a record", type_)]
    NotARecord { type_: Arc<Type> },
    #[fail(display = "Invalid unary operator: {}", op)]
    InvalidUnaryOp { op: Op },
    #[fail(display = "{} Cannot apply unary operator to {:?}", location, expr)]
    InvalidUnaryExpr {
        location: LocationRange,
        expr: ExprT,
    },
    #[fail(display = "Callee is not a function")]
    CalleeNotFunction,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub names: HashMap<Name, Arc<Type>>,
    pub parent: Option<usize>,
}

pub struct TypeChecker {
    symbol_table: SymbolTable,
    // Type names. Right now just has the primitives like string,
    // integer, float, char
    type_names: HashMap<Name, Arc<Type>>,
    // The return type for the typing context
    return_type: Option<Arc<Type>>,
    // Index for type variable names
    type_var_index: usize,
    // Symbol table
    name_table: NameTable,
}

fn build_type_names(name_table: &mut NameTable) -> HashMap<Name, Arc<Type>> {
    let primitive_types = vec![
        ("int", Arc::new(Type::Int)),
        ("float", Arc::new(Type::Float)),
        ("char", Arc::new(Type::Char)),
        ("string", Arc::new(Type::String)),
        ("bool", Arc::new(Type::Bool)),
    ];
    let mut type_names = HashMap::new();
    for (name, type_) in primitive_types {
        let id = name_table.insert(name.to_string());
        type_names.insert(id, type_);
    }
    type_names
}

impl TypeChecker {
    pub fn new(mut name_table: NameTable) -> TypeChecker {
        let mut symbol_table = SymbolTable::new();
        let print_id = name_table.insert("print".into());
        symbol_table.insert_function(print_id, Arc::new(Type::Int), Arc::new(Type::Unit));
        TypeChecker {
            symbol_table,
            type_names: build_type_names(&mut name_table),
            return_type: None,
            type_var_index: 0,
            name_table,
        }
    }

    pub fn get_tables(self) -> (NameTable, SymbolTable) {
        (self.name_table, self.symbol_table)
    }

    fn get_fresh_type_var(&mut self) -> Arc<Type> {
        let type_var = Type::Var(self.type_var_index);
        self.type_var_index += 1;
        Arc::new(type_var)
    }

    pub fn check_program(&mut self, program: Vec<Loc<Stmt>>) -> Result<Vec<Loc<StmtT>>, TypeError> {
        let mut typed_stmts = Vec::new();
        for stmt in program {
            typed_stmts.push(self.stmt(stmt)?);
        }
        Ok(typed_stmts.into_iter().flatten().collect())
    }

    pub fn stmt(&mut self, stmt: Loc<Stmt>) -> Result<Vec<Loc<StmtT>>, TypeError> {
        let location = stmt.location;
        match stmt.inner {
            Stmt::Expr(expr) => {
                let typed_expr = self.expr(expr)?;
                Ok(vec![Loc {
                    location,
                    inner: StmtT::Expr(typed_expr),
                }])
            }
            Stmt::Function(func_name, params, return_type_sig, body) => {
                let params_type = self.pat(&params)?;
                let return_type = if let Some(type_sig) = &return_type_sig {
                    self.lookup_type_sig(type_sig)?
                } else {
                    self.get_fresh_type_var()
                };
                self.symbol_table
                    .insert_function(func_name, params_type.clone(), return_type);
                let previous_scope = self.symbol_table.push_scope(true);
                let (params, body, return_type, local_variables) =
                    self.func(params, *body, return_type_sig)?;
                let func_scope = self.symbol_table.restore_scope(previous_scope);
                self.symbol_table.insert_function(
                    func_name,
                    params_type.clone(),
                    return_type.clone(),
                );
                Ok(vec![Loc {
                    location,
                    inner: StmtT::Function {
                        name: func_name,
                        params,
                        params_type,
                        return_type,
                        body,
                        local_variables,
                        scope: func_scope,
                    },
                }])
            }
            Stmt::Asgn(pat, rhs) => Ok(self.asgn(pat, rhs, location)?),
            Stmt::Return(expr) => {
                let typed_expr = self.expr(expr)?;
                match self.return_type.clone() {
                    Some(ref return_type) => {
                        if self.unify(&typed_expr.inner.get_type(), return_type) {
                            Ok(vec![Loc {
                                location,
                                inner: StmtT::Return(typed_expr),
                            }])
                        } else {
                            Err(TypeError::UnificationFailure {
                                location,
                                type1: typed_expr.inner.get_type(),
                                type2: return_type.clone(),
                            })
                        }
                    }
                    None => {
                        self.return_type = Some(typed_expr.inner.get_type());
                        Ok(vec![Loc {
                            location,
                            inner: StmtT::Return(typed_expr),
                        }])
                    }
                }
            }
            Stmt::Block(stmts) => {
                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    typed_stmts.push(self.stmt(stmt)?);
                }
                Ok(vec![Loc {
                    location,
                    inner: StmtT::Block(typed_stmts.into_iter().flatten().collect()),
                }])
            }
            Stmt::Export(name) => {
                self.symbol_table
                    .lookup_name(name)
                    .ok_or(TypeError::VarNotDefined {
                        location,
                        name: self.name_table.get_str(&name).to_string(),
                    })?;
                Ok(vec![Loc {
                    location,
                    inner: StmtT::Export(name),
                }])
            }
        }
    }

    fn value(&self, value: Value) -> ExprT {
        match value {
            Value::Integer(_i) => ExprT::Primary {
                value,
                type_: Arc::new(Type::Int),
            },
            Value::Float(_f) => ExprT::Primary {
                value,
                type_: Arc::new(Type::Float),
            },
            Value::Bool(_b) => ExprT::Primary {
                value,
                type_: Arc::new(Type::Bool),
            },
            Value::String(s) => ExprT::Primary {
                value: Value::String(s),
                type_: Arc::new(Type::String),
            },
        }
    }

    fn lookup_type_sig(&mut self, sig: &Loc<TypeSig>) -> Result<Arc<Type>, TypeError> {
        match &sig.inner {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(Arc::new(Type::Array(type_)))
            }
            TypeSig::Name(name) => {
                if let Some(type_) = self.type_names.get(name) {
                    Ok(type_.clone())
                } else {
                    Err(TypeError::TypeDoesNotExist {
                        location: sig.location,
                        type_name: self.name_table.get_str(name).to_string(),
                    })
                }
            }
            TypeSig::Empty => Ok(Arc::new(Type::Unit)),
            TypeSig::Arrow(param_sigs, return_sig) => {
                let param_types = if param_sigs.len() == 1 {
                    self.lookup_type_sig(&param_sigs[0])?
                } else {
                    let mut types = Vec::new();
                    for param in param_sigs {
                        types.push(self.lookup_type_sig(param)?);
                    }
                    Arc::new(Type::Tuple(types))
                };

                let return_type = self.lookup_type_sig(return_sig)?;
                Ok(Arc::new(Type::Arrow(param_types, return_type)))
            }
        }
    }

    fn pat(&mut self, pat: &Pat) -> Result<Arc<Type>, TypeError> {
        match pat {
            Pat::Id(_, Some(type_sig), _) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                Ok(type_)
            }
            Pat::Id(name, None, _) => {
                let type_ = self.get_fresh_type_var();
                self.symbol_table.insert_var(*name, type_.clone());
                Ok(type_)
            }
            Pat::Tuple(pats, _) => {
                let types: Result<Vec<_>, _> = pats.iter().map(|pat| self.pat(pat)).collect();
                Ok(Arc::new(Type::Tuple(types?)))
            }
            Pat::Record(pats, type_sig, _) => {
                // In the future I should unify these two if they both exist.
                if let Some(type_sig) = type_sig {
                    self.lookup_type_sig(&type_sig)
                } else {
                    let types: Result<Vec<_>, _> = pats
                        .iter()
                        .map(|name| Ok((*name, self.get_fresh_type_var())))
                        .collect();
                    Ok(Arc::new(Type::Record(types?)))
                }
            }
            Pat::Empty(_) => Ok(Arc::new(Type::Unit)),
        }
    }

    /// Gets the function parameters as a list of names with
    /// types. Useful for WebAssembly code generation.
    fn get_func_params(&mut self, pat: &Pat) -> Result<Vec<(Name, Arc<Type>)>, TypeError> {
        match pat {
            Pat::Id(name, Some(type_sig), _) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                Ok(vec![(*name, type_)])
            }
            Pat::Id(name, None, _) => {
                let type_ = self.get_fresh_type_var();
                self.symbol_table.insert_var(*name, type_.clone());
                Ok(vec![(*name, type_)])
            }
            Pat::Tuple(pats, _) => {
                let mut types = Vec::new();
                for pat in pats {
                    types.append(&mut self.get_func_params(pat)?)
                }
                Ok(types)
            }
            Pat::Record(names, type_sig, _) => {
                let type_ = if let Some(type_sig) = type_sig {
                    Some(self.lookup_type_sig(&type_sig)?)
                } else {
                    None
                };
                let mut params = Vec::new();
                for name in names {
                    let field_type = if let Some(t) = &type_ {
                        self.get_field_type(t, *name)?
                    } else {
                        self.get_fresh_type_var()
                    };
                    params.push((*name, field_type))
                }
                Ok(params)
            }
            Pat::Empty(_) => Ok(Vec::new()),
        }
    }

    fn get_field_type(
        &mut self,
        record_type: &Arc<Type>,
        field_name: usize,
    ) -> Result<Arc<Type>, TypeError> {
        if let Type::Record(fields) = &**record_type {
            if let Some((_, type_)) = fields.iter().find(|(name, _)| *name == field_name) {
                Ok(type_.clone())
            } else {
                Err(TypeError::FieldDoesNotExist {
                    name: self.name_table.get_str(&field_name).to_string(),
                })
            }
        } else {
            Err(TypeError::NotARecord {
                type_: record_type.clone(),
            })
        }
    }

    // Desugars an assignment statement into multiple bindings.
    // We pass in a location because these bindings don't have
    // an actual source code place so we just give them the location
    // of the original assignment
    fn generate_pattern_bindings(
        &mut self,
        pat: &Pat,
        owner_name: Name,
        rhs_type: &Arc<Type>,
        location: LocationRange,
    ) -> Result<Vec<Loc<StmtT>>, TypeError> {
        let mut bindings = Vec::new();
        match pat {
            Pat::Id(_, _, _) | Pat::Empty(_) => Ok(Vec::new()),
            Pat::Record(names, _, _) => {
                for name in names {
                    bindings.push(Loc {
                        location,
                        inner: StmtT::Asgn(
                            *name,
                            Loc {
                                location,
                                inner: ExprT::Field(
                                    Box::new(ExprT::Var {
                                        name: owner_name,
                                        type_: rhs_type.clone(),
                                    }),
                                    *name,
                                    self.get_field_type(rhs_type, *name)?,
                                ),
                            },
                        ),
                    });
                }
                Ok(bindings)
            }
            Pat::Tuple(pats, _) => {
                // TODO: Make this recursive so that we can
                // flatten bindings
                for (i, pat) in pats.iter().enumerate() {
                    let name = self.name_table.get_fresh_name();
                    bindings.push(Loc {
                        location,
                        inner: StmtT::Asgn(
                            name,
                            Loc {
                                location,
                                inner: ExprT::Field(
                                    Box::new(ExprT::Var {
                                        name: owner_name,
                                        type_: rhs_type.clone(),
                                    }),
                                    i,
                                    self.get_fresh_type_var(),
                                ),
                            },
                        ),
                    });
                    let type_ = self.get_fresh_type_var();
                    bindings
                        .append(&mut self.generate_pattern_bindings(pat, name, &type_, location)?)
                }
                Ok(bindings)
            }
        }
    }

    fn asgn(
        &mut self,
        pat: Pat,
        rhs: Loc<Expr>,
        location: LocationRange,
    ) -> Result<Vec<Loc<StmtT>>, TypeError> {
        let pat_type = self.pat(&pat)?;
        let typed_rhs = self.expr(rhs)?;
        if self.unify(&pat_type, &typed_rhs.inner.get_type()) {
            let name = if let Pat::Id(name, _, _) = pat {
                name
            } else {
                self.name_table.get_fresh_name()
            };
            self.symbol_table
                .insert_var(name, typed_rhs.inner.get_type());
            let mut pat_bindings =
                self.generate_pattern_bindings(&pat, name, &typed_rhs.inner.get_type(), location)?;
            let mut bindings = vec![Loc {
                location,
                inner: StmtT::Asgn(name, typed_rhs),
            }];
            bindings.append(&mut pat_bindings);
            Ok(bindings)
        } else {
            Err(TypeError::UnificationFailure {
                location,
                type1: pat_type,
                type2: typed_rhs.inner.get_type(),
            })
        }
    }

    /**
     *
     * BEFORE calling this function, please set up a new scope. Why before?
     * Because if we're calling this with a function binding, i.e. StmtT::Function,
     * then we need to insert the name into the scope, but if we're calling this with
     * an anonymous function, i.e. ExprT::Function, then we can't do that
     *
     **/
    fn func(
        &mut self,
        params: Pat,
        body: Loc<Stmt>,
        return_type: Option<Loc<TypeSig>>,
    ) -> Result<
        (
            Vec<(Name, Arc<Type>)>,
            Box<Loc<StmtT>>,
            Arc<Type>,
            Vec<Arc<Type>>,
        ),
        TypeError,
    > {
        let old_var_types = self.symbol_table.reset_vars();
        let func_params = self.get_func_params(&params)?;
        for (name, type_) in &func_params {
            self.symbol_table.insert_var(name.clone(), type_.clone());
        }
        // Insert return type into typechecker so that
        // typechecker can verify return statements.
        if let Some(return_type_sig) = return_type {
            self.return_type = Some(self.lookup_type_sig(&return_type_sig)?);
        }
        let body_location = body.location;
        // Check body
        let body = self.stmt(body)?;
        let mut return_type = None;
        std::mem::swap(&mut return_type, &mut self.return_type);
        let return_type = return_type.unwrap_or_else(|| Arc::new(Type::Unit));
        let func_var_types = self.symbol_table.restore_vars(old_var_types);
        Ok((
            func_params,
            Box::new(Loc {
                location: body_location,
                inner: StmtT::Block(body),
            }),
            return_type,
            func_var_types,
        ))
    }

    fn expr(&mut self, expr: Loc<Expr>) -> Result<Loc<ExprT>, TypeError> {
        let location = expr.location;
        match expr.inner {
            Expr::Primary { value } => Ok(Loc {
                location,
                inner: self.value(value),
            }),
            Expr::Var { name } => {
                let entry =
                    self.symbol_table
                        .lookup_name(name)
                        .ok_or(TypeError::VarNotDefined {
                            location,
                            name: self.name_table.get_str(&name).to_string(),
                        })?;
                match &entry.entry_type {
                    EntryType::Function {
                        index: _,
                        params_type,
                        return_type,
                    } => Ok(Loc {
                        location,
                        inner: ExprT::Var {
                            name,
                            type_: Arc::new(Type::Arrow(params_type.clone(), return_type.clone())),
                        },
                    }),
                    EntryType::Var { var_type, index: _ } => Ok(Loc {
                        location,
                        inner: ExprT::Var {
                            name,
                            type_: var_type.clone(),
                        },
                    }),
                }
            }
            Expr::BinOp { op, lhs, rhs } => {
                let typed_lhs = self.expr(*lhs)?;
                let typed_rhs = self.expr(*rhs)?;
                let lhs_type = typed_lhs.inner.get_type();
                let rhs_type = typed_rhs.inner.get_type();
                match self.op(&op, lhs_type, rhs_type) {
                    Some(op_type) => Ok(Loc {
                        location,
                        inner: ExprT::BinOp {
                            op,
                            lhs: Box::new(typed_lhs),
                            rhs: Box::new(typed_rhs),
                            type_: Arc::new(op_type),
                        },
                    }),
                    None => Err(TypeError::OpFailure {
                        location,
                        op: op.clone(),
                        lhs_type: typed_lhs.inner.get_type(),
                        rhs_type: typed_rhs.inner.get_type(),
                    }),
                }
            }
            Expr::Tuple(elems) => {
                let mut typed_elems = Vec::new();
                let mut types = Vec::new();
                for elem in elems {
                    let typed_elem = self.expr(elem)?;
                    types.push(typed_elem.inner.get_type());
                    typed_elems.push(typed_elem);
                }
                Ok(Loc {
                    location,
                    inner: ExprT::Tuple(typed_elems, Arc::new(Type::Tuple(types))),
                })
            }
            Expr::Function {
                params,
                body,
                return_type: return_type_sig,
            } => {
                let name = self.name_table.get_fresh_name();
                let params_type = self.pat(&params)?;
                let previous_scope = self.symbol_table.push_scope(true);
                let (params, body, return_type, local_variables) =
                    self.func(params, *body, return_type_sig)?;
                let func_scope = self.symbol_table.restore_scope(previous_scope);
                self.symbol_table
                    .insert_function(name, params_type.clone(), return_type.clone());
                let func = Loc {
                    location,
                    inner: ExprT::Function {
                        params,
                        params_type,
                        return_type,
                        body,
                        name,
                        local_variables,
                        scope_index: func_scope,
                    },
                };
                Ok(func)
            }
            Expr::UnaryOp { op, rhs } => {
                let typed_rhs = self.expr(*rhs)?;
                let rhs_type = typed_rhs.inner.get_type();
                let is_valid_types = match op {
                    UnaryOp::Minus => {
                        self.unify(&rhs_type, &Arc::new(Type::Int))
                            || self.unify(&rhs_type, &Arc::new(Type::Float))
                    }
                    UnaryOp::Not => self.unify(&rhs_type, &Arc::new(Type::Bool)),
                };
                if is_valid_types {
                    Ok(Loc {
                        location,
                        inner: ExprT::UnaryOp {
                            op,
                            rhs: Box::new(typed_rhs),
                            type_: rhs_type,
                        },
                    })
                } else {
                    Err(TypeError::InvalidUnaryExpr {
                        location: typed_rhs.location,
                        expr: typed_rhs.inner,
                    })
                }
            }
            Expr::Call { callee, args } => {
                let typed_callee = self.expr(*callee)?;
                let callee_type = typed_callee.inner.get_type();
                let (params_type, return_type) = match &(*callee_type) {
                    Type::Arrow(params_type, return_type) => {
                        (params_type.clone(), return_type.clone())
                    }
                    Type::Var(_) => (self.get_fresh_type_var(), self.get_fresh_type_var()),
                    _ => return Err(TypeError::CalleeNotFunction),
                };
                let typed_args = self.expr(*args)?;
                let args_type = typed_args.inner.get_type();
                if self.unify(&params_type, &args_type) {
                    Ok(Loc {
                        location,
                        inner: ExprT::Call {
                            callee: Box::new(typed_callee),
                            args: Box::new(typed_args),
                            type_: return_type,
                        },
                    })
                } else {
                    Err(TypeError::UnificationFailure {
                        location,
                        type1: params_type,
                        type2: args_type,
                    })
                }
            }
            Expr::Block(stmts, end_expr) => {
                let mut typed_stmts = Vec::new();
                let previous_scope = self.symbol_table.push_scope(false);
                for stmt in stmts {
                    typed_stmts.append(&mut self.stmt(stmt)?)
                }
                let (type_, typed_end_expr) = if let Some(expr) = end_expr {
                    let typed_expr = self.expr(*expr)?;
                    (typed_expr.inner.get_type(), Some(Box::new(typed_expr)))
                } else {
                    (Arc::new(Type::Unit), None)
                };
                let scope_index = self.symbol_table.restore_scope(previous_scope);
                Ok(Loc {
                    location,
                    inner: ExprT::Block {
                        stmts: typed_stmts,
                        end_expr: typed_end_expr,
                        scope_index,
                        type_,
                    },
                })
            }
            Expr::If(cond, then_block, else_block) => {
                let typed_cond = self.expr(*cond)?;
                let typed_then_block = self.expr(*then_block)?;
                let then_type = typed_then_block.inner.get_type();
                if let Some(else_block) = else_block {
                    let typed_else_block = self.expr(*else_block)?;
                    let else_type = typed_else_block.inner.get_type();
                    if !self.unify(&then_type, &else_type) {
                        return Err(TypeError::UnificationFailure {
                            location,
                            type1: then_type,
                            type2: else_type,
                        });
                    }
                    Ok(Loc {
                        location,
                        inner: ExprT::If(
                            Box::new(typed_cond),
                            Box::new(typed_then_block),
                            Some(Box::new(typed_else_block)),
                            then_type,
                        ),
                    })
                } else if !self.unify(&Arc::new(Type::Unit), &then_type) {
                    Err(TypeError::UnificationFailure {
                        location,
                        type1: Arc::new(Type::Unit),
                        type2: then_type,
                    })
                } else {
                    Ok(Loc {
                        location,
                        inner: ExprT::If(
                            Box::new(typed_cond),
                            Box::new(typed_then_block),
                            None,
                            Arc::new(Type::Unit),
                        ),
                    })
                }
            }
            _ => Err(TypeError::NotImplemented { location: location }),
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
        type_vector1: &[Arc<Type>],
        type_vector2: &[Arc<Type>],
    ) -> bool {
        if type_vector1.len() != type_vector2.len() {
            return false;
        }
        for (t1, t2) in type_vector1.iter().zip(type_vector2.iter()) {
            if !self.unify(t1, t2) {
                return false;
            }
        }
        true
    }

    fn unify(&mut self, type1: &Arc<Type>, type2: &Arc<Type>) -> bool {
        if Arc::ptr_eq(type1, type2) || type1 == type2 {
            return true;
        }
        match (&**type1, &**type2) {
            (Type::Tuple(ts), Type::Unit) | (Type::Unit, Type::Tuple(ts)) => ts.is_empty(),
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
