use crate::ast::{
    Expr, ExprT, Function, Loc, Name, Op, Pat, Program, ProgramT, Stmt, StmtT, Type, TypeDef,
    TypeId, TypeSig, UnaryOp, Value,
};
use crate::lexer::LocationRange;
use crate::loc;
use crate::printer::type_to_string;
use crate::symbol_table::{FunctionInfo, SymbolTable};
use crate::utils::{
    NameTable, TypeTable, BOOL_INDEX, CHAR_INDEX, FLOAT_INDEX, INT_INDEX, STR_INDEX, UNIT_INDEX,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeError {
    VarNotDefined {
        name: String,
    },
    OpFailure {
        op: Op,
        lhs_type: Type,
        rhs_type: Type,
    },
    UnificationFailure {
        type1: String,
        type2: String,
    },
    TypeDoesNotExist {
        type_name: String,
    },
    FieldDoesNotExist {
        name: String,
        record: String,
    },
    NotARecord {
        type_: String,
    },
    NotATuple {
        type_: String,
    },
    InvalidUnaryExpr,
    CalleeNotFunction,
    TopLevelReturn,
    ShadowingFunction,
    TupleIndexOutOfBounds {
        index: u32,
        tuple_type: String,
    },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::VarNotDefined { name } => write!(f, "Variable not defined: '{}'", name),
            TypeError::OpFailure {
                op,
                lhs_type,
                rhs_type,
            } => write!(
                f,
                "Could not find operation {} with arguments of type {} and {}",
                op, lhs_type, rhs_type
            ),
            TypeError::UnificationFailure { type1, type2 } => {
                write!(f, "Could not unify {} with {}", type1, type2)
            }
            TypeError::TypeDoesNotExist { type_name } => {
                write!(f, "Type {} does not exist", type_name)
            }
            TypeError::FieldDoesNotExist { name, record } => {
                write!(f, "Field {} does not exist on record {}", name, record)
            }
            TypeError::NotARecord { type_ } => write!(f, "Type {} is not a record", type_),
            TypeError::NotATuple { type_ } => write!(f, "Type {} is not a tuple", type_),
            TypeError::InvalidUnaryExpr => write!(f, "Cannot apply unary operator"),
            TypeError::CalleeNotFunction => write!(f, "Callee is not a function"),
            TypeError::TopLevelReturn => write!(f, "Cannot return at top level"),
            TypeError::ShadowingFunction => write!(
                f,
                "This declaration is the same name as a function in this block.
                 We can't allow this since functions must be in scope for the whole block"
            ),
            TypeError::TupleIndexOutOfBounds { index, tuple_type } => write!(
                f,
                "Index {} is out of bounds of tuple {}",
                index, tuple_type
            ),
        }
    }
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
    type_names: HashMap<Name, TypeId>,
    // The return type for the typing context
    return_type: Option<TypeId>,
    // Index for type variable names
    type_var_index: usize,
    // Type table
    type_table: TypeTable,
    // Symbol table
    name_table: NameTable,
    // Struct types
    struct_types: Vec<(Name, TypeId)>,
    // Expr function index. Used for codegen
    expr_func_index: usize,
}

fn build_type_names(name_table: &mut NameTable) -> HashMap<Name, TypeId> {
    let primitive_types = vec![
        ("int", INT_INDEX),
        ("float", FLOAT_INDEX),
        ("char", CHAR_INDEX),
        ("string", STR_INDEX),
        ("bool", BOOL_INDEX),
    ];
    let mut type_names = HashMap::new();
    for (name, type_id) in primitive_types {
        let name_id = name_table.insert(name.to_string());
        type_names.insert(name_id, type_id);
    }
    type_names
}

pub fn is_ref_type(type_id: TypeId) -> bool {
    !(type_id == INT_INDEX
        || type_id == BOOL_INDEX
        || type_id == UNIT_INDEX
        || type_id == CHAR_INDEX
        || type_id == FLOAT_INDEX)
}

impl TypeChecker {
    pub fn new(mut name_table: NameTable) -> TypeChecker {
        let mut symbol_table = SymbolTable::new();
        let mut type_table = TypeTable::new();
        let print_int_id = name_table.insert("printInt".into());
        symbol_table.insert_function(
            print_int_id,
            INT_INDEX,
            UNIT_INDEX,
            type_table.insert(Type::Arrow(INT_INDEX, UNIT_INDEX)),
        );
        let print_float_id = name_table.insert("printFloat".into());
        symbol_table.insert_function(
            print_float_id,
            FLOAT_INDEX,
            UNIT_INDEX,
            type_table.insert(Type::Arrow(FLOAT_INDEX, UNIT_INDEX)),
        );
        let print_string_id = name_table.insert("printString".into());
        symbol_table.insert_function(
            print_string_id,
            STR_INDEX,
            UNIT_INDEX,
            type_table.insert(Type::Arrow(STR_INDEX, UNIT_INDEX)),
        );
        let print_char_id = name_table.insert("printChar".into());
        symbol_table.insert_function(
            print_char_id,
            CHAR_INDEX,
            UNIT_INDEX,
            type_table.insert(Type::Arrow(CHAR_INDEX, UNIT_INDEX)),
        );

        TypeChecker {
            symbol_table,
            type_names: build_type_names(&mut name_table),
            return_type: None,
            type_var_index: 0,
            type_table,
            name_table,
            struct_types: Vec::new(),
            expr_func_index: 0,
        }
    }

    pub fn get_tables(self) -> (SymbolTable, NameTable, TypeTable) {
        (self.symbol_table, self.name_table, self.type_table)
    }

    pub fn get_expr_func_index(&self) -> usize {
        self.expr_func_index
    }

    #[allow(dead_code)]
    pub fn get_name_table(&self) -> &NameTable {
        &self.name_table
    }

    pub fn get_fresh_type_var(&mut self) -> TypeId {
        let type_var = Type::Var(self.type_var_index);
        self.type_var_index += 1;
        self.type_table.insert(type_var)
    }

    fn generate_field_info<'a, I>(&self, fields: I) -> Vec<bool>
    where
        I: Iterator<Item = &'a usize>,
    {
        // Stores whether or not a field is a reference
        let mut field_info = Vec::new();
        for field_type in fields {
            field_info.push(is_ref_type(*field_type))
        }
        field_info
    }

    fn generate_type_info(&self, type_id: TypeId) -> Option<(usize, Vec<bool>)> {
        match self.type_table.get_type(type_id) {
            Type::Record(_, fields) => {
                let field_info =
                    self.generate_field_info(fields.iter().map(|(_, type_id)| type_id));
                Some((type_id, field_info))
            }
            Type::Tuple(type_ids) => Some((type_id, self.generate_field_info(type_ids.iter()))),
            Type::Solved(type_id) => self.generate_type_info(*type_id),
            _ => None,
        }
    }

    pub fn generate_runtime_type_info(
        &self,
        named_types: &Vec<(Name, TypeId)>,
    ) -> Vec<(Name, Vec<bool>)> {
        let mut type_info = Vec::new();
        for (_, type_id) in named_types {
            let final_type_id = self.type_table.get_final_type(*type_id);
            self.generate_type_info(final_type_id)
                .map(|info| type_info.push(info));
        }
        type_info
    }

    pub fn check_program(&mut self, program: Program) -> ProgramT {
        let mut errors = Vec::new();
        for type_def in program.type_defs {
            match self.type_def(type_def) {
                Ok(struct_type) => {
                    self.struct_types.push(struct_type);
                }
                Err(err) => {
                    errors.push(err);
                }
            }
        }
        if let Err(err) = self.read_functions(&program.stmts) {
            errors.push(err);
        }
        let mut typed_stmts = Vec::new();
        for stmt in program.stmts {
            match self.stmt(stmt) {
                Ok(stmt_t) => {
                    let mut stmt_t = stmt_t;
                    typed_stmts.append(&mut stmt_t);
                }
                Err(err) => {
                    errors.push(err);
                }
            }
        }
        let mut named_types = Vec::new();
        std::mem::swap(&mut named_types, &mut self.struct_types);
        ProgramT {
            stmts: typed_stmts,
            named_types,
            errors,
        }
    }

    // Reads functions defined in this block
    fn read_functions(&mut self, stmts: &Vec<Loc<Stmt>>) -> Result<(), Loc<TypeError>> {
        for stmt in stmts {
            if let Stmt::Function(func_name, params, return_type_sig, _) = &stmt.inner {
                let params_type = self.pat(&params)?;
                let return_type = if let Some(type_sig) = return_type_sig {
                    self.lookup_type_sig(type_sig)?
                } else {
                    self.get_fresh_type_var()
                };
                let type_ = self
                    .type_table
                    .insert(Type::Arrow(params_type, return_type));
                self.symbol_table
                    .insert_function(*func_name, params_type, return_type, type_);
            }
        }
        Ok(())
    }

    fn type_def(&mut self, type_def: Loc<TypeDef>) -> Result<(Name, TypeId), Loc<TypeError>> {
        match type_def.inner {
            TypeDef::Struct(name, fields) => {
                let mut typed_fields = Vec::new();
                for (name, type_sig) in fields {
                    let field_type = self.lookup_type_sig(&type_sig)?;
                    typed_fields.push((name, field_type));
                }
                let type_id = self.type_table.insert(Type::Record(name, typed_fields));
                self.type_names.insert(name, type_id);
                Ok((name, type_id))
            }
        }
    }

    // If you have an if expression that's an expression statement, i.e.
    //   if is_greeting {
    //     print("hello");
    //   } else {
    //     print("goodbye");
    //   }
    // Then we convert it into a StmtT::If
    // This helps for codegen
    fn if_expr_stmt(
        &mut self,
        location: LocationRange,
        cond: Box<Loc<Expr>>,
        then_block: Box<Loc<Expr>>,
        else_block: Option<Box<Loc<Expr>>>,
    ) -> Result<Loc<StmtT>, Loc<TypeError>> {
        let cond_t = self.expr(*cond)?;
        self.unify_or_err(cond_t.inner.get_type(), BOOL_INDEX, cond_t.location)?;
        let then_t = self.expr(*then_block)?;
        let else_t = if let Some(else_block) = else_block {
            let else_t = self.expr(*else_block)?;
            self.unify_or_err(else_t.inner.get_type(), UNIT_INDEX, else_t.location)?;
            Some(else_t)
        } else {
            None
        };
        self.unify_or_err(then_t.inner.get_type(), UNIT_INDEX, then_t.location)?;
        Ok(loc!(
            StmtT::If {
                cond: cond_t,
                then_block: then_t,
                else_block: else_t
            },
            location
        ))
    }

    pub fn stmt(&mut self, stmt: Loc<Stmt>) -> Result<Vec<Loc<StmtT>>, Loc<TypeError>> {
        let location = stmt.location;
        match stmt.inner {
            Stmt::Expr(expr) => {
                if let Expr::If(cond, then_block, else_block) = expr.inner {
                    Ok(vec![
                        self.if_expr_stmt(location, cond, then_block, else_block)?
                    ])
                } else {
                    let typed_expr = self.expr(expr)?;
                    Ok(vec![Loc {
                        location,
                        inner: StmtT::Expr(typed_expr),
                    }])
                }
            }
            Stmt::Function(func_name, params, _, body) => {
                let sym_entry = if let Some(entry) = self.symbol_table.lookup_name(func_name) {
                    entry
                } else {
                    return Err(loc!(
                        TypeError::VarNotDefined {
                            name: self.name_table.get_str(&func_name).to_string(),
                        },
                        location
                    ));
                };
                let func_info = sym_entry.function_info.as_ref().unwrap();
                let params_type = func_info.params_type;
                let return_type = func_info.return_type;
                let (function, _) = self.function(func_name, params, *body)?;
                Ok(vec![Loc {
                    location,
                    inner: StmtT::Function {
                        name: func_name,
                        params_type,
                        return_type,
                        function,
                    },
                }])
            }
            Stmt::Asgn(pat, rhs) => Ok(self.asgn(pat, rhs, location)?),
            Stmt::Loop(block) => {
                let block_t = self.expr(block)?;
                self.unify_or_err(block_t.inner.get_type(), UNIT_INDEX, location)?;
                Ok(vec![loc!(StmtT::Loop(block_t), location)])
            }
            Stmt::Return(expr) => {
                let typed_expr = self.expr(expr)?;
                match self.return_type {
                    Some(return_type) => {
                        let type_ =
                            self.unify_or_err(typed_expr.inner.get_type(), return_type, location)?;
                        self.return_type = Some(type_);
                        Ok(vec![Loc {
                            location,
                            inner: StmtT::Return(typed_expr),
                        }])
                    }
                    None => Err(loc!(TypeError::TopLevelReturn, stmt.location)),
                }
            }
            Stmt::Export(name) => {
                self.symbol_table.lookup_name(name).ok_or(loc!(
                    TypeError::VarNotDefined {
                        name: self.name_table.get_str(&name).to_string(),
                    },
                    location
                ))?;
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
                type_: INT_INDEX,
            },
            Value::Float(_f) => ExprT::Primary {
                value,
                type_: FLOAT_INDEX,
            },
            Value::Bool(_b) => ExprT::Primary {
                value,
                type_: BOOL_INDEX,
            },
            Value::String(s) => ExprT::Primary {
                value: Value::String(s),
                type_: STR_INDEX,
            },
        }
    }

    fn lookup_type_sig(&mut self, sig: &Loc<TypeSig>) -> Result<TypeId, Loc<TypeError>> {
        match &sig.inner {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(self.type_table.insert(Type::Array(type_)))
            }
            TypeSig::Name(name) => self
                .type_names
                .get(name)
                .ok_or(loc!(
                    TypeError::TypeDoesNotExist {
                        type_name: self.name_table.get_str(name).to_string(),
                    },
                    sig.location
                ))
                .map(|t| *t),
            TypeSig::Empty => Ok(UNIT_INDEX),
            TypeSig::Arrow(param_sigs, return_sig) => {
                let param_types = if param_sigs.len() == 1 {
                    self.lookup_type_sig(&param_sigs[0])?
                } else {
                    let mut types = Vec::new();
                    for param in param_sigs {
                        types.push(self.lookup_type_sig(param)?);
                    }
                    self.type_table.insert(Type::Tuple(types))
                };

                let return_type = self.lookup_type_sig(return_sig)?;
                Ok(self
                    .type_table
                    .insert(Type::Arrow(param_types, return_type)))
            }
        }
    }

    fn pat(&mut self, pat: &Pat) -> Result<TypeId, Loc<TypeError>> {
        match pat {
            Pat::Id(_, Some(type_sig), _) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                Ok(type_)
            }
            Pat::Id(_, None, _) => {
                let type_ = self.get_fresh_type_var();
                Ok(type_)
            }
            Pat::Tuple(pats, _) => {
                let types: Result<Vec<_>, _> = pats.iter().map(|pat| self.pat(pat)).collect();
                Ok(self.type_table.insert(Type::Tuple(types?)))
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
                    Ok(self
                        .type_table
                        .insert(Type::Record(self.name_table.get_fresh_name(), types?)))
                }
            }
            Pat::Empty(_) => Ok(UNIT_INDEX),
        }
    }

    /// Gets the function parameters as a list of names with
    /// types. Useful for WebAssembly code generation.
    fn get_func_params(&mut self, pat: &Pat) -> Result<Vec<(Name, TypeId)>, Loc<TypeError>> {
        match pat {
            Pat::Id(name, Some(type_sig), _) => {
                let type_ = self.lookup_type_sig(&type_sig)?;
                self.symbol_table.insert_var(*name, type_);
                Ok(vec![(*name, type_)])
            }
            Pat::Id(name, None, _) => {
                let type_ = self.get_fresh_type_var();
                self.symbol_table.insert_var(*name, type_);
                Ok(vec![(*name, type_)])
            }
            Pat::Tuple(pats, _) => {
                let mut types = Vec::new();
                for pat in pats {
                    types.append(&mut self.get_func_params(pat)?)
                }
                Ok(types)
            }
            Pat::Record(names, type_sig, location) => {
                let type_ = if let Some(type_sig) = type_sig {
                    Some(self.lookup_type_sig(&type_sig)?)
                } else {
                    None
                };
                let mut params = Vec::new();
                for name in names {
                    let field_type = if let Some(t) = &type_ {
                        self.get_field_type(*t, *name, *location)?.1
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
        record_type: TypeId,
        field_name: usize,
        location: LocationRange,
    ) -> Result<(u32, TypeId), Loc<TypeError>> {
        if let Type::Record(_, fields) = self.type_table.get_type(record_type) {
            if let Some(pos) = fields.iter().position(|(name, _)| *name == field_name) {
                let (_, type_) = fields[pos];
                Ok((pos.try_into().unwrap(), type_))
            } else {
                Err(loc!(
                    TypeError::FieldDoesNotExist {
                        record: type_to_string(&self.name_table, &self.type_table, record_type),
                        name: self.name_table.get_str(&field_name).to_string(),
                    },
                    location
                ))
            }
        } else {
            Err(loc!(
                TypeError::NotARecord {
                    type_: type_to_string(&self.name_table, &self.type_table, record_type),
                },
                location
            ))
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
        rhs_type: TypeId,
        location: LocationRange,
    ) -> Result<Vec<Loc<StmtT>>, Loc<TypeError>> {
        let mut bindings = Vec::new();
        match pat {
            Pat::Id(_, _, _) | Pat::Empty(_) => Ok(Vec::new()),
            Pat::Record(names, _, _) => {
                for name in names {
                    let (index, type_) = self.get_field_type(rhs_type, *name, location)?;
                    self.symbol_table.insert_var(*name, type_.clone());
                    bindings.push(Loc {
                        location,
                        inner: StmtT::Asgn(
                            *name,
                            Loc {
                                location,
                                inner: ExprT::TupleField(
                                    Box::new(Loc {
                                        location,
                                        inner: ExprT::Var {
                                            name: owner_name,
                                            type_: rhs_type.clone(),
                                        },
                                    }),
                                    index,
                                    type_,
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
                                inner: ExprT::TupleField(
                                    Box::new(Loc {
                                        location,
                                        inner: ExprT::Var {
                                            name: owner_name,
                                            type_: rhs_type.clone(),
                                        },
                                    }),
                                    i.try_into().unwrap(),
                                    self.get_fresh_type_var(),
                                ),
                            },
                        ),
                    });
                    let type_ = self.get_fresh_type_var();
                    bindings
                        .append(&mut self.generate_pattern_bindings(pat, name, type_, location)?)
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
    ) -> Result<Vec<Loc<StmtT>>, Loc<TypeError>> {
        let pat_type = self.pat(&pat)?;
        let typed_rhs = self.expr(rhs)?;
        let type_ = self.unify_or_err(pat_type, typed_rhs.inner.get_type(), location)?;
        let name = if let Pat::Id(name, _, _) = pat {
            name
        } else {
            self.name_table.get_fresh_name()
        };
        if self
            .symbol_table
            .lookup_name(name)
            .map(|entry| entry.function_info.as_ref())
            .is_some()
        {
            return Err(loc!(TypeError::ShadowingFunction, location));
        }
        self.symbol_table.insert_var(name, type_);
        let mut pat_bindings =
            self.generate_pattern_bindings(&pat, name, typed_rhs.inner.get_type(), location)?;
        let mut bindings = vec![Loc {
            location,
            inner: StmtT::Asgn(name, typed_rhs),
        }];
        bindings.append(&mut pat_bindings);
        Ok(bindings)
    }

    fn create_captures_tuple(&mut self, location: LocationRange) -> Option<Loc<ExprT>> {
        let captures = self.symbol_table.get_captures().unwrap();
        if captures.len() == 0 {
            return None;
        }
        let mut fields = Vec::new();
        let mut types = Vec::new();
        for (name, _, type_) in captures {
            fields.push(Loc {
                location,
                inner: ExprT::Var {
                    name: *name,
                    type_: *type_,
                },
            });
            types.push(*type_);
        }
        let type_id = self.type_table.insert(Type::Tuple(types));
        let name = self.name_table.get_fresh_name();
        self.struct_types.push((name, type_id));
        Some(loc!(ExprT::Tuple(fields, type_id), location))
    }

    fn function(
        &mut self,
        name: Name,
        params: Pat,
        body: Loc<Expr>,
    ) -> Result<(Function, TypeId), Loc<TypeError>> {
        let entry = self.symbol_table.lookup_name(name);
        let entry = entry.as_ref().unwrap();
        let func_info = entry.function_info.as_ref().unwrap();
        let type_ = entry.var_type;
        let scope = func_info.func_scope;
        let return_type = func_info.return_type;
        self.symbol_table.swap_scope(scope);
        let old_var_types = self.symbol_table.reset_vars();
        let func_params = self.get_func_params(&params)?;
        // Save the current return type
        let mut old_return_type = self.return_type;

        self.return_type = Some(return_type);

        let body_location = body.location;
        // Check body
        let body = self.expr(body)?;

        let body_type = body.inner.get_type();
        std::mem::swap(&mut old_return_type, &mut self.return_type);
        // If the body type is unit, we don't try to unify the body type
        // with return type.
        let return_type = if body_type != UNIT_INDEX {
            self.unify_or_err(old_return_type.unwrap(), body_type, body_location)?
        } else {
            old_return_type.unwrap()
        };

        // If return type is a type var, we just set it be unit
        if let Type::Var(_) = self.type_table.get_type(return_type) {
            self.type_table.update(return_type, Type::Unit);
        };

        let captures = self.create_captures_tuple(body.location);

        let local_variables = self.symbol_table.restore_vars(old_var_types);
        let scope_index = self.symbol_table.restore_scope();
        Ok((
            Function {
                params: func_params,
                body: Box::new(body),
                local_variables,
                scope_index,
                captures: captures.map(Box::new),
            },
            type_,
        ))
    }

    fn expr(&mut self, expr: Loc<Expr>) -> Result<Loc<ExprT>, Loc<TypeError>> {
        let location = expr.location;
        match expr.inner {
            Expr::Primary { value } => Ok(Loc {
                location,
                inner: self.value(value),
            }),
            Expr::Var { name } => {
                let entry = self.symbol_table.lookup_name(name).ok_or(loc!(
                    TypeError::VarNotDefined {
                        name: self.name_table.get_str(&name).to_string(),
                    },
                    location
                ))?;
                Ok(Loc {
                    location,
                    inner: ExprT::Var {
                        name,
                        type_: entry.var_type,
                    },
                })
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
                            type_: op_type,
                        },
                    }),
                    None => {
                        let lhs_type = self.type_table.get_type(typed_lhs.inner.get_type()).clone();
                        let rhs_type = self.type_table.get_type(typed_rhs.inner.get_type()).clone();
                        Err(loc!(
                            TypeError::OpFailure {
                                op: op.clone(),
                                lhs_type,
                                rhs_type,
                            },
                            location
                        ))
                    }
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
                let name = self.name_table.get_fresh_name();
                let type_id = self.type_table.insert(Type::Tuple(types));
                self.struct_types.push((name, type_id));
                Ok(Loc {
                    location,
                    inner: ExprT::Tuple(typed_elems, type_id),
                })
            }
            Expr::Array(entries) => {
                let mut typed_entries = Vec::new();
                let mut entry_type = self.get_fresh_type_var();
                for entry in entries {
                    let typed_entry = self.expr(entry)?;
                    entry_type = self.unify_or_err(
                        entry_type,
                        typed_entry.inner.get_type(),
                        typed_entry.location,
                    )?;
                    typed_entries.push(typed_entry);
                }
                let array_type = self.type_table.insert(Type::Array(entry_type));
                Ok(loc!(
                    ExprT::Array {
                        entries: typed_entries,
                        entry_type,
                        type_: array_type
                    },
                    location
                ))
            }
            Expr::Function {
                params,
                body,
                return_type: return_type_sig,
            } => {
                let name = self.name_table.get_fresh_name();
                let params_type = self.pat(&params)?;
                let return_type = if let Some(type_sig) = return_type_sig {
                    self.lookup_type_sig(&type_sig)?
                } else {
                    self.get_fresh_type_var()
                };
                let type_ = self
                    .type_table
                    .insert(Type::Arrow(params_type, return_type));
                self.symbol_table
                    .insert_function(name, params_type, return_type, type_);

                let (function, type_) = self.function(name, params, *body)?;
                let mut function = function;
                let table_index = self.expr_func_index;
                let table_index_expr = ExprT::Primary {
                    value: Value::Integer(table_index.try_into().unwrap()),
                    type_: INT_INDEX,
                };
                let mut capture_struct_fields = vec![loc!(table_index_expr, expr.location)];
                let mut capture_struct_type = vec![INT_INDEX];
                if let Some(captures) = function.captures {
                    let captures_type = captures.inner.get_type();
                    capture_struct_type.push(captures_type);
                    capture_struct_fields.push(*captures);
                }
                let type_id = self.type_table.insert(Type::Tuple(capture_struct_type));
                self.struct_types
                    .push((self.name_table.get_fresh_name(), type_id));
                function.captures = Some(Box::new(loc!(
                    ExprT::Tuple(capture_struct_fields, type_id),
                    expr.location
                )));
                self.expr_func_index += 1;
                let func = Loc {
                    location,
                    inner: ExprT::Function {
                        function,
                        type_,
                        name,
                        table_index,
                    },
                };
                Ok(func)
            }
            Expr::UnaryOp { op, rhs } => {
                let typed_rhs = self.expr(*rhs)?;
                let rhs_type = typed_rhs.inner.get_type();
                let is_valid_types = match op {
                    UnaryOp::Minus => {
                        self.is_unifiable(rhs_type, INT_INDEX)
                            || self.is_unifiable(rhs_type, FLOAT_INDEX)
                    }
                    UnaryOp::Not => self.is_unifiable(rhs_type, BOOL_INDEX),
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
                    Err(loc!(TypeError::InvalidUnaryExpr, location))
                }
            }
            Expr::Call { callee, args } => {
                let typed_callee = self.expr(*callee)?;
                let typed_args = self.expr(*args)?;
                if let ExprT::Var { name, type_: _ } = &typed_callee.inner {
                    if let Some(entry) = self.symbol_table.lookup_name(*name) {
                        if let Some(FunctionInfo {
                            func_index,
                            func_scope: _,
                            params_type,
                            return_type,
                            is_top_level,
                        }) = entry.function_info
                        {
                            let captures_index = if is_top_level {
                                Some(entry.var_index)
                            } else {
                                None
                            };
                            self.unify_or_err(params_type, typed_args.inner.get_type(), location)?;
                            return Ok(Loc {
                                location,
                                inner: ExprT::DirectCall {
                                    callee: func_index,
                                    captures_index,
                                    args: Box::new(typed_args),
                                    type_: return_type,
                                },
                            });
                        }
                    }
                }
                let callee_type = typed_callee.inner.get_type();
                let (params_type, return_type) = match self.type_table.get_type(callee_type) {
                    Type::Arrow(params_type, return_type) => (*params_type, *return_type),
                    Type::Var(_) => {
                        let params_type = self.get_fresh_type_var();
                        let return_type = self.get_fresh_type_var();
                        let new_id = self
                            .type_table
                            .insert(Type::Arrow(params_type, return_type));
                        self.type_table.update(callee_type, Type::Solved(new_id));
                        (params_type, return_type)
                    }
                    _ => return Err(loc!(TypeError::CalleeNotFunction, location)),
                };
                let args_type = typed_args.inner.get_type();
                self.unify_or_err(params_type, args_type, typed_args.location)?;
                Ok(Loc {
                    location,
                    inner: ExprT::IndirectCall {
                        callee: Box::new(typed_callee),
                        args: Box::new(typed_args),
                        type_: return_type,
                    },
                })
            }
            Expr::Block(stmts, end_expr) => {
                let scope_index = self.symbol_table.push_scope();
                self.read_functions(&stmts)?;
                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    typed_stmts.append(&mut self.stmt(stmt)?);
                }
                let (type_, typed_end_expr) = if let Some(expr) = end_expr {
                    let typed_expr = self.expr(*expr)?;
                    (typed_expr.inner.get_type(), Some(Box::new(typed_expr)))
                } else {
                    (UNIT_INDEX, None)
                };
                self.symbol_table.restore_scope();
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
                    self.unify_or_err(then_type, else_type, location)?;
                    Ok(Loc {
                        location,
                        inner: ExprT::If(
                            Box::new(typed_cond),
                            Box::new(typed_then_block),
                            Some(Box::new(typed_else_block)),
                            then_type,
                        ),
                    })
                } else {
                    self.unify_or_err(UNIT_INDEX, then_type, location)?;
                    Ok(Loc {
                        location,
                        inner: ExprT::If(
                            Box::new(typed_cond),
                            Box::new(typed_then_block),
                            None,
                            UNIT_INDEX,
                        ),
                    })
                }
            }
            Expr::Record { name, fields } => {
                let type_id = if let Some(id) = self.type_names.get(&name) {
                    *id
                } else {
                    let name_str = self.name_table.get_str(&name);
                    return Err(loc!(
                        TypeError::TypeDoesNotExist {
                            type_name: name_str.to_string(),
                        },
                        location
                    ));
                };

                let mut field_types = Vec::new();
                let mut fields_t = Vec::new();
                for (name, expr) in fields {
                    let expr_t = self.expr(expr)?;
                    field_types.push((name, expr_t.inner.get_type()));
                    fields_t.push((name, expr_t));
                }
                let expr_type = self.type_table.insert(Type::Record(name, field_types));
                let type_ = self.unify_or_err(type_id, expr_type, location)?;
                self.type_names.insert(name, type_);
                Ok(Loc {
                    location,
                    inner: ExprT::Record {
                        name,
                        fields: fields_t,
                        type_,
                    },
                })
            }
            Expr::Index { lhs, index } => {
                let lhs_t = self.expr(*lhs)?;
                let index_t = self.expr(*index)?;
                let lhs_type = lhs_t.inner.get_type();
                self.unify_or_err(index_t.inner.get_type(), INT_INDEX, index_t.location)?;
                let type_ = if let Type::Array(entry_type) = self.type_table.get_type(lhs_type) {
                    *entry_type
                } else {
                    self.unify_or_err(lhs_t.inner.get_type(), STR_INDEX, lhs_t.location)?;
                    CHAR_INDEX
                };
                Ok(loc!(
                    ExprT::Index {
                        lhs: Box::new(lhs_t),
                        index: Box::new(index_t),
                        type_,
                    },
                    location
                ))
            }
            Expr::TupleField(lhs, index) => {
                let lhs_t = self.expr(*lhs)?;
                let lhs_type = self.type_table.get_type(lhs_t.inner.get_type());
                if let Type::Tuple(entries) = lhs_type {
                    let usize_index: usize = index.try_into().unwrap();
                    if usize_index < entries.len() {
                        let type_ = entries[usize_index];
                        Ok(loc!(
                            ExprT::TupleField(Box::new(lhs_t), index, type_),
                            location
                        ))
                    } else {
                        Err(loc!(
                            TypeError::TupleIndexOutOfBounds {
                                index,
                                tuple_type: type_to_string(
                                    &self.name_table,
                                    &self.type_table,
                                    lhs_t.inner.get_type(),
                                ),
                            },
                            location
                        ))
                    }
                } else {
                    Err(loc!(
                        TypeError::NotATuple {
                            type_: type_to_string(
                                &self.name_table,
                                &self.type_table,
                                lhs_t.inner.get_type(),
                            ),
                        },
                        location
                    ))
                }
            }
            Expr::Field(lhs, name) => {
                let lhs_t = self.expr(*lhs)?;
                let type_ = self.type_table.get_type(lhs_t.inner.get_type());
                if let Type::Record(_, fields) = type_ {
                    let position = fields
                        .iter()
                        .position(|(field_name, _)| *field_name == name);

                    if let Some(pos) = position {
                        Ok(Loc {
                            location,
                            inner: ExprT::TupleField(
                                Box::new(lhs_t),
                                pos.try_into().unwrap(),
                                fields[pos].1,
                            ),
                        })
                    } else {
                        let name_str = self.name_table.get_str(&name);
                        Err(loc!(
                            TypeError::FieldDoesNotExist {
                                name: name_str.to_string(),
                                record: type_to_string(
                                    &self.name_table,
                                    &self.type_table,
                                    lhs_t.inner.get_type()
                                )
                            },
                            location
                        ))
                    }
                } else {
                    Err(loc!(
                        TypeError::NotARecord {
                            type_: type_to_string(
                                &self.name_table,
                                &self.type_table,
                                lhs_t.inner.get_type(),
                            ),
                        },
                        location
                    ))
                }
            }
        }
    }

    fn op(&mut self, op: &Op, lhs_type: TypeId, rhs_type: TypeId) -> Option<TypeId> {
        match op {
            Op::Plus | Op::Minus | Op::Times | Op::Div => {
                let lhs_is_unifiable = self.is_unifiable(lhs_type, INT_INDEX);
                let rhs_is_unifiable = self.is_unifiable(rhs_type, INT_INDEX);
                if lhs_is_unifiable && rhs_is_unifiable {
                    Some(INT_INDEX)
                } else if lhs_type == FLOAT_INDEX && rhs_type == INT_INDEX {
                    Some(FLOAT_INDEX)
                } else if lhs_type == INT_INDEX && rhs_type == FLOAT_INDEX {
                    Some(FLOAT_INDEX)
                } else if lhs_type == FLOAT_INDEX && rhs_type == FLOAT_INDEX {
                    Some(FLOAT_INDEX)
                } else {
                    None
                }
            }
            Op::BangEqual | Op::EqualEqual => {
                if self.is_unifiable(lhs_type, rhs_type) {
                    Some(BOOL_INDEX)
                } else {
                    None
                }
            }
            Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
                if (self.is_unifiable(lhs_type, INT_INDEX)
                    && self.is_unifiable(rhs_type, INT_INDEX))
                    || (self.is_unifiable(lhs_type, FLOAT_INDEX)
                        && self.is_unifiable(rhs_type, FLOAT_INDEX))
                {
                    Some(BOOL_INDEX)
                } else {
                    None
                }
            }
            Op::LogicalAnd | Op::LogicalOr => {
                if self.is_unifiable(lhs_type, BOOL_INDEX)
                    && self.is_unifiable(rhs_type, BOOL_INDEX)
                {
                    Some(BOOL_INDEX)
                } else {
                    None
                }
            }
        }
    }

    fn unify_type_vectors(
        &mut self,
        type_vector1: &[TypeId],
        type_vector2: &[TypeId],
    ) -> Option<Vec<TypeId>> {
        if type_vector1.len() != type_vector2.len() {
            return None;
        }
        let mut types = Vec::new();
        for (t1, t2) in type_vector1.iter().zip(type_vector2.iter()) {
            if let Some(t) = self.unify(*t1, *t2) {
                types.push(t)
            } else {
                return None;
            }
        }
        Some(types)
    }

    fn unify<'a>(&mut self, type_id1: TypeId, type_id2: TypeId) -> Option<TypeId> {
        if type_id1 == type_id2 {
            return Some(type_id1);
        }
        let type1 = self.type_table.get_type(type_id1).clone();
        let type2 = self.type_table.get_type(type_id2).clone();
        match (type1, type2) {
            (Type::Record(name, fields), Type::Record(other_name, other_fields)) => {
                if fields.len() != other_fields.len() || name != other_name {
                    return None;
                }
                let mut unified_fields = Vec::new();
                for ((n1, t1), (n2, t2)) in fields.iter().zip(other_fields.iter()) {
                    if *n1 != *n2 {
                        return None;
                    }
                    if let Some(t) = self.unify(*t1, *t2) {
                        unified_fields.push((*n1, t));
                    } else {
                        return None;
                    }
                }
                let id = self.type_table.insert(Type::Record(name, unified_fields));
                self.type_table.update(type_id1, Type::Solved(id));
                self.type_table.update(type_id2, Type::Solved(id));
                Some(id)
            }
            (Type::Tuple(ts), Type::Unit) | (Type::Unit, Type::Tuple(ts)) => {
                if ts.is_empty() {
                    Some(type_id1)
                } else {
                    None
                }
            }
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if let Some(types) = self.unify_type_vectors(&t1, &t2) {
                    let id = self.type_table.insert(Type::Tuple(types));
                    Some(id)
                } else {
                    None
                }
            }
            (Type::Tuple(t1), _) => {
                if t1.len() == 1 {
                    self.unify(t1[0], type_id2)
                } else {
                    None
                }
            }
            (_, Type::Tuple(t2)) => {
                if t2.len() == 1 {
                    self.unify(type_id1, t2[0])
                } else {
                    None
                }
            }
            (Type::Arrow(param_type1, return_type1), Type::Arrow(param_type2, return_type2)) => {
                match (
                    self.unify(param_type1, param_type2),
                    self.unify(return_type1, return_type2),
                ) {
                    (Some(param_type), Some(return_type)) => {
                        let id = self.type_table.insert(Type::Arrow(param_type, return_type));
                        Some(id)
                    }
                    _ => None,
                }
            }
            (Type::Int, Type::Int) => Some(INT_INDEX),
            (Type::Int, Type::Bool) => Some(type_id1),
            (Type::Bool, Type::Int) => Some(type_id2),
            (Type::Var(_), _) => {
                self.type_table.update(type_id1, Type::Solved(type_id2));
                Some(type_id2)
            }
            (_, Type::Var(_)) => {
                self.type_table.update(type_id2, Type::Solved(type_id1));
                Some(type_id1)
            }
            (Type::Solved(t1), _) => self.unify(t1, type_id2),
            (_, Type::Solved(t2)) => self.unify(type_id1, t2),
            (Type::Array(t1), Type::Array(t2)) => {
                if let Some(t) = self.unify(t1, t2) {
                    let new_type = self.type_table.insert(Type::Array(t));
                    self.type_table.update(type_id1, Type::Solved(new_type));
                    self.type_table.update(type_id2, Type::Solved(new_type));
                    Some(new_type)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn is_unifiable(&mut self, type1: TypeId, type2: TypeId) -> bool {
        self.unify(type1, type2).is_some()
    }

    fn unify_or_err(
        &mut self,
        type1: TypeId,
        type2: TypeId,
        location: LocationRange,
    ) -> Result<TypeId, Loc<TypeError>> {
        self.unify(type1, type2).ok_or_else(|| {
            loc!(
                TypeError::UnificationFailure {
                    type1: type_to_string(&self.name_table, &self.type_table, type1),
                    type2: type_to_string(&self.name_table, &self.type_table, type2),
                },
                location
            )
        })
    }
}
