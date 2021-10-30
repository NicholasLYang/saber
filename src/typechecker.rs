use crate::ast::{
    Accessor, BuiltInTypes, Expr, ExprT, Function, FunctionId, Loc, Name, Op, OpT, Pat, Program,
    ProgramT, Stmt, StmtT, Target, Type, TypeDef, TypeId, TypeSig, UnaryOp, Value,
};
use crate::lexer::LocationRange;
use crate::loc;
use crate::printer::type_to_string;
use crate::symbol_table::{FunctionInfo, SymbolTable};
use crate::utils::{get_final_type, NameTable};
use id_arena::Arena;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::mem::{replace, take};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeError {
    VarNotDefined {
        name: String,
    },
    OpFailure {
        op: Op,
        lhs_type: String,
        rhs_type: String,
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
    NotAnArray {
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
    InvalidAsgnTarget,
    NoLoopBreak,
    NoElseInIfExpr,
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
            TypeError::NotAnArray { type_ } => write!(f, "Type {} is not an array", type_),
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
            TypeError::InvalidAsgnTarget => write!(f, "Invalid left hand side of an assignment"),
            TypeError::NoLoopBreak => write!(f, "Cannot break when not in loop"),
            // TODO: Make this intelligible to normal human beings
            TypeError::NoElseInIfExpr => {
                write!(f, "No else branch in if expression, means that type is (). How about you use it as a statement?")
            }
        }
    }
}

pub struct TypeChecker {
    symbol_table: SymbolTable,
    functions: HashMap<FunctionId, Loc<Function>>,
    // Standard types like int, bool, etc.
    builtin_types: BuiltInTypes,
    // Type names. Right now just has the primitives like string,
    // integer, float, char
    type_names: HashMap<Name, TypeId>,
    // The return type for the typing context
    return_type: Option<TypeId>,
    // Index for type variable names
    type_var_index: usize,
    // Type arena
    type_arena: Arena<Type>,
    // Symbol table
    name_table: NameTable,
    // Struct types
    struct_types: Vec<(Name, TypeId)>,
    // Expr function index. Used for codegen
    expr_func_index: usize,
    // TODO: Allow for more complicated loop
    // breaking patterns
    is_in_loop: bool,
}

fn build_type_names(
    name_table: &mut NameTable,
    builtin_types: &BuiltInTypes,
) -> HashMap<Name, TypeId> {
    let primitive_types = vec![
        ("int", builtin_types.int),
        ("float", builtin_types.float),
        ("char", builtin_types.char),
        ("string", builtin_types.string),
        ("bool", builtin_types.bool),
    ];
    let mut type_names = HashMap::new();
    for (name, type_id) in primitive_types {
        let name_id = name_table.insert(name.to_string());
        type_names.insert(name_id, type_id);
    }
    type_names
}

impl TypeChecker {
    pub fn new(mut name_table: NameTable) -> TypeChecker {
        let mut symbol_table = SymbolTable::new();
        let mut type_arena = Arena::<Type>::new();
        let builtin_types = BuiltInTypes::new(&mut type_arena);
        let type_names = build_type_names(&mut name_table, &builtin_types);

        TypeChecker {
            symbol_table,
            builtin_types,
            type_names,
            return_type: None,
            type_var_index: 0,
            functions: HashMap::new(),
            type_arena,
            name_table,
            struct_types: Vec::new(),
            expr_func_index: 0,
            is_in_loop: false,
        }
    }

    pub fn get_tables(self) -> (SymbolTable, NameTable, Arena<Type>, BuiltInTypes) {
        (
            self.symbol_table,
            self.name_table,
            self.type_arena,
            self.builtin_types,
        )
    }

    #[allow(dead_code)]
    pub fn get_name_table(&self) -> &NameTable {
        &self.name_table
    }

    pub fn get_fresh_type_var(&mut self) -> TypeId {
        let type_var = Type::Var(self.type_var_index);
        self.type_var_index += 1;
        self.type_arena.alloc(type_var)
    }

    fn generate_field_info<'a, I>(&self, fields: I) -> Vec<bool>
    where
        I: Iterator<Item = &'a TypeId>,
    {
        // Stores whether or not a field is a reference
        let mut field_info = Vec::new();
        for field_type in fields {
            field_info.push(self.type_arena[*field_type].is_ref_type())
        }
        field_info
    }

    fn generate_type_info(&self, type_id: TypeId) -> Option<(TypeId, Vec<bool>)> {
        match &self.type_arena[type_id] {
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

    pub fn generate_runtime_type_info(&self) -> HashMap<usize, Vec<bool>> {
        let mut type_info = HashMap::new();
        for (_, type_id) in &self.struct_types {
            self.generate_type_info(*type_id)
                .map(|(type_id, field_info)| type_info.insert(type_id.index(), field_info));
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
        if let Err(err) = self.hoist_functions(&program.stmts) {
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
        let mut functions = vec![None; self.symbol_table.get_function_index()];

        let functions_map = replace(&mut self.functions, HashMap::new());
        for (func_index, function) in functions_map {
            functions[func_index] = Some(function);
        }

        ProgramT {
            stmts: typed_stmts,
            functions: functions.into_iter().map(|f| f.unwrap()).collect(),
            named_types,
            errors,
        }
    }

    // Reads functions defined in this block and inserts them into symbol table.
    fn hoist_functions(&mut self, stmts: &[Loc<Stmt>]) -> Result<(), Loc<TypeError>> {
        for stmt in stmts {
            if let Stmt::Function(func_name, params, return_type_sig, _) = &stmt.inner {
                let params_type = self.pat(params)?;
                let return_type = if let Some(type_sig) = return_type_sig {
                    self.lookup_type_sig(type_sig)?
                } else {
                    self.get_fresh_type_var()
                };
                let type_ = self.type_arena.alloc(Type::Arrow(params_type, return_type));
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
                let type_id = self.type_arena.alloc(Type::Record(name, typed_fields));
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
        cond: Loc<Expr>,
        then_block: Loc<Expr>,
        else_block: Option<Box<Loc<Expr>>>,
    ) -> Result<Loc<StmtT>, Loc<TypeError>> {
        let cond_t = self.expr(cond)?;

        self.unify_or_err(
            cond_t.inner.get_type(),
            self.builtin_types.bool,
            cond_t.location,
        )?;

        let then_t = self.expr(then_block)?;
        let else_t = if let Some(else_block) = else_block {
            let else_t = self.expr(*else_block)?;
            self.unify_or_err(
                else_t.inner.get_type(),
                self.builtin_types.unit,
                else_t.location,
            )?;
            Some(else_t)
        } else {
            None
        };

        self.unify_or_err(
            then_t.inner.get_type(),
            self.builtin_types.unit,
            then_t.location,
        )?;

        Ok(loc!(
            StmtT::If {
                cond: Box::new(cond_t),
                then_block: Box::new(then_t),
                else_block: else_t.map(Box::new)
            },
            location
        ))
    }

    pub fn stmt(&mut self, stmt: Loc<Stmt>) -> Result<Vec<Loc<StmtT>>, Loc<TypeError>> {
        let location = stmt.location;
        match stmt.inner {
            Stmt::Expr(expr) => {
                if let Expr::If(cond, then_block, else_block) = expr.inner {
                    Ok(vec![self.if_expr_stmt(
                        location,
                        *cond,
                        *then_block,
                        else_block,
                    )?])
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
                let func_index = func_info.func_index;
                let function = self.function(func_name, params, *body)?;
                self.functions.insert(func_index, loc!(function, location));

                // If we have a return type, we're inside a function and need to have a captures struct.
                // Otherwise we're at the top level and can ignore it (for now)
                if self.return_type.is_some() {
                    Ok(vec![Loc {
                        location,
                        inner: StmtT::Function { func_index },
                    }])
                } else {
                    Ok(Vec::new())
                }
            }
            Stmt::Let(pat, rhs) => Ok(self.let_binding(pat, rhs, location)?),
            Stmt::Break => {
                if self.is_in_loop {
                    Ok(vec![loc!(StmtT::Break, location)])
                } else {
                    Err(loc!(TypeError::NoLoopBreak, location))
                }
            }
            Stmt::Loop(block) => {
                let old_is_in_loop = self.is_in_loop;
                self.is_in_loop = true;
                let block_t = self.expr(block)?;
                self.is_in_loop = old_is_in_loop;
                self.unify_or_err(block_t.inner.get_type(), self.builtin_types.unit, location)?;
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
                type_: self.builtin_types.int,
            },
            Value::Float(_f) => ExprT::Primary {
                value,
                type_: self.builtin_types.float,
            },
            Value::Bool(_b) => ExprT::Primary {
                value,
                type_: self.builtin_types.bool,
            },
            Value::String(s) => ExprT::Primary {
                value: Value::String(s),
                type_: self.builtin_types.string,
            },
        }
    }

    fn lookup_type_sig(&mut self, sig: &Loc<TypeSig>) -> Result<TypeId, Loc<TypeError>> {
        match &sig.inner {
            TypeSig::Array(sig) => {
                let type_ = self.lookup_type_sig(sig)?;
                Ok(self.type_arena.alloc(Type::Array(type_)))
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
            TypeSig::Empty => Ok(self.builtin_types.unit),
            TypeSig::Arrow(param_sigs, return_sig) => {
                let param_types = if param_sigs.len() == 1 {
                    self.lookup_type_sig(&param_sigs[0])?
                } else {
                    let mut types = Vec::new();
                    for param in param_sigs {
                        types.push(self.lookup_type_sig(param)?);
                    }
                    self.type_arena.alloc(Type::Tuple(types))
                };

                let return_type = self.lookup_type_sig(return_sig)?;
                Ok(self.type_arena.alloc(Type::Arrow(param_types, return_type)))
            }
        }
    }

    fn pat(&mut self, pat: &Pat) -> Result<TypeId, Loc<TypeError>> {
        match pat {
            Pat::Id(_, Some(type_sig), _) => {
                let type_ = self.lookup_type_sig(type_sig)?;
                Ok(type_)
            }
            Pat::Id(_, None, _) => {
                let type_ = self.get_fresh_type_var();
                Ok(type_)
            }
            Pat::Tuple(pats, _) => {
                let types: Result<Vec<_>, _> = pats.iter().map(|pat| self.pat(pat)).collect();
                Ok(self.type_arena.alloc(Type::Tuple(types?)))
            }
            Pat::Record(pats, type_sig, _) => {
                // In the future I should unify these two if they both exist.
                if let Some(type_sig) = type_sig {
                    self.lookup_type_sig(type_sig)
                } else {
                    let types: Result<Vec<_>, _> = pats
                        .iter()
                        .map(|name| Ok((*name, self.get_fresh_type_var())))
                        .collect();
                    Ok(self
                        .type_arena
                        .alloc(Type::Record(self.name_table.get_fresh_name(), types?)))
                }
            }
            Pat::Empty(_) => Ok(self.builtin_types.unit),
        }
    }

    /// Gets the function parameters as a list of names with
    /// types. Useful for WebAssembly code generation.
    fn get_func_params(&mut self, pat: &Pat) -> Result<Vec<(Name, TypeId)>, Loc<TypeError>> {
        match pat {
            Pat::Id(name, Some(type_sig), _) => {
                let type_ = self.lookup_type_sig(type_sig)?;
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
                    Some(self.lookup_type_sig(type_sig)?)
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
        if let Type::Record(_, fields) = &self.type_arena[record_type] {
            if let Some(pos) = fields.iter().position(|(name, _)| *name == field_name) {
                let (_, type_) = fields[pos];
                Ok((pos.try_into().unwrap(), type_))
            } else {
                Err(loc!(
                    TypeError::FieldDoesNotExist {
                        record: type_to_string(&self.name_table, &self.type_arena, record_type),
                        name: self.name_table.get_str(&field_name).to_string(),
                    },
                    location
                ))
            }
        } else {
            Err(loc!(
                TypeError::NotARecord {
                    type_: type_to_string(&self.name_table, &self.type_arena, record_type),
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
                    self.symbol_table.insert_var(*name, type_);
                    bindings.push(Loc {
                        location,
                        inner: StmtT::Let(
                            *name,
                            Loc {
                                location,
                                inner: ExprT::TupleField(
                                    Box::new(Loc {
                                        location,
                                        inner: ExprT::Var {
                                            name: owner_name,
                                            type_: rhs_type,
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
                        inner: StmtT::Let(
                            name,
                            Loc {
                                location,
                                inner: ExprT::TupleField(
                                    Box::new(Loc {
                                        location,
                                        inner: ExprT::Var {
                                            name: owner_name,
                                            type_: rhs_type,
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

    fn let_binding(
        &mut self,
        pat: Pat,
        rhs: Loc<Expr>,
        location: LocationRange,
    ) -> Result<Vec<Loc<StmtT>>, Loc<TypeError>> {
        let typed_rhs = self.expr(rhs)?;
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
        self.symbol_table
            .insert_var(name, typed_rhs.inner.get_type());
        let mut pat_bindings =
            self.generate_pattern_bindings(&pat, name, typed_rhs.inner.get_type(), location)?;
        let mut bindings = vec![Loc {
            location,
            inner: StmtT::Let(name, typed_rhs),
        }];
        bindings.append(&mut pat_bindings);
        Ok(bindings)
    }

    fn function(
        &mut self,
        name: Name,
        params: Pat,
        body: Loc<Expr>,
    ) -> Result<Function, Loc<TypeError>> {
        let entry = self.symbol_table.lookup_name(name);
        let entry = entry.as_ref().unwrap();
        let func_info = entry.function_info.as_ref().unwrap();
        let scope = func_info.func_scope;
        let return_type = func_info.return_type;

        self.symbol_table.swap_scope(scope);
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
        let return_type = if body_type != self.builtin_types.unit {
            self.unify_or_err(old_return_type.unwrap(), body_type, body_location)?
        } else {
            old_return_type.unwrap()
        };

        let scope_index = self.symbol_table.restore_scope();

        Ok(Function {
            params: func_params,
            body: Box::new(body),
            scope_index,
            return_type,
        })
    }

    fn asgn_lhs(&mut self, lhs: Loc<Expr>) -> Result<(Loc<Target>, TypeId), Loc<TypeError>> {
        let location = lhs.location;
        match lhs.inner {
            Expr::Var { name } => {
                let entry = self.symbol_table.lookup_name(name).ok_or(loc!(
                    TypeError::VarNotDefined {
                        name: self.name_table.get_str(&name).to_string(),
                    },
                    location
                ))?;

                Ok((
                    loc!(
                        Target {
                            ident: name,
                            accessors: Vec::new()
                        },
                        location
                    ),
                    entry.var_type,
                ))
            }
            Expr::Field(lhs, field_name) => {
                let (mut target, lhs_type) = self.asgn_lhs(*lhs)?;
                let (field_index, field_type) =
                    self.get_field_in_type(lhs_type, field_name, location)?;
                target
                    .inner
                    .accessors
                    .push(loc!(Accessor::Field(field_index), location));
                Ok((target, field_type))
            }
            Expr::Index { lhs, index } => {
                let (mut target, lhs_type) = self.asgn_lhs(*lhs)?;
                if let Type::Array(item_type) = &self.type_arena[lhs_type] {
                    let item_type = *item_type;
                    let index_t = self.expr(*index)?;
                    target
                        .inner
                        .accessors
                        .push(loc!(Accessor::Index(index_t), location));
                    Ok((target, item_type))
                } else {
                    Err(loc!(
                        TypeError::NotAnArray {
                            type_: type_to_string(&self.name_table, &self.type_arena, lhs_type)
                        },
                        location
                    ))
                }
            }
            _ => Err(loc!(TypeError::InvalidAsgnTarget, location)),
        }
    }

    fn expr(&mut self, expr: Loc<Expr>) -> Result<Loc<ExprT>, Loc<TypeError>> {
        let location = expr.location;
        match expr.inner {
            Expr::Asgn { lhs, rhs } => {
                let (target, target_type) = self.asgn_lhs(*lhs)?;
                let rhs_t = self.expr(*rhs)?;
                let type_ = self.unify_or_err(target_type, rhs_t.inner.get_type(), location)?;
                Ok(loc!(
                    ExprT::Asgn {
                        lhs: target,
                        rhs: Box::new(rhs_t),
                        type_
                    },
                    location
                ))
            }
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
                    Some((op, type_)) => Ok(Loc {
                        location,
                        inner: ExprT::BinOp {
                            op,
                            lhs: Box::new(typed_lhs),
                            rhs: Box::new(typed_rhs),
                            type_,
                        },
                    }),
                    None => {
                        let lhs_type = type_to_string(
                            &self.name_table,
                            &self.type_arena,
                            typed_lhs.inner.get_type(),
                        );
                        let rhs_type = type_to_string(
                            &self.name_table,
                            &self.type_arena,
                            typed_rhs.inner.get_type(),
                        );
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
                let type_id = self.type_arena.alloc(Type::Tuple(types));
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
                let array_type = self.type_arena.alloc(Type::Array(entry_type));
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
                let type_ = self.type_arena.alloc(Type::Arrow(params_type, return_type));
                let func_index =
                    self.symbol_table
                        .insert_function(name, params_type, return_type, type_);

                let function = self.function(name, params, *body)?;

                self.functions.insert(func_index, loc!(function, location));

                self.expr_func_index += 1;
                Ok(loc!(ExprT::Function { func_index, type_ }, location))
            }
            Expr::UnaryOp { op, rhs } => {
                let typed_rhs = self.expr(*rhs)?;
                let rhs_type = typed_rhs.inner.get_type();
                let is_valid_types = match op {
                    UnaryOp::Minus => {
                        self.is_unifiable(rhs_type, self.builtin_types.int)
                            || self.is_unifiable(rhs_type, self.builtin_types.float)
                    }
                    UnaryOp::Not => self.is_unifiable(rhs_type, self.builtin_types.bool),
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
                let typed_args = self.expr(*args)?;

                if let Expr::Var { name } = &callee.inner {
                    if self.name_table.get_str(name) == "print" {
                        return Ok(loc!(
                            ExprT::Print {
                                args: Box::new(typed_args),
                                type_: self.builtin_types.unit
                            },
                            location
                        ));
                    } else if let Some(entry) = self.symbol_table.lookup_name(*name) {
                        if let Some(FunctionInfo {
                            func_index,
                            func_scope: _,
                            params_type,
                            return_type,
                            is_top_level: _,
                        }) = entry.function_info
                        {
                            self.unify_or_err(params_type, typed_args.inner.get_type(), location)?;
                            return Ok(Loc {
                                location,
                                inner: ExprT::DirectCall {
                                    callee: func_index,
                                    args: Box::new(typed_args),
                                    type_: return_type,
                                },
                            });
                        }
                    }
                }

                let typed_callee = self.expr(*callee)?;
                let callee_type = typed_callee.inner.get_type();
                let (params_type, return_type) = match &self.type_arena[callee_type] {
                    Type::Arrow(params_type, return_type) => (*params_type, *return_type),
                    Type::Var(_) => {
                        let params_type = self.get_fresh_type_var();
                        let return_type = self.get_fresh_type_var();
                        let new_id = self.type_arena.alloc(Type::Arrow(params_type, return_type));
                        self.type_arena[callee_type] = Type::Solved(new_id);
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
                self.hoist_functions(&stmts)?;
                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    typed_stmts.append(&mut self.stmt(stmt)?);
                }
                let (type_, typed_end_expr) = if let Some(expr) = end_expr {
                    let typed_expr = self.expr(*expr)?;
                    (typed_expr.inner.get_type(), Some(Box::new(typed_expr)))
                } else {
                    (self.builtin_types.unit, None)
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
                let else_block = else_block.ok_or(loc!(TypeError::NoElseInIfExpr, location))?;
                let typed_else_block = self.expr(*else_block)?;
                let else_type = typed_else_block.inner.get_type();
                self.unify_or_err(then_type, else_type, location)?;

                Ok(Loc {
                    location,
                    inner: ExprT::If(
                        Box::new(typed_cond),
                        Box::new(typed_then_block),
                        Box::new(typed_else_block),
                        then_type,
                    ),
                })
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
                let expr_type = self.type_arena.alloc(Type::Record(name, field_types));
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
                self.unify_or_err(
                    index_t.inner.get_type(),
                    self.builtin_types.int,
                    index_t.location,
                )?;
                let type_ = if let Type::Array(entry_type) = &self.type_arena[lhs_type] {
                    *entry_type
                } else {
                    self.unify_or_err(
                        lhs_t.inner.get_type(),
                        self.builtin_types.string,
                        lhs_t.location,
                    )?;
                    self.builtin_types.char
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
                let lhs_type = &self.type_arena[lhs_t.inner.get_type()];
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
                                    &self.type_arena,
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
                                &self.type_arena,
                                lhs_t.inner.get_type(),
                            ),
                        },
                        location
                    ))
                }
            }
            Expr::Field(lhs, name) => {
                let lhs_t = self.expr(*lhs)?;
                let (field_index, field_type) =
                    self.get_field_in_type(lhs_t.inner.get_type(), name, location)?;
                Ok(loc!(
                    ExprT::TupleField(Box::new(lhs_t), field_index as u32, field_type),
                    location
                ))
            }
        }
    }

    fn get_field_in_type(
        &self,
        record_type_id: TypeId,
        field_name: Name,
        location: LocationRange,
    ) -> Result<(usize, TypeId), Loc<TypeError>> {
        let record_type = &self.type_arena[record_type_id];
        if let Type::Record(_, fields) = record_type {
            let position = fields.iter().position(|(name, _)| *name == field_name);

            if let Some(pos) = position {
                Ok((pos, fields[pos].1))
            } else {
                let name_str = self.name_table.get_str(&field_name);
                Err(loc!(
                    TypeError::FieldDoesNotExist {
                        name: name_str.to_string(),
                        record: type_to_string(&self.name_table, &self.type_arena, record_type_id)
                    },
                    location
                ))
            }
        } else {
            Err(loc!(
                TypeError::NotARecord {
                    type_: type_to_string(&self.name_table, &self.type_arena, record_type_id),
                },
                location
            ))
        }
    }

    fn op(&mut self, op: &Op, lhs_type: TypeId, rhs_type: TypeId) -> Option<(OpT, TypeId)> {
        let lhs_type = get_final_type(&self.type_arena, lhs_type);
        let rhs_type = get_final_type(&self.type_arena, rhs_type);

        if self.is_unifiable(lhs_type, self.builtin_types.int)
            && self.is_unifiable(rhs_type, self.builtin_types.int)
        {
            match op {
                Op::Plus => Some((OpT::I32Add, self.builtin_types.int)),
                Op::Minus => Some((OpT::I32Sub, self.builtin_types.int)),
                Op::Times => Some((OpT::I32Mul, self.builtin_types.int)),
                Op::Div => Some((OpT::I32Div, self.builtin_types.int)),
                Op::BangEqual => Some((OpT::I32NotEqual, self.builtin_types.bool)),
                Op::EqualEqual => Some((OpT::I32Equal, self.builtin_types.bool)),
                Op::Greater => Some((OpT::I32Greater, self.builtin_types.bool)),
                Op::GreaterEqual => Some((OpT::I32GreaterEqual, self.builtin_types.bool)),
                Op::Less => Some((OpT::I32Less, self.builtin_types.bool)),
                Op::LessEqual => Some((OpT::I32LessEqual, self.builtin_types.bool)),
                Op::LogicalAnd => Some((OpT::I32And, self.builtin_types.int)),
                Op::LogicalOr => Some((OpT::I32Or, self.builtin_types.int)),
                _ => None,
            }
        } else if self.is_unifiable(lhs_type, self.builtin_types.float)
            || self.is_unifiable(rhs_type, self.builtin_types.float)
        {
            match op {
                Op::Plus => Some((OpT::F32Add, self.builtin_types.float)),
                Op::Minus => Some((OpT::F32Sub, self.builtin_types.float)),
                Op::Times => Some((OpT::F32Mul, self.builtin_types.float)),
                Op::Div => Some((OpT::F32Div, self.builtin_types.float)),
                Op::BangEqual => Some((OpT::F32NotEqual, self.builtin_types.bool)),
                Op::EqualEqual => Some((OpT::F32Equal, self.builtin_types.bool)),
                Op::Greater => Some((OpT::F32Greater, self.builtin_types.bool)),
                Op::GreaterEqual => Some((OpT::F32GreaterEqual, self.builtin_types.bool)),
                Op::Less => Some((OpT::F32Less, self.builtin_types.bool)),
                Op::LessEqual => Some((OpT::F32LessEqual, self.builtin_types.bool)),
                _ => None,
            }
        } else if self.is_unifiable(lhs_type, self.builtin_types.string)
            && self.is_unifiable(rhs_type, self.builtin_types.string)
        {
            match op {
                Op::EqualEqual => Some((OpT::StringEqual, self.builtin_types.bool)),
                Op::Plus => Some((OpT::StringConcat, self.builtin_types.string)),
                _ => None,
            }
        } else {
            None
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

    fn unify(&mut self, type_id1: TypeId, type_id2: TypeId) -> Option<TypeId> {
        if type_id1 == type_id2 {
            return Some(type_id1);
        }
        // TODO: Figure out how bad these clones are perf-wise
        let type1 = &self.type_arena[type_id1].clone();
        let type2 = &self.type_arena[type_id2].clone();
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
                let id = self.type_arena.alloc(Type::Record(*name, unified_fields));
                self.type_arena[type_id1] = Type::Solved(id);
                self.type_arena[type_id2] = Type::Solved(id);
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
                if let Some(types) = self.unify_type_vectors(t1, t2) {
                    let id = self.type_arena.alloc(Type::Tuple(types));
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
                    self.unify(*param_type1, *param_type2),
                    self.unify(*return_type1, *return_type2),
                ) {
                    (Some(param_type), Some(return_type)) => {
                        let id = self.type_arena.alloc(Type::Arrow(param_type, return_type));
                        Some(id)
                    }
                    _ => None,
                }
            }
            (Type::Integer, Type::Integer) => Some(self.builtin_types.int),
            (Type::Integer, Type::Bool) => Some(type_id1),
            (Type::Bool, Type::Integer) => Some(type_id2),
            (Type::Var(_), _) => {
                self.type_arena[type_id1] = Type::Solved(type_id2);
                Some(type_id2)
            }
            (_, Type::Var(_)) => {
                self.type_arena[type_id2] = Type::Solved(type_id1);
                Some(type_id1)
            }
            (Type::Solved(t1), _) => self.unify(*t1, type_id2),
            (_, Type::Solved(t2)) => self.unify(type_id1, *t2),
            (Type::Array(t1), Type::Array(t2)) => {
                if let Some(t) = self.unify(*t1, *t2) {
                    let new_type = self.type_arena.alloc(Type::Array(t));
                    self.type_arena[type_id1] = Type::Solved(new_type);
                    self.type_arena[type_id2] = Type::Solved(new_type);
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
                    type1: type_to_string(&self.name_table, &self.type_arena, type1),
                    type2: type_to_string(&self.name_table, &self.type_arena, type2),
                },
                location
            )
        })
    }
}
