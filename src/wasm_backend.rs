use crate::ast::Name;
use crate::mir;
use crate::mir::{MirCompiler, UnaryOp};
use crate::utils::NameTable;
use std::collections::HashMap;
use walrus::{
    ActiveData, ActiveDataLocation, DataKind, FunctionBuilder, FunctionId, LocalId, MemoryId,
    Module, ValType,
};

struct WasmFunction {
    pub builder: FunctionBuilder,
    pub args: Vec<LocalId>,
    // TODO: We should optimize so that we don't need one local
    //   per instruction. We can go through instructions, see
    //   what instructions rely on past ones and if it's only one
    //   instruction, we make it a stack immediate.
    pub instruction_locals: Vec<Vec<Option<LocalId>>>,
    pub var_locals: HashMap<usize, LocalId>,
    export_name: Option<Name>,
}

pub struct WasmBackend {
    module: Module,
    memory_id: MemoryId,
    string_ptrs: Vec<u32>,
    name_table: NameTable,
    wasm_functions: Vec<WasmFunction>,
    current_function: Option<usize>,
    current_block: Option<usize>,
    print_int: FunctionId,
    print_float: FunctionId,
    print_pointer: FunctionId,
}

impl Into<ValType> for mir::Type {
    fn into(self) -> ValType {
        match self {
            mir::Type::I32 | mir::Type::Pointer => ValType::I32,
            mir::Type::F32 => ValType::F32,
        }
    }
}

impl Into<ValType> for &mir::Type {
    fn into(self) -> ValType {
        match self {
            mir::Type::I32 | mir::Type::Pointer => ValType::I32,
            mir::Type::F32 => ValType::F32,
        }
    }
}

impl mir::Block {
    // Go down the block and check if which instructions need local variables versus which
    // ones are just stack immediates
    pub fn make_instruction_locals(&self, module: &mut Module) -> Vec<Option<LocalId>> {
        let mut locals = vec![None; self.0.len()];
        for (idx, instr) in self.0.iter().enumerate() {
            match &instr.kind {
                mir::InstructionKind::If(id, _, _)
                | mir::InstructionKind::Unary(_, id)
                | mir::InstructionKind::Return(id) => {
                    if id.0 != idx - 1 {
                        let ty = self.0.get(id.0).unwrap().ty;
                        locals[id.0] = ty.map(|ty| module.locals.add(ty.into()))
                    }
                }
                mir::InstructionKind::Binary(_, lhs, rhs) => {
                    // We need the lhs and the rhs to be the latest and next-to-latest
                    // instructions if we want to avoid locals
                    if lhs.0 != idx - 1 || rhs.0 != idx - 2 {
                        let lhs_ty = self.0.get(lhs.0).unwrap().ty;
                        locals[lhs.0] = lhs_ty.map(|ty| module.locals.add(ty.into()));
                        let rhs_ty = self.0.get(rhs.0).unwrap().ty;
                        locals[rhs.0] = rhs_ty.map(|ty| module.locals.add(ty.into()))
                    }
                }
                mir::InstructionKind::VarSet(_, rhs_id) => {
                    let ty = self.0.get(rhs_id.0).unwrap().ty;
                    locals[rhs_id.0] = ty.map(|t| module.locals.add(t.into()));
                }
                mir::InstructionKind::VarGet(_)
                | mir::InstructionKind::Primary(_)
                | mir::InstructionKind::Noop
                | mir::InstructionKind::Br(_)
                | mir::InstructionKind::Alloc(_) => {
                    // Surprisingly with InstructionKind::VarSet we don't do anything because
                    // there's no case where we have to save an instruction, do something else, then set
                    // it to a variable
                }
                mir::InstructionKind::Call(_, args) => {
                    for (arg_idx, arg) in args.iter().enumerate() {
                        let arg_instr_idx = idx - args.len() + arg_idx;
                        if arg_instr_idx == arg.0 {
                            let arg_ty = self.0.get(arg_instr_idx).unwrap().ty;
                            locals[arg_instr_idx] = arg_ty.map(|ty| module.locals.add(ty.into()));
                        }
                    }
                }
            }
        }

        locals
    }
}

impl WasmFunction {
    pub fn new(
        module: &mut Module,
        params: &[ValType],
        returns: &[ValType],
        body: &[mir::Block],
        export_name: Option<Name>,
    ) -> Self {
        let mut args = Vec::new();

        for param in params {
            args.push(module.locals.add(*param));
        }

        let mut locals = Vec::new();

        for block in body {
            locals.push(block.make_instruction_locals(module));
        }

        WasmFunction {
            builder: FunctionBuilder::new(&mut module.types, params, returns),
            instruction_locals: locals,
            var_locals: HashMap::new(),
            args,
            export_name,
        }
    }
}

impl WasmBackend {
    pub fn new(mir_compiler: MirCompiler) -> Self {
        let MirCompiler {
            string_literals,
            name_table,
            ..
        } = mir_compiler;

        let mut module = Module::default();
        let memory_id = module.memories.add_local(false, 1, None);

        let string_ptrs =
            Self::add_strings_to_data_section(memory_id, &mut module, string_literals);

        let print_int_type = module.types.add(&[ValType::I32], &[]);
        let print_float_type = module.types.add(&[ValType::F32], &[]);
        let print_pointer_type = module.types.add(&[ValType::I32], &[]);
        let (print_int, _) = module.add_import_func("std", "print_int", print_int_type);
        let (print_float, _) = module.add_import_func("std", "print_float", print_float_type);
        let (print_pointer, _) = module.add_import_func("std", "print_pointer", print_pointer_type);
        module.exports.add("memory", memory_id);

        WasmBackend {
            module,
            memory_id,
            string_ptrs,
            name_table,
            wasm_functions: Vec::new(),
            current_function: None,
            current_block: None,
            print_int,
            print_float,
            print_pointer,
        }
    }

    pub fn generate_program(mut self, program: mir::Program) -> Vec<u8> {
        for func in &program.functions {
            let params: Vec<_> = func.params.iter().map(|p| p.into()).collect();
            let returns: Vec<_> = func.returns.iter().map(|p| p.into()).collect();

            self.wasm_functions.push(WasmFunction::new(
                &mut self.module,
                &params,
                &returns,
                &func.body,
                func.export_name,
            ))
        }

        for (idx, func) in program.functions.into_iter().enumerate() {
            self.current_function = Some(idx);
            self.generate_function(func)
        }

        for func in self.wasm_functions {
            let func_id = func.builder.finish(func.args, &mut self.module.funcs);

            if let Some(name) = func.export_name {
                let name = self.name_table.get_str(&name);
                self.module.exports.add(name, func_id);
            }
        }

        self.module.emit_wasm()
    }

    // Adds strings to data section and returns vector of pointers to memory
    fn add_strings_to_data_section(
        memory: MemoryId,
        module: &mut Module,
        strings: Vec<String>,
    ) -> Vec<u32> {
        let mut data_start = 0;
        let mut ptrs = Vec::new();

        for s in strings {
            let len = s.len();
            let mut bytes: Vec<u8> = vec![0, 0, 0, 0];
            let len_as_32 = len as u32;
            bytes.append(&mut len_as_32.to_le_bytes().to_vec());
            bytes.append(&mut s.into_bytes());

            module.data.add(
                DataKind::Active(ActiveData {
                    memory,
                    location: ActiveDataLocation::Absolute(data_start),
                }),
                bytes,
            );
            ptrs.push(data_start);
            data_start += len as u32;
        }

        ptrs
    }

    fn generate_function(&mut self, function: mir::Function) {
        self.current_block = Some(0);
        for (idx, instr) in function.body[0].0.iter().enumerate() {
            let fn_idx = self.current_function.unwrap();
            match &instr.kind {
                mir::InstructionKind::Primary(primary) => self.generate_primary(primary, idx),
                mir::InstructionKind::Unary(op, rhs) => self.generate_unary(*op, *rhs),
                mir::InstructionKind::VarSet(var_index, idx) => {
                    let var_local = self.get_local(*idx).unwrap();
                    let func = &mut self.wasm_functions[fn_idx];

                    func.var_locals.insert(*var_index, var_local);
                }
                mir::InstructionKind::VarGet(idx) => {
                    let func = &mut self.wasm_functions[fn_idx];
                    let var_local = *func.var_locals.get(idx).unwrap();
                    let mut builder = func.builder.func_body();

                    builder.local_get(var_local);
                }
                mir::InstructionKind::Return(id) => {
                    let local = self.get_local(*id);
                    let func = &mut self.wasm_functions[self.current_function.unwrap()];

                    let mut builder = func.builder.func_body();
                    if let Some(l) = local {
                        builder.local_get(l);
                    }

                    builder.return_();
                }
                _ => {}
            }

            let local = self.wasm_functions[fn_idx].instruction_locals[0][idx];
            let mut main_body = self.wasm_functions[fn_idx].builder.func_body();
            if let Some(id) = local {
                main_body.local_set(id);
            }
        }
    }

    fn get_local(&self, id: mir::InstrId) -> Option<LocalId> {
        let current_function = self.current_function.unwrap();
        let current_block = self.current_block.unwrap();

        self.wasm_functions[current_function].instruction_locals[current_block][id.0]
    }

    fn generate_primary(&mut self, primary: &mir::Primary, local_idx: usize) {
        let func = &mut self.wasm_functions[self.current_function.unwrap()];

        let mut builder = func.builder.func_body();

        match primary {
            mir::Primary::I32(i) => builder.i32_const(*i),
            // TODO: This could mess up in degenerate cases
            mir::Primary::String(idx) => builder.i32_const(self.string_ptrs[*idx] as i32),
            mir::Primary::F32(f) => builder.f32_const(*f),
        };
    }

    fn generate_unary(&mut self, op: mir::UnaryOp, rhs: mir::InstrId) {
        let rhs_local = self.get_local(rhs);
        let func = &mut self.wasm_functions[self.current_function.unwrap()];

        let mut builder = func.builder.func_body();

        if let Some(rhs) = rhs_local {
            builder.local_get(rhs);
        }

        let fn_id = match op {
            UnaryOp::PrintInt => self.print_int,
            UnaryOp::PrintFloat => self.print_float,
            UnaryOp::PrintPointer => self.print_pointer,
            _ => todo!(),
        };

        builder.call(fn_id);
    }
}
