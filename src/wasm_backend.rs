use crate::ast::Name;
use crate::mir;
use crate::mir::compiler::MirCompiler;
use crate::mir::UnaryOp;
use crate::utils::NameTable;
use std::collections::HashMap;
use walrus::ir::{BinaryOp, LoadKind, MemArg, StoreKind};
use walrus::{
    ActiveData, ActiveDataLocation, DataKind, FunctionBuilder, FunctionId, LocalId, MemoryId,
    Module, ValType,
};

pub struct WasmProgram {
    pub code: Vec<u8>,
    pub heap_start: u32,
}

struct WasmFunction {
    pub builder: FunctionBuilder,
    pub args: Vec<LocalId>,
    pub instruction_liveness: Vec<Vec<InstrLiveness>>,
    pub var_locals: HashMap<usize, InstrLiveness>,
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
    print_heap: FunctionId,
    alloc: FunctionId,
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

// Bad name, but we compute if the instruction is either saved in a local,
// a stack immediate. or if it's dead
#[derive(Copy, Clone, Debug)]
pub enum InstrLiveness {
    Saved(LocalId),
    Immediate,
    Dead,
}

impl mir::Block {
    // Get local for instruction. If instruction is an empty type, then we treat it as an Immediate
    fn get_instr_local(&self, module: &mut Module, id: &mir::InstrId) -> InstrLiveness {
        let ty = self.0.get(id.0).unwrap().ty;
        if let Some(ty) = ty {
            InstrLiveness::Saved(module.locals.add(ty.into()))
        } else {
            InstrLiveness::Immediate
        }
    }
    // Go down the block and check if which instructions need local variables versus which
    // ones are just stack immediates
    pub fn make_instruction_liveness(&self, module: &mut Module) -> Vec<InstrLiveness> {
        let mut liveness = vec![InstrLiveness::Dead; self.0.len()];

        for (idx, instr) in self.0.iter().enumerate() {
            match &instr.kind {
                mir::InstructionKind::Unary(op, id) => {
                    if matches!(
                        op,
                        UnaryOp::Drop
                            | UnaryOp::PrintFloat
                            | UnaryOp::PrintInt
                            | UnaryOp::PrintPointer
                    ) {
                        // These instructions are always alive because they have side effects
                        // TODO: Figure out how to track side effects and always keep those
                        //   instructions alive
                        liveness[idx] = InstrLiveness::Immediate;
                    }

                    liveness[id.0] = if id.0 != idx - 1 {
                        self.get_instr_local(module, id)
                    } else {
                        InstrLiveness::Immediate
                    };
                }
                mir::InstructionKind::If(id, _, _) | mir::InstructionKind::Return(id) => {
                    liveness[id.0] = if id.0 != idx - 1 {
                        self.get_instr_local(module, id)
                    } else {
                        InstrLiveness::Immediate
                    };
                }
                mir::InstructionKind::Binary(op, lhs, rhs) => {
                    // We need the lhs and the rhs to be the latest and next-to-latest
                    // instructions if we want to avoid locals
                    if lhs.0 != idx - 1 || rhs.0 != idx - 2 {
                        liveness[lhs.0] = self.get_instr_local(module, lhs);
                        liveness[rhs.0] = self.get_instr_local(module, rhs);
                    } else {
                        liveness[lhs.0] = InstrLiveness::Immediate;
                        liveness[rhs.0] = InstrLiveness::Immediate;
                    }

                    if matches!(
                        op,
                        mir::BinaryOp::I32Store
                            | mir::BinaryOp::PointerStore
                            | mir::BinaryOp::F32Store
                    ) {
                        liveness[idx] = InstrLiveness::Immediate;
                    }
                }
                mir::InstructionKind::VarSet(_, rhs_id) => {
                    liveness[rhs_id.0] = self.get_instr_local(module, rhs_id);
                    liveness[idx] = InstrLiveness::Immediate;
                }
                mir::InstructionKind::VarGet(_)
                | mir::InstructionKind::Primary(_)
                | mir::InstructionKind::Noop
                | mir::InstructionKind::Br(_)
                | mir::InstructionKind::Alloc(_) => {}
                mir::InstructionKind::Call(_, args) => {
                    for (arg_idx, arg) in args.iter().enumerate() {
                        let arg_instr_idx = idx - args.len() + arg_idx;
                        liveness[arg_instr_idx] = if arg_instr_idx == arg.0 {
                            InstrLiveness::Immediate
                        } else {
                            self.get_instr_local(module, arg)
                        };
                    }
                    liveness[idx] = InstrLiveness::Immediate;
                }
            }
        }

        println!("{:?}", liveness);
        liveness
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

        let mut liveness = Vec::new();

        for block in body {
            liveness.push(block.make_instruction_liveness(module));
        }

        WasmFunction {
            builder: FunctionBuilder::new(&mut module.types, params, returns),
            instruction_liveness: liveness,
            var_locals: HashMap::new(),
            args,
            export_name,
        }
    }
}

impl WasmBackend {
    pub fn new(mir_compiler: MirCompiler) -> Self {
        let MirCompiler { name_table, .. } = mir_compiler;

        let mut module = Module::default();
        let memory_id = module.memories.add_local(false, 1, None);

        let print_int_type = module.types.add(&[ValType::I32], &[]);
        let print_float_type = module.types.add(&[ValType::F32], &[]);
        let print_pointer_type = module.types.add(&[ValType::I32], &[]);
        let print_heap_type = module.types.add(&[], &[]);
        let alloc_type = module.types.add(&[ValType::I32], &[ValType::I32]);

        let (print_int, _) = module.add_import_func("std", "print_int", print_int_type);
        let (print_float, _) = module.add_import_func("std", "print_float", print_float_type);
        let (print_pointer, _) = module.add_import_func("std", "print_pointer", print_pointer_type);
        let (print_heap, _) = module.add_import_func("std", "print_heap", print_heap_type);
        let (alloc, _) = module.add_import_func("std", "alloc", alloc_type);

        module.exports.add("memory", memory_id);

        WasmBackend {
            module,
            memory_id,
            string_ptrs: Vec::new(),
            name_table,
            wasm_functions: Vec::new(),
            current_function: None,
            current_block: None,
            print_int,
            print_float,
            print_pointer,
            print_heap,
            alloc,
        }
    }

    pub fn generate_program(mut self, program: mir::Program) -> WasmProgram {
        let heap_start = self.add_strings_to_data_section(program.string_literals);

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

        WasmProgram {
            code: self.module.emit_wasm(),
            heap_start,
        }
    }

    // Adds strings to data section and returns vector of pointers to memory
    fn add_strings_to_data_section(&mut self, strings: Vec<String>) -> u32 {
        let mut data_start = 0;
        let memory = self.memory_id;

        for s in strings {
            let len = s.len();
            let mut bytes: Vec<u8> = vec![0, 0, 0, 0];
            let len_as_32 = len as u32;
            bytes.append(&mut len_as_32.to_le_bytes().to_vec());
            bytes.append(&mut s.into_bytes());

            self.module.data.add(
                DataKind::Active(ActiveData {
                    memory,
                    location: ActiveDataLocation::Absolute(data_start),
                }),
                bytes,
            );
            self.string_ptrs.push(data_start);
            data_start += len as u32;
            data_start += 8;
        }

        data_start
    }

    fn generate_function(&mut self, function: mir::Function) {
        self.current_block = Some(0);
        let fn_idx = self.current_function.unwrap();
        for (idx, instr) in function.body[0].0.iter().enumerate() {
            let liveness = self.wasm_functions[fn_idx].instruction_liveness[0][idx];
            // If we're dead, don't do anything
            if matches!(liveness, InstrLiveness::Dead) {
                continue;
            }
            match &instr.kind {
                mir::InstructionKind::Primary(primary) => self.generate_primary(primary),
                mir::InstructionKind::Unary(op, rhs) => self.generate_unary(*op, *rhs),
                mir::InstructionKind::VarSet(var_index, idx) => {
                    let var_liveness = self.get_liveness(*idx);
                    let func = &mut self.wasm_functions[fn_idx];

                    func.var_locals.insert(*var_index, var_liveness);
                }
                mir::InstructionKind::VarGet(idx) => {
                    let func = &mut self.wasm_functions[fn_idx];
                    let var_liveness = *func.var_locals.get(idx).unwrap();
                    let mut builder = func.builder.func_body();

                    if let InstrLiveness::Saved(local_id) = var_liveness {
                        builder.local_get(local_id);
                    }
                }
                mir::InstructionKind::Alloc(size_in_words) => {
                    let func = &mut self.wasm_functions[fn_idx];
                    let mut builder = func.builder.func_body();

                    let size_in_bytes = *size_in_words * 8;
                    builder.i32_const(size_in_bytes as i32).call(self.alloc);
                }
                mir::InstructionKind::Return(id) => {
                    let liveness = self.get_liveness(*id);
                    let func = &mut self.wasm_functions[self.current_function.unwrap()];

                    let mut builder = func.builder.func_body();
                    if let InstrLiveness::Saved(l) = liveness {
                        builder.local_get(l);
                    }

                    builder.return_();
                }
                mir::InstructionKind::Binary(op, lhs, rhs) => {
                    let lhs_liveness = self.get_liveness(*lhs);
                    let rhs_liveness = self.get_liveness(*rhs);
                    let func = &mut self.wasm_functions[self.current_function.unwrap()];

                    let mut builder = func.builder.func_body();
                    if let InstrLiveness::Saved(lhs_id) = lhs_liveness {
                        builder.local_get(lhs_id);
                    }
                    if let InstrLiveness::Saved(rhs_id) = rhs_liveness {
                        builder.local_get(rhs_id);
                    }

                    match op {
                        mir::BinaryOp::I32Store | mir::BinaryOp::PointerStore => {
                            builder.store(
                                self.memory_id,
                                StoreKind::I32 { atomic: false },
                                MemArg {
                                    align: 2,
                                    offset: 0,
                                },
                            );
                        }
                        mir::BinaryOp::I32Add | mir::BinaryOp::PointerAdd => {
                            builder.binop(BinaryOp::I32Add);
                        }
                        op => todo!("OP NOT IMPLEMENTED: {}", op),
                    }
                }
                instr => todo!("INSTR NOT IMPLEMENTED: {}", instr),
            }

            let mut main_body = self.wasm_functions[fn_idx].builder.func_body();
            if let InstrLiveness::Saved(id) = liveness {
                main_body.local_set(id);
            }
        }
    }

    fn get_liveness(&self, id: mir::InstrId) -> InstrLiveness {
        let current_function = self.current_function.unwrap();
        let current_block = self.current_block.unwrap();

        self.wasm_functions[current_function].instruction_liveness[current_block][id.0]
    }

    fn generate_primary(&mut self, primary: &mir::Primary) {
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
        let rhs_liveness = self.get_liveness(rhs);
        let func = &mut self.wasm_functions[self.current_function.unwrap()];

        let mut builder = func.builder.func_body();

        if let InstrLiveness::Saved(rhs_id) = rhs_liveness {
            builder.local_get(rhs_id);
        }

        match op {
            UnaryOp::PrintInt => builder.call(self.print_int),
            UnaryOp::PrintFloat => builder.call(self.print_float),
            UnaryOp::PrintPointer => builder.call(self.print_pointer),
            UnaryOp::Drop => builder.drop(),
            UnaryOp::PointerLoad => builder.load(
                self.memory_id,
                LoadKind::I32 { atomic: false },
                MemArg {
                    align: 2,
                    offset: 0,
                },
            ),
            op => todo!("UNARY OP: {:?}", op),
        };
    }
}
