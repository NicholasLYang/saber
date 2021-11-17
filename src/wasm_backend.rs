use crate::mir;
use walrus::{FunctionBuilder, LocalId, Module, ValType};

struct WasmFunction {
    pub builder: FunctionBuilder,
    pub args: Vec<LocalId>,
    // TODO: We should optimize so that we don't need one local
    //   per instruction. We can go through instructions, see
    //   what instructions rely on past ones and if it's only one
    //   instruction, we make it a stack immediate.
    pub locals: Vec<Vec<Option<LocalId>>>,
}

pub struct WasmBackend {
    module: Module,
    wasm_functions: Vec<WasmFunction>,
    current_function: Option<usize>,
    current_block: Option<usize>,
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
                mir::InstructionKind::VarSet(_, _)
                | mir::InstructionKind::VarGet(_)
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
            locals,
            args,
        }
    }
}

impl WasmBackend {
    pub fn new() -> Self {
        WasmBackend {
            module: Module::default(),
            wasm_functions: Vec::new(),
            current_function: None,
            current_block: None,
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
            ))
        }

        for (idx, func) in program.functions.into_iter().enumerate() {
            self.current_function = Some(idx);
            self.generate_function(func)
        }

        for func_builder in self.wasm_functions {
            func_builder
                .builder
                .finish(func_builder.args, &mut self.module.funcs);
        }

        self.module.emit_wasm()
    }

    fn generate_function(&mut self, function: mir::Function) {
        self.current_block = Some(0);
        for (idx, instr) in function.body[0].0.iter().enumerate() {
            match &instr.kind {
                mir::InstructionKind::Primary(primary) => self.generate_primary(primary, idx),
                _ => {}
            }
        }
    }

    fn generate_primary(&mut self, primary: &mir::Primary, local_idx: usize) {
        let func = &mut self.wasm_functions[self.current_function.unwrap()];
        let local_id = func.locals[self.current_block.unwrap()][local_idx].unwrap();

        let mut builder = func.builder.func_body();

        match primary {
            mir::Primary::I32(i) => builder.i32_const(*i),
            mir::Primary::String(usize) => todo!(),
            mir::Primary::F32(f) => builder.f32_const(*f),
        };

        builder.local_set(local_id);
    }
}
