use crate::mir;
use crate::utils::SaberProgram;
use walrus::{FunctionBuilder, LocalId, Module, ModuleConfig, ValType};

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
            let mut block_locals = Vec::new();

            for instr in &block.0 {
                block_locals.push(instr.ty.map(|t| module.locals.add(t.into())))
            }

            locals.push(block_locals)
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

    fn generate_function(&mut self, mut function: mir::Function) {
        self.current_block = Some(0);
        for (idx, instr) in function.body[0].0.iter().enumerate() {
            match &instr.kind {
                mir::InstructionKind::Primary(primary) => self.generate_primary(primary, idx),
                _ => {}
            }
        }
    }

    fn generate_primary(&mut self, primary: &mir::Primary, local_idx: usize) {
        let mut func = &mut self.wasm_functions[self.current_function.unwrap()];
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
