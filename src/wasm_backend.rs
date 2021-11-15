use crate::mir;
use crate::utils::SaberProgram;
use walrus::{FunctionBuilder, LocalId, Module, ModuleConfig, ValType};

struct WasmFunction {
    pub builder: FunctionBuilder,
    pub args: Vec<LocalId>,
}

pub struct WasmBackend {
    module: Module,
    wasm_functions: Vec<WasmFunction>,
    current_function: Option<usize>,
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

impl WasmBackend {
    pub fn new() -> Self {
        WasmBackend {
            module: Module::default(),
            wasm_functions: Vec::new(),
            current_function: None,
        }
    }

    pub fn generate_program(mut self, program: mir::Program) -> Vec<u8> {
        for func in &program.functions {
            let params: Vec<_> = func.params.iter().map(|p| p.into()).collect();
            let returns: Vec<_> = func.returns.iter().map(|p| p.into()).collect();
            let mut args = Vec::new();

            for param in &params {
                args.push(self.module.locals.add(*param));
            }

            self.wasm_functions.push(WasmFunction {
                builder: FunctionBuilder::new(&mut self.module.types, &params, &returns),
                args,
            })
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
        for instr in &function.body[0].0 {
            match instr {
                mir::Instruction::Primary(primary) => self.generate_primary(primary),
                _ => {}
            }
        }
    }

    fn generate_primary(&mut self, primary: &mir::Primary) {
        let mut builder = self.wasm_functions[self.current_function.unwrap()]
            .builder
            .func_body();

        match primary {
            mir::Primary::I32(i) => builder.i32_const(*i),
            mir::Primary::String(usize) => todo!(),
            mir::Primary::F32(f) => builder.f32_const(*f),
            mir::Primary::USize(u) => builder.i32_const(*u as i32),
        };
    }
}
