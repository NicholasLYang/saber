use crate::mir;
use crate::utils::SaberProgram;
use walrus::{FunctionBuilder, Module, ModuleConfig, ValType};

pub struct WasmBackend {
    functions: Vec<mir::Function>,
    module: Module,
    wasm_functions: Vec<FunctionBuilder>,
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
            functions: Vec::new(),
            module: Module::default(),
            wasm_functions: Vec::new(),
        }
    }

    pub fn generate_program(&mut self, program: mir::Program) -> Vec<u8> {
        for func in &program.functions {
            let params: Vec<_> = func.params.iter().map(|p| p.into()).collect();
            let returns: Vec<_> = func.returns.iter().map(|p| p.into()).collect();
            self.wasm_functions.push(FunctionBuilder::new(
                &mut self.module.types,
                &params,
                &returns,
            ))
        }

        for func in program.functions {}

        self.module.emit_wasm()
    }
}
