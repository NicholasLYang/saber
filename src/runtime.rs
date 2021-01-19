use wasmtime::{Instance, Module, Store};

#[derive(Debug, Fail, Clone)]
enum RuntimeError {
    NoMain,
}

fn run_code(wasm_bytes: Vec<u8>) -> Result<(), RuntimeError> {
    let store = Store::default();
    let module = Module::new(store.engine(), wasm_bytes)?;
    let instance = Instance::new(&store, &module, &[])?;
    let main = instance
        .get_func("main")
        .ok_or(RuntimeError::NoMain)?
        .get0::<()>()?;
    main()?;
    Ok(())
}
