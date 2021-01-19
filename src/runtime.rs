use anyhow::Result;
use wasmtime::{Extern, Func, Instance, Limits, Memory, MemoryType, Module, Store};

pub fn run_code(wasm_bytes: Vec<u8>) -> Result<()> {
    let store = Store::default();
    let module = Module::new(store.engine(), wasm_bytes)?;
    let memory = Memory::new(&store, MemoryType::new(Limits::new(1, None)));
    let alloc = Func::wrap(&store, |param: i32| 0);
    let dealloc = Func::wrap(&store, |param: i32| ());
    let clone = Func::wrap(&store, |p1: i32| ());
    let streq = Func::wrap(&store, |p1: i32, p2: i32| 0);
    let print_heap = Func::wrap(&store, || println!("Printing heap!"));
    let print_int = Func::wrap(&store, |param: i32| {
        println!("{}", param);
    });
    let print_float = Func::wrap(&store, |param: f32| {
        println!("{}", param);
    });

    let print_string = Func::wrap(&store, |param: i32| {
        println!("PRINT STRING {}", param);
    });

    let print_char = Func::wrap(&store, |param: i32| {
        println!("PRINT CHAR {}", param);
    });
    let instance = Instance::new(
        &store,
        &module,
        &[
            memory.into(),
            alloc.into(),
            dealloc.into(),
            clone.into(),
            streq.into(),
            print_heap.into(),
            print_int.into(),
            print_float.into(),
            print_string.into(),
            print_char.into(),
        ],
    )?;
    let main = instance
        .get_func("main")
        .expect("No main function")
        .get1::<i32, ()>()?;
    main(0)?;
    Ok(())
}
