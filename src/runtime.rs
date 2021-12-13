//use crate::code_generator::BOX_ARRAY_ID;
static BOX_ARRAY_ID: i32 = 0;
use crate::utils::SaberProgram;
use anyhow::Result;
use std::collections::HashMap;
use std::convert::TryInto;
use wasmtime::{Caller, Extern, Func, Instance, Memory, Module, Store, Trap, Val};

#[inline]
fn get_u32(store: &mut Caller<'_, ()>, ptr: usize, mem: &mut Memory) -> Result<u32> {
    let mut bytes = vec![0u8; 4];
    mem.read(store, ptr, &mut bytes)
        .map_err(|_| Trap::new("cannot read memory"))?;

    Ok((bytes[0] as u32)
        + ((bytes[1] as u32) << 8)
        + ((bytes[2] as u32) << 16)
        + ((bytes[3] as u32) << 24))
}

#[inline]
fn set_u32(store: &mut Caller<'_, ()>, ptr: usize, num: u32, mem: &Memory) -> Result<(), Trap> {
    mem.write(store, ptr, &num.to_le_bytes())
        .map_err(|_| Trap::new("Cannot read from memory"))
}

fn alloc(mut caller: Caller<'_, ()>, size_in_bytes: i32) -> Result<i32, Trap> {
    let aligned_size: u32 = (((size_in_bytes + 8) + 3) & !0x03) as u32;
    let mut mem = match caller.get_export("memory") {
        Some(Extern::Memory(mem)) => mem,
        _ => return Err(Trap::new("failed to find host memory")),
    };
    let mem_len = mem.data_size(&mut caller);
    let mut ptr: usize = 0;
    while ptr < mem_len {
        let len = get_u32(&mut caller, ptr as usize, &mut mem)?;
        // If length is 0, block is not initialized. We can assume everything
        // from here to end of array is free
        if len == 0 {
            // We multiply (memArray.length - ptr) by 4 because we're dealing with
            // a 32 bit array
            if ((mem_len - ptr) * 4) < aligned_size as usize {
                mem.grow(
                    &mut caller,
                    std::cmp::max((aligned_size / PAGE_SIZE) as u64, 1),
                )?;
            }
            // Or with 1 to indicate allocated
            set_u32(&mut caller, ptr, aligned_size | 1, &mem)?;
            set_u32(&mut caller, ptr + 1, 1, &mem)?;

            return Ok(((ptr + 2) * 4).try_into().unwrap());
        // If the block is allocated, we move on
        } else if len & 1 != 0 {
            ptr += ((len - 1) / 4) as usize;
        // If the length is big enough, we allocate the block
        } else if len > aligned_size {
            // Or with 1 to indicate allocated
            set_u32(&mut caller, ptr, aligned_size | 1, &mem)?;
            set_u32(&mut caller, ptr + 1, 1, &mem)?;

            return Ok(((ptr + 2) * 4).try_into().unwrap());
        } else {
            ptr += (len / 4) as usize;
        }
    }
    mem.grow(
        &mut caller,
        std::cmp::max((aligned_size / PAGE_SIZE) as u64, 1),
    )?;
    set_u32(&mut caller, ptr, aligned_size | 1, &mem)?;
    set_u32(&mut caller, ptr + 1, 1, &mem)?;

    Ok(((ptr + 2) * 4).try_into().unwrap())
}

fn print_string(mut caller: Caller<'_, ()>, ptr: i32) -> Result<(), Trap> {
    let mut mem = match caller.get_export("memory") {
        Some(Extern::Memory(mem)) => mem,
        _ => return Err(Trap::new("failed to find host memory")),
    };

    //
    let ptr_to_len = ptr + 4;
    let len = get_u32(&mut caller, ptr_to_len as usize, &mut mem)?;
    let ptr_to_contents = ptr + 8;

    let mut data = vec![0u8; len as usize];
    mem.read(&mut caller, ptr_to_contents as u32 as usize, &mut data)
        .map_err(|_| Trap::new("cannot read memory"))?;

    let string = match std::str::from_utf8(&data) {
        Ok(s) => s,
        Err(_) => return Err(Trap::new("invalid utf-8")),
    };

    println!("{}", string);

    Ok(())
}

fn dealloc(
    caller: &mut Caller<'_, ()>,
    mem: &mut Memory,
    ptr: usize,
    runtime_types: &HashMap<usize, Vec<bool>>,
) -> Result<(), Trap> {
    let ref_count_ptr = ptr + 4;
    let ref_count = get_u32(caller, ref_count_ptr, mem)?;
    set_u32(caller, ref_count_ptr, ref_count - 1, mem)?;

    if ref_count == 1 {
        // Remove allocated flag
        let num = get_u32(caller, ptr, mem)? - 1;
        set_u32(caller, ptr as usize, num, mem)?;
        let type_id = get_u32(caller, ptr + 8, mem)? as usize;

        if type_id as i32 == BOX_ARRAY_ID {
            let len = get_u32(caller, ptr + 16, mem)? as usize;
            for idx in 0..len {
                dealloc(caller, mem, ptr + 16 + (idx * 4), runtime_types)?;
            }
        } else if let Some(struct_info) = runtime_types.get(&type_id) {
            for (idx, is_ref) in struct_info.iter().enumerate() {
                if *is_ref {
                    let field_ptr = get_u32(caller, ptr + 3 + (idx * 4), mem)?;
                    dealloc(caller, mem, field_ptr as usize, runtime_types)?;
                }
            }
        }
    }

    Ok(())
}

macro_rules! get_memory {
    ($caller:expr) => {
        match ($caller).get_export("memory") {
            Some(Extern::Memory(mem)) => mem,
            _ => return Err(Trap::new("failed to find host memory")),
        }
    };
}

static PAGE_SIZE: u32 = 65536;

pub fn run_code(program: SaberProgram) -> Result<()> {
    let wasm_bytes = program.wasm_bytes;
    let runtime_types = program.runtime_types;
    let mut store = Store::default();
    let module = Module::new(store.engine(), wasm_bytes)?;
    let alloc = Func::wrap(&mut store, alloc);

    let dealloc = Func::wrap(&mut store, move |mut caller: Caller<'_, ()>, ptr: i32| {
        let mut mem = get_memory!(caller);

        dealloc(&mut caller, &mut mem, ptr as usize, &runtime_types)
    });

    let clone = Func::wrap(&mut store, |mut caller: Caller<'_, ()>, ptr: i32| {
        let mut mem = get_memory!(caller);
        let ref_count = get_u32(&mut caller, (ptr - 4) as usize, &mut mem)?;
        if ref_count < 1 {
            Err(Trap::new("Internal Error: Invalid refcount for struct"))
        } else {
            set_u32(&mut caller, (ptr - 4) as usize, ref_count + 1, &mem)?;
            Ok(())
        }
    });

    let streq = Func::wrap(
        &mut store,
        |mut caller: Caller<'_, ()>, p1: i32, p2: i32| -> Result<u32, Trap> {
            if p1 == p2 {
                return Ok(1);
            }
            let mut mem = match caller.get_export("memory") {
                Some(Extern::Memory(mem)) => mem,
                _ => return Err(Trap::new("failed to find host memory")),
            };

            let p1_len = get_u32(&mut caller, (p1 + 4) as usize, &mut mem)? as usize;
            let p2_len = get_u32(&mut caller, (p2 + 4) as usize, &mut mem)? as usize;

            if p1_len != p2_len {
                return Ok(0);
            }

            for i in 0..p1_len {
                let mem_slice = mem.data(&caller);
                if mem_slice.get(p1 as usize + 4 + i).cloned()
                    != mem_slice.get(p2 as usize + 4 + i).cloned()
                {
                    return Ok(0);
                }
            }
            Ok(1)
        },
    );

    let print_heap = Func::wrap(&mut store, |mut caller: Caller<'_, ()>| {
        let mem = match caller.get_export("memory") {
            Some(Extern::Memory(mem)) => mem,
            _ => return Err(Trap::new("failed to find host memory")),
        };
        println!("{:?}", mem.data(&caller));
        Ok(())
    });

    let print_int = Func::wrap(&mut store, |param: i32| {
        println!("{}", param);
    });
    let print_float = Func::wrap(&mut store, |param: f32| {
        println!("{}", param);
    });

    let print_string = Func::wrap(&mut store, print_string);

    let print_char = Func::wrap(&mut store, |param: i32| {
        println!("{}", std::char::from_u32(param as u32).unwrap());
    });

    let instance = Instance::new(
        &mut store,
        &module,
        &[
            print_int.into(),
            print_float.into(),
            print_string.into(),
            alloc.into(),
        ],
    )?;

    let mut results = vec![Val::I32(0)];
    instance
        .get_func(&mut store, "main")
        .expect("No main function")
        .call(&mut store, &[], &mut results)?;

    Ok(())
}
