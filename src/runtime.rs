use crate::code_generator::BOX_ARRAY_ID;
use crate::utils::SaberProgram;
use anyhow::Result;
use byteorder::{LittleEndian, WriteBytesExt};
use std::collections::HashMap;
use std::convert::TryInto;
use wasmtime::{Caller, Extern, Func, Instance, Memory, Module, Store, Trap};

#[inline]
fn get_u32(ptr: usize, mem: &Memory) -> Result<u32> {
    unsafe {
        let mem_slice = mem
            .data_unchecked()
            .get(ptr..)
            .ok_or_else(|| Trap::new("pointer/length out of bounds"))?;
        Ok((mem_slice[0] as u32)
            + ((mem_slice[1] as u32) << 8)
            + ((mem_slice[2] as u32) << 16)
            + ((mem_slice[3] as u32) << 24))
    }
}

#[inline]
fn set_u32(ptr: usize, num: u32, mem: &Memory) -> Result<(), Trap> {
    unsafe {
        mem.data_unchecked_mut()
            .get_mut(ptr..)
            .ok_or_else(|| Trap::new("failed to access memory"))?
            .write_u32::<LittleEndian>(num)
            .map_err(|_| Trap::new("Unable to write to memory"))
    }
}

fn alloc(caller: Caller<'_>, size: i32) -> Result<i32, Trap> {
    let aligned_size: u32 = (((size + 8) + 3) & !0x03) as u32;
    let mem = match caller.get_export("memory") {
        Some(Extern::Memory(mem)) => mem,
        _ => return Err(Trap::new("failed to find host memory")),
    };
    unsafe {
        let mem_slice = mem.data_unchecked_mut();
        let mut ptr: usize = 0;
        while ptr < mem_slice.len() {
            let len = get_u32(ptr as usize, &mem)?;
            // If length is 0, block is not initialized. We can assume everything
            // from here to end of array is free
            if len == 0 {
                // We multiply (memArray.length - ptr) by 4 because we're dealing with
                // a 32 bit array
                if ((mem_slice.len() - ptr) * 4) < aligned_size as usize {
                    mem.grow(std::cmp::max(aligned_size / PAGE_SIZE, 1))?;
                }
                // Or with 1 to indicate allocated
                set_u32(ptr, aligned_size | 1, &mem)?;
                set_u32(ptr + 1, 1, &mem)?;

                return Ok(((ptr + 2) * 4).try_into().unwrap());
            // If the block is allocated, we move on
            } else if len & 1 != 0 {
                ptr += ((len - 1) / 4) as usize;
            // If the length is big enough, we allocate the block
            } else if len > aligned_size {
                // Or with 1 to indicate allocated
                set_u32(ptr, aligned_size | 1, &mem)?;
                set_u32(ptr + 1, 1, &mem)?;

                return Ok(((ptr + 2) * 4).try_into().unwrap());
            } else {
                ptr += (len / 4) as usize;
            }
        }
        mem.grow(std::cmp::max(aligned_size / PAGE_SIZE, 1))?;
        set_u32(ptr, aligned_size | 1, &mem)?;
        set_u32(ptr + 1, 1, &mem)?;

        Ok(((ptr + 2) * 4).try_into().unwrap())
    }
}

fn print_string(caller: Caller<'_>, ptr: i32) -> Result<(), Trap> {
    let mem = match caller.get_export("memory") {
        Some(Extern::Memory(mem)) => mem,
        _ => return Err(Trap::new("failed to find host memory")),
    };

    //
    let ptr_to_len = ptr + 4;
    let len = get_u32(ptr_to_len as usize, &mem)?;
    let ptr_to_contents = ptr + 8;

    // We're reading raw wasm memory here so we need `unsafe`. Note
    // though that this should be safe because we don't reenter wasm
    // while we're reading wasm memory, nor should we clash with
    // any other memory accessors (assuming they're well-behaved
    // too).
    unsafe {
        let data = mem
            .data_unchecked()
            .get(ptr_to_contents as u32 as usize..)
            .and_then(|arr| arr.get(..len as u32 as usize));
        let string = match data {
            Some(data) => match std::str::from_utf8(data) {
                Ok(s) => s,
                Err(_) => return Err(Trap::new("invalid utf-8")),
            },
            None => return Err(Trap::new("pointer/length out of bounds")),
        };
        println!("{}", string);
    }
    Ok(())
}

fn dealloc(
    mem: &Memory,
    ptr: usize,
    runtime_types: &HashMap<usize, Vec<bool>>,
) -> Result<(), Trap> {
    let ref_count_ptr = ptr + 4;
    let ref_count = get_u32(ref_count_ptr, mem)?;
    set_u32(ref_count_ptr, ref_count - 1, mem)?;

    if ref_count == 1 {
        // Remove allocated flag
        set_u32(ptr as usize, get_u32(ptr, mem)? - 1, mem)?;
        let type_id = get_u32(ptr + 8, mem)? as usize;

        if type_id as i32 == BOX_ARRAY_ID {
            let len = get_u32(ptr + 16, mem)? as usize;
            for idx in 0..len {
                dealloc(mem, ptr + 16 + (idx * 4), runtime_types)?;
            }
        } else if let Some(struct_info) = runtime_types.get(&type_id) {
            for (idx, is_ref) in struct_info.iter().enumerate() {
                if *is_ref {
                    let field_ptr = get_u32(ptr + 3 + (idx * 4), mem)?;
                    dealloc(mem, field_ptr as usize, runtime_types)?;
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
    let store = Store::default();
    let module = Module::new(store.engine(), wasm_bytes)?;
    let alloc = Func::wrap(&store, alloc);

    let dealloc = Func::wrap(&store, move |caller: Caller<'_>, ptr: i32| {
        let mem = get_memory!(caller);

        dealloc(&mem, ptr as usize, &runtime_types)
    });

    let clone = Func::wrap(&store, |caller: Caller<'_>, ptr: i32| {
        let mem = get_memory!(caller);
        let ref_count = get_u32((ptr - 4) as usize, &mem)?;
        if ref_count < 1 {
            Err(Trap::new("Internal Error: Invalid refcount for struct"))
        } else {
            set_u32((ptr - 4) as usize, ref_count + 1, &mem)?;
            Ok(())
        }
    });

    let streq = Func::wrap(
        &store,
        |caller: Caller<'_>, p1: i32, p2: i32| -> Result<u32, Trap> {
            if p1 == p2 {
                return Ok(1);
            }
            let mem = match caller.get_export("memory") {
                Some(Extern::Memory(mem)) => mem,
                _ => return Err(Trap::new("failed to find host memory")),
            };

            let p1_len = get_u32((p1 + 4) as usize, &mem)? as usize;
            let p2_len = get_u32((p2 + 4) as usize, &mem)? as usize;

            if p1_len != p2_len {
                return Ok(0);
            }

            for i in 0..p1_len {
                unsafe {
                    let mem_slice = mem.data_unchecked();
                    if mem_slice.get(p1 as usize + 4 + i).cloned()
                        != mem_slice.get(p2 as usize + 4 + i).cloned()
                    {
                        return Ok(0);
                    }
                }
            }
            Ok(1)
        },
    );

    let print_heap = Func::wrap(&store, |caller: Caller<'_>| {
        let mem = match caller.get_export("memory") {
            Some(Extern::Memory(mem)) => mem,
            _ => return Err(Trap::new("failed to find host memory")),
        };
        unsafe { println!("{:?}", mem.data_unchecked()) }
        Ok(())
    });

    let print_int = Func::wrap(&store, |param: i32| {
        println!("{}", param);
    });
    let print_float = Func::wrap(&store, |param: f32| {
        println!("{}", param);
    });

    let print_string = Func::wrap(&store, print_string);

    let print_char = Func::wrap(&store, |param: i32| {
        println!("{}", std::char::from_u32(param as u32).unwrap());
    });
    let instance = Instance::new(
        &store,
        &module,
        &[
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
