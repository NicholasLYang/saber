const { typeInfo, STR_INDEX } = require("./type_info.js");

export const printHeap = (memory: WebAssembly.Memory) => {
    const memArray = new Uint8Array(memory.buffer);
    console.log("--------");
    for (let i = 0; i < 2 ** 7; i++) {
        if (memArray[i] !== 0) {
            console.log(`${i}: ${memArray[i]}`);
        }
    }
    console.log("--------");
};

export const printString = (memory: WebAssembly.Memory) => (bytePtr: number) => {
    const memArray = new Uint32Array(memory.buffer);
    let ptr = bytePtr/4;
    const len = memArray[ptr];
    const out = new Uint32Array(len/4);
    for (let offset = 0; offset < len/4; offset++) {
        const i = offset + ptr + 1;
        out[offset] = memArray[i];
    }
    const decoder = new TextDecoder();
    console.log(decoder.decode(out));
};

// Prints value
// @param {number} ptr - An index into 32 bit array. Should point to type id
export function printValue(memory: WebAssembly.Memory, ptr: number) {
    const memArray = new Uint32Array(memory.buffer);
    const size = memArray[ptr - 2] & ~1;
    const refCount = memArray[ptr - 1];
    const typeId = memArray[ptr];
    console.log(`SIZE: ${size}, REF COUNT ${refCount}, TYPE ID ${typeId}`);
}

const PAGE_SIZE = 65536;

// Takes in size in bytes
export const alloc = (memory: WebAssembly.Memory) => (size: number) => {
    // size + 8 because we gotta tack on a block length and refcount
    const alignedSize = ((size + 8) + 3) & ~0x03;
    let memArray = new Uint32Array(memory.buffer);
    let ptr = 0;
    while (ptr < memArray.length) {
        const len = memArray[ptr];
        // If length is 0, block is not initialized. We can assume everything
        // from here to end of array is free
        if (len === 0) {
            // We multiply (memArray.length - ptr) by 4 because they're dealing with
            // a 32 bit array
            if ((memArray.length - ptr) * 4 > alignedSize) {
                // Or with 1 to indicate allocated
                memArray[ptr] = alignedSize | 1;
                memArray[ptr + 1] = 1;
                return (ptr + 2) * 4;
            } else {
                memory.grow(Math.max(alignedSize/PAGE_SIZE, 1));
                memArray = new Uint32Array(memory.buffer);
                memArray[ptr] = alignedSize | 1;
                memArray[ptr + 1] = 1;
                return (ptr + 2) * 4;
            }
            // If the block is allocated, we move on
        } else if ((len & 1)) {
            ptr += (len - 1)/4;
            // If the length is big enough, we allocate the block
        } else if (len > alignedSize) {
            memArray[ptr] = len + 1;
            memArray[ptr + 1] = 1;
            return (ptr + 2) * 4;
        } else {
            ptr += len/4;
        }
    }
    memory.grow(Math.max(alignedSize/PAGE_SIZE, 1));
    memArray = new Uint32Array(memory.buffer);
    memArray[ptr] = alignedSize | 1;
    memArray[ptr + 1] = 1;
    return (ptr + 2) * 4;
};

export const dealloc = (memory: WebAssembly.Memory) => (bytePtr: number) => {
    let memArray = new Uint32Array(memory.buffer);
    const ptr = (bytePtr - 8)/4;
    memArray[ptr + 1] -= 1;
    if (memArray[ptr + 1] === 0) {
        // Remove allocated flag
        memArray[ptr] = memArray[ptr] ^ 1;
        const typeId = memArray[ptr + 2];
        if (typeId === STR_INDEX) {
            return;
        }
        if (!(typeId in typeInfo)) {
            throw new Error(`RuntimeError: Invalid type for struct. Most likely internal error`);
        }
        const structInfo = typeInfo[typeId];
        structInfo.forEach((isRef: boolean, index: number) => {
            if (isRef) {
                const ptr2 = memArray[ptr + 3 + index];
                dealloc(memory)(ptr2);
            }
        });
    }
}

export const clone = (memory: WebAssembly.Memory) => (bytePtr: number) => {
    const ptr = bytePtr/4;
    const memArray = new Uint32Array(memory.buffer);
    if (memArray[ptr - 1] < 1) {
        throw new Error(`RuntimeError: Invalid refcount for struct. Most likely internal error`);
    }
    memArray[ptr - 1] += 1;
    return ptr;
}

export const streq = (memory: WebAssembly.Memory) => (s1: number, s2: number) => {
    if (s1 === s2) {
        return true;
    }
    let memArray = new Uint32Array(memory.buffer);
    const s1Len = memArray[s1/4 + 1];
    const s2Len = memArray[s2/4 + 1];
    if (s1Len !== s2Len) {
        return false;
    }
    for (let i = 0; i < s1Len/4; i++) {
        if (memArray[(s1 + i)/4 + 1] !== memArray[(s2 + i)/4 + 1]) {
            return false;
        }
    }
    return true;
};