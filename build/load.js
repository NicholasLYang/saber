const { readFileSync, writeFileSync } = require("fs");
const code = require("./code.js");
const typeInfo = require("./type_info.js");

function atob(a) {
    return new Buffer(a, 'base64').toString('binary');
};

function base64ToArray(base64) {
    const characters = atob(base64);
    const array = new Uint8Array(characters.length);

    for (let i = 0; i < characters.length; i++) {
        array[i] = characters.charCodeAt(i);
    }
    return array;
}

const instantiate = async () => {
    const buffer = base64ToArray(code);
    const module = await WebAssembly.compile(buffer);
    const memory = new WebAssembly.Memory({ initial: 1 });
    const importObject = {
        std: { alloc: alloc(memory), streq: streq(memory), printInt: console.log, printFloat: console.log, printString: printString(memory) },
        mem: { heap: memory }};
    const instance = await WebAssembly.instantiate(module, importObject);
    let wasm = instance.exports;
    wasm.foo();
    printHeap(memory);
};

const printHeap = memory => {
    const memArray = new Uint8Array(memory.buffer);
    console.log("--------");
    for (let i = 0; i < 2 ** 7; i++) {
        if (memArray[i] !== 0) {
            console.log(`${i}: ${memArray[i]}`);
        }
    }
    console.log("--------");
}

const printString = memory => bytePtr => {
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

const PAGE_SIZE = 65536;

// Takes in size in bytes
const alloc = memory => size => {
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
        } else if ((len & 1) === 1) {
            ptr += len
        // If the length is big enough, we allocate the block
        } else if (len > alignedSize) {
            memArray[ptr] = len + 1;
            memArray[ptr + 1] = 1;
            return (ptr + 2) * 4;
        } else {
            ptr += len;
        }
    }
    memory.grow(Math.max(alignedSize/PAGE_SIZE, 1));
    memArray = new Uint32Array(memory.buffer);
    memArray[ptr] = alignedSize | 1;
    memArray[ptr + 1] = 1;
    return (ptr + 2) * 4;
};

const dealloc = memory => ptr => {
    let memArray = new Uint32Array(memory.buffer);
    memArray[ptr + 1] -= 1;
    if (memArray[ptr + 1] === 0) {
        typeInfo[memArray[ptr + 2]]
    }
}

const streq = memory => (s1, s2) => {
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

instantiate()
    .then(res => {
	console.log(res);
    })
    .catch(error => {
	console.log(error);
    });
