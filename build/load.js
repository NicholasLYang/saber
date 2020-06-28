const { readFileSync, writeFileSync } = require("fs");
const wabt = require("wabt")();
const path = require("path");

const instantiate = async (fileName) => {
    const buffer = readFileSync(path.join(__dirname, `${fileName}.wasm`));
    const module = await WebAssembly.compile(buffer);
    const memory = new WebAssembly.Memory({ initial: 1 });
    const memArray = new Uint8Array(memory.buffer);
    const importObject = {
        std: { printInt: console.log, printFloat: console.log, printString: printString(memArray) },
        mem: { heap: memory }};
    const instance = await WebAssembly.instantiate(module, importObject);
    let wasm = instance.exports;
    printHeap(memArray);
    wasm.foo();
    printHeap(memArray);
};

const printHeap = memArray => {
    console.log("--------");
    for (let i = 0; i < 2 ** 7; i++) {
        if (memArray[i] !== 0) {
            console.log(`${i}: ${memArray[i]}`);
        }
    }
    console.log("--------");
}

const printString = memArray => ptr => {
    const len = memArray[ptr]
        + (memArray[ptr + 1] << 8)
        + (memArray[ptr + 2]<< 16)
        + (memArray[ptr + 3] << 24);
    const out = new Uint8Array(len);
    for (let offset = 0; offset < len; offset++) {
        const i = offset + ptr + 4;
        out[offset] = memArray[i];
    }
    const decoder = new TextDecoder();
    console.log(decoder.decode(out));
}

const buildToWasm = (fileName) => {
    const watFile = `${fileName}.wat`;
    const watModule = readFileSync(watFile, "utf8");
    const wasmModule = wabt.parseWat(watFile, watModule);
    const { buffer } = wasmModule.toBinary({});
    return writeFileSync(`${fileName}.wasm`, Buffer.from(buffer));
}

instantiate("out")
    .then(res => {
	console.log(res);
    })
    .catch(error => {
	console.log(error);
    });
