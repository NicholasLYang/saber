const { readFileSync, writeFileSync } = require("fs");
const wabt = require("wabt")();
const path = require("path");

const instantiate = async (fileName) => {
    const buffer = readFileSync(path.join(__dirname, `${fileName}.wasm`));
    const module = await WebAssembly.compile(buffer);
    const memory = new WebAssembly.Memory({ initial: 1 });
    const memArray = new Uint32Array(memory.buffer);
    const importObject = {
        std: { printInt: console.log, printFloat: console.log, printString: printString(memArray) },
        mem: { heap: memory }};
    const instance = await WebAssembly.instantiate(module, importObject);
    let wasm = instance.exports;
    wasm.foo();
};

const printString = memArray => ptr => {
    const len = memArray[ptr];
    const out = new Uint32Array(len/4);
    for (let offset = 0; offset < len/4; offset++) {
        const i = offset + ptr + 1;
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
