const { readFileSync, writeFileSync } = require("fs");
const wabt = require("wabt")();
const path = require("path")

const instantiate = async (fileName) => {
    const buffer = readFileSync(path.join(__dirname, `${fileName}.wasm`));
    const module = await WebAssembly.compile(buffer);
    const importObject = { std: { print: arg => console.log(arg) }};
    const instance = await WebAssembly.instantiate(module, importObject);
    let wasm = instance.exports;
    for (let i = 1; i < 11; i++) {
        console.log(wasm.foo(i));
    };
};

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
