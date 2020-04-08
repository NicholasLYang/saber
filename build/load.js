const { readFileSync, writeFileSync } = require("fs");
const wabt = require("wabt")();
const path = require("path")

const instantiate = async (fileName) => {
    const buffer = readFileSync(path.join(__dirname, `${fileName}.wasm`));
    const module = await WebAssembly.compile(buffer);
    const instance = await WebAssembly.instantiate(module);
    let wasm = instance.exports;
    for (let i = 0; i < 10; i++) {
	  console.log(wasm.fact(i));
    }
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
