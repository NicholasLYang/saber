const { readFileSync, writeFileSync } = require("fs");
const wabt = require("wabt")();

const instantiate = async (fileName) => {
    const buffer = readFileSync(`./${fileName}.wasm`);
    const module = await WebAssembly.compile(buffer);
    const instance = await WebAssembly.instantiate(module);
    let wasm = instance.exports;
    return wasm.main();
};

const buildToWasm = (fileName) => {
    const watFile = `${fileName}.wat`;
    const watModule = readFileSync(watFile, "utf8");
    const wasmModule = wabt.parseWat(watFile, watModule);
    const { buffer } = wasmModule.toBinary({});
    return writeFileSync(`${fileName}.wasm`, Buffer.from(buffer));
}
buildToWasm("test");
instantiate("test")
    .then(res => {
	console.log(res);
    })
    .catch(error => {
	console.log(error);
    });
