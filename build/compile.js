const { readFileSync, writeFileSync } = require("fs");
const wabt = require("wabt")();
const path = require("path");

const inputWat = "test.wat";
const outputWasm = "test.wasm";

const watModule = wabt.parseWat(inputWat, readFileSync(inputWat, "utf8"));
const { buffer } = watModule.toBinary({});

writeFileSync(outputWasm, new Buffer(buffer));
