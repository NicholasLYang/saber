import { code } from "./code";
import {alloc, clone, dealloc, debugAlloc, printChar, printHeap, printString, streq} from "./runtime";

function atob(a: string) {
    return Buffer.from(a, 'base64').toString('binary');
};

function base64ToArray(base64: string) {
    const characters = atob(base64);
    const array = new Uint8Array(characters.length);

    for (let i = 0; i < characters.length; i++) {
        array[i] = characters.charCodeAt(i);
    }
    return array;
}

async function instantiate() {
    const buffer = base64ToArray(code);
    const module = await WebAssembly.compile(buffer);
    const memory = new WebAssembly.Memory({ initial: 1 });
    const importObject = {
        std: {
            alloc: alloc(memory),
            dealloc: dealloc(memory),
            clone: clone(memory),
            streq: streq(memory),
            printInt: console.log,
            printFloat: console.log,
            printString: printString(memory),
            printChar: printChar(memory)
        },
        mem: { heap: memory }};
    const instance = await WebAssembly.instantiate(module, importObject);
    let wasm: any = instance.exports;
    wasm.main();
};




instantiate()
    .catch(error => {
	console.log(error);
    });


