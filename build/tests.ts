import {alloc, printHeap} from "./runtime";

function testAlloc() {
    const memory = new WebAssembly.Memory({ initial: 1 });
    printHeap(memory);
    const ptr = alloc(memory)(16);
    const memArray = new Uint32Array(memory.buffer);
}
