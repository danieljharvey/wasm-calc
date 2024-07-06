// this file is used in tests to check imports work correctly
import fs from "fs/promises";

const filename = process.argv[2];
const wasmBytes = await fs.readFile(filename);

async function go() {
  const imports = {
    console : {log : a => console.log(a)},
    env : {memory : new WebAssembly.Memory({initial : 1})}
  };

  const {instance} = await WebAssembly.instantiate(wasmBytes, imports);
  const {test} = instance.exports;

  return test()
}

go()
