import fs from "fs/promises";

const filename = "./test.wasm"
const wasmBytes = await fs.readFile(filename);

async function go() {

  const imports = {
    console : {
      log : console.log,
    },
  };

  const {instance} = await WebAssembly.instantiate(wasmBytes, imports);
  const {main} = instance.exports;
  main()
}

go()
