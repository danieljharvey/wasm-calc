import fs from "fs/promises";

const filename = "./test.wasm" // "./-4597904435544001656.wasm"
const wasmBytes = await fs.readFile(filename);

async function go() {

  const imports = {
    console : {
      log : a => console.log(a, "horse"),
    },
  };

  const {instance} = await WebAssembly.instantiate(wasmBytes, imports);
  const {main} = instance.exports;

  console.log(main)

  return main()
}

go().then(console.log)
