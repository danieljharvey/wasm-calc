# demo

This is a small demo that runs in the browser, passing a `draw` function into a
WASM module. 

To open it in a browser, run `serve .` and navigate to
`localhost:5000/draw.html`.

To change the file and see results, run `watchexec -w ./**/*.calc make
run-build-drawing-demo-7`. This will watch all `.calc` files and recompile on file changes.

You will need to reload the browser after each change.
