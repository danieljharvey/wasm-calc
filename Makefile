HS_FILES = $(shell git ls-files '*.hs' | grep -v 'vendored/')
CABAL_FILES = $(shell git ls-files '*.cabal' | grep -v 'vendored/')
STATIC_FILES_7 = "./wasm-calc7/test/static/"
STATIC_FILES_8 = "./wasm-calc8/test/static/"
STATIC_FILES_9 = "./wasm-calc9/test/static/"

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all -j4 --enable-tests

# calculator 1

.PHONY: test-wasm-calc
test-wasm-calc:
	cabal run wasm-calc:tests

.PHONY: run-wasm-calc
run-wasm-calc:
	cabal run wasm-calc

# calculator 2

.PHONY: test-wasm-calc2
test-wasm-calc2:
	cabal run wasm-calc2:tests

.PHONY: run-wasm-calc2
run-wasm-calc2:
	cabal run wasm-calc2

# calculator 3

.PHONY: test-wasm-calc3
test-wasm-calc3:
	cabal run wasm-calc3:tests

.PHONY: run-wasm-calc3
run-wasm-calc3:
	cabal run wasm-calc3

# calculator 4

.PHONY: test-wasm-calc4
test-wasm-calc4:
	cabal run wasm-calc4:tests

.PHONY: run-wasm-calc4
run-wasm-calc4:
	cabal run wasm-calc4

# calculator 5

.PHONY: test-wasm-calc5
test-wasm-calc5:
	cabal run wasm-calc5:tests

.PHONY: run-wasm-calc5
run-wasm-calc5:
	cabal run wasm-calc5

# calculator 6

.PHONY: test-wasm-calc6
test-wasm-calc6:
	cabal run wasm-calc6:tests

.PHONY: run-wasm-calc6
run-wasm-calc6:
	cabal run wasm-calc6

# calculator 7

.PHONY: test-wasm-calc7
test-wasm-calc7:
	cabal run wasm-calc7:tests

.PHONY: run-wasm-calc7
run-wasm-calc7:
	cabal run wasm-calc7 -- repl

.PHONY: run-build-drawing-demo-7
run-build-drawing-demo-7:
	cabal run wasm-calc7 -- build wasm-calc7/demo/draw.calc > wasm-calc7/demo/draw.wasm

.PHONY: format-all-files-7
format-all-files-7:
	find $(STATIC_FILES_7) -maxdepth 1 -type f -exec cabal run wasm-calc7 -- format {} \;

# calculator 8

.PHONY: test-wasm-calc8
test-wasm-calc8:
	cabal run wasm-calc8:tests

.PHONY: run-wasm-calc8
run-wasm-calc8:
	cabal run wasm-calc8 -- repl

.PHONY: run-build-drawing-demo-8
run-build-drawing-demo-8:
	cabal run wasm-calc8 -- build wasm-calc8/demo/draw.calc > wasm-calc8/demo/draw.wasm

.PHONY: format-all-files-8
format-all-files-8:
	find $(STATIC_FILES_8) -maxdepth 1 -type f -exec cabal run wasm-calc8 -- format {} \;

# calculator 9

.PHONY: test-wasm-calc9
test-wasm-calc9:
	cabal run wasm-calc9:tests

.PHONY: run-wasm-calc9
run-wasm-calc9:
	cabal run wasm-calc9 -- repl

.PHONY: run-build-drawing-demo-9
run-build-drawing-demo-9:
	cabal run wasm-calc9 -- build wasm-calc9/demo/draw.calc > wasm-calc9/demo/draw.wasm

.PHONY: run-build-malloc-9
run-build-malloc-9:
	cabal build wasm-calc9
	cabal run wasm-calc9 -- build wasm-calc9/static/malloc.calc > wasm-calc9/static/malloc.wasm
	wasm2wat wasm-calc9/static/malloc.wasm > wasm-calc9/static/malloc-new.wat

.PHONY: format-all-files-9
format-all-files-9:
	find $(STATIC_FILES_9) -maxdepth 1 -type f -exec cabal run wasm-calc9 -- format {} \;

# end of calcs

.PHONY: freeze
freeze:
	cabal freeze --enable-tests --enable-benchmarks

.PHONY: bench
bench:
	cabal bench benchmarks:benchmarks

.PHONY: format
format:
	@ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"

.PHONY: hlint
hlint:
	@hlint $(HS_FILES)

.PHONY: format-cabal
format-cabal:
	@cabal-fmt -i $(CABAL_FILES)
