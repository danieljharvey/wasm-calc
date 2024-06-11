HS_FILES = $(shell git ls-files '*.hs' | grep -v 'vendored/')
CABAL_FILES = $(shell git ls-files '*.cabal' | grep -v 'vendored/')

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all -j4 --enable-tests

# calculator 7

.PHONY: run-build-drawing-demo-7
run-build-drawing-demo-7:
	cabal run wasm-calc7 -- build wasm-calc7/demo/draw.calc > wasm-calc7/demo/draw.wasm

# calculator 8

.PHONY: run-build-drawing-demo-8
run-build-drawing-demo-8:
	cabal run wasm-calc8 -- build wasm-calc8/demo/draw.calc > wasm-calc8/demo/draw.wasm

# calculator 9

.PHONY: run-build-malloc-9
run-build-malloc-9:
	cabal build wasm-calc9
	cabal run wasm-calc9 -- build wasm-calc9/static/malloc.calc > wasm-calc9/static/malloc.wasm
	wasm2wat wasm-calc9/static/malloc.wasm > wasm-calc9/static/malloc-new.wat

# calculator 10

.PHONY: run-build-malloc-10
run-build-malloc-10:
	cabal build wasm-calc10
	cabal run wasm-calc10 -- build wasm-calc10/static/malloc.calc > wasm-calc10/static/malloc.wasm
	wasm2wat wasm-calc10/static/malloc.wasm > wasm-calc10/static/malloc-new.wat

# end of calcs

# run with `make watch version=9` to run tests / ghci for wasm-calc9
.PHONY: watch
version = 10
watch:
	ghciwatch --watch wasm-calc$(version) --command "cabal repl wasm-calc$(version)-tests" --after-startup-ghci 'main'

# run with `make run version=8` to run wasm-calc8
.PHONY: run
version = 10
run:
	cabal run wasm-calc$(version) -- repl

# run with `make test version=7` to run wasm-calc7 tests
.PHONY: test
version = 10
test:
	cabal run wasm-calc$(version):tests

# run with `make format-all-files version=7` to format all static `.calc` files for wasm-calc7
.PHONY: format-all-files
version = 10
STATIC_FILES = "./wasm-calc$(version)/test/static/"
format-all-files:
	find $(STATIC_FILES) -maxdepth 1 -type f -exec cabal run wasm-calc$(version) -- format {} \;

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
