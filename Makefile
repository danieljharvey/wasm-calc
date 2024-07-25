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
	nix run .#wasm-calc9 build wasm-calc9/static/malloc.calc > wasm-calc9/static/malloc.wasm
	wasm2wat wasm-calc9/static/malloc.wasm > wasm-calc9/static/malloc-new.wat
	diff wasm-calc9/static/malloc-new.wat wasm-calc9/static/malloc.wat

# calculator 10

.PHONY: run-build-malloc-10
run-build-malloc-10:
	nix run .#wasm-calc10 build wasm-calc10/static/malloc.calc > wasm-calc10/static/malloc.wasm
	wasm2wat wasm-calc10/static/malloc.wasm > wasm-calc10/static/malloc-new.wat

# calculator 11

.PHONY: run-build-malloc-11
run-build-malloc-11:
	nix run .#wasm-calc11 build wasm-calc11/static/malloc.calc > wasm-calc11/static/malloc.wasm
	wasm2wat wasm-calc11/static/malloc.wasm > wasm-calc11/static/malloc-new.wat

.PHONY: run-build-drawing-demo-11
run-build-drawing-demo-11:
	cabal run wasm-calc11 -- build wasm-calc11/demo/draw.calc > wasm-calc11/demo/draw.wasm

# end of calcs

# run with `make watch version=9` to run tests / ghci for wasm-calc9
.PHONY: watch
version = 11
watch:
	ghciwatch --watch wasm-calc$(version) \
		--command "cabal repl wasm-calc$(version)-tests" \
		--test-ghci 'main'

# run with `make watch version=9` to run tests / ghci for wasm-calc9
.PHONY: watch-app
version = 11
watch-app:
	ghciwatch --watch wasm-calc$(version) \
		--command "cabal repl exe:wasm-calc$(version)" \
		--test-shell 'cabal install wasm-calc11 --overwrite-policy=always'

# run with `make watch version=9` to run tests / ghci for wasm-calc9
.PHONY: lsp
version = 11
lsp:
	ghciwatch --watch wasm-calc$(version) \
		--command "cabal repl calc-language-server$(version)" \
		--test-ghci 'main'

# run with `make run version=8` to run wasm-calc8
.PHONY: run
version = 11
run:
	cabal run wasm-calc$(version) -- repl

# run with `make test version=7` to run wasm-calc7 tests
.PHONY: test
version = 11
test:
	cabal run wasm-calc$(version):tests

# run with `make format-all-files version=7` to format all static `.calc` files for wasm-calc7
.PHONY: format-all-files
version = 11
STATIC_FILES = "./wasm-calc$(version)/test/static/"
format-all-files:
	find $(STATIC_FILES) -maxdepth 1 -type f -exec cabal run wasm-calc$(version) -- format {} \;

# run with `make build-malloc version=9` to build and diff malloc.calc for
# wasm-calc9
.PHONY: build-malloc
version = 11
build-malloc:
	nix run .#wasm-calc$(version) build wasm-calc$(version)/static/malloc.calc > wasm-calc$(version)/static/malloc-new.wasm
	diff wasm-calc$(version)/static/malloc-new.wasm wasm-calc$(version)/static/malloc.wasm

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
