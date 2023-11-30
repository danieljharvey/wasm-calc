HS_FILES = $(shell git ls-files '*.hs' | grep -v 'vendored/')
CABAL_FILES = $(shell git ls-files '*.cabal' | grep -v 'vendored/')

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all -j4

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
