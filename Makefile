
HS_FILES = $(shell git ls-files '*.hs')

.PHONY: ghcid
ghcid:
	ghcid -c "cabal repl mimsa" -l=hlint

.PHONY: ghcid-test
ghcid-test:
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint

.PHONY: ghcid-repl
ghcid-repl:
	ghcid -c "cabal repl mimsa:exe:mimsa" -l=hlint

.PHONY: ghcid-server
ghcid-server:
	ghcid -c "cabal repl server:exe:mimsa-server" -l=hlint

.PHONY: update
update:
	cabal update

.PHONY: build
build: update
	cabal build all

.PHONY: install
install: update
	cabal install mimsa:exe:mimsa --overwrite-policy=always
	cabal install server:exe:mimsa-server --overwrite-policy=always

.PHONY: run-server
run-server: update
	cabal run server:exe:mimsa-server

.PHONY: test
test: update
	cabal run mimsa:test:mimsa-test

.PHONY: test-watch
test-watch: update
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint --test="main"

.PHONY: freeze
freeze:
	cabal freeze --enable-tests --enable-benchmarks

.PHONY: bench
bench:
	cabal bench mimsa

.PHONY: format
format:
	@ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"

.PHONY: generate-swagger
generate-swagger: install
	$(shell cabal list-bin server:exe:mimsa-server) generate-swagger