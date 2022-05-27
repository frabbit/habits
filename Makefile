.PHONY: test-unit test-integration repl test-unit-ghcid test-integration-ghcid build-watch test-unit-watch test-e2e

build-watch:
	stack build --file-watch

build-watch-all:
	stack build --file-watch --no-run-tests --ghc-options " -fprint-potential-instances" habits:habits-test-unit habits:habits-test-integration habits:habits-test-e2e

build-watch-unit:
	stack build --file-watch --no-run-tests --ghc-options " -fprint-potential-instances" habits:habits-test-unit

build-watch-integration:
	stack build --file-watch --no-run-tests --ghc-options " -fprint-potential-instances" habits:habits-test-integration

test-unit-watch:
	stack test --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS -src -itest -Wno-redundant-constraints -Wno-unused-matches -Wno-unused-binds -Wno-partial-type-signatures -Wno-unused-imports -Wno-unused-foralls" habits:habits-test-unit

test-integration-watch:
	stack test --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS -src -itest -Wno-redundant-constraints -Wno-unused-matches -Wno-unused-binds -Wno-partial-type-signatures -Wno-unused-imports -Wno-unused-foralls" habits:habits-test-integration

test-unit:
	stack test --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-unit

test-e2e:
	stack test --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-e2e

test-integration:
	stack test --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-integration

repl:
	stack repl

test-unit-ghcid:
	reset && clear
	ghcid --restart=package.yaml --test=Main.main --command="stack ghci --ghci-options -isrc --ghci-options -itest --ghci-options -Wno-unused-matches --ghci-options -Wno-redundant-constraints --ghci-options -Wno-unused-binds --ghci-options -Wno-partial-type-signatures --ghci-options -Wno-unused-imports --ghci-options -Wno-unused-foralls habits:habits-test-unit"
	reset && clear


test-integration-ghcid:
	reset && clear
	ghcid --restart=package.yaml --test=Main.main --command="stack ghci --ghci-options -isrc --ghci-options -itest --ghci-options -Wno-unused-matches --ghci-options -Wno-redundant-constraints --ghci-options -Wno-unused-binds --ghci-options -Wno-partial-type-signatures --ghci-options -Wno-unused-imports --ghci-options -Wno-unused-foralls  habits:habits-test-integration"
	reset && clear