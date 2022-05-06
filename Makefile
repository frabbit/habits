.PHONY: test-unit test-integration repl test-unit-ghcid test-integration-ghcid build-watch test-unit-watch

build-watch:
	stack build --file-watch

test-unit-watch:
	stack test --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-unit

test-integration-watch:
	stack test --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-integration

test-unit:
	stack test --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-unit

test-integration:
	stack test --ghc-options "-j4 +RTS -A128m -n2m -RTS" habits:habits-test-integration

repl:
	stack repl

test-unit-ghcid:
	reset && clear
	ghcid --restart=package.yaml --test=Main.main --command="stack ghci --ghci-options -isrc --ghci-options -itest --ghci-options -Wno-redundant-constraints --ghci-options -Wno-unused-binds --ghci-options -Wno-partial-type-signatures --ghci-options -Wno-unused-imports --ghci-options -Wno-unused-foralls habits:habits-test-unit"
	reset && clear

test-integration-ghcid:
	reset && clear
	ghcid --restart=package.yaml --test=Main.main --command="stack ghci --ghci-options -isrc --ghci-options -itest --ghci-options -Wno-redundant-constraints --ghci-options -Wno-unused-binds --ghci-options -Wno-partial-type-signatures --ghci-options -Wno-unused-imports --ghci-options -Wno-unused-foralls  habits:habits-test-integration"
	reset && clear