.PHONY: test-unit test-integration repl test-unit-ghcid test-integration-ghcid build-watch test-unit-watch test-e2e build-all clean test-compile-ghcid

test-mode=-fbyte-code # or -fobject-code

ghci-options= --no-build --ghc-options $(test-mode)

ghci-disabled-warnings=  --ghci-options -Wno-unused-matches \
	--ghci-options -Wno-redundant-constraints \
	--ghci-options -Wno-unused-binds \
	--ghci-options -Wno-partial-type-signatures \
	--ghci-options -Wno-unused-imports \
	--ghci-options -Wno-unused-foralls

clean:
	stack clean

test-all:
	stack test --ghc-options "-O0 -j4 +RTS -A128m -n2m -RTS" habits:habits-test-all

test-unit:
	stack test --ghc-options "-O0 -j4 +RTS -A128m -n2m -RTS" habits:habits-test-unit

test-e2e:
	stack test --ghc-options "-O0 -j4 +RTS -A128m -n2m -RTS" habits:habits-test-e2e

test-integration:
	stack test --ghc-options "-O0 -j4 +RTS -A128m -n2m -RTS" habits:habits-test-integration

repl:
	stack ghci $(ghci-options) habits:habits-test-all


test-ghcid:
	reset && clear
	ghcid --restart=habits.cabal --reload=.reload-ghcid --test=:quit \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
		habits:habits-test-unit"
	reset && clear

test-unit-ghcid:
	reset && clear
	ghcid --restart=habits.cabal --reload=.reload-ghcid --test=UnitSpec.main \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
		habits:habits-test-all"
	reset && clear

test-e2e-ghcid:
	reset && clear
	ghcid --restart=habits.cabal --reload=.reload-ghcid --test=E2ESpec.main \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
			habits:habits-test-all"
	reset && clear

test-all-ghcid:
	reset && clear
	ghcid --restart=habits.cabal --reload=.reload-ghcid --test=Main.main \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
			habits:habits-test-all"
	reset && clear

test-integration-ghcid:
	reset && clear
	ghcid --restart=habits.cabal --reload=.reload-ghcid --test=IntegrationSpec.main \
		--command="stack ghci \
		$(ghci-options) $(ghci-disabled-warnings) \
		habits:habits-test-all"
	reset && clear

test-compile-ghcid:
	reset && clear
	ghcid --restart=habits.cabal --reload=.reload-ghcid \
		--command="stack ghci \
		$(ghci-options) \
		habits:habits-test-all"
	reset && clear


reload-ghcid:
	touch .reload-ghcid

restart-ghcid:
	touch habits.cabal