all: local

local:
	cabal install --prefix=$(HOME) --user

sdist:
	cabal sdist

check:
	cabal check

build:
	cabal build

clean:
	cabal clean
