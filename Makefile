SHELL:=cmd

all:
	cabal build

conf:
	cabal configure --enable-tests

doc:
	dist\build\docgen\docgen
	cabal haddock

lint:
	hlint .

run:
	dist\build\test\test > out.txt 2>&1

urun:
	./dist/build/test/test > out.txt 2>&1

