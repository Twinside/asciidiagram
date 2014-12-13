SHELL:=cmd

all:
	cabal build

conf:
	cabal configure --disable-library-profiling

lint:
	hlint .

run:
	dist\build\test\test > out.txt 2>&1

