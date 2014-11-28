
all:
	cabal build

conf:
	cabal configure --disable-library-profiling

run:
	./dist/build/test/test

