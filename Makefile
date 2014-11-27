
all:
	cabal build

conf:
	cabal configure --disable-library-profiling
