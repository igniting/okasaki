all: ex2_1
	./ex2_1 10000 +RTS -p > /dev/null
ex2_1: ex2_1.hs
	ghc -prof -fprof-auto -rtsopts ex2_1.hs
clean:
	rm -f ex2_1 ex2_1.hi ex2_1.o ex2_1.prof ex2_1.hp ex2_1.aux ex2_1.ps
