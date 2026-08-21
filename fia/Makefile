
.PHONY : build clean dist

BINARY=fia

GHCOPTS=-fwarn-incomplete-patterns

build :
	ghc -isrc/ $(GHCOPTS) --make src/Main.hs -o ${BINARY}

clean :
	rm -f src/*.o
	rm -f src/*.hi

