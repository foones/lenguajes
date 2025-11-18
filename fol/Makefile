
.PHONY : build clean

BINARY=fol

GHCOPTS=-fwarn-incomplete-patterns

build :
	ghc -isrc/ $(GHCOPTS) --make src/Main.hs -o ${BINARY}

clean :
	rm -f src/*.{o,hi}

