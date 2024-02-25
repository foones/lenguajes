
.PHONY : build clean dist

BINARY=hdp

GHCOPTS=-fwarn-incomplete-patterns

build :
	ghc -isrc/ $(GHCOPTS) --make app/Main.hs -o ${BINARY}

clean :
	rm -f app/*.{o,hi} \
          src/*.{o,hi} \
          src/Calculus/*.{o,hi} \
          src/Desugaring/*.{o,hi} \
          src/Lexer/*.{o,hi} \
          src/ModuleSystem/*.{o,hi} \
          src/Parser/*.{o,hi} \
          src/Syntax/*.{o,hi} \

