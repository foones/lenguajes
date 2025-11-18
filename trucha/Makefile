
.PHONY : all test clean

all : trucha

trucha : src/* src/Syntax/* src/Test/*
	ghc -Wincomplete-patterns --make -isrc/ -outputdir obj -o trucha src/Main.hs

test : trucha
	./trucha -T

clean :
	rm -f trucha
	rm -rf obj/

