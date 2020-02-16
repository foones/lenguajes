
eightfold : src/Lang.hs src/Main.hs src/Parser.hs src/Lexer.hs
	ghc --make src/Main.hs -isrc/ -o eightfold

src/Parser.hs : src/Parser.y
	happy src/Parser.y
