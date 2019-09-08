CC=gcc
FLEX=flex
BISON=bison
SRC=src/

OBJS=$(SRC)/sorcho.o $(SRC)/show.o $(SRC)/redu.o $(SRC)/env.o $(SRC)/lex.yy.o $(SRC)/parser.tab.o $(SRC)/err.o $(SRC)/kang.o
INCS=$(SRC)/Sor.h $(SRC)/sorcho.h $(SRC)/show.h $(SRC)/env.h $(SRC)/err.h $(SRC)/kang.h
sorcho : $(OBJS) $(INCS)
	$(CC) -o sorcho $(OBJS)

$(SRC)/lex.yy.c : $(SRC)/lexer.l $(SRC)/parser.tab.h
	$(FLEX) -o $(SRC)/lex.yy.c $(SRC)/lexer.l

$(SRC)/parser.tab.h $(SRC)/parser.tab.c : $(SRC)/parser.y
	$(BISON) -o $(SRC)/parser.tab.c $(SRC)/parser.y -d
