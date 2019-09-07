SHELL = /bin/sh
CC = gcc
FLEX = flex
BISON = bison
AR = ar

.PHONY : all limpia clean

all : xul
	
INC = l/abs.h l/Len.h l/res.h
OUT = l/arbol.o l/ary.o l/bool.o l/built.o l/codigo.o \
l/cons.o l/file.o l/float.o l/func.o l/global.o l/hash.o \
l/int.o l/lose.o l/objs.o l/res.o l/seq.o l/str.o \
l/tipo.o l/tipos.o
liblen.a : $(OUT) $(PRG)
	$(AR) cr liblen.a $(OUT)

lex.yy.c : len.l
	$(FLEX) len.l

len.tab.h : len.tab.c

len.tab.c : len.y
	$(BISON) len.y -d

PAR = lex.yy.c len.tab.c parser.c
MAI = main.c
xul : liblen.a $(PAR) $(MAI) len.tab.h
	$(CC) -o xul -Il -L. $(PAR) $(MAI) -llen

limpia :
	rm *~ -f
	rm l/*~ -f

clean :
	rm *~ *.o -f
	rm lex.yy.c
	rm l/*~ l/*.o -f
