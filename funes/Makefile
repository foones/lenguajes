CC=gcc
FLEX=flex
BISON=bison

LDIR=src/
IDIR=src/include
GCDIR=../gc
CFLAGS=-I$(GCDIR)/include -I$(IDIR) -Wall -O2 -fPIC
LDFLAGS=-Wall -fPIC -lm -ldl -lgc

# Enable debug?
CFLAGS+= -g -ggdb

# Enable profiling?
CFLAGS+= -ggdb -pg
LDFLAGS+= -ggdb -pg -lgc

OBJS=$(LDIR)/hash.o $(LDIR)/out.o $(LDIR)/vec.o $(LDIR)/cproc.o $(LDIR)/vm.o $(LDIR)/sym.o $(LDIR)/comp.o $(LDIR)/list.o $(LDIR)/env.o $(LDIR)/special.o $(LDIR)/lex.yy.o $(LDIR)/parser.tab.o $(LDIR)/port.o $(LDIR)/str.o $(LDIR)/built.o $(LDIR)/int.o $(LDIR)/error.o $(LDIR)/std.o $(LDIR)/macro.o $(LDIR)/char.o $(LDIR)/setter.o $(LDIR)/inline.o $(LDIR)/tipo.o $(LDIR)/struct.o $(LDIR)/float.o

INCS=$(IDIR)/hash.h $(IDIR)/out.h $(IDIR)/vec.h $(IDIR)/cproc.h $(IDIR)/vm.h $(IDIR)/sym.h $(IDIR)/list.h $(IDIR)/comp.h $(IDIR)/env.h $(IDIR)/special.h $(IDIR)/port.h $(IDIR)/str.h $(IDIR)/built.h $(IDIR)/int.h $(IDIR)/error.h $(IDIR)/std.h $(IDIR)/macro.h $(IDIR)/char.h $(IDIR)/setter.h $(IDIR)/inline.h $(IDIR)/tipo.h

MAIN=$(LDIR)/fu.o

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

fu : $(MAIN) $(INCS) libfunes.so
	$(CC) -o funes $(MAIN) $(OBJS) $(LDFLAGS)

libfunes.so : $(OBJS)
	$(CC) -shared -o libfunes.so $(OBJS) $(LDFLAGS)
	
#-Wl,--add-stdcall-alias

$(LDIR)/lex.yy.c : $(LDIR)/lexer.l $(LDIR)/parser.tab.h
	$(FLEX) -o $(LDIR)/lex.yy.c $(LDIR)/lexer.l

$(LDIR)/parser.tab.h $(LDIR)/parser.tab.c : $(LDIR)/parser.y
	$(BISON) $(LDIR)/parser.y -d -o $(LDIR)/parser.tab.c

clean:
	rm -f $(LDIR)/*.o $(LDIR)/fu.o

