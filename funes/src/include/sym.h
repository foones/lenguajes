/* SYMTABLE_SIZE deberia ser un primo */
#define SYMTABLE_SIZE	211

typedef struct _package PACKAGE;
struct _package {
	char *name;
	RES *symbols;
};

typedef struct _symbol SYMBOL;
struct _symbol {
	char *name;
	RES *package;
};

void fu_init_symbol();

/* packages */
RES *fu_mkpackage(RES *name);
RES *fu_in_package(RES *name);
RES *fu_current_package();
RES *fu_set_current_package(RES *p);
RES *fu_find_package(RES *name);
RES *fu_package_name(RES *p);
RES *fu_package_symbols(RES *p);
/* symbols */
RES *fu_symbol_in(char *package_name, char *symbol_name);
RES *fu_symbol_name(RES *s);
RES *fu_funes_symbol(char *cadena);
RES *fu_local_symbol(char *cadena);
RES *fu_sym(char *cadena);
RES *fu_keyword(char *cadena);
RES *fu_keyword_p(RES *symbol);
RES *fu_keyword_argument(char *cadena);
RES *fu_exception_symbol(char *cadena);
RES *fu_gensym();
RES *fu_mksym(RES *args);
RES *fu_mkkey(RES *args);
RES *fu_mk_uninterned_sym(RES *args);
RES *fu_get_symbol(RES *x, RES *p);
RES *fu_intern(RES *x, RES *p);
RES *fu_unintern(RES *x, RES *p);
RES *fu_symbol_package(RES *s);

#define VAL_SYMNAME(S)		(((SYMBOL *) VAL(S))->name)
#define VAL_SYMPACK(S)		(((SYMBOL *) VAL(S))->package)

#define VAL_PACKNAME(S)		(((PACKAGE *) VAL(S))->name)
#define VAL_PACKSYMS(S)		(((PACKAGE *) VAL(S))->symbols)
