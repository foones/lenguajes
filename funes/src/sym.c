#include "Fu.h"

RES *Meta_Package;
RES *Sym_Table;

RES *Funes_Package;
RES *Keyword_Package;
RES *Kwarg_Package;
RES *Exception_Package;

void
fu_init_symbol()
{
	Meta_Package = fu_make_hash(fu_int(SYMTABLE_SIZE));
	Keyword_Package = fu_mkpackage(fu_str("funes-keyword"));
	Kwarg_Package = fu_mkpackage(fu_str("funes-kwarg"));
	Exception_Package = fu_mkpackage(fu_str("funes-exception"));
	Sym_Table = Funes_Package = fu_mkpackage(fu_str("funes"));
}

RES *
fu_mkpackage(name)
	RES *name;
{
	unsigned s, l;
	RES *package;
   
	if (TIPO_P(tipo_sym, name))
		name = fu_str(VAL_SYMNAME(name));
	
	if ( fu_find_package(name) != NIL ) 
		fu_throw(fu_exception_symbol("package-error"),
					fu_str("mkpackage -- package already exists"));

	package = NEW(RES);
	TIPO(package) = tipo_package;
	VAL(package) = NEW(PACKAGE);
	VAL_PACKNAME(package) = VAL_STR(name);
	VAL_PACKSYMS(package) = fu_make_hash(fu_int(3));

	fu_set_hash(Meta_Package, name, package);
	s = HASH_SIZE(Meta_Package);
	l = HASH_LEN(Meta_Package);
	if (s / l > 1) fu_rehash(Meta_Package, fu_int(2 * l + 1));
	return package;
}

RES *
fu_current_package()
{
	return Sym_Table;
}

RES *
fu_set_current_package(RES *p)
{
	if (!TIPO_P(tipo_package, p))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("set_current_package -- no es un paquete"));
	return Sym_Table = p;
}

RES *
fu_package_name(RES *p)
{
	if (!TIPO_P(tipo_package, p))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("package-name -- no es un paquete"));
	return fu_str(VAL_PACKNAME(p));
}

RES *
fu_package_symbols(RES *p)
{
	if (!TIPO_P(tipo_package, p))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("package-symbols -- no es un paquete"));
	return VAL_PACKSYMS(p);
}

RES *
fu_symbol_package(RES *s)
{
	if (!TIPO_P(tipo_sym, s))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("symbol-package -- no es un simbolo"));
	return VAL_SYMPACK(s);
}

RES *
fu_find_package(name)
	RES *name;
{
	RES *r;
	if (TIPO_P(tipo_sym, name)) name = fu_str(VAL_SYMNAME(name));

	if (!STR_P(name))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("find-package -- no es un nombre"));

	r = fu_get_hash(Meta_Package, name);
	if (r != NIL)
		r = CDR(r);
	return r;
}

RES *
fu_sym(s)
	char *s;
{
	RES *v;

	v = NEW(RES);
	TIPO(v) = tipo_sym;
	VAL(v) = NEW(SYMBOL);
	VAL_SYMNAME(v) = s;
	VAL_SYMPACK(v) = NIL;
	return v;
}

RES *
fu_symbol_in_package(RES *package, char *cadena)
{
	RES *dict;
	RES *resultado, *cad;

	if (!TIPO_P(tipo_package, package))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("symbol_in_package -- no es un paquete"));

	dict = VAL_PACKSYMS(package);

	cad = fu_str(cadena);
	resultado = fu_get_hash(dict, cad);
	if ( resultado == NIL ) {
		RES *simbolo;
		unsigned s, l;

		simbolo = fu_sym(cadena);
		VAL_SYMPACK(simbolo) = package;
		resultado = fu_set_hash(dict, cad, simbolo);
		s = HASH_SIZE(dict);
		l = HASH_LEN(dict);
		if (s / l > 1) fu_rehash(dict, fu_int(2 * l + 1));
	}
	return CDR(resultado);
}

RES *
fu_keyword(cadena)
	char *cadena;
{
	return fu_symbol_in_package(Keyword_Package, cadena);
}

RES *
fu_keyword_argument(cadena)
	char *cadena;
{
	return fu_symbol_in_package(Kwarg_Package, cadena);
}

RES *
fu_exception_symbol(cadena)
	char *cadena;
{
	return fu_symbol_in_package(Exception_Package, cadena);
}

RES *
fu_keyword_p(symbol)
	RES *symbol;
{
	return (TIPO_P(tipo_sym, symbol) &&
				(VAL_SYMPACK(symbol) == Keyword_Package))
			? T : NIL;
}

RES *
fu_symbol_in(char *package_name, char *symbol_name) {
	RES *r = fu_get_hash(Meta_Package, fu_str(package_name));
	if ( r == NIL ) 
		fu_throw(fu_exception_symbol("package-error"),
					fu_str("in-package -- package does not exist"));
	return fu_symbol_in_package(CDR(r), symbol_name);
}

/* en funes::... */
RES *
fu_funes_symbol(cadena)
	char *cadena;
{
	return fu_symbol_in_package(Funes_Package, cadena);
}

RES *
fu_local_symbol(cadena)
	char *cadena;
{
	return fu_symbol_in_package(Sym_Table, cadena);
}

RES *
fu_mk_uninterned_sym(args)
	RES *args;
{
	return fu_sym(VAL_STR(fu_mkstr(args)));
}


RES *
fu_mksym(args)
	RES *args;
{
	return fu_symbol_in_package(Sym_Table, VAL_STR(fu_mkstr(args)));
}

RES *
fu_mkkey(args)
	RES *args;
{
	return fu_keyword(VAL_STR(fu_mkstr(args)));
}

RES *
fu_symbol_name(s)
	RES *s;
{
	if (!TIPO_P(tipo_sym, s))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("symbol-name -- no es un simbolo"));
	return fu_str(VAL_SYMNAME(s));
}

RES *
fu_gensym()
{
	static unsigned last_gensym = 0;
	char *s;
	unsigned l;

	//l = (last_gensym % 10) + 2;
	l = 6;
	s = NEWQ(char, l);
	sprintf(s, "G%.4i", (last_gensym++ % 1000));
	s[l - 1] = '\0';
	return fu_sym(s);
}

RES *
fu_get_symbol(symbol, package)
	RES *symbol;
	RES *package;
{
	if (!TIPO_P(tipo_sym, symbol))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("intern -- no es un simbolo"));

	if (package == UNDEF)
		package = Sym_Table;

	if (!TIPO_P(tipo_package, package))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("intern -- no es un paquete"));

	return fu_symbol_in_package(package, VAL_SYMNAME(symbol));
}

RES *
fu_intern(symbol, package)
	RES *symbol;
	RES *package;
{
	unsigned s, l;
	RES *dict;
	RES *cad;
	RES *resultado;

	if (!TIPO_P(tipo_sym, symbol))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("intern -- no es un simbolo"));

	if (package == UNDEF)
		package = Sym_Table;

	if (!TIPO_P(tipo_package, package))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("intern -- no es un paquete"));

	dict = VAL_PACKSYMS(package);
	cad = fu_str(VAL_SYMNAME(symbol));
	resultado = fu_get_hash(dict, cad);
	if ( resultado != NIL ) 
		fu_throw(fu_exception_symbol("name-conflict"),
					fu_mkstr(fu_make_list("xx",
						fu_str("intern -- shadows symbol: "),
						symbol)));
	resultado = fu_set_hash(dict, cad, symbol);
	s = HASH_SIZE(dict);
	l = HASH_LEN(dict);
	if (s / l > 1) fu_rehash(dict, fu_int(2 * l + 1));
	if (VAL_SYMPACK(symbol) == NIL)
			VAL_SYMPACK(symbol) = package;
	return CDR(resultado);
}

RES *
fu_unintern(symbol, package)
	RES *symbol;
	RES *package;
{
	RES *r;

	if (!TIPO_P(tipo_sym, symbol))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("unintern -- no es un simbolo"));

	if (package == UNDEF)
		package = Sym_Table;

	if (!TIPO_P(tipo_package, package))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("unintern -- no es un paquete"));
   
	r = fu_del_hash(VAL_PACKSYMS(package), fu_str(VAL_SYMNAME(symbol)));
	if (VAL_SYMPACK(symbol) == package)
			VAL_SYMPACK(symbol) = NIL;
	return r;
}

