#include "Fu.h"
#include <stdlib.h>

#define BIND(SYMB, COSA)	fu_def_env(fu_funes_symbol(SYMB), COSA)
#define CON_SETTER(SYMB, COSA, SETTER)	{\
				RES *_pr;\
				_pr = COSA;\
				BIND(SYMB, _pr);\
				fu_setter_set(_pr, SETTER);\
				}
#define INLINE(SYMB, COSA, INLINED)	{\
				RES *_pr;\
				_pr = COSA;\
				BIND(SYMB, _pr);\
				fu_inlined_set(_pr, INLINED);\
				}
#define INLINE_CON_SETTER(SYMB, COSA, INLINED, SETTER)	{\
				RES *_pr;\
				_pr = COSA;\
				BIND(SYMB, _pr);\
				fu_inlined_set(_pr, INLINED);\
				fu_setter_set(_pr, SETTER);\
				}

/*
 * nil		0000 0011
 * t		0001 0011
 * eof		0010 0011
 * undef	0011 0011
 * argset	0100 0011
 */
RES* NIL = (RES *)		0x03;
RES* T = (RES *)	 	0x13;
RES* EOF_OBJECT = (RES *)	0x23;
RES* UNDEF = (RES *)		0x33;
RES* ARGSET = (RES *)		0x43;
RES *Fu_Argv;

void
fu_init()
{
	Fu_Argv = NIL;

	fu_init_symbol();
	fu_init_env();
	fu_init_tipos();
	fu_init_setter();
	fu_init_inline();
	fu_init_special();
	fu_init_port();
	fu_init_builtins();
	fu_init_macro();
	fu_init_reader();

	Fu_Err_Salto = NEW(jmp_buf);
	if ( setjmp(*Fu_Err_Salto) ) {
		fprintf(stderr, "Excepcion no cachada: ");
		fu_fprint(Std_Error, Fu_Err_Excepcion, T);
		fprintf(stderr, "\n");
		fu_fprint(Std_Error, Fu_Err_Descripcion, T);
		fprintf(stderr, "\n");
		exit(1);
	}

}

RES *
fu_getenv(s)
	RES *s;
{
	char *r;

	if (!STR_P(s))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("getenv -- debe ser una cadena"));

	r = getenv(VAL_STR(s));
	if (r == NULL)
		return NIL;
	else
		return fu_str(r);
}

void
fu_load_init_file()
{
	RES *p;

	p = fu_getenv(fu_str("FUNES_PATH"));
	if (p == NIL) {
		fprintf(stderr, "warning: variable FUNES_PATH indefinida\n");
	} else {
		fu_load(fu_str_cat(p, fu_str("/init.fu")), NIL);
	}
}

RES *
fu_args()
{
	return Fu_Argv;
}

void
fu_init_builtins()
{
		RES *p;

		BIND("cons", fu_make_proc(fu_cons, 2, 0, 0));
		BIND("acons", fu_make_proc(fu_acons, 3, 0, 0));

		INLINE_CON_SETTER("car",
						fu_make_proc(fu_car, 1, 0, 0),
						special_form_car,
						fu_make_proc(fu_car_set, 2, 0, 0));
		INLINE_CON_SETTER("cdr",
						fu_make_proc(fu_cdr, 1, 0, 0),
						special_form_cdr,
						fu_make_proc(fu_cdr_set, 2, 0, 0));

		/* INT */
		BIND("+", fu_make_proc(fu_int_sumar, 0, 0, 1));
		BIND("-", fu_make_proc(fu_int_restar, 0, 0, 1));
		BIND("*", fu_make_proc(fu_int_multiplicar, 0, 0, 1));
		BIND("expt", fu_make_proc(fu_int_elevar, 2, 0, 0));
		BIND("=", fu_make_proc(fu_int_igual, 0, 0, 1));
		BIND("<", fu_make_proc(fu_int_menor, 0, 0, 1));
		BIND("<=", fu_make_proc(fu_int_menor_o_igual, 0, 0, 1));
		BIND(">", fu_make_proc(fu_int_mayor, 0, 0, 1));
		BIND(">=", fu_make_proc(fu_int_mayor_o_igual, 0, 0, 1));

		BIND("%", fu_make_proc(fu_int_mod, 2, 0, 0));
		BIND("remainder", fu_make_proc(fu_int_remainder, 2, 0, 0));
		BIND("quotient", fu_make_proc(fu_int_quotient, 2, 0, 0));

		/* FLOAT */
		BIND("f+", fu_make_proc(fu_float_sumar, 0, 0, 1));
		BIND("f-", fu_make_proc(fu_float_restar, 0, 0, 1));
		BIND("f*", fu_make_proc(fu_float_multiplicar, 0, 0, 1));
		BIND("fexpt", fu_make_proc(fu_float_elevar, 2, 0, 0));
		BIND("f<", fu_make_proc(fu_float_menor, 0, 0, 1));
		BIND("f>", fu_make_proc(fu_float_mayor, 0, 0, 1));

		BIND("f/", fu_make_proc(fu_float_dividir, 0, 0, 1));

		BIND("float-floor", fu_make_proc(fu_float_floor, 1, 0, 0));
		BIND("float-ceil", fu_make_proc(fu_float_ceil, 1, 0, 0));
		BIND("int->float", fu_make_proc(fu_int_to_float, 1, 0, 0));

		/* CLOSURES */
		CON_SETTER("fenv", fu_make_proc(fu_fenv, 1, 0, 0),
						  fu_make_proc(fu_fenv_set, 2, 0, 0));
		/* macros */
		BIND("set-macro-flag", fu_make_proc(fu_set_macro_flag, 1, 0, 0));

		/* ENTORNO */
		CON_SETTER("environment", fu_make_proc(fu_env, 0, 0, 0),
						          fu_make_proc(fu_with_env, 1, 0, 0));
		INLINE("dir", fu_make_proc(fu_dir, 0, 0, 0),
						special_form_dir);
		BIND("envdef", fu_make_proc(fu_def_env, 2, 0, 0));
		BIND("envset", fu_make_proc(fu_set_env, 2, 0, 0));
		BIND("envget", fu_make_proc(fu_get_env, 1, 0, 0));

		CON_SETTER("compile-value",
			fu_make_proc(fu_compile_value, 1, 0, 0),
			fu_make_proc(fu_set_compile_value, 2, 0, 0));
		CON_SETTER("compile-environment",
			fu_make_proc(fu_compile_env, 0, 0, 0),
			fu_make_proc(fu_set_compile_env, 1, 0, 0));
		BIND("compile-dir", fu_make_proc(fu_compile_dir, 0, 0, 0));

		/* reader */
		BIND("read", fu_make_proc(fu_read, 0, 1, 0));
		BIND("set-reader-macro", fu_make_proc(fu_set_reader_macro, 2, 0, 0));
		BIND("set-reader-funapply", fu_make_proc(fu_set_reader_funapply, 1, 0, 0));

		/* mas cosas */
		BIND("eval", fu_make_proc(fu_eval, 1, 0, 0));
		BIND("stread", fu_make_proc(fu_stread, 1, 0, 0));
		INLINE("eq", fu_make_proc(fu_eq, 2, 0, 0),
						special_form_eq);

		BIND("input", fu_make_proc(fu_input, 0, 0, 0));

		BIND("dynl", fu_make_proc(fu_dynl, 1, 0, 1));

		p = fu_make_proc(fu_null, 1, 0, 0);
		BIND("null", p);
		INLINE("not", p, special_form_not);

		p = fu_make_proc(fu_throw, 1, 1, 0);
		BIND("err", p);
		BIND("throw", p);

		BIND("handle", fu_make_proc(fu_handle, 3, 0, 0));
		BIND("map", fu_make_proc(fu_map, 1, 0, 1));
		BIND("mapc", fu_make_proc(fu_mapc, 1, 0, 1));
		BIND("maplist", fu_make_proc(fu_maplist, 1, 0, 1));
		BIND("mapl", fu_make_proc(fu_mapl, 1, 0, 1));
		BIND("mapt", fu_make_proc(fu_mapt, 1, 0, 1));
		BIND("list", fu_make_proc(fu_list, 0, 0, 1));
		BIND("list*", fu_make_proc(fu_list_1, 0, 0, 1));
		BIND("vec", fu_make_proc(fu_vector_builtin, 0, 0, 1));
		BIND("mkvec", fu_make_proc(fu_make_vector, 1, 1, 0));
		BIND("consp", fu_make_proc(fu_consp, 1, 0, 0));
		BIND("ord", fu_make_proc(fu_char_int, 1, 0, 0));
		BIND("chr", fu_make_proc(fu_int_char, 1, 0, 0));
		/*BIND("call", fu_make_proc(fu_call, 1, 0, 1));*/

		/* paquetes */
		CON_SETTER("package", fu_make_proc(fu_current_package, 0, 0, 0),
						fu_make_proc(fu_set_current_package, 1, 0, 0));
		BIND("mkpackage", fu_make_proc(fu_mkpackage, 1, 0, 0));
		BIND("find-package", fu_make_proc(fu_find_package, 1, 0, 0));
		BIND("package-name", fu_make_proc(fu_package_name, 1, 0, 0));
		BIND("package-symbols", fu_make_proc(fu_package_symbols, 1, 0, 0));
		/* simbolos */
		BIND("symbol-name", fu_make_proc(fu_symbol_name, 1, 0, 0));
		BIND("symbol-package", fu_make_proc(fu_symbol_package, 1, 0, 0));
		BIND("gensym", fu_make_proc(fu_gensym, 0, 0, 0));
		BIND("sym", fu_make_proc(fu_mksym, 0, 0, 1));
		BIND("key", fu_make_proc(fu_mkkey, 0, 0, 1));
		BIND("mksym", fu_make_proc(fu_mk_uninterned_sym, 0, 0, 1));
		BIND("get-symbol", fu_make_proc(fu_get_symbol, 1, 1, 0));
		BIND("intern", fu_make_proc(fu_intern, 1, 1, 0));
		BIND("unintern", fu_make_proc(fu_unintern, 1, 1, 0));

		/* cadenas */
		BIND("str", fu_make_proc(fu_mkstr, 0, 0, 1));
		BIND("fmt", fu_make_proc(fu_fmt, 1, 0, 1));
		BIND("num->str", fu_make_proc(fu_num_to_str, 1, 5, 0));

		/* secuencias */
		BIND("cat", fu_make_proc(fu_cat, 0, 0, 1));
		BIND("join", fu_make_proc(fu_join, 1, 0, 1));
		BIND("len", fu_make_proc(fu_length, 1, 0, 0));
		BIND("rev", fu_make_proc(fu_rev, 1, 0, 0));
		BIND("xrev", fu_make_proc(fu_xrev, 1, 0, 0));
		CON_SETTER("elt", fu_make_proc(fu_elt, 2, 0, 0),
						  fu_make_proc(fu_elt_set, 3, 0, 0));
		BIND("copy", fu_make_proc(fu_copy_shallow, 1, 0, 0));
		BIND("index", fu_make_proc(fu_index, 2, 1, 0));
		BIND("slice", fu_make_proc(fu_slice, 1, 3, 0));
		BIND("any", fu_make_proc(fu_any, 2, 0, 0));
		BIND("all", fu_make_proc(fu_all, 2, 0, 0));
		BIND("not-any", fu_make_proc(fu_not_any, 2, 0, 0));
		BIND("not-all", fu_make_proc(fu_not_all, 2, 0, 0));

		/* listas */
		BIND("mklist", fu_make_proc(fu_mklist, 1, 1, 0));
		CON_SETTER("nth", fu_make_proc(fu_nth, 2, 0, 0),
						  fu_make_proc(fu_nth_set, 3, 0, 0));
		CON_SETTER("ntl", fu_make_proc(fu_ntl, 2, 0, 0),
						  fu_make_proc(fu_ntl_set, 3, 0, 0));
		BIND("member", fu_make_proc(fu_member, 2, 1, 0));
		BIND("assoc", fu_make_proc(fu_assoc, 2, 1, 0));
		BIND("last", fu_make_proc(fu_last, 1, 0, 0));
		BIND("range", fu_make_proc(fu_range, 1, 2, 0));
		BIND("iota", fu_make_proc(fu_iota, 1, 2, 0));

		/* vectores */
		CON_SETTER("vref", fu_make_proc(fu_get_vector, 2, 0, 0),
						   fu_make_proc(fu_set_vector, 3, 0, 0));

		/* hashes */
		CON_SETTER("get", fu_make_proc(fu_get_hash_eq, 2, 0, 0),
						   fu_make_proc(fu_set_hash_eq, 3, 0, 0));
		CON_SETTER("hget", fu_make_proc(fu_get_hash, 2, 0, 0),
						  fu_make_proc(fu_set_hash, 3, 0, 0));
		CON_SETTER("fget", fu_make_proc(fu_get_hash_f, 4, 0, 0),
						   fu_make_proc(fu_set_hash_f, 5, 0, 0));
		BIND("del", fu_make_proc(fu_del_hash_eq, 2, 0, 0));
		BIND("rehash", fu_make_proc(fu_rehash_eq, 2, 0, 0));
		BIND("hdel", fu_make_proc(fu_del_hash, 2, 0, 0));
		BIND("hrehash", fu_make_proc(fu_rehash, 2, 0, 0));
		BIND("fdel", fu_make_proc(fu_del_hash_f, 4, 0, 0));
		BIND("frehash", fu_make_proc(fu_rehash_f, 4, 0, 0));

		BIND("hash-eq", fu_make_proc(fu_hash_eq, 2, 0, 0));
		BIND("db", fu_make_proc(fu_db, 0, 0, 1));
		BIND("mkdb", fu_make_proc(fu_make_hash, 0, 1, 0));
		BIND("data", fu_make_proc(fu_db_data, 1, 1, 0));
		BIND("db->vec", fu_make_proc(fu_db_vec, 1, 0, 0));
		BIND("keys", fu_make_proc(fu_db_keys, 1, 0, 0));
		BIND("dbsize", fu_make_proc(fu_db_size, 1, 0, 0));

		/* coerciones */
		BIND("list->vec", fu_make_proc(fu_list_to_vec, 1, 1, 0));
		BIND("list->str", fu_make_proc(fu_list_to_str, 1, 1, 0));
		BIND("vec->list", fu_make_proc(fu_vec_to_list, 1, 0, 0));

		/* estructuras */
		BIND("mkstype", fu_make_proc(fu_make_stype, 1, 0, 1));
		BIND("stype-accessor", fu_make_proc(fu_stype_accessor, 2, 0, 0));
		BIND("stype-constructor", fu_make_proc(fu_stype_constructor, 2, 0, 0));
		BIND("stype-slots", fu_make_proc(fu_stype_slots, 1, 0, 0));
		BIND("stype-super", fu_make_proc(fu_stype_super, 1, 0, 0));
		BIND("mkstruct", fu_make_proc(fu_make_struct, 1, 0, 0));
		BIND("struct-type", fu_make_proc(fu_struct_type, 1, 0, 0));
		BIND("struct-data", fu_make_proc(fu_struct_data, 1, 0, 0));
		BIND("struct-copy", fu_make_proc(fu_struct_copy, 1, 0, 0));
		p = fu_make_proc(fu_slot_value_set, 3, 0, 0);
		CON_SETTER("slot-value",
						fu_make_proc(fu_slot_value, 2, 0, 0),
						p);
		BIND("slot-value-set", p);
		BIND("make", fu_make_proc(fu_make, 1, 0, 1));

		/* archivos */
		BIND("load", fu_make_proc(fu_load, 1, 0, 1));
		BIND("args", fu_make_proc(fu_args, 0, 0, 0));

		BIND("open", fu_make_proc(fu_open, 2, 0, 0));
		BIND("close", fu_make_proc(fu_close, 1, 0, 0));
		BIND("exists", fu_make_proc(fu_exists, 1, 0, 0));

		BIND("fread", fu_make_proc(fu_fread, 1, 0, 0));
		BIND("flines", fu_make_proc(fu_flines, 1, 0, 0));
		BIND("fwrite", fu_make_proc(fu_fwrite, 2, 0, 0));

		/* sistema */
		BIND("sys", fu_make_proc(fu_sys, 0, 0, 1));
		BIND("getenv", fu_make_proc(fu_getenv, 1, 0, 0));

		/* finalizacion */
		BIND("die", fu_make_proc(fu_die, 0, 2, 0));
		BIND("exit", fu_make_proc(fu_exit, 0, 1, 0));

		/* output */
		BIND("out", fu_make_proc(fu_fprint, 1, 2, 0));
		BIND("pr", fu_make_proc(fu_pr, 1, 1, 0));
		BIND("pr1", fu_make_proc(fu_pr1, 1, 1, 0));
		BIND("wr", fu_make_proc(fu_wr, 1, 1, 0));
		BIND("wr1", fu_make_proc(fu_wr1, 1, 1, 0));
		BIND("cr", fu_make_proc(fu_cr, 0, 1, 0));
		BIND("lf", fu_make_proc(fu_lf, 0, 1, 0));
		BIND("crlf", fu_make_proc(fu_crlf, 0, 1, 0));

		/* random */
		BIND("rand", fu_make_proc(fu_rand, 1, 0, 0));
		BIND("choice", fu_make_proc(fu_choice, 1, 2, 0));

		/* tipos */
		BIND("typep", fu_make_proc(fu_typep, 2, 0, 0));
		BIND("type", fu_make_proc(fu_type, 1, 0, 0));
		BIND("type-of", fu_make_proc(fu_type_of, 1, 0, 0));
		BIND("check", fu_make_proc(fu_check, 2, 2, 0));
		
		BIND("os-type", fu_make_proc(fu_os_type, 0, 0, 0));

		CON_SETTER("setter", fu_make_proc(fu_setter, 1, 0, 0),
					   fu_make_proc(fu_setter_set, 2, 0, 0));
		CON_SETTER("inlined", fu_make_proc(fu_inlined, 1, 0, 0),
					   fu_make_proc(fu_inlined_set, 2, 0, 0));
}

