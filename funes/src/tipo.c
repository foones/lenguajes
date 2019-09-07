#include "Fu.h"

#define MK_BUILTIN(X,TB,NOMBRE,TBLOCAL)	 {\
	TIPO *t = NEW(TIPO);\
	TBLOCAL = (X) = NEW(RES);\
	TIPO(X) = tipo_tipo;\
	TIPO_NOMBRE(t) = NOMBRE;\
	TIPO_DES(t) = (RES *) ((TB << 2) | 1);\
	VAL(X) = (void *) t;\
}
#define MK_COMPUESTO(X,DES) {\
	TIPO *t = NEW(TIPO);\
	(X) = NEW(RES);\
	TIPO(X) = tipo_tipo;\
	TIPO_DES(t) = DES;\
	VAL(X) = (void *) t;\
}
#define TBUILT(N, Y, YLOCAL) {\
	RES *__p;\
	MK_BUILTIN(__p, Y, N, YLOCAL);\
	BIND(N, __p);\
}
#define TCOMPU(N, Y) {\
	RES *__p;\
	MK_COMPUESTO(__p, Y);\
	BIND(N, __p);\
}
#define BIND(SYMB, COSA) fu_def_env(fu_funes_symbol(SYMB), COSA)
#define TI(SYMB) CDR(fu_get_env(fu_funes_symbol(SYMB)))

RES *_tipo_sym;
RES *_tipo_vector;
RES *_tipo_proc;
RES *_tipo_special_form;
RES *_tipo_closure;
RES *_tipo_port;
RES *_tipo_reader;
RES *_tipo_stype;
RES *_tipo_struct;
RES *_tipo_tipo;
RES *_tipo_float;
RES *_tipo_cont;
RES *_tipo_package;
RES *_tipo_magic_char;
RES *_tipo_magic_cons;
RES *_tipo_magic_int;
RES *_tipo_magic_null;
RES *_tipo_magic_t;
RES *_tipo_magic_eof;
RES *_tipo_magic_undef;
RES *_tipo_magic_macro;
RES *_tipo_magic_str;
RES *_tipo_magic_hash;
RES *_tipo_magic_any;

void
fu_init_tipos()
{
	TBUILT("<sym>", tipo_sym, _tipo_sym);
	TBUILT("<vec>", tipo_vector, _tipo_vector);
	TBUILT("<special-form>", tipo_special_form, _tipo_special_form);
	TBUILT("<proc>", tipo_proc, _tipo_proc);
	TBUILT("<closure>", tipo_closure, _tipo_closure);
	TBUILT("<port>", tipo_port, _tipo_port);
	TBUILT("<reader>", tipo_reader, _tipo_reader);
	TBUILT("<stype>", tipo_stype, _tipo_stype);
	TBUILT("<struct>", tipo_struct, _tipo_struct);
	TBUILT("<type>", tipo_tipo, _tipo_tipo);
	TBUILT("<float>", tipo_float, _tipo_float);
	TBUILT("<cont>", tipo_cont, _tipo_cont);
	TBUILT("<package>", tipo_package, _tipo_package);

	/* tipos magicos (ningun objeto tiene realmente
	 * ese tipo, sino que esta taggeado de otra manera)
	 */
	TBUILT("<int>", tipo_magic_int, _tipo_magic_int);
	TBUILT("<char>", tipo_magic_char, _tipo_magic_char);
	TBUILT("<cons>", tipo_magic_cons, _tipo_magic_cons);
	TBUILT("<null>", tipo_magic_null, _tipo_magic_null);
	TBUILT("<t>", tipo_magic_t, _tipo_magic_t);
	TBUILT("<eof>", tipo_magic_eof, _tipo_magic_eof);
	TBUILT("<undef>", tipo_magic_undef, _tipo_magic_undef);
	TBUILT("<macro>", tipo_magic_macro, _tipo_magic_macro);
	TBUILT("<str>", tipo_magic_str, _tipo_magic_str);
	TBUILT("<hash>", tipo_magic_hash, _tipo_magic_hash);
	TBUILT("<any>", tipo_magic_any, _tipo_magic_any);

	/*tipo_str,*/

	TCOMPU("<number>", fu_make_list("xxx",
				fu_funes_symbol("union"),
				TI("<int>"),
				TI("<float>")));
	TCOMPU("<list>", fu_make_list("xxx",
				fu_funes_symbol("union"),
				TI("<cons>"),
				TI("<null>")));
	TCOMPU("<seq>", fu_make_list("xxxx",
				fu_funes_symbol("union"),
				TI("<cons>"),
				TI("<null>"),
				TI("<vec>")));
	TCOMPU("<function>", fu_make_list("xxx",
				fu_funes_symbol("union"),
				TI("<proc>"),
				TI("<closure>")));
}

RES *
fu_type(des)
	RES *des;
{
	RES *p;
	MK_COMPUESTO(p, des);
	return p;
}
#undef TI
#undef BIND
#undef MK_BUILTIN
#undef MK_COMPUESTO
#undef TBUILT
#undef TCOMPU

RES *
fu_typep(tipo, expr)
	RES *tipo, *expr;
{
	TIPO *t;

	if (TIPO_P(tipo_stype, tipo) )
		return fu_struct_stypep(tipo, expr);

	if (!TIPO_P(tipo_tipo, tipo))	
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("typep -- el primer argumento debe ser un tipo"));

	t = VAL_TIPO(tipo);

	if (TIPO_BUILTIN_P(t)) {
		int tag = TIPO_BUILT(t);

		switch (tag) {
		case tipo_magic_int:
			return INT_P(expr) ? T : NIL;
		case tipo_magic_char:
			return CHAR_P(expr) ? T : NIL;
		case tipo_magic_cons:
			return CONS_P(expr) ? T : NIL;
		case tipo_magic_null:
			return (expr == NIL) ? T : NIL;
		case tipo_magic_t:
			return (expr == T) ? T : NIL;
		case tipo_magic_eof:
			return (expr == EOF_OBJECT) ? T : NIL;
		case tipo_magic_undef:
			return (expr == UNDEF) ? T : NIL;
		case tipo_magic_macro:
			return MACROP(expr) ? T : NIL;
		case tipo_magic_str:
			return STR_P(expr) ? T : NIL;
		case tipo_magic_hash:
			return HASH_P(expr) ? T : NIL;
		case tipo_magic_any:
			return T;
		default:
			return (TIPO_P(TIPO_BUILT(t), expr)) ? T : NIL;
		}
	} else {
		RES *des = TIPO_DES(t);
		RES *op;

		if (!CONS_P(des))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("typep -- no es un tipo valido"));

		op = CAR(des);
		if ( op == fu_funes_symbol("union") ) {
			RES *p;
			/* or */
			for (p = CDR(des); CONS_P(p); p = CDR(p) )
				if (fu_typep(CAR(p), expr) != NIL) return T;
			return NIL;
		}
		else if ( op == fu_funes_symbol("intersection") ) {
			RES *p;
			/* and */
			for (p = CDR(des); CONS_P(p); p = CDR(p) )
				if (fu_typep(CAR(p), expr) == NIL) return NIL;
			return T;
		}
		else if ( op == fu_funes_symbol("complement") ) {
			/* not */
			if (!CONS_P(CDR(des)))
				fu_throw(fu_exception_symbol("wrong-type"),
					fu_str("typep -- tipo deforme"));
			return (fu_typep(CADR(des), expr) != NIL) ? NIL : T;
		}
		else if ( op == fu_funes_symbol("satisfies") ) {
			RES *p;
			if (!CONS_P(CDR(des)))
				fu_throw(fu_exception_symbol("wrong-type"),
					fu_str("typep -- tipo deforme"));
			p = CADR(des);
			return (fu_apply(p, fu_cons(expr, NIL)) != NIL) ? T : NIL;
		}
		else if ( op == fu_funes_symbol("enum") ) {
			return (fu_member(expr, CDR(des), UNDEF) != NIL) ? T : NIL;
		}
		else if ( op == fu_funes_symbol("container") ) {
			RES *p, *t;
			if (!CONS_P(CDR(des)))
				fu_throw(fu_exception_symbol("wrong-type"),
					fu_str("typep -- tipo deforme"));

			if (expr == NIL) {
				return T;
			} else if (CONS_P(expr))  {
				t = CADR(des);
				for (p = expr; p != NIL; p = CDR(p)) {
					if (fu_typep(t, CAR(p)) == NIL)
						return NIL;
				}
			} else if (VECTOR_NO_UNIFORME_P(expr)) {
				unsigned i, l;
				RES **tabla = VECTOR_TABLA(VAL_VECTOR(expr));

				t = CADR(des);
				l = VECTOR_LENGTH(VAL_VECTOR(expr));
				for (i = 0; i < l; i++) {
					if (fu_typep(t, tabla[i]) == NIL)
						return NIL;
				}
			} else {
				return NIL;
			}
			return T;
		}
		else if ( op == fu_funes_symbol("product") ) {
			RES *p, *q;

			if (expr == NIL) {
				return T;
			} else if (CONS_P(expr))  {
				for (p = expr, q = CDR(des); p != NIL && q != NIL; p = CDR(p), q = CDR(q)) {
					if (fu_typep(CAR(q), CAR(p)) == NIL)
						return NIL;
				}
				if (p != NIL || q != NIL) {
					return NIL;
				}
			} else if (VECTOR_NO_UNIFORME_P(expr)) {
				unsigned i, l;
				RES **tabla = VECTOR_TABLA(VAL_VECTOR(expr));

				l = VECTOR_LENGTH(VAL_VECTOR(expr));
				for (i = 0, q = CDR(des); i < l && q != NIL; i++, q = CDR(q)) {
					if (fu_typep(CAR(q), tabla[i]) == NIL)
						return NIL;
				}
				if (i < l || q != NIL) {
					return NIL;
				}
			} else {
				return NIL;
			}
			return T;
		}
	}

	return NIL;
}


RES *
fu_type_of(expr)
	RES *expr;
{
	if (INT_P(expr))
		return _tipo_magic_int;
	if (CHAR_P(expr))
		return _tipo_magic_char;
	if (CONS_P(expr))
		return _tipo_magic_cons;
	if ((expr == NIL))
		return _tipo_magic_null;
	if ((expr == T))
		return _tipo_magic_t;
	if ((expr == EOF_OBJECT))
		return _tipo_magic_eof;
	if ((expr == UNDEF))
		return _tipo_magic_undef;
	if (MACROP(expr))
		return _tipo_magic_macro;
	if (STR_P(expr))
		return _tipo_magic_str;
	if (HASH_P(expr))
		return _tipo_magic_hash;
 	
	switch (TIPO(expr)) {
	case tipo_sym:
		return _tipo_sym;
	case tipo_vector:
		return _tipo_vector;
	case tipo_proc:
		return _tipo_proc;
	case tipo_special_form:
		return _tipo_special_form;
	case tipo_closure:
		return _tipo_closure;
	case tipo_port:
		return _tipo_port;
	case tipo_reader:
		return _tipo_reader;
	case tipo_stype:
		return _tipo_stype;
	case tipo_struct:
		return _tipo_struct;
	case tipo_tipo:
		return _tipo_tipo;
	case tipo_float:
		return _tipo_float;
	case tipo_cont:
		return _tipo_cont;
	case tipo_package:
		return _tipo_package;
	}
	return UNDEF;
}

RES *
fu_check(tipo, expr, errstr, exc)
	RES *tipo, *expr, *exc, *errstr;
{
	if (exc == UNDEF)
		exc = fu_exception_symbol("wrong-type");
	if (errstr == UNDEF)
		errstr = fu_str("check -- la expresion no es del tipo esperado");
	if (fu_typep(tipo, expr) == NIL)
		fu_throw(exc, errstr);
	return expr;
}
