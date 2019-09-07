#include <stdarg.h>
#include "Fu.h"

RES *
fu_cons(car,cdr)
	RES *car, *cdr;
{
	RES *v;
	CONS *c;

	/*v = NEW(RES);*/
	/*TIPO(v) = tipo_cons;*/
	c = NEW(CONS);
	/*VAL(v) = (void *) c;*/
	v = (RES *) (((fuint) c) | 2);
	c->car = car;
	c->cdr = cdr;
	return v;
}

RES *
fu_acons(a, b, c)
	RES *a, *b, *c;
{
	return fu_cons(fu_cons(a, b), c);
}

RES *
fu_consp(v)
	RES *v;
{
	return (CONS_P(v) ? T : NIL);
}

RES *
fu_car(v)
	RES *v;
{
	if (v == NIL)
		return NIL;
	else if (CONS_P(v))
		return CAR(v);
	else {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("car -- no es un cons"));
	}
	return NIL;
}

RES *
fu_cdr(v)
	RES *v;
{
	if (v == NIL)
		return NIL;
	else if (CONS_P(v))
		return CDR(v);
	else {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("cdr -- no es un cons"));
	}
	return NIL;
}

RES *
fu_car_set(v, w)
	RES *v, *w;
{
	if (CONS_P(v))
		return CAR(v) = w;
	else {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("car_set -- no es un cons"));
	}
	return NIL;
}

RES *
fu_cdr_set(v, w)
	RES *v, *w;
{
	if (CONS_P(v))
		return CDR(v) = w;
	else {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("cdr_set -- no es un cons"));
	}
	return NIL;
}

RES *
fu_list_length(l)
	RES *l;
{
	int i = 0;
	RES *p;

	for (p = l; CONS_P(p); p = CDR(p)) i++;
	return fu_int(i);
}

RES *
fu_list_to_vector(l)
	RES *l;
{
	int i;
	RES *lon = fu_list_length(l), *vec;
	RES **t;

	vec = fu_make_vector(lon, NIL);
	t = VECTOR_TABLA(VAL_VECTOR(vec));

	for (i = 0; i < VAL_INT(lon); i++, l = CDR(l))
		t[i] = CAR(l);
	return vec;
}

RES *
fu_make_list(char *formato, ...)
{
		char *c;
		va_list args;
		RES *q, *final;

		final = q = fu_cons(NIL, NIL);
		va_start(args, formato);
		for (c = formato; *c; c++) {
				if ( *c == 'x' ) {
						CDR(q) = fu_cons(va_arg(args, RES *), NIL);
						q = CDR(q);
				} else if ( *c == 'X' ) {
						CDR(q) = va_arg(args, RES *);
						q = CDR(q);
				}
		}
		va_end(args);
		return CDR(final);
}

RES *
fu_mklist(longitud, inicial)
	RES *longitud, *inicial;
{
	unsigned i;
	RES *p = NIL;

	if (!INT_P(longitud))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("mklist -- la longitud no es entera"));
	if (inicial == UNDEF) inicial = NIL;

	for (i = VAL_INT(longitud); i > 0; i--)
		p = fu_cons(inicial, p);
	return p;
}

RES *
fu_list(args)
		RES *args;
{
		return args;
	/*	RES *p, *q, *final;

		final = q = fu_cons(NIL, NIL);
		for (p = args; CONS_P(p); p = CDR(p)) {
			CDR(q) = fu_cons(CAR(p), NIL);
			q = CDR(q);
		}
		return CDR(final);*/
}

RES *
fu_list_1(args)
	RES *args;
{
	RES *p, *q, *final;

	final = q = fu_cons(NIL, NIL);
	for (p = args; CONS_P(p); p = CDR(p)) {
		if (CONS_P(CDR(p)))
			CDR(q) = fu_cons(CAR(p), NIL);
		else
			CDR(q) = CAR(p);
		q = CDR(q);
	}
	return CDR(final);
}

RES *
fu_nth(v, idx)
	RES *v, *idx;
{
	unsigned l, i = 0;
	RES *p;

	if ( !CONS_P(v) && v != NIL )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("nth -- el argumento no es una lista"));

	if ( !INT_P(idx) )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("nth -- el subindice no es entero"));
	l = VAL_INT(idx);

	for (p = v; CONS_P(p); p = CDR(p), i++)
		if ( i == l )
			return CAR(p);

	fu_throw(fu_exception_symbol("out-of-range"),
		fu_str("nth -- subindice fuera del rango"));

	return NIL;
}

RES *
fu_nth_set(v, idx, valor)
	RES *v, *idx, *valor;
{
	unsigned l, i = 0;
	RES *p;

	if ( !CONS_P(v) && v != NIL )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("nth_set -- el argumento no es una lista"));

	if ( !INT_P(idx) )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("nth_set -- el subindice no es entero"));
	l = VAL_INT(idx);

	for (p = v; CONS_P(p); p = CDR(p), i++)
		if ( i == l )
			return CAR(p) = valor;

	fu_throw(fu_exception_symbol("out-of-range"),
		fu_str("nth_set -- subindice fuera del rango"));

	return NIL;
}

RES *
fu_ntl(v, idx)
	RES *v, *idx;
{
	unsigned l, i = 0;
	RES *p;

	if ( !CONS_P(v) && v != NIL )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("ntl -- el argumento no es una lista"));

	if ( !INT_P(idx) )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("ntl -- el subindice no es entero"));
	l = VAL_INT(idx);

	for (p = v; CONS_P(p); p = CDR(p), i++)
		if ( i == l )
			return p;

	if (i == l)
		return p;

	fu_throw(fu_exception_symbol("out-of-range"),
		fu_str("ntl -- subindice fuera del rango"));

	return NIL;
}

RES *
fu_ntl_set(v, idx, valor)
	RES *v, *idx, *valor;
{
	unsigned l, i = 0;
	RES *p;

	if ( !CONS_P(v) && v != NIL )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("ntl -- el argumento no es una lista"));

	if ( !INT_P(idx) )
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("ntl -- el subindice no es entero"));
	l = VAL_INT(idx);

	for (p = v; CONS_P(p); p = CDR(p), i++)
		if (i == l)
			return CDR(p) = valor;

	fu_throw(fu_exception_symbol("out-of-range"),
		fu_str("ntl -- subindice fuera del rango"));

	return NIL;
}
