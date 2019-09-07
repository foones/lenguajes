#include <math.h>
#include "Fu.h"

__inline__ RES *
fu_int(i)
	fuint i;
{
	return (RES *) ((i << 2) | 1);
}

RES *
fu_int_sumar(resto)
	RES *resto;
{
	fuint r;
	RES *p;

	r = 0;
	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!INT_P(CAR(p))) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("int_sumar -- no son enteros"));
		}
		r = r + VAL_INT(CAR(p));
	}
	return fu_int(r);
}

RES *
fu_int_restar(resto)
	RES *resto;
{
	fuint r;
	RES *p;

	if ( !CONS_P(resto))
		r = 0;
	else {
		r = VAL_INT(CAR(resto));
 		if (NULL_P(CDR(resto)))
			r = -r;
		for (p = CDR(resto); CONS_P(p); p = CDR(p)) {
			if (!INT_P(CAR(p)))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("int_restar -- no son enteros"));
			r = r - VAL_INT(CAR(p));
		}
	}
	return fu_int(r);
}

RES *
fu_int_multiplicar(resto)
	RES *resto;
{
	fuint r;
	RES *p;

	r = 1;
	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!INT_P(CAR(p))) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("int_multiplicar -- no son enteros"));
		}
		r = r * VAL_INT(CAR(p));
	}
	return fu_int(r);
}

RES *
fu_int_mod(v,w)
	RES *v, *w;
{
	return fu_int(VAL_INT(v) % VAL_INT(w));
}

RES *
fu_int_remainder(v,w)
	RES *v, *w;
{
	fuint r;

	r = VAL_INT(v) % VAL_INT(w);
	r += (r < 0 ? VAL_INT(w) : 0);
	return fu_int(r);
}

RES *
fu_int_quotient(v,w)
	RES *v, *w;
{
	return fu_int(VAL_INT(v) / VAL_INT(w));
}

#define COMPARACION(NOMBRE, CMP) RES * NOMBRE(resto)\
	RES *resto;\
	{\
		fuint ant = -1, sig;\
		char primero = TRUE;\
		RES *p;\
		for (p = resto; CONS_P(p); p = CDR(p)) {\
			if (!INT_P(CAR(p)))\
				fu_throw(fu_exception_symbol("wrong-type-arg"),\
					fu_str("int_menor -- no son enteros"));\
			if ( (ant CMP (sig = VAL_INT(CAR(p)))) || primero )\
				ant = sig;\
			else\
				return NIL;\
			if (primero) primero = FALSE;\
		}\
		return T;\
	}

COMPARACION(fu_int_menor, <);
COMPARACION(fu_int_menor_o_igual, <=);
COMPARACION(fu_int_mayor, >);
COMPARACION(fu_int_mayor_o_igual, >=);
COMPARACION(fu_int_igual, ==);

RES *
fu_int_elevar(a, b)
	RES *a, *b;
{
	if (!INT_P(a) || !INT_P(b))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("int_elevar -- no son enteros"));

	return fu_int(pow(VAL_INT(a), VAL_INT(b)));
}
