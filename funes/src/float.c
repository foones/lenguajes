#include <math.h>
#include "Fu.h"

RES *
fu_float(f)
	double f;
{
	RES *p;
	double *f2;

	p = NEW(RES);
	TIPO(p) = tipo_float;
	f2 = NEW(double);
	*f2 = f;

	VAL(p) = (void *) f2;
	return p;
}

RES *
fu_float_sumar(resto)
	RES *resto;
{
	double r;
	RES *p;

	r = 0;
	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!FLOAT_P(CAR(p))) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_sumar -- no son flotantes"));
		}
		r = r + VAL_FLOAT(CAR(p));
	}
	return fu_float(r);
}

RES *
fu_float_restar(resto)
	RES *resto;
{
	double r;
	RES *p;

	if ( !CONS_P(resto))
		r = 0;
	else {
		if (!FLOAT_P(CAR(resto)))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_restar -- no son flotantes"));
		r = VAL_FLOAT(CAR(resto));
 		if (NULL_P(CDR(resto)))
			r = -r;
		for (p = CDR(resto); CONS_P(p); p = CDR(p)) {
			if (!FLOAT_P(CAR(p)))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("float_restar -- no son flotantes"));
			r = r - VAL_FLOAT(CAR(p));
		}
	}
	return fu_float(r);
}

RES *
fu_float_multiplicar(resto)
	RES *resto;
{
	double r;
	RES *p;

	r = 1;
	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!FLOAT_P(CAR(p))) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_multiplicar -- no son flotantes"));
		}
		r = r * VAL_FLOAT(CAR(p));
	}
	return fu_float(r);
}

RES *
fu_float_dividir(resto)
	RES *resto;
{
	double r;
	RES *p;

	if ( !CONS_P(resto))
		r = 1;
	else {
		if (!FLOAT_P(CAR(resto)))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_dividir -- no son flotantes"));
		r = VAL_FLOAT(CAR(resto));
 		if (NULL_P(CDR(resto))) {
			if (r == 0)
				fu_throw(fu_exception_symbol("zero-division"),
					fu_str("float_dividir -- division por cero"));
			r = 1/r;
		}
		for (p = CDR(resto); CONS_P(p); p = CDR(p)) {
			double divisor;

			if (!FLOAT_P(CAR(p)))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("float_dividir -- no son flotantes"));
			divisor = VAL_FLOAT(CAR(p));
			if (divisor == 0)
				fu_throw(fu_exception_symbol("zero-division"),
					fu_str("float_dividir -- division por cero"));
			r = r / divisor;
		}
	}
	return fu_float(r);
}

RES *
fu_float_menor(resto)
	RES *resto;
{
	double ant, sig;
	char primero = TRUE;
	RES *p;

	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!FLOAT_P(CAR(p)))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_menor -- no son flotantes"));
		if ( (ant < (sig = VAL_FLOAT(CAR(p)))) || primero )
			ant = sig;
		else
			return NIL;

		if (primero) primero = FALSE;
	}
	return T;
}

RES *
fu_float_mayor(resto)
	RES *resto;
{
	double ant, sig;
	char primero = TRUE;
	RES *p;

	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!FLOAT_P(CAR(p)))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_mayor -- no son flotantes"));
		if ( (ant > (sig = VAL_INT(CAR(p)))) || primero )
			ant = sig;
		else
			return NIL;

		if (primero) primero = FALSE;
	}
	return T;
}

RES *
fu_float_iguales(resto)
	RES *resto;
{
	double valor;
	char primero = TRUE;
	RES *p;

	for (p = resto; CONS_P(p); p = CDR(p)) {
		if (!FLOAT_P(CAR(p)))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("float_mayor -- no son flotantes"));
		if ( !primero && VAL_INT(CAR(p)) != valor)
			return NIL;

		if (primero) {
			valor = VAL_INT(CAR(p));
			primero = FALSE;
		}
	}
	return T;
}

RES *
fu_float_elevar(a, b)
	RES *a, *b;
{
	if (!FLOAT_P(a) || !FLOAT_P(b))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("float_elevar -- no son flotantes"));

	return fu_float(exp(log(VAL_FLOAT(a)) * VAL_FLOAT(b)));
}

RES *
fu_float_floor(a)
	RES *a;
{
	if (!FLOAT_P(a))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("float_floor -- no es un flotante"));

	return fu_int(floor(VAL_FLOAT(a)));
}

RES *
fu_float_ceil(a)
	RES *a;
{
	if (!FLOAT_P(a))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("float_ceil -- no es un flotante"));

	return fu_int(ceil(VAL_FLOAT(a)));
}

RES *
fu_int_to_float(a)
	RES *a;
{
	if (!INT_P(a))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("int_to_float -- no es un entero"));

	return fu_float(VAL_INT(a));
}

